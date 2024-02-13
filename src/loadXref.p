/*-----------------------------------------------------------------------
  File : loadXref.p
  Desc : Import one Xref XML file into the Xref Database
  ----------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pcXrefFolder    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcPathToStrip   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcSkipFolders   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcRelationTypes AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcObjectTypes   AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttFile NO-UNDO
  FIELD cFullName AS CHARACTER.

{xrefd0005.i}

DEFINE VARIABLE gcProgramName AS CHARACTER NO-UNDO. /* name of the program being evaluated */

FUNCTION getFileHash RETURNS CHARACTER(pcFile AS CHARACTER) FORWARD.
FUNCTION stripPathNames RETURNS CHARACTER
  ( pcFileName AS CHARACTER ) FORWARD.

/* ***************************  Main Block  *************************** */

PAUSE 0 BEFORE-HIDE.

IF pcRelationTypes = '' THEN pcRelationTypes = '*'.
IF pcObjectTypes = '' THEN pcObjectTypes = '*'.

RUN getSkipFolders(pcSkipFolders, OUTPUT pcSkipFolders).
RUN processFiles.


/* **********************  Internal Procedures  *********************** */
PROCEDURE getSkipFolders:
  /* Build can-do-able list of folders 
  */
  DEFINE INPUT  PARAMETER pcListIn  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pcListOut AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cSkipFolders AS CHARACTER NO-UNDO.

  DO i = 1 TO NUM-ENTRIES(pcListIn):
    pcListOut = pcListOut + '*\' + ENTRY(i,pcListIn) + '\*,'.
  END.
END PROCEDURE. /* getSkipFolders */


PROCEDURE processFiles:
  /* Process all XML files one by one
  */
  DEFINE VARIABLE iTodo AS INTEGER NO-UNDO.
  DEFINE VARIABLE iDone AS INTEGER NO-UNDO.
  DEFINE BUFFER bFile FOR ttFile.

  RUN readFolder(pcXrefFolder).
  
  FOR EACH bFile: 
    iTodo = iTodo + 1.
  END. 
  
  FOR EACH bFile:
    iDone = iDone + 1.
    PUBLISH "xref-log" (SUBSTITUTE('&1 Loading &2 of &3: &4', STRING(TIME,'hh:mm:ss'), iDone, iTodo, bFile.cFullName)).
    PROCESS EVENTS. 

    RUN loadXref(bFile.cFullName).

    PROCESS EVENTS.
  END. /* FOR EACH bFile */
END PROCEDURE. /* processFiles */


PROCEDURE readFolder:
  /* Read files in folder and process subfolders 
  */
  DEFINE INPUT PARAMETER pcFolder AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE cFile AS CHARACTER NO-UNDO EXTENT 3.
  DEFINE BUFFER bFile FOR ttFile.
  
  INPUT FROM OS-DIR(pcFolder).
  REPEAT:       
    IMPORT cFile.
    IF cFile[1] BEGINS '.' THEN NEXT.
    IF cFile[3] BEGINS 'D' THEN RUN readFolder(cFile[2]).
    IF CAN-DO(pcSkipFolders, cFile[2]) THEN NEXT. 
    IF cFile[3] BEGINS 'F' THEN
    DO:
      CREATE bFile.
      ASSIGN bFile.cFullName = cFile[2].
    END. /* regular file */
  END. /* repeat */
  INPUT CLOSE. 
END PROCEDURE. /* readFolder */ 


PROCEDURE loadXref:
  /* Load the Given Xref Xml File into the Xref Database
  */
  DEFINE INPUT PARAMETER pcFileName AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cHandler       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dtFileModified AS DATETIME  NO-UNDO.
  DEFINE VARIABLE cFileMD5       AS CHARACTER NO-UNDO.
                          
  DEFINE BUFFER bXmlFile   FOR xref_XmlFile.
  DEFINE BUFFER bSource    FOR ttSource.
  DEFINE BUFFER bReference FOR ttReference.

  PUBLISH 'debugInfo'(SUBSTITUTE("Reading &1", pcFileName)).
  PROCESS EVENTS. 

  /* Check the XML file */
  FILE-INFO:FILE-NAME = pcFileName.
  IF FILE-INFO:FULL-PATHNAME = ? THEN
  DO:
    PUBLISH 'debugInfo'( SUBSTITUTE("Xref file &1 not found", pcFileName )).
    PROCESS EVENTS. 
    RETURN.
  END.

  /* Register XML file with last modified time */
  DO TRANSACTION:
    FIND bXmlFile EXCLUSIVE-LOCK
      WHERE bXmlFile.cFullName = pcFileName
            NO-ERROR.

    IF NOT AVAILABLE bXmlFile THEN
    DO:
      CREATE bXmlFile.
      ASSIGN bXmlFile.cFullName = pcFileName.
    END.

    /* Check if file on disk has been changed since the last time */
    dtFileModified = DATETIME(FILE-INFO:FILE-MOD-DATE, FILE-INFO:FILE-MOD-TIME * 1000).
    IF bXmlFile.dtLastModified >= dtFileModified THEN RETURN.

    /* Or if the contents have not really changed */
    cFileMD5 = getFileHash(FILE-INFO:FULL-PATHNAME).
    IF bXmlFile.cHashValue = cFileMD5 THEN RETURN. 

    ASSIGN 
      bXmlFile.dtLastModified = dtFileModified
      bXmlFile.cHashValue     = cFileMD5.
  END.

  /* Load Dataset */
  DATASET dsCrossRef:READ-XML ("FILE",pcFileName,"EMPTY",?,?) NO-ERROR.
 
  /* Correct file names in ttSource:
   * - strip excess path names
   * - set source type
   */
  FOR EACH bSource:
    bSource.File-name = REPLACE(bSource.File-name,"~\","~/").
    IF bSource.File-name BEGINS './' THEN bSource.File-name = SUBSTRING(bSource.File-name ,3).

    /* Limit nr of paths */
    bSource.FILE-NAME = stripPathNames(bSource.FILE-NAME).

    /* Set objecttype */
    bSource.cObjectType = (IF bSource.File-name MATCHES "*.i" THEN "Include" ELSE "Program").
  END. /* for each bSource */

  /* Clean db and set vars */
  RUN initXref.

  /* Process the Xref XML */
  DO TRANSACTION:
    FOR EACH bReference
      WHERE CAN-DO(pcRelationTypes, bReference.Reference-type):

      /* Compose handler name and run it if it is defined */
      cHandler = 'Process_' + bReference.Reference-type.
      IF LOOKUP(cHandler, THIS-PROCEDURE:INTERNAL-ENTRIES) > 0 THEN
        RUN VALUE(cHandler) (BUFFER bReference).
      ELSE
        PUBLISH 'NewReferenceType'(bReference.Reference-type).

      PROCESS EVENTS.
    END. /* For Each Reference */
  END. /* transaction */
END PROCEDURE. /* LoadXref */


PROCEDURE initXref:
  /* Init load: delete old records and set program name
   */
  DEFINE BUFFER bReference FOR ttReference.
  DEFINE BUFFER bRelation FOR xref_Relation.

  /* Find reference to compilation unit */
  FIND FIRST bReference WHERE bReference.Reference-type = 'compile' NO-ERROR.
  IF NOT AVAILABLE bReference THEN
  DO:
    PUBLISH 'debugInfo'("No COMPILE reference found in xref").
    PROCESS EVENTS. 
    RETURN.
  END.

  /* Strip all directory structures */
  gcProgramName = stripPathNames(bReference.Object-identifier).

  /* Delete all relations for a certain program. This procedure does not
   * clean up objects which remain unused after deleting a relation and
   * relations like DB<->FILE, FILE<->FIELD, FILE<->INDEX since these
   * are not tied to a specific program. 
   */
  FOR EACH bRelation EXCLUSIVE-LOCK
    WHERE bRelation.cParentType = 'Program'
      AND bRelation.cParentName = gcProgramName:
    DELETE bRelation.
  END. /* FOR EACH bRelation */

END PROCEDURE. /* initXref */


PROCEDURE addRelation:
  /* Create a relation in the database between two objects.
  */
  DEFINE INPUT PARAMETER pcParentType   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcParentName   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcChildType    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcChildName    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcRelationType AS CHARACTER NO-UNDO.

  DEFINE BUFFER bRelation     FOR xref_Relation.
  DEFINE BUFFER bRelationType FOR xref_RelationType.
  DEFINE BUFFER bObject       FOR xref_Object.
  DEFINE BUFFER bObjectType   FOR xref_ObjectType.

  /* Ignore incomplete relations */
  IF   pcParentType   = ''
    OR pcParentName   = ''
    OR pcChildType    = ''
    OR pcChildName    = ''
    OR pcRelationType = '' THEN RETURN. 

  /* Ignore blocked objects/relationtypes */
  IF   NOT CAN-DO(pcObjectTypes, pcParentType) 
    OR NOT CAN-DO(pcObjectTypes, pcChildType) 
    OR NOT CAN-DO(pcRelationTypes, pcRelationType) THEN RETURN.

  /* If relation already exists, go back */
  IF CAN-FIND(bRelation
    WHERE bRelation.cRelationType = pcRelationType
      AND bRelation.cParentType   = pcParentType
      AND bRelation.cParentName   = pcParentName
      AND bRelation.cChildType    = pcChildType
      AND bRelation.cChildName    = pcChildName) THEN RETURN.

  CREATE bRelation.
  ASSIGN
    bRelation.cRelationType = pcRelationType
    bRelation.cParentType   = pcParentType
    bRelation.cParentName   = pcParentName
    bRelation.cChildType    = pcChildType
    bRelation.cChildName    = pcChildName
    .

  /* Below blocks of finding/creating objects is inline and 
  ** not as a separate function, since this is faster.
  */
  /* Add parent object if needed */
  IF NOT CAN-FIND(bObject
    WHERE bObject.cObjectType  = pcParentType
      AND bObject.cObjectName  = pcParentName) THEN
  DO:
    CREATE bObject.
    ASSIGN bObject.cObjectType  = pcParentType
           bObject.cObjectName  = pcParentName.

    IF NOT CAN-FIND(bObjectType WHERE bObjectType.cObjectType = pcParentType) THEN
    DO:
      CREATE bObjectType.
      ASSIGN bObjectType.cObjectType = pcParentType.
    END.
  END.

  /* Add child object if needed */
  IF NOT CAN-FIND(bObject
    WHERE bObject.cObjectType  = pcChildType
      AND bObject.cObjectName  = pcChildName) THEN
  DO:
    CREATE bObject.
    ASSIGN bObject.cObjectType  = pcChildType
           bObject.cObjectName  = pcChildName.

    IF NOT CAN-FIND(bObjectType WHERE bObjectType.cObjectType = pcChildType) THEN
    DO:
      CREATE bObjectType.
      ASSIGN bObjectType.cObjectType = pcChildType.
    END.
  END.

  /* Add relationtype */
  IF NOT CAN-FIND(bRelationType WHERE bRelationType.cRelationType = pcRelationType) THEN
  DO:
    CREATE bRelationType.
    ASSIGN bRelationType.cRelationType = pcRelationType.
  END.

END PROCEDURE. /* addRelation */


PROCEDURE addSchemaRelations:
  /* Add extra relations for meta schema
   */
  DEFINE INPUT PARAMETER pcDb    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcFile  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcField AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcIndex AS CHARACTER NO-UNDO.

  IF pcDb <> ''   AND pcFile <> ''  THEN RUN addRelation('Db'  , pcDb  , 'File' , pcFile , 'FILE' ).
  IF pcFile <> '' AND pcField <> '' THEN RUN addRelation('File', pcFile, 'Field', pcField, 'FIELD').
  IF pcFile <> '' AND pcIndex <> '' THEN RUN addRelation('File', pcFile, 'Index', pcIndex, 'INDEX').

  IF pcDb    <> '' THEN RUN addRelation('Program', gcProgramName, 'Db'   , pcDb   , 'PROG-DB').
  IF pcFile  <> '' THEN RUN addRelation('Program', gcProgramName, 'File' , pcFile , 'PROG-FILE').
  IF pcField <> '' THEN RUN addRelation('Program', gcProgramName, 'Field', pcField, 'PROG-FIELD').
  IF pcIndex <> '' THEN RUN addRelation('Program', gcProgramName, 'Index', pcIndex, 'PROG-INDEX').

END PROCEDURE. /* addSchemaRelations */


PROCEDURE Process_ACCESS:
  /* Process ACCESS
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.
  DEFINE VARIABLE cDb    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cField AS CHARACTER NO-UNDO.
  /*<Reference Reference-type="ACCESS" Object-identifier="arti-nr">
      <Object-context>ttPlti</Object-context>
      <Temp-ref>T</Temp-ref>
    </Reference>
  */  
  IF bReference.Object-context = "SHARED" THEN
  DO:
    RUN addRelation('Program', gcProgramName, 'Variable', bReference.Object-identifier, 'ACCESS').
  END.

  ELSE
  IF bReference.Temp-ref = "T" THEN /* TEMPTABLE */
  DO: 
    /*<Reference Reference-type="ACCESS" Object-identifier="">
        <Object-context>bttEzorl</Object-context>
        <Temp-ref>T</Temp-ref>
      </Reference> 
    */
    IF bReference.Object-identifier = '' THEN 
    DO:
      RUN addRelation('Program'  , gcProgramName, 'TempTable', bReference.Object-context, 'ACCESS').
      RUN addRelation('Program'  , gcProgramName, 'TempTable', bReference.Object-context, 'TEMPTABLE').
    END.
    ELSE 
    DO:
      RUN addRelation('Program'  , gcProgramName, 'TT-Field', bReference.Object-context + '.' + bReference.Object-identifier, 'ACCESS').
      RUN addRelation('Program'  , gcProgramName, 'TempTable', bReference.Object-context, 'TEMPTABLE').
      RUN addRelation('TempTable', bReference.Object-context, 'TT-Field', bReference.Object-context + '.' + bReference.Object-identifier, 'TT-Field').
    END.
  END.

  ELSE
  IF NUM-ENTRIES(bReference.Object-context,'.') = 2 THEN
  DO:
    cDb    = ENTRY(1,bReference.Object-context,'.').
    cFile  = bReference.Object-context.
    cField = SUBSTITUTE('&1.&2', bReference.Object-context, bReference.Object-identifier).

    RUN addRelation('Program', gcProgramName, 'Field', cField, "ACCESS").
    RUN addSchemaRelations(cDb, cFile, cField, '').
  END.

END PROCEDURE. /* Process_Access */


PROCEDURE Process_CPSTREAM:
  /* Process CPINTERNAL
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.
  /*<Reference Reference-type="CPSTREAM" Object-identifier="1252">
      <Object-context/>
      <Temp-ref/>
    </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'CodePage', bReference.Object-identifier, 'CPSTREAM').

END PROCEDURE. /* Process_CPSTREAM */


PROCEDURE Process_CPINTERNAL:
  /* Process CPINTERNAL
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.
  /*<Reference Reference-type="CPINTERNAL" Object-identifier="1252">
      <Object-context/>
      <Temp-ref/>
    </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'CodePage', bReference.Object-identifier, 'CPINTERNAL').

END PROCEDURE. /* Process_CPINTERNAL */


PROCEDURE Process_CREATE:
  /* Process CREATE
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.
  DEFINE VARIABLE cDb    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
  DEFINE BUFFER bSource FOR ttSource. 
  
  /*<Reference Reference-type="CREATE" Object-identifier="ttPlti">
      <Object-context/>
      <Temp-ref>T</Temp-ref>
    </Reference>
  */
  IF bReference.Temp-ref = "T" THEN
  DO: /* CREATE <temptable> TEMPTABLE */
    RUN addRelation('Program', gcProgramName, 'TempTable', bReference.Object-identifier, 'CREATE').
  END.

  ELSE
  IF NUM-ENTRIES(bReference.Object-identifier,'.') = 2 THEN
  DO: /* CREATE <db>.<file> */
    cDb    = ENTRY(1,bReference.Object-identifier,'.').
    cFile  = bReference.Object-identifier.

    FIND bSource
      WHERE bSource.Source-guid = bReference.Source-guid
        AND bSource.File-num    = bReference.File-num NO-ERROR.

    IF AVAILABLE bSource THEN 
    DO:
      RUN addRelation(bSource.cObjectType, bSource.File-name, 'File' , cFile , "CREATE").
      RUN addSchemaRelations(cDb, cFile, '', '').
    END.
  END.

END PROCEDURE. /* Process_Create */


PROCEDURE Process_DELETE:
  /* Process DELETE
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.
  DEFINE VARIABLE cDb    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.

  /*<Reference Reference-type="DELETE" Object-identifier="pkf.aadr">
      <Object-context/>
      <Temp-ref/>
    </Reference>
  */
  IF bReference.Temp-ref = "T" THEN
  DO: /* DELETE <temptable> TEMPTABLE */
    RUN addRelation('Program', gcProgramName, 'TempTable', bReference.Object-identifier, "DELETE").
  END.

  ELSE
  IF NUM-ENTRIES(bReference.Object-identifier,'.') = 2 THEN
  DO: /* DELETE <db>.<file> */
    cDb    = ENTRY(1,bReference.Object-identifier,'.').
    cFile  = bReference.Object-identifier.

    RUN addRelation('Program', gcProgramName, 'File' , cFile , "DELETE").
    RUN addSchemaRelations(cDb, cFile, '', '').
  END.

END PROCEDURE. /* Process_Delete */


PROCEDURE Process_DLL-ENTRY:
  /* Process DLL-ENTRY
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*<Reference Reference-type="DLL-ENTRY" Object-identifier="RegOpenKeyA">
      <Object-context/>
      <Temp-ref/>
    </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'DLL-Entry', bReference.Object-identifier, 'DLL-ENTRY').

END PROCEDURE. /* Process_Function */


PROCEDURE Process_EXTERN:
  /* Process EXTERN
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*    <Reference Reference-type="EXTERN" Object-identifier="setAdmNrFinance">
      <Source-guid>4I6QnhimMobpEZJFrhAllw</Source-guid>
      <Object-context/>
      <Detail>INTEGER</Detail>
    </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'Function', bReference.Object-identifier, 'EXTERN').

END PROCEDURE. /* Process_EXTERN */


PROCEDURE Process_FUNCTION:
  /* Process FUNCTION
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*<Reference Reference-type="FUNCTION" Object-identifier="newstate">
      <Object-context/>
      <Temp-ref/>
    </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'Function', bReference.Object-identifier, 'FUNCTION').

END PROCEDURE. /* Process_Function */


PROCEDURE Process_GLOBAL-VARIABLE:
  /* Process GLOBAL-VARIABLE
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /* <Reference Reference-type="GLOBAL-VARIABLE" Object-identifier="adm-broker-hdl">
      <Object-context/>
      <Temp-ref/>
    </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'Variable', bReference.Object-identifier, 'GLOBAL-VARIABLE').

END PROCEDURE. /* Process_GLOBAL-VARIABLE */


PROCEDURE Process_INCLUDE:
  /* Process INCLUDE
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.
  DEFINE VARIABLE cInclude AS CHARACTER NO-UNDO.

  /*<Reference Reference-type="INCLUDE" Object-identifier="src/adm/method/browser.i">
      <Object-context/>
    </Reference>
  */
  cInclude = stripPathNames(bReference.Object-identifier).

  /* Strip arguments/line feeds from the include name */
  cInclude = ENTRY(1,cInclude,'~n').
  cInclude = ENTRY(1,cInclude,'&').
  cInclude = REPLACE(cInclude,'"', '').
  cInclude = TRIM(cInclude).

  IF cInclude = '"'
    OR cInclude = ''
    OR cInclude = '""'
    OR cInclude MATCHES '&*'
    OR cInclude MATCHES "<*" THEN
  DO:
    PUBLISH 'debugInfo'(SUBSTITUTE("Strange include &1, refstring=&2", cInclude,TRIM(bReference.Object-identifier)) ).
    PROCESS EVENTS. 
  END.

  RUN addRelation('Program', gcProgramName, 'Include', cInclude, 'INCLUDE').

END PROCEDURE. /* Process_Include */


PROCEDURE Process_NEW-SHR-DATASET:
  /* Process SHR-DATASET
  */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*<Reference Reference-type="NEW-SHR-DATASET" Object-identifier="dsCust">
    <Object-context/>
    <Temp-ref/>
  </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'DataSet', bReference.Object-identifier, 'NEW-SHR-DATASET').

END PROCEDURE. /* Process_NEW-SHR-DATASET */


PROCEDURE Process_NEW-SHR-FRAME:
  /* Process NEW-SHR-FRAME
  */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*<Reference Reference-type="NEW-SHR-FRAME" Object-identifier="fCust">
    <Object-context/>
    <Temp-ref/>
  </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'Frame', bReference.Object-identifier, 'NEW-SHR-FRAME').

END PROCEDURE. /* Process_NEW-SHR-FRAME */


PROCEDURE Process_NEW-SHR-TEMPTABLE:
  /* Process NEW-SHR-TEMPTABLE
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*<Reference Reference-type="NEW-SHR-TEMPTABLE" Object-identifier="tmp-prod">
      <Object-context/>
    </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'TempTable', bReference.Object-identifier, 'NEW-SHR-TEMPTABLE').

END PROCEDURE. /* NEW-SHR-TEMPTABLE */


PROCEDURE Process_NEW-SHR-VARIABLE:
  /* Process NEW-SHR-VARIABLE
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*  <Reference Reference-type="NEW-SHR-VARIABLE" Object-identifier="h_PDFinc">
      <Object-context/>
    </Reference> 
  */
  RUN addRelation('Program', gcProgramName, 'Variable', bReference.Object-identifier, 'NEW-SHR-VARIABLE').

END PROCEDURE. /* NEW-SHR-VARIABLE */


PROCEDURE Process_NEW-SHR-WORKFILE:
  /* Process NEW-SHR-WORKFILE
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*  <Reference Reference-type="NEW-SHR-WORKFILE" Object-identifier="wOrders">
      <Object-context/>
    </Reference> 
  */
  RUN addRelation('Program', gcProgramName, 'WorkFile', bReference.Object-identifier, 'NEW-SHR-WORKFILE').

END PROCEDURE. /* NEW-SHR-VARIABLE */


PROCEDURE Process_PRIVATE-FUNCTION:
  /* Process PRIVATE-FUNCTION
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*<Reference Reference-type="PRIVATE-FUNCTION" Object-identifier="newstate">
      <Object-context/>
      <Temp-ref/>
    </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'Function', bReference.Object-identifier, 'PRIVATE-FUNCTION').

END PROCEDURE. /* Process_PRIVATE-FUNCTION */


PROCEDURE Process_PRIVATE-PROCEDURE:
  /* Process PRIVATE-PROCEDURE
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*<Reference Reference-type="PRIVATE-PROCEDURE" Object-identifier="adm-open-query-cases">
      <Object-context/>
    </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'Procedure', bReference.Object-identifier, 'PRIVATE-PROCEDURE').

END PROCEDURE. /* Process_PRIVATE-PROCEDURE */


PROCEDURE Process_PROCEDURE:
  /* Process PROCEDURE
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*<Reference Reference-type="PROCEDURE" Object-identifier="adm-open-query-cases">
      <Object-context/>
    </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'Procedure', bReference.Object-identifier, 'PROCEDURE').

END PROCEDURE. /* Process_PROCEDURE */


PROCEDURE Process_PUBLISH:
  /* Process PUBLISH 
  */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.
  
  /*<Reference Reference-type="PUBLISH" Object-identifier="getCalcPeildatum ">
      <Object-context/>
    </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'Procedure', bReference.Object-identifier, 'PUBLISH').

END PROCEDURE. /* Process_PUBLISH */


PROCEDURE Process_REFERENCE:
  /* Process REFERENCE
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.
  DEFINE VARIABLE cDb    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cField AS CHARACTER NO-UNDO.


  /*<Reference Reference-type="REFERENCE" Object-identifier="ttPlti">
      <Object-context/>
    </Reference>
  */
  IF bReference.Temp-ref = "T" THEN
  DO: /* REFERENCE <temptable> TEMPTABLE */
    RUN addRelation('Program', gcProgramName, 'TempTable', bReference.Object-identifier, 'REFERENCE').
  END. /* IF bReference.Temp-ref = "T" THEN */

  ELSE
  IF NUM-ENTRIES(bReference.Object-identifier,'.') = 2 THEN /* table name */
  DO: /* REFERENCE <db>.<file> [<field>] */

    cDb    = ENTRY(1,bReference.Object-identifier,'.').
    cFile  = bReference.Object-identifier.

    /* Object-context might contain field name */
    IF bReference.Object-context = '' THEN
    DO:
      RUN addRelation('Program', gcProgramName, 'File', cFile, "REFERENCE").
    END.

    ELSE
    DO:
      cField = SUBSTITUTE('&1.&2', cFile, bReference.Object-context).
      RUN addRelation('Program', gcProgramName, 'Field', cField, "REFERENCE").
    END. /* Object-context = '' */

    RUN addSchemaRelations(cDb, cFile, cField, '').
  END.

END PROCEDURE. /* Process_REFERENCE */


PROCEDURE Process_RUN:
  /* Process RUN
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.
  DEFINE VARIABLE cProgram AS CHARACTER NO-UNDO.
  DEFINE BUFFER bReference2 FOR ttReference.

  /* <Reference Reference-type="RUN" Object-identifier="GET-ATTRIBUTE">
      <Object-context/>
    </Reference>
  */

  /* find out if we are calling an internal or external procedure by checking
  ** the extention of the called program. If .p / .w / .r then probably an
  ** external one, otherwise probably an internal one.
  */
  IF bReference.Object-identifier MATCHES "*~~.p" OR
    bReference.Object-identifier MATCHES "*~~.w" OR
    bReference.Object-identifier MATCHES "*~~.r" THEN
    RUN addRelation('Program', gcProgramName, 'Program', stripPathNames(bReference.Object-identifier), 'RUN').
  ELSE
    RUN addRelation('Program', gcProgramName, 'Procedure', bReference.Object-identifier, 'RUN-IP').

  /* Intercept startup of smart objects */
  IF bReference.Object-identifier = "init-object" /* ADM1 */
  OR bReference.Object-identifier = "constructObject" /* ADM2 */ THEN 
  DO:
    /* Find corresponding STRING type that contains the name of the smart object */
    FOR EACH bReference2
      WHERE bReference2.Reference-type = 'STRING'
        AND bReference2.Source-guid    = bReference.Source-guid
        AND bReference2.file-num       = bReference.file-num
        AND bReference2.line-num       = bReference.line-num:

      /* ADM-2 might add chr(3) + 'db-aware' to program name */
      cProgram = ENTRY(1,bReference2.Object-identifier,CHR(3)).

      IF   cProgram MATCHES "*~~.p" 
        OR cProgram MATCHES "*~~.w" 
        OR cProgram MATCHES "*~~.r" THEN
        RUN addRelation('Program', gcProgramName, 'Program', stripPathNames(cProgram), 'RUN').
    END.
  END.

END PROCEDURE. /* Process_RUN */


PROCEDURE Process_SEARCH:
  /* Process SEARCH
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.
  DEFINE VARIABLE cDb    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cIndex AS CHARACTER NO-UNDO.

  /*<Reference Reference-type="SEARCH" Object-identifier="ttPlti">
      <Object-context>i-plti</Object-context>
    </Reference>
  */
  IF bReference.Temp-ref = 'T' THEN
  DO: /* SEARCH <temptable> <index> TEMPTABLE */
    RUN addRelation('Program', gcProgramName, 'TempTable', bReference.Object-identifier, 'TEMPTABLE').
    RUN addRelation('Program', gcProgramName, 'TT-Index', bReference.Object-identifier + '.' + bReference.Object-context, 'SEARCH').
    RUN addRelation('TempTable', bReference.Object-identifier, 'TT-Index', bReference.Object-identifier + '.' + bReference.Object-context, 'TT-Index').
  END.

  ELSE
  IF NUM-ENTRIES(bReference.Object-identifier,'.') = 2 THEN
  DO: /* SEARCH <db>.<file> <index> [WHOLE-INDEX] */

    cDb    = ENTRY(1,bReference.Object-identifier,'.').
    cFile  = bReference.Object-identifier.
    cIndex = SUBSTITUTE('&1.&2', bReference.Object-identifier, bReference.Object-context).

    RUN addRelation('Program', gcProgramName, 'Index', cIndex, "SEARCH").
    RUN addSchemaRelations(cDb, cFile, '', cIndex).

    /* create relation for prog<->index if WHOLE-INDEX*/
    IF bReference.Detail = 'WHOLE-INDEX' THEN
      RUN addRelation('Program', gcProgramName, 'Index', cIndex, 'WHOLE-INDEX').
  END.

END PROCEDURE. /* Process_SEARCH */


PROCEDURE Process_SHR-DATASET:
  /* Process SHR-DATASET
  */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*<Reference Reference-type="SHR-DATASET" Object-identifier="dsCust">
    <Object-context/>
    <Temp-ref/>
  </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'DataSet', bReference.Object-identifier, 'SHR-DATASET').

END PROCEDURE. /* Process_SHR-DATASET */


PROCEDURE Process_SHR-FRAME:
  /* Process SHR-FRAME
  */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*<Reference Reference-type="SHR-FRAME" Object-identifier="fCust">
    <Object-context/>
    <Temp-ref/>
  </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'Frame', bReference.Object-identifier, 'SHR-FRAME').

END PROCEDURE. /* Process_SHR-FRAME */


PROCEDURE Process_SHR-TEMPTABLE:
  /* Process SHR-TEMPTABLE
  */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*<Reference Reference-type="SHR-TEMPTABLE" Object-identifier="tmp-pop">
    <Object-context/>
    <Temp-ref/>
  </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'TempTable', bReference.Object-identifier, 'SHR-TEMPTABLE').

END PROCEDURE. /* Process_SHR-TEMPTABLE */


PROCEDURE Process_SHR-WORKFILE:
  /* Process SHR-WORKFILE 
  */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /*<Reference Reference-type="SHR-WORKFILE" Object-identifier="tmp-pop">
    <Object-context/>
    <Temp-ref/>
  </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'WorkFile', bReference.Object-identifier, 'SHR-WORKFILE').

END PROCEDURE. /* Process_SHR-TEMPTABLE */


PROCEDURE Process_SUBSCRIBE:
  /* Process SUBSCRIBE 
  */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.

  /* <Reference Reference-type="SUBSCRIBE" Object-identifier="write-log ">
      <Object-context/>
      <Temp-ref/>
    </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'Procedure', bReference.Object-identifier, 'SUBSCRIBE').

END PROCEDURE. /* Process_SUBSCRIBE */

    
PROCEDURE Process_UNSUBSCRIBE:
  /* Process UNSUBSCRIBE 
  */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.
  /* <Reference Reference-type="UNSUBSCRIBE" Object-identifier="write-log ">
      <Object-context/>
      <Temp-ref/>
    </Reference>
  */
  RUN addRelation('Program', gcProgramName, 'Procedure', bReference.Object-identifier, 'UNSUBSCRIBE').

END PROCEDURE. /* Process_UNSUBSCRIBE */


PROCEDURE Process_UPDATE:
  /* Process UPDATE
   */
  DEFINE PARAMETER BUFFER bReference FOR ttReference.
  DEFINE VARIABLE cDb    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cField AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cIndex AS CHARACTER NO-UNDO.

  /*<Reference Reference-type="UPDATE" Object-identifier="adm-broker-hdl">
      <Object-context>SHARED</Object-context>
      <Temp-ref/>
    </Reference>
  */
  IF bReference.Object-identifier = 'SHARED' THEN
  DO: /* UPDATE SHARED <var> */
    RUN addRelation('Program', gcProgramName, 'Variable', bReference.Object-context, 'UPDATE').
  END.

  ELSE
  IF bReference.Temp-ref = "T" THEN
  DO: /* UPDATE <temptable> <field> TEMPTABLE */
    IF bReference.Object-identifier = '' THEN 
    DO:
      RUN addRelation('Program'  , gcProgramName, 'TempTable', bReference.Object-context, 'ACCESS').
      RUN addRelation('Program'  , gcProgramName, 'TempTable', bReference.Object-context, 'TEMPTABLE').
    END.
    ELSE 
    DO:
      RUN addRelation('Program'  , gcProgramName, 'TT-Field', bReference.Object-context + '.' + bReference.Object-identifier, 'UPDATE').
      RUN addRelation('Program'  , gcProgramName, 'TempTable', bReference.Object-context, 'TEMPTABLE').
      RUN addRelation('TempTable', bReference.Object-context, 'TT-Field', bReference.Object-context + '.' + bReference.Object-identifier, 'TT-Field').
    END.
  END.

  ELSE
  IF NUM-ENTRIES(bReference.Object-identifier,'.') = 1 THEN
  DO:  /* UPDATE <db>.<file> <field> */
    cDb    = ENTRY(1,bReference.Object-context ,'.').
    cFile  = bReference.Object-context.
    cField = SUBSTITUTE('&1.&2', cFile, bReference.Object-identifier).

    RUN addRelation('Program', gcProgramName, 'Field', cField, "UPDATE").
    RUN addSchemaRelations(cDb, cFile, cField, '').
  END.
END PROCEDURE. /* Process_UPDATE */


FUNCTION getFileHash RETURNS CHARACTER (pcFile AS CHARACTER):
  /* Return a hash value for the xref file.
  ** Note that the tag <Source-guid> needs to be removed
  */
  DEFINE VARIABLE cData  AS LONGCHAR  NO-UNDO.
  DEFINE VARIABLE iStart AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iEnd   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cGuid  AS CHARACTER NO-UNDO.

  COPY-LOB FILE pcFile TO cData.
  iStart = INDEX(cData,'<Source-guid>').
  iEnd   = INDEX(cData,'</Source-guid>').
  cGuid  = SUBSTRING(cData,iStart,iEnd - iStart + 14).
  cData  = REPLACE(cData,cGuid,'<Source-guid></Source-guid>').

  RETURN STRING(MD5-DIGEST( cData )).
END FUNCTION. /* getFileHash */


FUNCTION stripPathNames RETURNS CHARACTER
  ( pcFileName AS CHARACTER ):

  pcFileName = TRIM(pcFileName).
  pcFileName = REPLACE(pcFileName,"~/","~\"). /* use one style of slashes */
  pcFileName = ENTRY(1,pcFileName, ' '). /* strip trailing parameter info */

  IF pcFileName BEGINS pcPathToStrip THEN
    pcFileName = SUBSTRING(pcFileName,LENGTH(pcPathToStrip) + 1). /* strip base path */

  IF pcFileName BEGINS '.\' THEN
    pcFileName = SUBSTRING(pcFileName,3). /* strip .\ */

  RETURN pcFileName.
END FUNCTION. /* stripPathNames */

