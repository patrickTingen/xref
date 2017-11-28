/*------------------------------------------------------------------------
    File        : loadXref.p
    Purpose     : Import one Xref XML file into the Xref Database

    Syntax      : RUN loadXref.p( <name of xref.xml to read> ).

    Changes: 
    27-11-2017 Patrick Tingen Created
    
  ----------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pcEnvironment AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcXrefFile    AS CHARACTER NO-UNDO.

/* Timers to measure performance */
&GLOBAL-DEFINE timerStart PUBLISH 'timerStart' (ENTRY(1,PROGRAM-NAME(1),' ')).
&GLOBAL-DEFINE timerStop  FINALLY: PUBLISH 'timerStop'  (ENTRY(1,PROGRAM-NAME(1),' ')). END.

DEFINE TEMP-TABLE ttSource NO-UNDO       XML-NODE-NAME "source"
  FIELD File-name   AS CHARACTER         XML-NODE-TYPE "attribute" 
  FIELD Source-guid AS CHARACTER 
  FIELD File-num    AS INTEGER           XML-DATA-TYPE "byte" 
  FIELD cObjectType AS CHARACTER.

DEFINE TEMP-TABLE ttReference NO-UNDO    XML-NODE-NAME "reference"
  FIELD Reference-type    AS CHARACTER   XML-NODE-TYPE "attribute" 
  FIELD Object-identifier AS CHARACTER   XML-NODE-TYPE "attribute" 
  FIELD Source-guid       AS CHARACTER 
  FIELD File-num          AS INTEGER     XML-DATA-TYPE "byte" 
  FIELD Ref-seq           AS INTEGER     XML-DATA-TYPE "short" 
  FIELD Line-num          AS INTEGER     XML-DATA-TYPE "short" 
  FIELD Object-context    AS CHARACTER 
  FIELD Access-mode       AS CHARACTER 
  FIELD Data-member-ref   AS CHARACTER 
  FIELD Temp-ref          AS CHARACTER 
  FIELD Detail            AS CHARACTER 
  FIELD Is-static         AS CHARACTER 
  FIELD Is-abstract       AS CHARACTER 
  FIELD Source_id         AS RECID       XML-NODE-TYPE "hidden".

DEFINE TEMP-TABLE ttStringRef NO-UNDO XML-NODE-NAME "string-ref"
  FIELD Source-guid   AS CHARACTER 
  FIELD Ref-seq       AS INTEGER      XML-DATA-TYPE "short" 
  FIELD Max-length    AS INTEGER      XML-DATA-TYPE "byte" 
  FIELD Justification AS CHARACTER 
  FIELD Translatable  AS CHARACTER 
  FIELD Reference_id  AS RECID        XML-NODE-TYPE "HIDDEN".

DEFINE DATASET dsCrossRef XML-NODE-NAME "Cross-reference"
  FOR ttSource, ttReference, ttStringRef
  PARENT-ID-RELATION RELATION1 FOR ttReference, ttStringRef
  PARENT-ID-FIELD Reference_id
  PARENT-ID-RELATION RELATION2 FOR ttSource, ttReference
  PARENT-ID-FIELD Source_id.

DEFINE VARIABLE glProcessSharedElements AS LOGICAL   NO-UNDO INIT YES. /* process shared elements y/n */ 
DEFINE VARIABLE glProcessTempTables     AS LOGICAL   NO-UNDO INIT YES. /* process temptables y/n */
DEFINE VARIABLE glProcessWorkFiles      AS LOGICAL   NO-UNDO INIT YES. /* process workfiles y/n */

/* Global Counters */
DEFINE VARIABLE gcProgramName AS CHARACTER NO-UNDO. /* name of the program being evaluated */
DEFINE VARIABLE giEnvNr       AS INT64     NO-UNDO. 

FUNCTION stripPathNames RETURNS CHARACTER
  ( pcFileName AS CHARACTER ):
  
  {&timerStart}
  pcFileName = TRIM(pcFileName).
  pcFileName = REPLACE(pcFileName,"~\","~/"). /* backward to forward slash */
  pcFileName = ENTRY(1,pcFileName, ' '). /* strip trailing parameter info */

  REPEAT WHILE NUM-ENTRIES(pcFileName,'/') > 2:
    pcFileName = SUBSTRING(pcFileName,INDEX(pcFileName,'/') + 1).
  END.
  
  RETURN pcFileName.
  {&timerStop}
END FUNCTION. /* stripPathNames */
  
/* ***************************  Main Block  *************************** */

RUN initObject.
RUN loadXref(INPUT pcXrefFile).

/* **********************  Internal Procedures  *********************** */

PROCEDURE initObject:
  /* Initialize program
   */   
  DEFINE BUFFER b_Environment FOR xref_Environment.
  PAUSE 0 BEFORE-HIDE.
  
  /* Determine Environment */
  FIND b_Environment NO-LOCK
    WHERE b_Environment.cEnvironment = pcEnvironment
    NO-ERROR.
    
  IF NOT AVAILABLE b_Environment THEN
  DO:
    FIND LAST b_Environment NO-ERROR.
    IF NOT AVAILABLE b_Environment THEN giEnvNr = 1.
    ELSE giEnvNr = b_Environment.iEnvironmentNr + 1.
      
    CREATE b_Environment.
    ASSIGN
      b_Environment.iEnvironmentNr = giEnvNr
      b_Environment.cEnvironment   = pcEnvironment.
  END.
  
  /* Set global var */
  giEnvNr = b_Environment.iEnvironmentNr.
  
END PROCEDURE. /* initObject */


PROCEDURE loadXref:
  /* Load the Given Xref Xml File into the Xref Database
  */
  DEFINE INPUT  PARAMETER pcFileName AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE cHandler  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cModified AS CHARACTER NO-UNDO.

  DEFINE BUFFER b_XmlFile FOR xref_XmlFile.
	
  PUBLISH 'debugInfo'(SUBSTITUTE("Reading &1", pcFileName)).
	  
  /* Check the XML file */
  FILE-INFO:FILE-NAME = pcFileName.
  IF FILE-INFO:FULL-PATHNAME = ? THEN
  DO:
    PUBLISH 'debugInfo'( SUBSTITUTE("Xref file &1 not found", pcFileName )).
    RETURN.
  END.
  
  /* Register XML file with last modified time */
  DO TRANSACTION:
    FIND b_XmlFile EXCLUSIVE-LOCK
      WHERE b_XmlFile.cFullName = pcFileName
            NO-ERROR.
            
    IF NOT AVAILABLE b_XmlFile THEN
    DO:
      CREATE b_XmlFile. 
      ASSIGN b_XmlFile.cFullName = pcFileName.
    END.  

    /* Modified date/time of file on disk */
    cModified = SUBSTITUTE('&1 &2'
                          , ISO-DATE(FILE-INFO:FILE-MOD-DATE)
                          , STRING(FILE-INFO:FILE-MOD-TIME,'hh:mm:ss')
                          ).

    /* Check if file on disk has been changed since the last time */
    IF b_XmlFile.cLastModified = cModified THEN RETURN.   
    b_XmlFile.cLastModified = cModified.
  END.
  
  /* Load Dataset */
  DATASET dsCrossRef:READ-XML ("FILE",pcFileName,"EMPTY","",FALSE).
  
  /* Correct file names in ttSource: 
   * - strip excess path names 
   * - set source type
   */
  FOR EACH ttSource:
    ttSource.File-name = REPLACE(ttSource.File-name,"~\","~/").
    IF ttSource.File-name BEGINS './' THEN ttSource.File-name = SUBSTRING(ttSource.File-name ,3).
    
    /* Limit nr of paths */
    ttSource.FILE-NAME = stripPathNames(ttSource.FILE-NAME).
    
    /* Set objecttype */
    ttSource.cObjectType = (IF ttSource.File-name MATCHES "*.i" THEN "Include" ELSE "Program").
  END.

  /* Clean db and set vars */
  RUN initXref.

  /* Process the Xref XML 
   * For some reason I don't understand sorting the table speeds it up dramatically
   */
  DO TRANSACTION:
    FOR EACH ttReference
      , FIRST ttSource
        WHERE ttSource.Source-guid = ttReference.Source-guid
          AND ttSource.File-num    = ttReference.File-num
      BREAK BY ttReference.Source-guid
            BY ttReference.Reference-type 
            BY ttReference.Temp-ref
            BY ttReference.Detail
            BY ttReference.Object-identifier 
            BY ttReference.Object-context:
      
      IF FIRST-OF(Object-context) THEN
      DO:
        /* Compose handler name and run it if it is defined */
        cHandler = 'Process_' + ttReference.Reference-type.
        IF LOOKUP(cHandler, THIS-PROCEDURE:INTERNAL-ENTRIES) > 0 THEN 
          RUN VALUE(cHandler).  
        ELSE 
          PUBLISH 'NewReferenceType'(ttReference.Reference-type).
      END.

      PROCESS EVENTS.
    END. /* For Each Reference */
  END.
  
END PROCEDURE. /* LoadXref */


PROCEDURE initXref:
  /* 
   * Init load: delete old records and set program name
   */
  {&timerStart}
  
  /* Find reference to compilation unit */
  FIND FIRST ttReference WHERE ttReference.Reference-type = 'compile' NO-ERROR.
  IF NOT AVAILABLE ttReference THEN
  DO:
    PUBLISH 'debugInfo'("No COMPILE reference found in xref").
    RETURN. 
  END.
  
  /* Strip all directory structures */
  gcProgramName = stripPathNames(ttReference.Object-identifier).
	
  /* Delete all relations for a certain program. This procedure does not
   * clean up objects which remain unused after deleting a relation and
   * relations like DB<->FILE, FILE<->FIELD, FILE<->INDEX since these
   * are not tied to a specific program. Eventually, this may clutter up
   * your database. I recommend doing a complete compile, read & generate
   * periodically.
   */
  DEFINE BUFFER b_relation FOR xref_Relation.
  
  FOR EACH b_relation EXCLUSIVE-LOCK
    WHERE b_relation.cParentType = 'Program'
      AND b_relation.cParentName = gcProgramName:
    DELETE b_relation.
  END.
  
  {&timerStop}
END PROCEDURE. /* initXref */


PROCEDURE addRelation:
  /* Create a relation in the database between two objects.
  */
  DEFINE INPUT PARAMETER pcParentType   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcParentName   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcChildType    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcChildName    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcRelationType AS CHARACTER NO-UNDO.

  DEFINE BUFFER b_Relation     FOR xref_Relation.
  DEFINE BUFFER b_RelationType FOR xref_RelationType.
  DEFINE BUFFER b_Object       FOR xref_Object.
  DEFINE BUFFER b_ObjectType   FOR xref_ObjectType.
  {&timerStart}
 
  /* Ignore incomplete relations */
  IF   pcParentType   = ''
    OR pcParentName   = ''
    OR pcChildType    = ''
    OR pcChildName    = ''
    OR pcRelationType = '' THEN RETURN.
  
  /* If relation already exists, go back */
  IF CAN-FIND(b_relation
    WHERE b_relation.iEnvironment  = giEnvNr
      AND b_relation.cRelationType = pcRelationType 
      AND b_relation.cParentType   = pcParentType
      AND b_relation.cParentName   = pcParentName 
      AND b_relation.cChildType    = pcChildType 
      AND b_relation.cChildName    = pcChildName) THEN RETURN. 

  CREATE b_relation.
  ASSIGN 
    b_relation.iEnvironment  = giEnvNr
    b_relation.cRelationType = pcRelationType 
    b_relation.cParentType   = pcParentType
    b_relation.cParentName   = pcParentName 
    b_relation.cChildType    = pcChildType 
    b_relation.cChildName    = pcChildName
    .      
    
  /* Add parent object if needed */
  IF NOT CAN-FIND(b_object
    WHERE b_object.iEnvironment = giEnvNr
      AND b_object.cObjectType  = pcParentType
      AND b_object.cObjectName  = pcParentName) THEN 
  DO:
    CREATE b_object.
    ASSIGN b_object.iEnvironment = giEnvNr
           b_object.cObjectType  = pcParentType 
           b_object.cObjectName  = pcParentName.
           
    IF NOT CAN-FIND(b_ObjectType WHERE b_ObjectType.cObjectType = pcParentType) THEN
    DO:
      CREATE b_ObjectType.
      ASSIGN b_ObjectType.cObjectType = pcParentType.
    END.
  END.

  /* Add child object if needed */
  IF NOT CAN-FIND(b_object
    WHERE b_object.iEnvironment = giEnvNr
      AND b_object.cObjectType  = pcChildType
      AND b_object.cObjectName  = pcChildName) THEN 
  DO:
    CREATE b_object.
    ASSIGN b_object.iEnvironment = giEnvNr
           b_object.cObjectType  = pcChildType 
           b_object.cObjectName  = pcChildName.
           
    IF NOT CAN-FIND(b_ObjectType WHERE b_ObjectType.cObjectType = pcChildType) THEN
    DO:
      CREATE b_ObjectType.
      ASSIGN b_ObjectType.cObjectType = pcChildType.
    END.
  END.

  /* Add relationtype */
  IF NOT CAN-FIND(b_relationType WHERE b_relationType.cRelationType = pcRelationType) THEN 
  DO:
    CREATE b_relationType.
    ASSIGN b_relationType.cRelationType = pcRelationType.
  END.

  {&timerStop}
END PROCEDURE. /* addRelation */


PROCEDURE addSchemaRelations:
  /* 
   * Add extra relations for meta schema 
   */
  DEFINE INPUT PARAMETER pcDb    AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pcFile  AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pcField AS CHARACTER   NO-UNDO.
  DEFINE INPUT PARAMETER pcIndex AS CHARACTER   NO-UNDO.
  {&timerStart}
  
  IF pcDb <> ''   AND pcFile <> ''  THEN RUN addRelation('Db'  , pcDb  , 'File' , pcFile , 'FILE' ).
  IF pcFile <> '' AND pcField <> '' THEN RUN addRelation('File', pcFile, 'Field', pcField, 'FIELD').
  IF pcFile <> '' AND pcIndex <> '' THEN RUN addRelation('File', pcFile, 'Index', pcIndex, 'INDEX').

  IF pcDb    <> '' THEN RUN addRelation('Program', gcProgramName, 'Db'   , pcDb   , 'PROG-DB').
  IF pcFile  <> '' THEN RUN addRelation('Program', gcProgramName, 'File' , pcFile , 'PROG-FILE').
  IF pcField <> '' THEN RUN addRelation('Program', gcProgramName, 'Field', pcField, 'PROG-FIELD').
  IF pcIndex <> '' THEN RUN addRelation('Program', gcProgramName, 'Index', pcIndex, 'PROG-INDEX').
  
  {&timerStop}
END PROCEDURE. /* addSchemaRelations */


PROCEDURE Process_Access:
  /*
   * Process Access Reference type
   */
  DEFINE VARIABLE cDb    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cField AS CHARACTER NO-UNDO.
  
  {&timerStart}
  
  IF ttReference.Object-context = "SHARED" 
    AND glProcessSharedElements THEN 
    RUN addRelation('Program', gcProgramName, 'Variable', ttReference.Object-identifier, 'ACCESS').
  
  ELSE 
  IF ttReference.Temp-ref = "T" THEN /* TEMPTABLE */ 
  DO: /* ACCESS <temptable> <field> TEMPTABLE */
    IF NOT glProcessTempTables THEN RETURN.
		
    /* create relation for prog-field */  
    RUN addRelation('Program', gcProgramName, 'TT-Field', ttReference.Object-identifier + '.' + ttReference.Object-context, 'ACCESS').
    
    /* create relation for prog-temptable */  
    RUN addRelation('Program', gcProgramName, 'TempTable', ttReference.Object-identifier, 'TEMPTABLE').
    
    /* create relation for tt-field */  
    RUN addRelation('TempTable', ttReference.Object-identifier, 'TT-Field', ttReference.Object-identifier + '.' + ttReference.Object-context, 'TT-Field').
  END.
  
  ELSE 
  IF NUM-ENTRIES(ttReference.Object-context,'.') = 2 THEN 
  DO:
    cDb    = ENTRY(1,ttReference.Object-context,'.').
    cFile  = ttReference.Object-context.
    cField = ttReference.Object-context + '.' + ttReference.Object-identifier.

    RUN addRelation('Program', gcProgramName, 'Field', cField, "ACCESS"). 

    /* Add schema relations */
    RUN addSchemaRelations(cDb, cFile, cField, '').
  END.

  {&timerStop}
END PROCEDURE. /* Process_Access */


PROCEDURE Process_Create:
  /*
   * Process the Create Reference Type
   */
  DEFINE VARIABLE cDb    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFile  AS CHARACTER   NO-UNDO.
  {&timerStart}
  
  IF ttReference.Temp-ref = "T" THEN 
  DO: /* CREATE <temptable> TEMPTABLE */
    IF NOT glProcessTempTables THEN RETURN.
				
    RUN addRelation('Program', gcProgramName, 'TempTable', ttReference.Object-identifier, 'CREATE').
  END.
  
  ELSE
  IF NUM-ENTRIES(ttReference.Object-identifier,'.') = 2 THEN 
  DO: /* CREATE <db>.<file> */
    cDb    = ENTRY(1,ttReference.Object-identifier,'.').
    cFile  = ttReference.Object-identifier.
    
    RUN addRelation(ttSource.cObjectType, ttSource.File-name, 'File' , cFile , "CREATE").
    
    /* Add schema relations */
    RUN addSchemaRelations(cDb, cFile, '', '').
    
  END.
		
  {&timerStop}
END PROCEDURE. /* Process_Create */


PROCEDURE Process_Delete:
  /*
   * Process Delete Reference Type
   */ 
  DEFINE VARIABLE cDb    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFile  AS CHARACTER NO-UNDO.
  {&timerStart}
	 
  IF ttReference.Temp-ref = "T" THEN 
  DO: /* DELETE <temptable> TEMPTABLE */
    IF NOT glProcessTempTables THEN RETURN.
    RUN addRelation('Program', gcProgramName, 'TempTable', ttReference.Object-identifier, "DELETE").
  END.
  
  ELSE
  IF NUM-ENTRIES(ttReference.Object-identifier,'.') = 2 THEN 
  DO: /* DELETE <db>.<file> */
    cDb    = ENTRY(1,ttReference.Object-identifier,'.').
    cFile  = ttReference.Object-identifier.
    
    RUN addRelation('Program', gcProgramName, 'File' , cFile , "DELETE").
    RUN addSchemaRelations(cDb, cFile, '', '').
  END.
  
  {&timerStop}
END PROCEDURE. /* Process_Delete */


PROCEDURE Process_DLL-Entry:
  /*
   * Process DLL-Entry
   */
  {&timerStart}
  
  RUN addRelation('Program', gcProgramName, 'DLL-Entry', ttReference.Object-identifier, 'DLL-ENTRY').
  
  {&timerStop}
END PROCEDURE. /* Process_Function */


PROCEDURE Process_Function:
  /*
   * Process Function Reference Type
   */
  {&timerStart}
  
  RUN addRelation('Program', gcProgramName, 'Function', ttReference.Object-identifier, 'FUNCTION').
  
  {&timerStop}
END PROCEDURE. /* Process_Function */


PROCEDURE Process_Global-Variable:
  /*
   * Process GLOBAL-VARIABLE Reference Type
   */
  {&timerStart}
  
  RUN addRelation('Program', gcProgramName, 'Variable', ttReference.Object-identifier, 'GLOBAL-VARIABLE').
  
  {&timerStop}
END PROCEDURE. /* Process_Global-Variable */


PROCEDURE Process_Include:
  /*
   * Process Include Reference Type
   */
  DEFINE VARIABLE cInclude AS CHARACTER NO-UNDO.
  {&timerStart}

  cInclude = stripPathNames(ttReference.Object-identifier).
	
  IF cInclude = '"'
    OR cInclude = ''
    OR cInclude = '""'
    OR cInclude MATCHES '&*'
    OR cInclude MATCHES "<*" THEN
  DO:
    PUBLISH 'debugInfo'(SUBSTITUTE("Strange include &1, refstring=&2", cInclude,TRIM(ttReference.Object-identifier)) ).
  END. 
  
  RUN addRelation('Program', gcProgramName, 'Include', cInclude, 'INCLUDE').
  
  {&timerStop}
END PROCEDURE. /* Process_Include */


PROCEDURE Process_Procedure:
  /* 
   * Process Procedure Reference Type
   */
  {&timerStart}
  
  RUN addRelation('Program', gcProgramName, 'Procedure', ttReference.Object-identifier, 'PROCEDURE').
  
  {&timerStop}
END PROCEDURE. /* Process_Procedure */


PROCEDURE Process_Reference:
  /* 
   * Process Reference Reference Type
   */
  DEFINE VARIABLE cDb    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFile  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cField AS CHARACTER   NO-UNDO.
  
  {&timerStart}
		 
  IF ttReference.Temp-ref = "T" THEN 
  DO: /* REFERENCE <temptable> TEMPTABLE */
    IF NOT glProcessTempTables THEN RETURN.
    RUN addRelation('Program', gcProgramName, 'TempTable', ttReference.Object-identifier, 'REFERENCE').
  END. /* IF ttReference.Temp-ref = "T" THEN */
  
  ELSE
  IF NUM-ENTRIES(ttReference.Object-identifier,'.') = 2 THEN /* table name */
  DO: /* REFERENCE <db>.<file> [<field>] */
  
    cDb    = ENTRY(1,ttReference.Object-identifier,'.').
    cFile  = ttReference.Object-identifier.
    
    /* Object-context might contain field name */
    IF ttReference.Object-context = '' THEN
    DO:
      RUN addRelation('Program', gcProgramName, 'File', cFile, "REFERENCE"). 
    END.
    
    ELSE 
    DO:
      cField = SUBSTITUTE('&1.&2.&3', cDb,cFile, ttReference.Object-context).
      RUN addRelation('Program', gcProgramName, 'Field', cField, "REFERENCE"). 
    END. /* Object-context = '' */   
    
    RUN addSchemaRelations(cDb, cFile, cField, '').
  END.
    
  {&timerStop}
END PROCEDURE. /* Process_Reference */


PROCEDURE Process_Run:
  /* 
   * Process Run Reference Type
   */
  {&timerStart}
	
  /* find out if we are calling an internal or external procedure by checking
  ** the extention of the called program. If .p / .w / .r then probably an
  ** external one, otherwise probably an internal one.
  */
  IF ttReference.Object-identifier MATCHES "*~~.p" OR
    ttReference.Object-identifier MATCHES "*~~.w" OR
    ttReference.Object-identifier MATCHES "*~~.r" THEN
    RUN addRelation('Program', gcProgramName, 'Program', ttReference.Object-identifier, 'RUN').
  ELSE
    RUN addRelation('Program', gcProgramName, 'Procedure', ttReference.Object-identifier, 'RUN-IP').
    
  {&timerStop}
END PROCEDURE. /* Process_Run */


PROCEDURE Process_Search:
  /* 
   * Process Search Reference Type
   */
  DEFINE VARIABLE cDb    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFile  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cIndex AS CHARACTER   NO-UNDO.
  
  {&timerStart}
  
  IF ttReference.Temp-ref = 'T' THEN 
  DO: /* SEARCH <temptable> <index> TEMPTABLE */
    IF NOT glProcessTempTables THEN RETURN.
          
    RUN addRelation('Program', gcProgramName, 'TempTable', ttReference.Object-identifier, 'TEMPTABLE').
    RUN addRelation('Program', gcProgramName, 'TT-Index', ttReference.Object-identifier + '.' + ttReference.Object-context, 'SEARCH').
    RUN addRelation('TempTable', ttReference.Object-identifier, 'TT-Index', ttReference.Object-identifier + '.' + ttReference.Object-context, 'TT-Index').
  END.
  
  ELSE
  IF NUM-ENTRIES(ttReference.Object-identifier,'.') = 2 THEN 
  DO: /* SEARCH <db>.<file> <index> [WHOLE-INDEX] */
            
    cDb    = ENTRY(1,ttReference.Object-identifier,'.').
    cFile  = ttReference.Object-identifier.
    cIndex = SUBSTITUTE('&1.&2.&3', cDb,cFile, ttReference.Object-context).
    
    RUN addRelation('Program', gcProgramName, 'Index', cIndex, "SEARCH"). 
    RUN addSchemaRelations(cDb, cFile, '', cIndex).

    /* create relation for prog<->index if WHOLE-INDEX*/
    IF ttReference.Detail = 'WHOLE-INDEX' THEN
      RUN addRelation('Program', gcProgramName, 'Index', cIndex, 'WHOLE-INDEX').
  END.
  
  {&timerStop}
END PROCEDURE. /* Process_Search */


PROCEDURE Process_Update:
  /* 
   * Process Update Reference Type
   */
  DEFINE VARIABLE cDb    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFile  AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cField AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cIndex AS CHARACTER   NO-UNDO.
  
  {&timerStart}
	 
  IF ttReference.Object-identifier = 'SHARED' THEN 
  DO: /* UPDATE SHARED <var> */
    IF NOT glProcessSharedElements THEN RETURN.
    RUN addRelation('Program', gcProgramName, 'Variable', ttReference.Object-context, 'UPDATE').
  END.
  ELSE
    IF ttReference.Temp-ref = "T" THEN 
    DO: /* UPDATE <temptable> <field> TEMPTABLE */
      IF NOT glProcessTempTables THEN RETURN.
        
      /* create relation for prog-temptable */  
      RUN addRelation('Program', gcProgramName, 'TempTable', ttReference.Object-identifier, 'TEMPTABLE').
      /* create relation for prog-field */  
      RUN addRelation('Program', gcProgramName, 'TT-Field', ttReference.Object-identifier + '.' + ttReference.Object-context, 'UPDATE').
      /* create relation for tt-field */  
      RUN addRelation('TEMPTABLE', ttReference.Object-identifier, 'TT-Field', ttReference.Object-identifier + '.' + ttReference.Object-context, 'TT-Field').
    END.
    
    ELSE
    IF NUM-ENTRIES(ttReference.Object-identifier,'.') = 1 THEN 
    DO:  /* UPDATE <db>.<file> <field> */
      cDb    = ENTRY(1,ttReference.Object-context ,'.').
      cFile  = ttReference.Object-identifier.
      cField = SUBSTITUTE('&1.&2.&3', cDb,cFile, ttReference.Object-identifier).
      
      RUN addRelation('Program', gcProgramName, 'Field', cField, "UPDATE"). 
      RUN addSchemaRelations(cDb, cFile, cField, '').

    END.
  {&timerStop}
END PROCEDURE. /* Process_Update */