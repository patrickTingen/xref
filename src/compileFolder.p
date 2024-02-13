/*-----------------------------------------------------------------------
  File : compileFolder.p
  Desc : Compile all files in a folder recursively
  ------------------------------------------------------------------------*/

&IF NOT "{&FILE-NAME}" MATCHES "*.cmp" &THEN /* regular */
  DEFINE INPUT PARAMETER pcSrcFolder   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcXrefFolder  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcSkipFolders AS CHARACTER NO-UNDO.
&ELSE
  DEFINE VARIABLE pcSrcFolder   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcXrefFolder  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcSkipFolders AS CHARACTER NO-UNDO.

  /* Path to the source files of your application */
  pcSrcFolder  = 'c:\Work\xref\src\'.
  
  /* Path where the intermediate xref xml files should be saved */
  pcXrefFolder = 'c:\work\xref\xml\'.

  /* Folders to skip */
  pcSkipFolders = ''.
&ENDIF

DEFINE TEMP-TABLE ttFile NO-UNDO
  FIELD cFullName AS CHARACTER.

DEFINE STREAM strLog.

PAUSE 0 BEFORE-HIDE.

/* Make sure folders end in a slash */
pcSrcFolder  = RIGHT-TRIM(pcSrcFolder,'\') + '\'.
pcXrefFolder = RIGHT-TRIM(pcXrefFolder,'\') + '\'.

/* Redirect errors to the log file */
OUTPUT TO VALUE(SESSION:TEMP-DIR + '\compile-full.log'). 
OUTPUT STREAM strLog TO VALUE(SESSION:TEMP-DIR + '\compile.log'). 

RUN getSkipFolders(pcSkipFolders, OUTPUT pcSkipFolders).
RUN getFiles(pcSrcFolder).
RUN compileFiles(pcSrcFolder, pcXrefFolder).

OUTPUT STREAM strLog CLOSE.
OUTPUT CLOSE. 


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


PROCEDURE getFiles:
  /* Collect all sources, recursively
  */
  DEFINE INPUT PARAMETER pcFolder AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cFile AS CHARACTER EXTENT 3 NO-UNDO.
  DEFINE VARIABLE cExt  AS CHARACTER NO-UNDO.
  DEFINE BUFFER bFile FOR ttFile.

  INPUT FROM OS-DIR(pcFolder).
  REPEAT:
    IMPORT cFile[1 FOR 3]. /* 1=OrigBasename 2=OrigFullname 3=tokens */

    IF cFile[1] BEGINS '.' THEN NEXT. 
    IF cFile[3] BEGINS 'D' THEN RUN getFiles(cFile[2]).
    IF CAN-DO(pcSkipFolders, cFile[2]) THEN NEXT.

    IF cFile[3] BEGINS 'F' THEN 
    DO:
      cExt = ENTRY(NUM-ENTRIES(cFile[1],'.'),cFile[1],'.').
      IF LOOKUP(cExt,'p,w,cls') = 0 THEN NEXT. 

      CREATE bFile.
      ASSIGN bFile.cFullName = cFile[2].
    END. /* file */
  END. /* repeat */
  INPUT CLOSE. 
END PROCEDURE. /* getFiles */


PROCEDURE compileFiles:
  /* Compile all files, save XREF info as XML
  */
  DEFINE INPUT PARAMETER pcSrcFolder  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER pcXrefFolder AS CHARACTER NO-UNDO.

  DEFINE VARIABLE cObj  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cXml  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iTodo AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iDone AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER   NO-UNDO.

  DEFINE BUFFER bFile FOR ttFile.
  
  FOR EACH bFile:
    iTodo = iTodo + 1.
  END.

  FOR EACH bFile:
  
    iDone = iDone + 1.
    PUBLISH "xref-log" (SUBSTITUTE('&1 Compiling &2 of &3: &4', STRING(TIME,'hh:mm:ss'), iDone, iTodo, bFile.cFullName)).
    PROCESS EVENTS. 

    PUT UNFORMATTED SKIP(1) bFile.cFullName.

    cXml = SUBSTITUTE('&1.xref.xml', REPLACE(bFile.cFullName, pcSrcFolder, pcXrefFolder)).
    RUN createFolder(SUBSTRING(cXml,1,R-INDEX(cXml,'\'))).

    /* If an .r file exists, no xref will be generated, so delete it */
    cObj = SUBSTRING(bFile.cFullName, 1, R-INDEX(bFile.cFullName,'.')) + 'r'.
    IF SEARCH(cObj) <> ? THEN OS-DELETE VALUE(cObj).

    COMPILE VALUE(bFile.cFullName) XREF-XML VALUE(cXml) NO-ERROR.
  
    /* If a file gives a compile error or warning, we keep the 
    ** message in the log by setting a new offset */
    IF COMPILER:ERROR OR COMPILER:WARNING OR COMPILER:NUM-MESSAGES > 0 THEN 
    DO:
      PUT STREAM strLog UNFORMATTED SKIP(1) bFile.cFullName.

      DO i = 1 TO COMPILER:NUM-MESSAGES:
        PUT STREAM strLog UNFORMATTED SKIP COMPILER:GET-MESSAGE(i).
      END.
    END.    
  END. /* f/e bFile */

END PROCEDURE. /* compileFiles */


PROCEDURE createFolder:
  /* Make sure a folder exist. Create if needed
  */
  DEFINE INPUT PARAMETER pcNewFolder AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cFolder AS CHARACTER NO-UNDO.

  pcNewFolder = REPLACE(pcNewFolder,'/','\').

  DO i = 1 TO NUM-ENTRIES(pcNewFolder,'\'):
    cFolder = SUBSTITUTE('&1&2&3', cFolder, (IF i = 1 THEN '' ELSE '\'), ENTRY(i,pcNewFolder,'\')).
    OS-CREATE-DIR VALUE(cFolder). 
  END.
END PROCEDURE. /* createFolder */

