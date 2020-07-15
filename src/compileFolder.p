/*-----------------------------------------------------------------------
  File : compileFolder.p
  Desc : Compile all files in a folder recursively
  ------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pcSrcFolder  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcXrefFolder AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttFile NO-UNDO
  FIELD cFullName AS CHARACTER.

PAUSE 0 BEFORE-HIDE.

RUN getFiles(pcSrcFolder).
RUN compileFiles(pcSrcFolder, pcXrefFolder).


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

  DEFINE VARIABLE cXml    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cExt    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iOffset AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iTodo   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iDone   AS INTEGER   NO-UNDO.

  DEFINE BUFFER bFile FOR ttFile.
  
  FOR EACH bFile:
    iTodo = iTodo + 1.
  END.

  FOR EACH bFile:
    iDone = iDone + 1.
    MESSAGE SUBSTITUTE('&1 Compiling &2 of &3: &4', STRING(TIME,'hh:mm:ss'), iDone, iTodo, bFile.cFullName).

    /* Redirect errors to the log file, remember position. */
    OUTPUT TO VALUE(SESSION:TEMP-DIR + '\compile.log') APPEND. 
    SEEK OUTPUT TO iOffset.

    PUT UNFORMATTED SKIP(1) bFile.cFullName.
    cXml = SUBSTITUTE('&1.xref.xml', REPLACE(bFile.cFullName, pcSrcFolder, pcXrefFolder)).
    RUN createFolder(SUBSTRING(cXml,1,R-INDEX(cXml,'\'))).
    COMPILE VALUE(bFile.cFullName) XREF-XML VALUE(cXml).

    /* If a file gives a compile error or warning, we keep the 
    ** message in the log by setting a new offset */
    IF COMPILER:ERROR OR COMPILER:WARNING OR COMPILER:NUM-MESSAGES > 0 THEN iOffset = SEEK(OUTPUT).

    PROCESS EVENTS. 
    OUTPUT CLOSE.
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
