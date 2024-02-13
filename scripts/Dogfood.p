/*------------------------------------------------------------------------
  File : Dogfood.p
  Desc : Reference implementation for xref
  
  This program shows how you can compile your application and read
  the resulting XREF information into the database. Adapt the program
  to your own needs. As an example, the sources of the XREF application
  itself are read into the database. 
 
  ----------------------------------------------------------------------*/

DEFINE VARIABLE cSrcPath     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXrefPath    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHtmlPath    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lWipeDB      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cSkipFolders AS CHARACTER NO-UNDO.

/* Path to the source files of your application */
cSrcPath  = 'src\'.

/* Path for the intermediate xref xml files */
cXrefPath = 'xml\Dogfood'.

/* Path to the generated HTML pages 
** Leave blank if you don't want HTML
*/
cHtmlPath  = 'html\Dogfood'.

/* You can choose to clean up old data in the xref db before
** loading. You could also just delete the whole db and then create 
** it again, but normally I have a server running for it and bringing it 
** down and then up again is too much trouble, so I simply delete the records.
** 
** If you choose not to clean up, the read program will then simply 
** refresh the data inside the database. Only xref files that have changed
** will be read. This speeds it up dramatically on subsequent runs. 
**
** My recommendation is to leave this to NO
*/
lWipeDB = NO.

/* There may be some folders you want to exclude from the xref
** database, like your own tooling or unittests or what have you
** Use the comma delimited list cSkipFolders for this
*/
cSkipFolders = 'db'.

/* Connect your own database(s) here */
/* CONNECT -db myDatabase -H server -S service. */

/* ------------------------------------------------------------------------ */
/* You're done. There should be no need to change things below this comment */

/* Make sure the sources are in the propath */
IF SEARCH('compileFolder.p') = ? THEN
  PROPATH = PROPATH + ',src'.

/* If needed, connect the XREF db */
IF NOT CONNECTED('xrefdb') THEN 
  CONNECT db\xrefdb -1.

/* Prepare window for showing reading progress */
CURRENT-WINDOW:WIDTH = 120.
CURRENT-WINDOW:HEIGHT = 5.
PAUSE 0 BEFORE-HIDE. 
OS-DELETE VALUE(SESSION:TEMP-DIR + '\compile.log').

/* Clean up database if needed */
IF lWipeDB THEN RUN emptyDB.p.

/* Clean up pre-existing XREF XML files */
OS-DELETE VALUE(cXrefPath) RECURSIVE.
OS-CREATE-DIR VALUE(cXrefPath).

/* Compile recursively */
MESSAGE 'Building file list'. 
RUN compileFolder.p(cSrcPath, cXrefPath, cSkipFolders). 

/* Process XML data */
RUN src\loadXref.p
  ( cXrefPath    /* input folder */
  , cSrcPath     /* src path to strip */
  , cSkipFolders /* folders to skip */
  , '*'          /* relation types (we want all) */
  , '*'          /* object types (we want all) */
  ).

/* Generate HTML pages */
IF cHtmlPath <> "" THEN 
DO:
  OS-DELETE VALUE(cHtmlPath) RECURSIVE.
  OS-CREATE-DIR VALUE(cHtmlPath).
  RUN generateHtml.p(cHtmlPath).
END.
