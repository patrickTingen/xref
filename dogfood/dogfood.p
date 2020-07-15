/*------------------------------------------------------------------------
  File : dogfood.p
  Desc : Reference implementation for xref
  
  This program shows how you can compile your application and read
  the resulting XREF information into the database. Adapt the program
  to your own needs. As an example, the sources of the XREF application
  itself are read into the database. 
  
  Assumed path for XREF is c:\Data\Progress\xref
  
  ----------------------------------------------------------------------*/

DEFINE VARIABLE cSrcPath  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXrefPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHtmlPath AS CHARACTER NO-UNDO.

/* Path to the source files of your application */
cSrcPath  = 'c:\Data\Progress\xref\src\'.

/* Path where the intermediate xref xml files should be saved */
cXrefPath = 'c:\Data\Progress\xref\xml\'.

/* Path to the generated HTML pages */
cHtmlPath  = 'c:\Data\Progress\xref\html\'.

/* Make sure the sources are in the propath 
*/
IF SEARCH('compileFolder.p') = ? THEN
  PROPATH = PROPATH + ',c:\Data\Progress\xref\src'.

/* If needed, connect the XREF db 
*/
IF NOT CONNECTED('xrefdb') THEN 
  CONNECT c:\Data\Progress\xref\db\xrefdb -1.

/* Prepare window for showing reading progress 
*/
CURRENT-WINDOW:WIDTH = 120.
CURRENT-WINDOW:HEIGHT = 5.
PAUSE 0 BEFORE-HIDE. 
OS-DELETE VALUE(SESSION:TEMP-DIR + '\compile.log').

/* Clean up old data. We could also just delete the whole db and then create 
** it again, but normally I have a server running for it and bringing it 
** down and then up again is too much trouble, so I simply delete the records.
** 
** You can also choose not to clean up. The read program will then simply 
** refresh the data inside the database. Only xref files that have changed
** will be read. This speeds it up dramatically on subsequent runs. 
*/
RUN emptyDB.p.

/* Clean up pre-existing XREF XML files 
*/
OS-DELETE VALUE(cXrefPath) RECURSIVE.
OS-CREATE-DIR VALUE(cXrefPath).

/* Compile recursively 
*/
MESSAGE 'Building file list'. 
RUN compileFolder.p(cSrcPath, cXrefPath). 

/* If you have folders with additional code, like unit test that you 
** don't want to have in the XREF db, delete the XML folders before  
** you read in the data
*/
OS-DELETE c:\Data\Progress\xref\xml\unittest\ RECURSIVE.

/* Process XML data 
*/
RUN c:\Data\Progress\xref\src\loadXref.p
  ( cXrefPath /* input folder */
  , cSrcPath  /* src path to strip */
  , '*'       /* relation types (we want all) */
  , '*'       /* object types (we want all) */
  ).

/* Generate HTML pages, 
*/
OS-DELETE VALUE(cHtmlPath) RECURSIVE.
OS-CREATE-DIR VALUE(cHtmlPath).
RUN generateHtml.p(cHtmlPath).