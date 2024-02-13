/*-----------------------------------------------------------------------
  File : generateHtml.p
  Desc : create html docs for all relations
  ----------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pcOutputDir AS CHARACTER NO-UNDO.

FUNCTION getObjectUrl RETURNS CHARACTER
  ( BUFFER buf_object FOR xref_object )  FORWARD.

DEFINE STREAM sWrite.

PAUSE 0 BEFORE-HIDE.
RUN generateHtml.

/* **********************  Internal Procedures  *********************** */

PROCEDURE generateHtml:
  /* Main procedure
  */
  DEFINE VARIABLE iTodo AS INTEGER NO-UNDO.
  DEFINE VARIABLE iDone AS INTEGER NO-UNDO.
  DEFINE BUFFER bObject FOR xref_Object.
  
  /* Fix forward slashes */
  pcOutputDir = RIGHT-TRIM(pcOutputDir,'\/') + '\'.

  RUN createFolders.
  RUN writeFrameset.
  RUN writeStyleSheet.
  RUN writeNavigationPages.
  
  FOR EACH bObject NO-LOCK:
    iTodo = iTodo + 1.
  END. 
  
  /* generate html docs */
  FOR EACH bObject NO-LOCK:
    RUN writeObjectPage(BUFFER bObject).
    iDone = iDone + 1.
    IF ETIME > 1000 THEN DO:
      MESSAGE SUBSTITUTE('&1 Generating HTML &2 of &3: &4', STRING(TIME,'hh:mm:ss'), iDone, iTodo, bObject.cObjectName).
      PROCESS EVENTS. 
      ETIME(YES).
    END.
    PROCESS EVENTS.
  END.
  
  MESSAGE 'Done'.
END PROCEDURE. /* generateHtml */


PROCEDURE createFolders :
  /* Create a subfolder for all objecttypes
  */
  DEFINE BUFFER bObjectType FOR xref_ObjectType.
  
  /* create dir FOR EACH ObjectType if needed */
  FOR EACH bObjectType NO-LOCK:
    OS-CREATE-DIR VALUE(pcOutputDir + bObjectType.cObjectType).
  END.  

END PROCEDURE. /* createFolders */


PROCEDURE writeFooter :
  /* Write HTML footer
  */
  DEFINE INPUT PARAMETER picDicWeb AS CHARACTER NO-UNDO.

  PUT STREAM sWrite UNFORMATTED
    SKIP '<P>'
    SKIP '<TABLE border=0 width=100%>'
    SKIP '  <TR> '
    SKIP '    <TD width=50% align=left><small><A HREF="#Begin">Go to top of document</A></small></TD>'
    SKIP '    <TD width=50% align=right><small><a href="../index.html" target=_top>Go home</a></small></TD>'
    SKIP '  </tr> '
    SKIP '</table>'
    SKIP '<p>'
    SKIP '<small>Generated ' DAY(TODAY) ' ' ENTRY(MONTH(TODAY),'jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec') ' ' YEAR(TODAY) ' ' STRING(TIME,'hh:mm:ss') '</small>'
    SKIP '</body>'
    SKIP '</html>'
    .

END PROCEDURE. /* writeFooter */


PROCEDURE writeFrameset :
  /* Write frameset
  */
  OUTPUT STREAM sWrite TO VALUE(pcOutputDir + 'index.html').

  RUN writeHeader(INPUT 'Cross reference', INPUT NO).

  PUT STREAM sWrite UNFORMATTED
    SKIP '<FRAMESET rows="80,*" border=0>'
    SKIP '  <Frame Name="nav"  Src="program/index.html" >'
    SKIP '  <Frame Name="body" Src="" >'
    SKIP '</FrameSet>'
    SKIP '</html>'
    .
  OUTPUT STREAM sWrite close.

END PROCEDURE. /* writeFrameset */


PROCEDURE writeHeader :
  /* Write HTML header
  */
  DEFINE INPUT PARAMETER pcTitle      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER plFullHeader AS LOGICAL   NO-UNDO.

  PUT STREAM sWrite UNFORMATTED
         '<HTML> '
    SKIP '<HEAD> '
    SKIP '  <TITLE>' pcTitle '</TITLE> '
    SKIP '  <META NAME="author"      CONTENT="Patrick Tingen"> '
    SKIP '  <META NAME="copyright"   CONTENT="Copyright ' YEAR(TODAY) '"> '
    SKIP '  <META NAME="date"        CONTENT="' DAY(TODAY) '-' MONTH(TODAY) '-' YEAR(TODAY) ' ' STRING(TIME,'hh:mm') 'h"> '
    SKIP '  <META NAME="generator"   CONTENT="' PROGRAM-NAME(2) ' v1.0'  '"> '
    SKIP '  <LINK REL="stylesheet" TYPE="text/css" HREF="../styles.css"> '
    .

  IF plFullHeader THEN 
  PUT STREAM sWrite UNFORMATTED
    SKIP '</HEAD> '
    SKIP '<BODY>'
    SKIP '<A NAME="Begin"> </A>'
    .

END PROCEDURE. /* writeHeader */


PROCEDURE writeNavigationPages :
  /* Write html page per objecttype
  */
  DEFINE BUFFER bObjectType  FOR xref_ObjectType.
  DEFINE BUFFER bObjectType2 FOR xref_ObjectType.
  DEFINE BUFFER bObject      FOR xref_Object.

  /* navigation */
  FOR EACH bObjectType NO-LOCK:
          
    OUTPUT STREAM sWrite TO VALUE(pcOutputDir + bObjectType.cObjectType + '\' + 'index.html').
    RUN writeHeader(INPUT 'Cross reference: ' + bObjectType.cObjectType, INPUT NO).
    PUT STREAM sWrite UNFORMATTED
      SKIP '</HEAD> '
      SKIP '<BODY Class=body OnLoad="parent.frames[1].location.href=~'list.html~'" link=#000066 alink=#000066 vlink=#000066>'
      SKIP '<A NAME="Begin"> </A>'.
      
    /* navigation bar */
    PUT STREAM sWrite UNFORMATTED
      SKIP '<form method="POST">'
      SKIP '<TABLE CellPadding=3 CellSpacing=3> <TR Class=navbar>'.
  
    FOR EACH bObjectType2 NO-LOCK BY bObjectType2.cObjectType:
      IF ROWID(bObjectType) <> ROWID(bObjectType2) THEN 
        PUT STREAM sWrite UNFORMATTED
          SKIP '  <td> <A href="../' bObjectType2.cObjectType '/index.html">' bObjectType2.cObjectType ' </a></td>'.
      ELSE
        PUT STREAM sWrite UNFORMATTED
          SKIP '  <td> ' bObjectType2.cObjectType ' </td>'.
    END.
  
    /* open pull-down */
    PUT STREAM sWrite UNFORMATTED
      SKIP '  <TD> '
      SKIP '  <SELECT name="place" size="1" '
      SKIP '          onChange="parent.frames[1].location.href=document.forms[0].place.options[document.forms[0].elements[0].selectedIndex].value">'
      SKIP '    <OPTION selected value="list.html"> select ' bObjectType.cObjectType '</option>'
      SKIP .
  
    /* add all objects of this type to the pull-down */
    FOR EACH bObject NO-LOCK WHERE bObject.cObjectType = bObjectType.cObjectType BY bObject.cObjectName:
      PUT STREAM sWrite UNFORMATTED
         SKIP '    <OPTION value="../' + getObjectUrl(BUFFER bObject) + '" >' bObject.cObjectName '</option>'.
    END. /* FOR EACH object */
  
    /* close pull-down */
    PUT STREAM sWrite UNFORMATTED
      SKIP '  </select> </td>'
      SKIP '  </tr></table>'
      SKIP '</form>  '.
  
    OUTPUT STREAM sWrite CLOSE.

    RUN writeObjectTypePage(bObjectType.cObjectType).   
  END.

END PROCEDURE. /* writeNavigationPages */


PROCEDURE writeObjectPage :
  /* Write HTML page for an xref object
  */
  DEFINE PARAMETER BUFFER bObject FOR xref_Object.

  DEFINE BUFFER bParentObject FOR xref_Object.
  DEFINE BUFFER bChildObject  FOR xref_Object.
  DEFINE BUFFER bRelation     FOR xref_relation.

  DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.

  cFile = pcOutputDir + getObjectUrl(BUFFER bObject).
  OUTPUT STREAM sWrite TO VALUE(cFile).

  RUN writeHeader (SUBSTITUTE('Cross reference of &1 &2', bObject.cObjectType, bObject.cObjectName), YES).

  PUT STREAM sWrite UNFORMATTED
    SKIP SUBSTITUTE('<P Class=title>&1 &2</P>', bObject.cObjectType, bObject.cObjectName)
    SKIP '<TABLE CellPadding=3 CellSpacing=3 width=100%>'
    SKIP '  <TR Class=header> <TD width=30%>Parent</td> <TD width=20%>Relation</td> <TD width=50%>Child</td> </tr>'
    SKIP.

  FOR EACH bRelation NO-LOCK
   WHERE (bRelation.cParentType = bObject.cObjectType AND bRelation.cParentName = bObject.cObjectName)
      OR (bRelation.cChildType  = bObject.cObjectType AND bRelation.cChildName  = bObject.cObjectName)
    , EACH bParentObject NO-LOCK WHERE bParentObject.cObjectType = bRelation.cParentType AND bParentObject.cObjectName = bRelation.cParentName
    , EACH bChildObject  NO-LOCK WHERE bChildObject.cObjectType  = bRelation.cChildType  AND bChildObject.cObjectName  = bRelation.cChildName
   BREAK BY bRelation.cRelationType
         BY bParentObject.cObjectName
         BY bChildObject.cObjectName:

    PUT STREAM sWrite UNFORMATTED
      '  <TR Class=detail>'.

    /* Is the object itself the parent? */
    IF (bObject.cObjectType = bParentObject.cObjectType AND bObject.cObjectName = bParentObject.cObjectName ) THEN
      PUT STREAM sWrite UNFORMATTED
        '      <td>' (IF FIRST-OF(bParentObject.cObjectName) THEN bParentObject.cObjectName ELSE '&nbsp;') '</td>' SKIP.
    ELSE
      PUT STREAM sWrite UNFORMATTED
        '      <td> <A href="../' getObjectUrl(BUFFER bParentObject) '" alt="' bParentObject.cObjectName '"> '
               bParentObject.cObjectName '</a></td>' SKIP.

    /* type of relation */
    PUT STREAM sWrite UNFORMATTED
        '      <td>' (IF FIRST-OF(bRelation.cRelationType) THEN bRelation.cRelationType ELSE '&nbsp;') '</td>' SKIP.

    /* Is the object itself the child? */
    IF (bObject.cObjectType = bChildObject.cObjectType AND bObject.cObjectName = bChildObject.cObjectName ) THEN
      /* just the name */
      PUT STREAM sWrite UNFORMATTED
        '      <td>' bChildObject.cObjectName '</td></tr>' SKIP.
    ELSE
      PUT STREAM sWrite UNFORMATTED
        '      <td> <A href="../' getObjectUrl(BUFFER bChildObject) '" alt="' bChildObject.cObjectName '"> '
               bChildObject.cObjectName '</a></td></tr>' SKIP.
  END.

  PUT STREAM sWrite UNFORMATTED
    SKIP '</table>'.

  RUN writeFooter(bObject.cObjectName).

  OUTPUT STREAM sWrite CLOSE.
END PROCEDURE. /* writeObjectPage */


PROCEDURE writeObjectTypePage :
  /* Screen with all objects of one type 
  */
  DEFINE INPUT PARAMETER pcObjectType AS CHARACTER NO-UNDO.

  DEFINE VARIABLE iColumn AS INTEGER NO-UNDO.
  DEFINE BUFFER bObject FOR xref_Object.

  OUTPUT STREAM sWrite TO VALUE(pcOutputDir + pcObjectType + '\' + 'list.html').
  RUN writeHeader(INPUT 'Cross reference: ' + pcObjectType, INPUT YES).

  PUT STREAM sWrite UNFORMATTED
    SKIP '<P Class=title> List of ' pcObjectType '-objects. </P>'
    SKIP '<TABLE CellPadding=3 CellSpacing=3 width=100%>'
    SKIP '<TR Class=detail>'.

  FOR EACH bObject NO-LOCK WHERE bObject.cObjectType = pcObjectType BY bObject.cObjectName:
    PUT STREAM sWrite UNFORMATTED
      SKIP '  <TD> <A href="../' getObjectUrl(BUFFER bObject) '" alt="' bObject.cObjectName '"> ' bObject.cObjectName ' </td>'.

    ASSIGN iColumn = iColumn + 1.
    IF iColumn MODULO 3 = 0 THEN 
      PUT STREAM sWrite UNFORMATTED SKIP '</tr>' SKIP '<tr Class=detail>'.
  END. /* FOR EACH object */

  PUT STREAM sWrite UNFORMATTED 
    SKIP '</tr>'
    SKIP '</table>'.

  RUN writeFooter('').
  
  OUTPUT STREAM sWrite CLOSE.
END PROCEDURE. /* writeObjectTypePage */


PROCEDURE writeStyleSheet :
  /* Write the stylesheet
  */
  OUTPUT STREAM sWrite to value(pcOutputDir + 'styles.css').

  PUT STREAM sWrite UNFORMATTED
    SKIP 'body    ~{ background-color:#FAFAFA; color:#000000; font-size:8pt; font-family: Arial; } '
    SKIP '.navbar ~{ background-color:#E4E4E4; color:#000000; font-weight:bold; font-size:80% }    '
    SKIP '.title  ~{ background-color:#000066; color:#FFFF00; font-size:250% }                     '
    SKIP '.header ~{ background-color:#00CCFF; color:#000000; font-weight:bold }                   '
    SKIP '.detail ~{ background-color:#F0F0F0; color:#000000 }                                     '
    .
  OUTPUT STREAM sWrite close.

END PROCEDURE. /* writeStyleSheet */


FUNCTION getObjectUrl RETURNS CHARACTER
  ( BUFFER buf_object FOR xref_object ) :
  /* 
  ** Create a unique url based on the type and name of an object,
  ** relative to the html directory. Note: without preceding '../'
  */
  DEFINE VARIABLE cName AS CHARACTER NO-UNDO.
  
  cName = buf_object.cObjectName.
  cName = REPLACE( cName,'/','\' ).
  cName = REPLACE( cName,'~~n','' ).
  cName = ENTRY(NUM-ENTRIES(cName,'\'),cName,'\').
  
  RETURN SUBSTITUTE( "&1\&2.html", buf_object.cObjectType, cName ).

END FUNCTION. /* getObjectUrl */
