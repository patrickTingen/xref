/* Check unused schema elements
*/
DEFINE VARIABLE cName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iCount   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMinimum AS INTEGER   NO-UNDO INITIAL 0.

FORM 
  cType   FORMAT 'x(10)'
  cName   FORMAT 'x(40)'
  iCount  FORMAT 'zzzzzz'
  WITH FRAME fMain DOWN STREAM-IO.

OUTPUT TO c:\temp\xref-schema-check.txt.

/* Infrequently used tables 
*/
FOR EACH dictdb._file
  WHERE dictdb._file._file-num > 0
    AND dictdb._file._file-num < 32000:

  cName = "dictdb." + _file._file-name.
  iCount = 0.
  cType = 'File'.

  FOR EACH xref_relation NO-LOCK
    WHERE xref_relation.cParentType   = 'Program'
      AND xref_relation.cRelationType = 'PROG-FILE'
      AND xref_relation.cChildName    = "dictdb." + _file._file-name:
    iCount = iCount + 1.
  END.

  IF iCount <= iMinimum THEN 
  DO:
    DISPLAY cType cName iCount WITH FRAME fMain.
    DOWN WITH FRAME fMain.
  END.
END. 
PUT UNFORMATTED SKIP(1).

/* Infrequently used fields
*/
FOR EACH dictdb._file
  WHERE dictdb._file._file-num > 0
    AND dictdb._file._file-num < 32000:

  FOR EACH dictdb._field OF dictdb._file:
    cName  = "dictdb." + _file._file-name + "." + _field._field-name.
    iCount = 0.
    cType  = 'Field'.

    FOR EACH xref_relation NO-LOCK
      WHERE xref_relation.cParentType   = 'Program'
        AND xref_relation.cRelationType = 'ACCESS'
        AND xref_relation.cChildName    = cName:
      iCount = iCount + 1.
    END.

    IF iCount <= iMinimum THEN 
    DO:
      DISPLAY cType cName iCount WITH FRAME fMain.
      DOWN WITH FRAME fMain.
    END.
  END. 
END. 
PUT UNFORMATTED SKIP(1).

/* Infrequently used indexes
*/
FOR EACH dictdb._file
  WHERE dictdb._file._file-num > 0
    AND dictdb._file._file-num < 32000:

  FOR EACH dictdb._index OF dictdb._file:
    cName  = "dictdb." + _file._file-name + "." + _index._index-name.
    iCount = 0.
    cType  = 'Index'.

    FOR EACH xref_relation NO-LOCK
      WHERE xref_relation.cParentType = 'Program'
        AND xref_relation.cChildName = cName:
      iCount = iCount + 1.
    END.

    IF iCount <= iMinimum THEN 
    DO:
      DISPLAY cType cName iCount WITH FRAME fMain.
      DOWN WITH FRAME fMain.
    END.
  END.
END.

OUTPUT CLOSE. 
OS-COMMAND NO-WAIT START 'c:\temp\xref-schema-check.txt'.
