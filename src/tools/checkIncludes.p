/* CheckIncludes.p
*/
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

OUTPUT TO c:\temp\xref-check-includes.txt.

FOR EACH xref_object NO-LOCK
  WHERE xref_object.cObjectType = 'include':

  iCount = 0.

  FOR EACH xref_relation NO-LOCK
    WHERE xref_relation.cChildType = 'Include'
      AND xref_relation.cChildName = xref_object.cObjectName: 
    iCount = iCount + 1.
  END. 

  IF iCount < 3 THEN 
    DISPLAY xref_object.cObjectName WITH STREAM-IO.

END. 
PUT UNFORMATTED SKIP(1).


OUTPUT CLOSE. 
OS-COMMAND NO-WAIT START 'c:\temp\xref-check-includes.txt'.