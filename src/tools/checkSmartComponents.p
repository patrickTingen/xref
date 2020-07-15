/* CheckSmartComponents.p
**
** Find unused smart components
*/
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

OUTPUT TO c:\temp\xref-check-smartcomponents.txt.

FOR EACH xref_object NO-LOCK
  WHERE (xref_object.cObjectType = 'Program' AND xref_object.cObjectName MATCHES '*-v*.w')
     OR (xref_object.cObjectType = 'Program' AND xref_object.cObjectName MATCHES '*-b*.w')
     OR (xref_object.cObjectType = 'Program' AND xref_object.cObjectName MATCHES '*-q*.w')
  :

  iCount = 0.

  FOR EACH xref_relation NO-LOCK
    WHERE xref_relation.cChildType = 'Program'
      AND xref_relation.cChildName = xref_object.cObjectName: 
    iCount = iCount + 1.
  END. 

  IF iCount = 0 THEN 
    DISPLAY xref_object.cObjectName iCount WITH STREAM-IO.

END. 
PUT UNFORMATTED SKIP(1).


OUTPUT CLOSE. 
OS-COMMAND NO-WAIT START 'c:\temp\xref-check-smartcomponents.txt'.