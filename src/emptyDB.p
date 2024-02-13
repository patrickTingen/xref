/*-----------------------------------------------------------------------
  File : emptyDB.p
  Desc : Clean up all files in the db
  ----------------------------------------------------------------------*/

PAUSE 0 BEFORE-HIDE.

PUBLISH "xref-log" ("Cleaning up database").
PROCESS EVENTS. 

RUN delete_general_tables.
RUN delete_xref_Object.
RUN delete_xref_Relation.

PROCEDURE delete_general_tables:
  DEFINE BUFFER bXref_RelationType FOR xref_RelationType.
  DEFINE BUFFER bXref_ObjectType   FOR xref_ObjectType.
  DEFINE BUFFER bXref_XmlFile      FOR xref_XmlFile.

  FOR EACH bXref_RelationType:
    DELETE bXref_RelationType.
  END.
  
  FOR EACH bXref_ObjectType:
    DELETE bXref_ObjectType.
  END.
  
  FOR EACH bXref_XmlFile:
    DELETE bXref_XmlFile.
  END.
END PROCEDURE. 


PROCEDURE delete_xref_Object:
  DEFINE BUFFER bXref_Object FOR xref_Object.
  DEFINE VARIABLE iDeleted AS INTEGER NO-UNDO.

  REPEAT WHILE CAN-FIND(FIRST bXref_Object):
    DO TRANSACTION:
      FOR EACH bXref_Object:
        DELETE bXref_Object.
        iDeleted = iDeleted + 1.
        IF iDeleted MOD 1000 = 0 THEN LEAVE. 
      END.
      PUBLISH "xref-log" (SUBSTITUTE("&1 xref_Object records deleted", iDeleted)).
      PROCESS EVENTS. 
    END. /* trans */
  END.
END PROCEDURE. 


PROCEDURE delete_xref_Relation:
  DEFINE BUFFER bXref_Relation FOR xref_Relation.
  DEFINE VARIABLE iDeleted AS INTEGER NO-UNDO.

  REPEAT WHILE CAN-FIND(FIRST bXref_Relation):
    DO TRANSACTION:
      FOR EACH bXref_Relation:
        DELETE bXref_Relation.
        iDeleted = iDeleted + 1.
        IF iDeleted MOD 1000 = 0 THEN LEAVE. 
      END.
      PUBLISH "xref-log" (SUBSTITUTE("&1 xref_Relation records deleted", iDeleted)).
      PROCESS EVENTS. 
    END. /* trans */
  END.
END PROCEDURE. 
