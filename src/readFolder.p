/*------------------------------------------------------------------------
    File        : readFolder.p
    Purpose     : Import all Xref XML files from a folder into the xrefdb

    Changes: 
    27-11-2017 Patrick Tingen Created
  ----------------------------------------------------------------------*/

/* Timer temp-table */
DEFINE TEMP-TABLE tt_timer NO-UNDO RCODE-INFORMATION
  FIELD Seq           AS INTEGER   
  FIELD Timer_Name    AS CHARACTER FORMAT "X(60)"       
  FIELD Num_Runs      AS INTEGER   FORMAT ">>>>>>9"     
  FIELD Start_Time    AS DECIMAL    
  FIELD Total_Time_ms AS DECIMAL   FORMAT ">>>>>>>>>9"
  FIELD Avg_Time_ms   AS DECIMAL   FORMAT ">>>>>>9.99"
  FIELD Is_Active     AS LOGICAL           
  INDEX idxName Timer_Name.
	
DEFINE TEMP-TABLE ttFile NO-UNDO
  FIELD cFullName AS CHARACTER.

DEFINE VARIABLE gcRefTypes AS CHARACTER   NO-UNDO.
DEFINE STREAM dbg.

/* Subscribe to the timers */
SUBSCRIBE TO "timerStart" ANYWHERE.
SUBSCRIBE TO "timerStop"  ANYWHERE.
SUBSCRIBE TO "debugInfo"  ANYWHERE.
SUBSCRIBE TO "NewReferenceType" ANYWHERE.

OUTPUT STREAM dbg TO VALUE(SUBSTITUTE("&1\XrefLoad_&2.log", SESSION:TEMP-DIRECTORY, ISO-DATE(TODAY))) APPEND.

/* RUN emptyDb. */
RUN readFolder('c:\Data\progress\xref\xml\').
RUN readFiles('MyApplicationName').
RUN showTimers.
PUBLISH 'debugInfo'(SUBSTITUTE('Unsupported reference types: &1', gcRefTypes)).

OUTPUT STREAM dbg CLOSE.

/* Implementation */
PROCEDURE emptyDb:                   
  DELETE FROM xref_Environment. 
  DELETE FROM xref_Object.
  DELETE FROM xref_ObjectType.
  DELETE FROM xref_Relation.
  DELETE FROM xref_RelationType.
  DELETE FROM xref_XmlFile.  
END PROCEDURE. 

PROCEDURE readFolder:
  /* Read files in folder and process subfolders 
  */
  DEFINE INPUT PARAMETER pcFolder AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE cFile     AS CHARACTER NO-UNDO EXTENT 3.
  DEFINE VARIABLE cModified AS CHARACTER NO-UNDO.
  DEFINE BUFFER b_XmlFile FOR xref_XmlFile.
  
  MESSAGE 'Reading folder' pcFolder.
  INPUT FROM OS-DIR(pcFolder).
  REPEAT:       
    IMPORT cFile.
    IF cFile[1] BEGINS '.' THEN NEXT. 
                         
    IF cFile[3] BEGINS 'D' THEN 
      RUN readFolder(cFile[2]).
    ELSE 
    DO:
      /* If file is unmodified, skip it */
      FIND b_XmlFile NO-LOCK
        WHERE b_XmlFile.cFullName = cFile[2]
              NO-ERROR.
              
      FILE-INFO:FILE-NAME = cFile[2].
      cModified = SUBSTITUTE('&1 &2', ISO-DATE(FILE-INFO:FILE-MOD-DATE), STRING(FILE-INFO:FILE-MOD-TIME,'hh:mm:ss')).
      
      IF NOT AVAILABLE b_XmlFile 
        OR b_XmlFile.cLastModified < cModified THEN 
      DO:  
        CREATE ttFile.
        ASSIGN ttFile.cFullName = cFile[2].
      END.
    END.
  END.              
  INPUT CLOSE. 
END PROCEDURE. /* readFolder */    


PROCEDURE readFiles:
  /* Read the XML files 
  */
  DEFINE INPUT PARAMETER pcEnv AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE iTotal AS INTEGER NO-UNDO.
  DEFINE VARIABLE iFile  AS INTEGER NO-UNDO.
  
  FOR EACH ttFile:
    iTotal = iTotal + 1.
  END. 

  FOR EACH ttFile:
    iFile = iFile + 1.
    PUBLISH 'timerStart' ('loadXref.p').
    RUN c:\Data\progress\xref\src\loadXref.p(pcEnv, ttFile.cFullName).
    PUBLISH 'timerStop' ('loadXref.p').
    MESSAGE SUBSTITUTE('&1 / &2 &3', iFile, iTotal, ttFile.cFullName).
    PROCESS EVENTS. 
  END.

END PROCEDURE. /* readFiles */


PROCEDURE NewReferenceType:
  /* Catch reference types that are not handled in the load procedure 
  */
  DEFINE INPUT PARAMETER pcRefType AS CHARACTER NO-UNDO.
  IF LOOKUP(pcRefType,gcRefTypes) = 0 THEN
    gcRefTypes = gcRefTypes + ',' + pcRefType.
    
END PROCEDURE. /* NewReferenceType */


/* Implement event handlers */
PROCEDURE timerStart:
  DEFINE INPUT PARAMETER pcTimer AS CHARACTER NO-UNDO.
  DEFINE BUFFER bf_timer FOR tt_timer.
                        
  FIND bf_timer WHERE bf_timer.Timer_Name = pcTimer NO-ERROR.
  IF NOT AVAILABLE bf_timer THEN
  DO:
    CREATE bf_timer.
    ASSIGN bf_timer.Timer_Name = pcTimer.
  END.

  ASSIGN 
    bf_timer.Num_Runs   = bf_timer.Num_Runs + 1
    bf_timer.Start_Time = MTIME
    bf_timer.Is_Active  = TRUE.
END PROCEDURE. /* timerStart */


PROCEDURE timerStop:
  DEFINE INPUT PARAMETER pcTimer AS CHARACTER NO-UNDO.
  DEFINE BUFFER bf_timer FOR tt_timer.
  
  FIND bf_timer WHERE bf_timer.Timer_Name = pcTimer NO-ERROR.
  IF AVAILABLE bf_timer AND bf_timer.Is_Active THEN 
    ASSIGN 
      bf_timer.Total_Time_ms = bf_timer.Total_Time_ms + (MTIME - bf_timer.Start_Time)
      bf_timer.Avg_Time_ms   = bf_timer.Total_Time_ms / bf_timer.Num_Runs
      bf_timer.Is_Active     = FALSE.
END PROCEDURE. /* timerStop */


PROCEDURE showTimers.
  DEFINE BUFFER bf_timer FOR tt_timer.
  
  PUBLISH 'debugInfo' ('Timername                                | NumRuns | AvgTime | TotTime |').
  PUBLISH 'debugInfo' ('-----------------------------------------+---------+---------+---------+').

  FOR EACH bf_timer BY bf_timer.Timer_Name:
    PUBLISH 'debugInfo' (SUBSTITUTE('&1 | &2 | &3 | &4 |'
                                   , STRING(bf_timer.Timer_Name,'x(40)')
                                   , STRING(bf_timer.Num_Runs,'>>>,>>9')
                                   , STRING(bf_timer.Avg_Time_ms,'>>>,>>9')
                                   , STRING(bf_timer.Total_Time_ms,'>>>,>>9')
                                   )).
  END.   
  PUBLISH 'debugInfo' ('-----------------------------------------+---------+---------+---------+').
  
END PROCEDURE. /* showTimers */


PROCEDURE debugInfo:
  DEFINE INPUT PARAMETER pcMessage AS CHARACTER   NO-UNDO.
  PUT STREAM dbg UNFORMATTED STRING(TIME,'HH:MM:SS') ' ' pcMessage SKIP.
END PROCEDURE. /* debugInfo */  
