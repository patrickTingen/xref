/* Create a new database if it does not already exist */
DEFINE VARIABLE cDatabase AS CHARACTER NO-UNDO INITIAL 'xrefdb.db'.
DEFINE VARIABLE lReplace  AS LOGICAL   NO-UNDO.

IF SEARCH(cDatabase) <> ? THEN 
DO:
  ASSIGN FILE-INFO:FILE-NAME = cDatabase.
  MESSAGE "Database" FILE-INFO:FULL-PATHNAME "already exists." SKIP(1)
          "Do you want to replace it?"
          VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE lReplace.
  IF lReplace = FALSE THEN QUIT.
END.

CREATE DATABASE 'xrefdb' FROM OS-GETENV('dlc') + '\empty8.db' REPLACE.
ASSIGN FILE-INFO:FILE-NAME = cDatabase.
CONNECT 'xrefdb' -1.
RUN prodict/load_df.p (INPUT "xrefdb.df").

MESSAGE "Database" FILE-INFO:FULL-PATHNAME "created" VIEW-AS ALERT-BOX INFO.
  
QUIT.