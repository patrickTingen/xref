:: Start prowin session, might be 32 or 64 bit version
::
if exist %dlc%\bin\prowin32.exe set prowin=%dlc%\bin\prowin32.exe
if exist %dlc%\bin\prowin.exe set prowin=%dlc%\bin\prowin.exe

%PROWIN% -p create_db.p
