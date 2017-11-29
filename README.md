# xref
A database to save your Progress / OpenEdge xref information

### Setup
- Download all files to your computer
- Open a proenv command prompt
- Go to the DB folder and start create_db.cmd

### Fill database
Compile your application and make sure that you do it with the XREF-XML option set. 
You can save the xref files to a separate folder if you like, but it does not really matter.
Start a session and set your propath to the SRC folder
Then load and run the program 'readFolder.p'. You might need to change the folder and your application name.

Wait until the data is loaded. This might take a while. 

Then use a tool like [DataDigger](https://github.com/patrickTingen/DataDigger) to explore the data. 

Have fun!
