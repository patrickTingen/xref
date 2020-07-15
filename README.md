# xref
A database to save your Progress / OpenEdge xref information

## Setup
- Download all files to your computer
- Open a proenv command prompt
- Go to the DB folder and start create_db.cmd
- Run your driver program


## Create a driver
You need a program that compiles your source code, saves the xref info to XML files and then starts the reading program. Because you need to eat your own dogfood, you can find an example file in the folder 'dogfood' that does exactly that. It compiles the xref tools and feeds it to the database. It assumes you have it installed in c:\data\progress\xref

As a bonus, it generates HTML pages for all relations. This is not needed and in fact, you might prefer consulting the data in other ways. I can highly recommend a tool like [DataDigger](https://github.com/patrickTingen/DataDigger) to explore the data. 

## Sitting, waiting, wishing ...
Wait until the data is loaded. This might take a while. I know. The good news is that it only takes a long time on the first run. If you don't clean up the database, then the load program will simply refresh the data that has changed from the previous time. This is ideal if you change include files. All code will be recompiled, but only the files that use the include will be reloaded. 

To give you some idea: compiling 1400 sources took ~4 minutes on my computer, reading the XML data 12 minutes and generating HTML ~6 minutes. But ymmv.

## Then what?
As said, you can use a tool like DataDigger or the generated HTML pages to find out what is used by whom, but you can also run analysis programs to find unused tables, fields and indexes. In the folder 'tools' you can find a few programs that I use. 

Have fun!
