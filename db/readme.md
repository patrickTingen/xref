# Tables in the db

| Table | Description |
| --- | ---- |
| xref_XmlFile | XML files with xref info |
| xref_ObjectType | Type of xref object (file, field etc) |
| xref_RelationType | Type of relation between 2 xref objects |
| xref_Object | Element in the XREF file (programs, fields, includes etc) |
| xref_Relation | Relation between 2 xref objects |

After compiling, the XML files are read into the xref_XmlFile table and then processed. All references in the XML file are then stored in xref_Relation. When saving this, the master tables xref_ObjectType, xref_RelationType and xref_Object are updated if needed. 

When all is loaded, you should end up with 1 xref_XmlFile record for each .w, .p or .cls file, a handful of object- and relation types, a couple of thousand xref_Object records and a whole lot of xref_Relation records. Depending on your application this can easily run into hundreds of thousands. Typical application:
| Table | Nr of records |
| --- | ----: |
| xref_XmlFile | 1382 |
| xref_ObjectType | 14 |
| xref_RelationType | 34 |
| xref_Object | 9.423 |
| xref_Relation | 185.322 |
