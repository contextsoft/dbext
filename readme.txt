================================================================
Context Database Extensions Suite 2 for Delphi 5,6,7,2005,2006
Copyright (C) 2003-2006, Michael Baytalsky
All Rights Reserved
----------------------------------------------------------------
Version: 2.07


   WHAT'S IN THIS FILE
   ===================
   I. ABOUT DATABASE EXTENSIONS SUITE 2
   II. HISTORY OF RELEASES
   III. INSTALLATION INSTRUCTIONS
   IV. REGISTRATION

------------------------------------------
I. ABOUT DATABASE EXTENSIONS SUITE 2

Context Database Extensions Suite is desinged to enhance database
related functionality provided by Borland Delphi VCL library. It
combines some generic imporvements as well as improvements
pertaining to a specific implementation of base VCL classes designed
for navigational database access. Context Database Extensions
supports two types of database adapters: generic & extended.
Generic adapters provide basic database access functionality,
like transaction management, executing a query or creating 
an instance of engine specific TDataSet descendant. This package
contains following generic adapters: 

 - BDE (Borland Database Engine, see BDEExt.pas unit);
 - ADO (Access Data Objects, see ADOExt.pas unit);
 - DBX (DBExpress, see SQLExt.pas unit);
 - and IB (Interbase components, see IBExt.pas unit);

Extended adapters beside basic functionality also provide 
client-side implementation of referential integrity (RI) constraints, 
client-side triggers, change logging (used for database replications), 
additional events and properties and more. This functionality
is being partially deprecated and we slowely phasing it out
as more and more databases begin to support native server-side
RI constraints, triggers, stored procs and such. Currently,
extended adapters are provided for:

 - DBISAM Database Engine (versions 3 & 4) by Elevate Software (www.elevatesoft.com)
 - NexusDB (versions 1 & 2) by Nexus Database Systems Pty Ltd. (www.nexusdb.com)

For compatibility with different versions of database engines,
these adapters come in source code and located in adapters
subfolder.

Context Database Extensions packages Context Database Designer, 
used as a component designer for TDatabaseSchema component.
A license for 1 copy of Context Database Designer is included 
into the license for Context Database Extensions. For more
information about Context Database Designer, please visit our
web site at: http://www.contextsoft.com/products/dbdesign

The main features, provided by Context Database Extensions are:

    - Persistent Database Schema. Database Schema contains the
    description of tables, relations between them as well as triggers,
    domains, views, stored procedures, generators/sequences
    and sql statements that should be executed to update a database to 
    the latest version required by the application. Database
    Schema may be stored within application, in the database
    (System tables) or as an external file.

    - Integrated Database Designer is registered as a component
    editor for TDatabaseSchema component. It gives access for 
    feature-rich diagramming tool right from Delphi IDE. 

    - Includes a number of usefull database components, like
    memory tables, enumerations and sequences.

    - Provides engine-specific highly customzable SQL generation 
    functionality via TDBEngineProfile component.

    - Provides classes for business logic incapsulation, like
    TDataSetReference, TDBDocument, TDBDocumentType & TDBJournal.
   

Extended adapters also provide the following additional functionality:

    - RI Constraints. Relations between tables, defined within
    database schema, allows to implement various types of referential
    integrity (RI) constrains (Error, Cascade and Nullify for delete
    & update operations). Please, note, that processing of RI
    constraints might sometimes considerably slow down some delete
    and update operations. 
    
    - Replications. All changes made to records can be automatically
    written into a separate table ('Objects' tables) and used for
    database replications. Replication & serialization functionality
    is encapsulated into TDBManager component, using TxxDatabaseExt
    components as database adapters.
    
    - Additional events and properties provided for database, table
    and query components (see ctxdbext.hlp for more details).

Please see Demo projects included in this package to learn how to
add these features to your projects by replacing 
TxxxTable, TxxxQuery & TxxxDatabase, components with
TxxxTableExt, TxxxQueryExt & TxxxDatabaseExt components.


II. HISTORY OF RELEASES
=======================

  06/26/2006  v2.07  Fixed minor problems is sql import.

  06/20/2006  v2.06  Restored StartTransaction method interface for dbisam 4.


  05/29/2006  v2.05  Minor improvements and bug fixes in dbSchema.pas unit;
                     Added select stataments execution;
                     Added DSDFileName property to TDatabaseSchema (design-time only);
                     Added LibVersion property to TDatabaseSchema (design-time only);

  04/29/2006  v2.04  Improved connectivity with schema component in IDE;
                     Published collections made read-only for TDatabaseSchema.
                     This prevents schema objects from being modified from IDE;
                     Fixed minor bugs;

  04/04/2006  v2.03  First final release of Database Extensions v.2.
                     Fixed problem with TDbMemDataSet incorrectly
                     writing Blob data when saving to binary file.
                     Added support for Delphi 2006.
                     Added support for Nexus 2.

  12/19/2005  v2.02  Second beta release of Database Extensions v.2.
                     Added dbSQLLexer unit, with generic SQL lexer & parser 
                     implementation. This allows and run complex scripts
                     SQL scripts against engines, that do not support 
                     multi-statement batches.
                     Changed the model of relationships implementation.
                     All relationships are stored within Relationships 
                     collection of TDatabaseSchema. Each table also contains
                     TRelation objects (compatible in interface with same 
                     TRelation(s) in prior releases of extensions) build 
                     automatically from TRelationship(s). This helps avoiding 
                     creation of duplicate and mismatching pairs of relations.
                     Added TTableConstraints collection, holding table level
                     check constraints (TTableConstraint).
                     Added TIndexFields collection for TIndexDefinition class,
                     holding individual index fields and their extended properties.
                     
  10/31/2005  v2.01  First beta release of Database Extensions v.2
                     Most of the extended client-side functionality,
                     like replication and serialization has been moved
                     to TDBManager component and dbExtUtils unit.
                     Added TDBEngineProfile component that can be used
                     to generate various SQL scripts based on database
                     engine profile (see .dbp files included with 
                     Context Database Designer under 'dbdefs' folder).
                     TDatabaseSchema component is enhanced to contain
                     for types of metadata, like domains, views, 
                     sequences and stored procedures.
                     Objects contained in schema extended to have custom
                     (user defined & engine specific) properties.
                     Added generic adapters for BDE, ADO, DBX, IB
                     database access.

  09/21/2005  v1.70  Restored compatibility with DBISAM 4.21

  03/21/2005  v1.60  Restored compatibility with DBISAM 4.18
                     Fixed problem with Diagram Editor viewd in large fonts.
                     Fixed problem with dbEnumeration lookup for more then
                     one field.                     

  11/21/2004  v1.50  Added support for NexusDB database. 
                     Code refactoring now allows adding support for
                     database engines.
                     Added database functions (reverse engineer,
                     restructure, etc.) to Schema Editor.

  06/28/2004  v1.31  Fixed several issues in Database Schema Editor.

  06/11/2004  v1.30  Restored compatibility with DBISAM v.4.08 by
                     using conditional defines in DBISAMExt to remedy
                     changed in StartTransaction method interface.

  03/17/2004  v1.22  {fSchemaEditor:}
                     Bugs fixed.
                     {Demos:}
                     Fixed minor problem, causing erros when working with
                     regional settings where floating numbers are separated
                     by comma ('4,2') rather then by a dot ('4.2').

  02/19/2004  v1.21  {DBISAMExt:}
                     Fixed bug in WriteObjectChanges method in
                     TxxxDatabaseExt component.
                     {dbSchema:}
                     Fixed bugs related to assigning different types
                     of objects to each other.
                     {dbRecord:}
                     Added TDBRecord component that implements a generic
                     one-record dataset that could be used to store form's
                     data in memory. It is usefull to implement forms using
                     only db-aware controls with less coding.
                     TDBRecord component is database independant. 
                     {dbSequence:}
                     Added TDBSequences component implementing generic sequences, that
                     could be used as unque key generators (similar to Oracle's
                     sequences). TDBSequence component is database independant. 
                     {fSchemaEditor:}
                     Fixed several minor problems in Schema Editor, 
                     improved importing functionality allowing importing
                     parts of schema (including tables, enumerations and
                     diagram pages), which makes it easy creating business
                     objects repository.
                     Added demos for dbRecord and dbSequences components.

  01/23/2004  v1.20  Fixed bug with the transaction not being started before
                     executing some triggers.
                     Fixed small bugs in calculations of aggregated fields.
                     Fixed minor problems in the Schema Editor.
                     Added schema analysis to the Schema Editor to report
                     inconsistencies within schema (dbSchemaTest.pas unit).
                     Extensively tested compatibility with DBISAM v4.00.

  12/10/2003  v1.19  {DBISAMExt:}
                     Added support for TriggerActive property for triggers. This property
                     determines whether the trigger is active always, only
                     during database replication, except during database replication
                     or never. Changed the way triggers and calculated fields are
                     processed during replication.
                     {dbSchema:}
                     Added TriggerActive property for triggers. This property
                     determines whether the trigger is active always, only
                     during database replication, except during database replication
                     or never. Some of the generic code as well as declaration of
                     IDatabaseExt interface is moved to dbSchema. This allowed to 
                     deprecate TDBDocumentStorage component which is no longer
                     required and completely replaced by TXXXDatabaseExt component.
                     Added compatibility with DBISAM V4.

  09/18/2003  v1.18  Added support for Expressions in Condition and ForeignCondition fields
                     (before Condition only supported one boolean field)
                     Added support for auto-calculated sum & count aggregated fields based on
                     expressions. See TFieldDefinition.AggregateExpression property.
                     This makes possible to evaluate pretty complex expressions, like
                     Orders.OrderTotal ::= sum(OrderLines.Amount * OrderLines.Quantity))
                     where OrderLines.Enabled

                     Changed the way aggregated fields are described. Now
                     Any field may be defined as aggregate based on a certain
                     relation and expression.
                     Added count & sum types of aggregate. If aggregate expression
                     is not specified, count will return number of all records '*'.
                     Relations allow expressions within Condition and
                     ForeignCondition fields.

  07/17/2003  v1.17  Added Save/Load to stream and file methods for table
                     and database. Added Category property for table
                     definitions.

  04/11/2003  v1.16  Added support for macros in TxxxQueryExt
                     Added property FAutoRefreshOnRollback (default False), causing
                     the database to refresh active tables after rollback of transaction
                     Fixed bug in cascade operations for new records.
                     Currently the engine does not process any cascade operations
                     for newly inserted records.

  03/04/2003  v1.15  Added generation of the Delphi declarations
                     corresponding to enumerations and tables described
                     in the database schema. This functionality is implemented
                     as a Component Editor for TDatabaseSchema component.

  02/16/2003  v1.14  Added RestructureFromSchema method allowing to
                     restructure the database according to the
                     information, conatined in schema without loosing data.
                     Added support for enumerations, declared in schema.
                     Enumerations can now be exposed, by TDBSchemaEnum
                     or TDBEnumeration components and used to provide value
                     for lookup fields. Added components supporting
                     Document-Journal processing model: TDBDocument,
                     TDBDocumentType, TDBJournal, TDBDocumentStorage and
                     TxxxDocumentStorage.

  02/07/2003  v1.13  Added suport for DefaultExpression property.
                     Corrected some spelling errors in error messages.

  01/31/2003  v1.12  Fixed problem with the field Abort of TProgressCounter not
                     properly initialized. Also has fixed the problem with
                     invalid CRLN and debugging under D6/7.

  01/16/2003  v1.11  Fixed severe (!) problem with reading field maps from
                     data exchange file (method ImportChanges). Problem
                     resulted in 'No field name found for table ...' error.

  01/10/2003  v1.10  Fixed problem with Temporary tables
                     (property Temporary = True) not being destroyed
                     when the component is destroyed upon closing the form.

  01/07/2003  v1.09  Fixed problem with failing to set KeyFieldCount
                     when assigning range. This problem resulted in
                     constrained being ignored if the number of fields
                     in index is larger, then the number of fields in key.

  12/27/2002  v1.08  Fixed problems with D6&7 compatibility

  12/07/2002  v1.07  *Beta* release

  11/26/2002  v1.06  Finished most of the work on replications

  11/01/2002  v1.05  Fixed small bugs & added replication support features

  10/22/2002  v1.04  Added compatibility with Delphi 6.

  10/22/2002  v1.03  Included TxxxEvents into EBISAMExt package

  10/12/2002  v1.02  Improvements made in cascade Delete/Nullify procedures.

  10/12/2002  v1.01  *Alpha* release


III. INSTALLATION INSTRUCTIONS
==============================
                            
Installing Components
~~~~~~~~~~~~~~~~~~~~~
Before installing this package, please uninstall any previously installed 
versions of Content Database Extensions or DBISAM/Nexus Extensions from 
Delphi IDE. Close Delphi IDE and start it over to ensure, that none of
the prior versions of the above named components are installed.

Please also cleanup $(DELPHI)\Projects\Bpl directory from
files: dbExtPkgDX.bpl, dbExtPkgDX.dcp, DBISAMExtPkgDX.bpl, 
DBISAMExtPkgDX.dcp, nxDBExtPkgDX.bpl, nxDBExtPkgDX.dcp.

Context Database Extensions package will be automatically installed 
into Delphi IDE and all corresponding library and source paths will
be added to Delphi settings. However, the adapters for each particular
database engine will not be added to the Delphi IDE automatically,
because they are supplied in full source code and must be compiled.
See below on how to add these packages manually.

Installing Adapters
~~~~~~~~~~~~~~~~~~~
Additional adapters for DBISAM, Nexus and other databases are located
in the adapters folder. You must install them individually. Package 
files for each version of Delphi are located in source\packages subfolder.

- Use "Tools\Environment Options" menu item of Delphi IDE to 
access Library tab page. Make sure, that path to the 'libdX', 
'source' (if you have version with the full source code)
and 'adapters\xxx\source' folders are added to the
Library Search & Browse Paths settings. It should be added there
automatically during the installation. Default installation
folder (recommended) is C:\Program Files\Context Software\DBExt2

- Use "File\Open..." menu item of Delphi IDE to open design-time package 
xxxExtPkgDx.dpk. 

- In the "Package..." window click "Compile" button to compile the
package and then "Install" button to register the package in the 
component palette. When package is installed the components will be 
added to the 'Database Extensions' page. Delphi will automatically 
add all required, database specific, packages.


Installing Help File
~~~~~~~~~~~~~~~~~~~~
Help file should be automatically integrated into Delphi IDE
during the installation. If you're installing Context Database Extensions
into C++ Builder or BDS you should performs the steps below to register 
the help file manually.

To include a help file into Delphi/BCB IDE help system environment
and make it available in design time you need to make
several steps:

1. Run Borland Delphi/CB, select item Help | Customize... from the
main menu. 

2. Choose Index tab to include index information from ctxdbext.hlp file
into Delphi common help system. Pick Edit | Add Files from menu
and then select ctxdbext.hlp file in the File Open dialog and press OK.
If you have installed other adapters, please locate their corresponding
help files within adapters\<adapter>\help folder and add them also.

3. Choose Link tab and do the same to include help topics into the Delphi
help system as well.

4. Go to Contents tab and add ctxdbext.cnt file.

5. Select File | Save Project from the main menu to save changes you made
and close the OpenHelp utility.

The included help file will be available immediately within Delphi/BCB IDE.


IV. REGISTRATION
================

You may NOT DISTRIBUTE the source code conatined in this package.
You also may not sell, lease or rent these components "as is" or
with any changes. You may freely use it for trial purposes or to
create software for your own use (see license.txt for more
information).

You must REGISTER your copy of the Context Database Extensions Suite in
order to use it to design, develop and deploy commercial
software. In order to register Context Database Extensions Suite, please
visit our web site at http://www.contextsoft.com/products/ctxdbext
and fill in electronic ordering form. If you have questions
or problems accessing our website, please contact me directly
(via e-mail, fax or regular mail) and you will be given
instruction on how to register using other means of communication.

If you have any questions, comments or suggestions, please
contact us at sales@contextsoft.com.

Thank you for choosing Context Database Extensions Suite!
