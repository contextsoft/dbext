================================================================
Context Database Extensions Suite 3 for Delphi
Copyright (C) 2003-2018, Michael Baytalsky
All Rights Reserved
----------------------------------------------------------------

   WHAT'S IN THIS FILE
   ===================
   I. ABOUT DATABASE EXTENSIONS SUITE 3
   II. INSTALLATION INSTRUCTIONS
   III. REGISTRATION

------------------------------------------
I. ABOUT DATABASE EXTENSIONS SUITE 3

Context Database Extensions Suite is desinged to enhance database
related functionality provided by Borland Delphi VCL library. It
provides a number of generic imporvements as well as improvements
related to a specific implementation of base VCL classes designed
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

Extended adapters additionally provide client-side implementation 
of referential integrity (RI) constraints, client-side triggers, 
change logging (used for database replications), additional events,
properties and more. This functionality is being partially deprecated 
and we slowely phasing it out as more and more databases begin to 
support native server-side RI constraints, triggers, stored procedures,
etc. Extended adapters are provided in source code and available for 
free download at:

  http://www.contextsoft.com/products/ctxdbext/downloads.jsp

These adapters include:

 - DBISAM Database Engine (versions 3 & 4) by Elevate Software (www.elevatesoft.com)
 - NexusDB (versions 1 & 2) by Nexus Database Systems Pty Ltd. (www.nexusdb.com)
 - Absolute Database by Component Ace (www.componentace.com)
 - FB\IB adapter via FIBPlus components by DevRace (www.devrace.com)
 - MySQL adapter via MyDAC components by Core Lab (www.crlab.com)

For compatibility with different versions of database engines,
these adapters come in full source code. Please, refere to readme.txt
file contained in each adapter archive for information on how to 
install them.

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


II. INSTALLATION INSTRUCTIONS
=============================
                            
Installing Components
~~~~~~~~~~~~~~~~~~~~~
Before installing this package, please uninstall any previously installed 
versions of Content Database Extensions or DBISAM/Nexus Extensions from 
Delphi IDE. Close Delphi IDE and start it over to ensure, that none of
the prior versions of the above named components are installed.

Please also cleanup $(DELPHI)\Projects\Bpl directory from
files: dbExtPkgDX.bpl, dbExtPkgDX.dcp.

Context Database Extensions package will be automatically installed 
into Delphi IDE and all corresponding library and source paths will
be added to Delphi settings. However, the adapters for each particular
database engine will not be added to the Delphi IDE automatically,
because they are supplied in full source code and must be compiled.
See below on how to add these packages manually.

Downloading & Installing Adapters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Additional adapters are available for download at:

  http://www.contextsoft.com/products/ctxdbext/downloads.jsp

Please, refer to readme.txt file included in each package
for more information on how to install the components into
Delphi IDE.

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


III. REGISTRATION
=================

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
