# ---------------------------------------------------------------------------
!if !$d(BCB)
BCB = $(MAKEDIR)\..
!endif

# ---------------------------------------------------------------------------
# IDE SECTION
# ---------------------------------------------------------------------------
# The following section of the project makefile is managed by the BCB IDE.
# It is recommended to use the IDE to change any of the values in this
# section.
# ---------------------------------------------------------------------------

VERSION = BCB.06.00
# ---------------------------------------------------------------------------
PROJECT = dbExtPkgCB6.bpl
OBJFILES = dbExtPkgCB6.obj ..\source\ADOExt.obj ..\source\BDEExt.obj \
    ..\source\CtxDBDesignerAdapter.obj ..\source\CtxDBIntf.obj \
    ..\source\dbDocument.obj ..\source\dbEngProfile.obj ..\source\dbEnum.obj \
    ..\source\dbExtReg.obj ..\source\dbManager.obj ..\source\dbExtParser.obj \
    ..\source\dbRecord.obj ..\source\dbSchema.obj ..\source\dbSchemaEnum.obj \
    ..\source\dbSchemaTest.obj ..\source\dbSequence.obj \
    ..\source\dbSQLLexer.obj ..\source\dbSQLParser.obj ..\source\IBExt.obj \
    ..\source\SQLExt.obj ..\source\CtxDataTypes.obj ..\source\CtxData.obj \
    ..\source\CtxDataSet.obj ..\source\CtxDBAdapter.obj \
    ..\source\CtxDataSetCommand.obj ..\source\CtxDataSetProvider.obj \
    ..\source\designtime\fSelFields.obj \
    ..\source\designtime\fCommandBuilder.obj \
    ..\source\designtime\fDataContainerEditor.obj \
    ..\source\designtime\fDataExplorer.obj \
    ..\source\designtime\fDataRelationEditor.obj \
    ..\source\designtime\fDataTableWizard.obj \
    ..\source\designtime\fDBAdapterEditor.obj \
    ..\source\designtime\fSelectCommandTypes.obj \
    ..\..\common\source\CtxGridView.obj \
    ..\..\common\source\CtxProfiles.obj \
    ..\..\common\source\CtxPropView.obj

RESFILES = dbExtPkgCB6.res ..\source\BDEExt.dcr ..\source\CtxDBDesigner_TLB.dcr \
    ..\source\dbExtReg.dcr ..\source\dbManager.dcr ..\source\dbRecord.dcr \
    ..\source\dbSequence.dcr ..\source\CtxData.dcr

MAINSOURCE = dbExtPkgCB6.cpp
RESDEPEN = $(RESFILES) ..\source\designtime\fSelFields.dfm \
    ..\source\designtime\fCommandBuilder.dfm \
    ..\source\designtime\fDataContainerEditor.dfm \
    ..\source\designtime\fDataExplorer.dfm \
    ..\source\designtime\fDataRelationEditor.dfm \
    ..\source\designtime\fDataTableWizard.dfm \
    ..\source\designtime\fDBAdapterEditor.dfm \
    ..\source\designtime\fSelectCommandTypes.dfm
LIBFILES = 
IDLFILES = 
IDLGENFILES = 
LIBRARIES = bcb2kaxserver.lib dbxcds.lib dclocx.lib dsnapcon.lib dss.lib \
    bdecds.lib vcldbx.lib
PACKAGES = rtl.bpi designide.bpi vcl.bpi dbrtl.bpi vclx.bpi vcldb.bpi designdgm.bpi \
    bdertl.bpi adortl.bpi ibxpress.bpi cds.bpi dsnap.bpi dbexpress.bpi
SPARELIBS = rtl.lib vcldb.lib adortl.lib bdertl.lib vcldbx.lib dbrtl.lib ibxpress.lib \
    cds.lib dsnap.lib bdecds.lib qrpt.lib teeui.lib teedb.lib tee.lib dss.lib \
    teeqr.lib visualdbclx.lib dsnapcrba.lib dsnapcon.lib bcbsmp.lib dclocx.lib \
    dbexpress.lib dbxcds.lib indy.lib bcb2kaxserver.lib
DEFFILE = 
OTHERFILES = 
# ---------------------------------------------------------------------------
DEBUGLIBPATH = $(BCB)\lib\debug
RELEASELIBPATH = $(BCB)\lib\release
USERDEFINES = 
SYSDEFINES = NO_STRICT;USEPACKAGES
INCLUDEPATH = ..\source\designtime;..\source;..;$(BCB)\include;$(BCB)\include\vcl;$(BCB)\Projects\BPL;$(BCB)\Projects\Lib;..\..\common\source
LIBPATH = $(BCB)\Lib;..\source\designtime;..\source;..;..\..\common\source
WARNINGS= -w-par
PATHCPP = .;
PATHASM = .;
PATHPAS = .;..\source;..\source\designtime;..\..\common\source
PATHRC = .;
PATHOBJ = .;$(LIBPATH)
# ---------------------------------------------------------------------------
CFLAG1 = -O2 -Vx -Ve -X- -a8 -5 -b- -k- -vi -c -tWM
IDLCFLAGS = -I..\source\designtime -I..\source -I.. -I..\..\common\source -I$(BCB)\include \
    -I$(BCB)\include\vcl -I$(BCB)\Projects\BPL -I$(BCB)\Projects\Lib \
    -src_suffix cpp -no_stdstream -no_tie -boa
PFLAGS = -$Y- -$L- -$J -$D- -$C- -$A8 -v -JPHNE -M -LUDesignIde -LUDclDB
RFLAGS = 
AFLAGS = /mx /w2 /zn
LFLAGS = -D"Context Database Extensions" -aa -Tpp -Gpd -GD -x -Gn -Gl -Gi -M
# ---------------------------------------------------------------------------
ALLOBJ = c0pkg32.obj $(PACKAGES) sysinit.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) $(LIBRARIES) import32.lib cp32mt.lib ws2_32.lib
# ---------------------------------------------------------------------------
!ifdef IDEOPTIONS

[Version Info]
IncludeVerInfo=0
AutoIncBuild=0
MajorVer=1
MinorVer=0
Release=0
Build=0
Debug=0
PreRelease=0
Special=0
Private=0
DLL=0

[Version Info Keys]
CompanyName=
FileDescription=
FileVersion=1.0.0.0
InternalName=
LegalCopyright=
LegalTrademarks=
OriginalFilename=
ProductName=
ProductVersion=1.0.0.0
Comments=

[Debugging]
DebugSourceDirs=$(BCB)\source\vcl

!endif





# ---------------------------------------------------------------------------
# MAKE SECTION
# ---------------------------------------------------------------------------
# This section of the project file is not used by the BCB IDE.  It is for
# the benefit of building from the command-line using the MAKE utility.
# ---------------------------------------------------------------------------

.autodepend
# ---------------------------------------------------------------------------
!if "$(USERDEFINES)" != ""
AUSERDEFINES = -d$(USERDEFINES:;= -d)
!else
AUSERDEFINES =
!endif

!if !$d(BCC32)
BCC32 = bcc32
!endif

!if !$d(CPP32)
CPP32 = cpp32
!endif

!if !$d(DCC32)
DCC32 = dcc32
!endif

!if !$d(TASM32)
TASM32 = tasm32
!endif

!if !$d(LINKER)
LINKER = ilink32
!endif

!if !$d(BRCC32)
BRCC32 = brcc32
!endif


# ---------------------------------------------------------------------------
!if $d(PATHCPP)
.PATH.CPP = $(PATHCPP)
.PATH.C   = $(PATHCPP)
!endif

!if $d(PATHPAS)
.PATH.PAS = $(PATHPAS)
!endif

!if $d(PATHASM)
.PATH.ASM = $(PATHASM)
!endif

!if $d(PATHRC)
.PATH.RC  = $(PATHRC)
!endif

!if $d(PATHOBJ)
.PATH.OBJ  = $(PATHOBJ)
!endif
# ---------------------------------------------------------------------------
$(PROJECT): $(OTHERFILES) $(IDLGENFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)
    $(BCB)\BIN\$(LINKER) @&&!
    $(LFLAGS) -L$(LIBPATH) +
    $(ALLOBJ), +
    $(PROJECT),, +
    $(ALLLIB), +
    $(DEFFILE), +
    $(ALLRES)
!
# ---------------------------------------------------------------------------
.pas.hpp:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.pas.obj:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.cpp.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.cpp.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.asm.obj:
    $(BCB)\BIN\$(TASM32) $(AFLAGS) -i$(INCLUDEPATH:;= -i) $(AUSERDEFINES) -d$(SYSDEFINES:;= -d) $<, $@

.rc.res:
    $(BCB)\BIN\$(BRCC32) $(RFLAGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -fo$@ $<



# ---------------------------------------------------------------------------




