//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop

//---------------------------------------------------------------------------
USEFORMNS("..\source\designtime\fSelFields.pas", Fselfields, frmSelectFields);
USEFORMNS("..\source\designtime\fCommandBuilder.pas", Fcommandbuilder, frmCommandBuilder);
USEFORMNS("..\source\designtime\fDataContainerEditor.pas", Fdatacontainereditor, frmDataContainerEditor);
USEFORMNS("..\source\designtime\fDataExplorer.pas", Fdataexplorer, frmDataExplorer);
USEFORMNS("..\source\designtime\fDataRelationEditor.pas", Fdatarelationeditor, frmDataRelationEditor);
USEFORMNS("..\source\designtime\fDataTableWizard.pas", Fdatatablewizard, frmDataTableWizard);
USEFORMNS("..\source\designtime\fDBAdapterEditor.pas", Fdbadaptereditor, frmDBAdapterEditor);
USEFORMNS("..\source\designtime\fSelectCommandTypes.pas", Fselectcommandtypes, frmSelectCommandTypes);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------

#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
