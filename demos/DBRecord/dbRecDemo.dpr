program dbRecDemo;

uses
  Forms,
  fMain in 'fMain.pas' {frmDbRecordDemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDbRecordDemo, frmDbRecordDemo);
  Application.Run;
end.
