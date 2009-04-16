program VersionDemo;

uses
  Forms,
  fMain in 'fMain.pas' {frmVersionDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmVersionDemo, frmVersionDemo);
  Application.Run;
end.
