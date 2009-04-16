program MemDataSetDemo;

uses
  Forms,
  fMain in 'fMain.pas' {fmMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfmMainForm, fmMainForm);
  Application.Run;
end.
