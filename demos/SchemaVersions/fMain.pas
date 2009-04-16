unit fMain;

interface

uses
  Windows, Messages, SysUtils, {$IFnDEF VER130}Variants, {$ENDIF} Classes, Graphics, Controls, Forms,
  Dialogs, dbSchema, DB, DBTables, BDEExt, Grids, DBGrids, ComCtrls,
  StdCtrls;

type
  TfrmVersionDemo = class(TForm)
    DatabaseSchema: TDatabaseSchema;
    DatabaseExt: TDatabaseExt;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Query1: TQuery;
    lblStatus: TLabel;
    ProgressBar: TProgressBar;
    edtLog: TMemo;
    Button1: TButton;
    Label1: TLabel;
    edtDatabase: TEdit;
    Button2: TButton;
    Label2: TLabel;
    lblVersion: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure OnDatabaseProgress(Sender: TObject; const Status: String; PercentDone: Byte; var Abort: Boolean);
    procedure ReOpenDatabase;
    procedure GetDatabaseVersion;
  end;

var
  frmVersionDemo: TfrmVersionDemo;

implementation

uses FileCtrl;

{$R *.dfm}

procedure TfrmVersionDemo.FormCreate(Sender: TObject);
begin
  edtDatabase.Text := ExtractFilePath(Application.ExeName) + 'Data';
  GetDatabaseVersion;
end;

procedure TfrmVersionDemo.OnDatabaseProgress(Sender: TObject; const Status: String;
  PercentDone: Byte; var Abort: Boolean);
begin
  ProgressBar.Position := PercentDone;
  lblStatus.Caption := Status;
  lblStatus.Update;
  edtLog.Lines.Add(Status);
end;

procedure TfrmVersionDemo.Button1Click(Sender: TObject);
var
  Directory: string;
begin
  Directory := edtDatabase.Text;
  if SelectDirectory(Directory, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
  begin
    edtDatabase.Text := Directory;
    GetDatabaseVersion;
  end;
end;

procedure TfrmVersionDemo.GetDatabaseVersion;
begin
  DatabaseExt.Connected := False;
  try
    DatabaseExt.DatabaseURL := 'STANDARD:\\PATH\' + edtDatabase.Text;
    DatabaseExt.Connected := True;
  finally
    lblVersion.Caption := Format('Required Version: %s;   Actual Version: %s',
      [DatabaseSchema.VersionLabel, DatabaseExt.VersionLabel]);
    DatabaseExt.Connected := False;
  end;
end;

procedure TfrmVersionDemo.ReOpenDatabase;
begin
  DatabaseExt.Connected := False;
  DatabaseExt.DatabaseURL := 'STANDARD:\\PATH\' + edtDatabase.Text;
  DatabaseExt.Connected := True;
  edtLog.Lines.Clear;
  if not DatabaseExt.IsVersionCurrent(True) then
    UpdateDatabase(DatabaseExt, OnDatabaseProgress)
  else begin
    ProgressBar.Position := 0;
    lblStatus.Caption := 'Done.';
    edtLog.Lines.Add(lblStatus.Caption + ' Database structure is current.');
  end;
  lblVersion.Caption := Format('Required Version: %s;   Actual Version: %s',
    [DatabaseSchema.VersionLabel, DatabaseExt.VersionLabel]);

  Query1.Active := True;
end;

procedure TfrmVersionDemo.Button2Click(Sender: TObject);
begin
  ReOpenDatabase;
end;

end.
