unit fMain;

interface

uses
  Windows, Messages, SysUtils, {$IFnDEF VER130}Variants,{$ENDIF} Classes, 
  Graphics, Controls, Forms, Dialogs, ExtCtrls, DBCtrls, ToolWin, ComCtrls, 
  Menus, StdActns, ActnList, ImgList, Grids, DBGrids, DB,  
  StdCtrls, Mask, ExtDlgs, CtxDataSet;

type
  TfmMainForm = class(TForm)
    DbMemDataSet: TCtxDataSet;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    MainMenu: TMainMenu;
    Actions: TActionList;
    Images: TImageList;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    FileExit1: TFileExit;
    File1: TMenuItem;
    Open1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    ToolBar1: TToolBar;
    DBNavigator1: TDBNavigator;
    Panel1: TPanel;
    DBEdit1: TDBEdit;
    DBMemo1: TDBMemo;
    Label1: TLabel;
    Label2: TLabel;
    DBImage1: TDBImage;
    OpenPictureDialog: TOpenPictureDialog;
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DBImage1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMainForm: TfmMainForm;

implementation

{$R *.dfm}

procedure TfmMainForm.FileOpen1Accept(Sender: TObject);
begin
  DbMemDataSet.LoadFromFile(FileOpen1.Dialog.FileName);
end;

procedure TfmMainForm.FileSaveAs1Accept(Sender: TObject);
begin
  DbMemDataSet.SaveToFile(FileSaveAs1.Dialog.FileName);
end;

procedure TfmMainForm.FormCreate(Sender: TObject);
begin
  FileOpen1.Dialog.InitialDir := ExtractFilePath(Application.ExeName);
  FileSaveAs1.Dialog.InitialDir := ExtractFilePath(Application.ExeName);
  DbMemDataSet.Active := True;
end;

procedure TfmMainForm.DBImage1DblClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    DBImage1.Picture.LoadFromFile(OpenPictureDialog.FileName);
end;

end.
