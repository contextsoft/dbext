unit fDataExplorer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, Menus, ToolWin, ComCtrls, ActnList, Grids,
  ExtCtrls, DB, DBGrids, DBCtrls, CtxData, CtxDataSet, CtxProfiles;

type
  TfrmDataExplorer = class(TForm)
    MainMenu: TMainMenu;
    ToolBar1: TToolBar;
    lvTables: TListView;
    Splitter: TSplitter;
    Panel1: TPanel;
    pnlTableHeader: TPanel;
    Images: TImageList;
    Actions: TActionList;
    File1: TMenuItem;
    ToolButton1: TToolButton;
    grdData: TDBGrid;
    DataSource: TDataSource;
    DBNavigator1: TDBNavigator;
    tDataSet: TCtxDataSet;
    StatusBar: TStatusBar;
    CtxFormSettings: TCtxFormSettings;
    AppProfile: TCtxAppProfile;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    actExit: TAction;
    Exit1: TMenuItem;
    actImport: TAction;
    actExport: TAction;
    ImportData1: TMenuItem;
    Savedata1: TMenuItem;
    N1: TMenuItem;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    actFill: TAction;
    actClear: TAction;
    actSaveChanges: TAction;
    actDiscardChanges: TAction;
    Data1: TMenuItem;
    FillDataContainer1: TMenuItem;
    ClearDataContainer1: TMenuItem;
    N2: TMenuItem;
    SaveChanges1: TMenuItem;
    DiscardChanges1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure lvTablesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure actExitExecute(Sender: TObject);
    procedure actImportExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actFillExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actSaveChangesExecute(Sender: TObject);
    procedure actDiscardChangesExecute(Sender: TObject);
  private
    { Private declarations }
    FDataContainer: TCtxDataContainer;
    FCurrentTable: TCtxDataTable;
    FLastTableName: String;
  public
    { Public declarations }
    procedure ReloadTables;
    procedure ReloadTableDetails(Table: TCtxDataTable);

    property DataContainer: TCtxDataContainer read FDataContainer;
  end;

var
  frmDataExplorer: TfrmDataExplorer;

  procedure ExploreContainer(ADataContainer: TCtxDataContainer);

resourcestring
  STableNotSelected = ' Table: (not selected)';
  STableFormat = ' Table: %s';

implementation

{$R *.dfm}

procedure ExploreContainer(ADataContainer: TCtxDataContainer);
begin
  with TfrmDataExplorer.Create(nil) do
  try
    FDataContainer := ADataContainer;
    FDataContainer.Active := True;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmDataExplorer.FormShow(Sender: TObject);
begin
  tDataSet.DataContainer := FDataContainer;
  ReloadTables;
end;

procedure TfrmDataExplorer.ReloadTables;
var
  Item: TListItem;
  OldIdx, I: Integer;
begin
  if lvTables.Selected <> nil then
    OldIdx := lvTables.Selected.Index
  else OldIdx := 0;
  lvTables.Items.BeginUpdate;
  try
    FCurrentTable := nil;
    lvTables.Items.Clear;
    for I := 0 to DataContainer.Tables.Count - 1 do
    begin
      Item := lvTables.Items.Add;
      Item.Caption := DataContainer.Tables[I].Name;
      Item.Data := DataContainer.Tables[I];
      Item.ImageIndex := 6;
    end;
    if FLastTableName <> '' then
    begin
      FCurrentTable := DataContainer.Tables.Find(FLastTableName);
      if FCurrentTable <> nil then
        lvTables.Selected := lvTables.FindData(0, FCurrentTable, False, False);
    end;
  finally
    lvTables.Items.EndUpdate;
    if lvTables.Selected = nil then
      if lvTables.Items.Count > 0 then
      begin
        if OldIdx >= lvTables.Items.Count then
          OldIdx := lvTables.Items.Count - 1;
        if OldIdx < 0 then
          OldIdx := 0;
        lvTables.Selected := lvTables.Items[OldIdx];
      end else
        ReloadTableDetails(nil);
  end;
end;

procedure TfrmDataExplorer.ReloadTableDetails(Table: TCtxDataTable);
var
  I: Integer;
begin
  if Table = nil then
  begin
    tDataSet.Active := False;
    FLastTableName := '';
    tDataSet.DataTableName := '';
    pnlTableHeader.Caption := STableNotSelected;
  end else begin
    tDataSet.Active := False;
    FLastTableName := Table.Name;
    tDataSet.DataTableName := Table.Name;
    pnlTableHeader.Caption := Format(STableFormat, [Table.Name]);
    tDataSet.DisableControls;
    try
      tDataSet.Active := True;
      for I := 0 to tDataSet.FieldCount - 1 do
        if tDataSet.Fields[I].DisplayWidth > 20 then
          tDataSet.Fields[I].DisplayWidth := 20;
    finally
      tDataSet.EnableControls;
    end
  end;
end;

procedure TfrmDataExplorer.lvTablesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if (Item <> nil) and Selected then
  begin
    FCurrentTable := TCtxDataTable(Item.Data);
    ReloadTableDetails(FCurrentTable);
  end;
end;

procedure TfrmDataExplorer.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmDataExplorer.actImportExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    {if AnsiSameText(AnsiUpperCase(ExtractFileExt(OpenDialog.FileName)), '.csv') then
      DataContainer.LoadFromFile(OpenDialog.FileName)
    else}
    DataContainer.LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure TfrmDataExplorer.actExportExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
    DataContainer.SaveToFile(SaveDialog.FileName, False, True);
end;

procedure TfrmDataExplorer.ActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  StatusBar.Panels[0].Text := Format('%d tables', [DataContainer.Tables.Count]);
  if FCurrentTable <> nil then
    StatusBar.Panels[1].Text := Format('%d columns, %d rows', [FCurrentTable.Columns.Count, FCurrentTable.RowCount])
  else StatusBar.Panels[1].Text := '';

  actFill.Enabled := DataContainer.DataAdapter <> nil;
  actSaveChanges.Enabled := DataContainer.DataAdapter <> nil;
end;

procedure TfrmDataExplorer.actFillExecute(Sender: TObject);
begin
  if DataContainer.DataAdapter = nil then exit;
  DataContainer.Fill;
end;

procedure TfrmDataExplorer.actClearExecute(Sender: TObject);
begin
  DataContainer.Clear;
end;

procedure TfrmDataExplorer.actSaveChangesExecute(Sender: TObject);
begin
  if DataContainer.DataAdapter = nil then exit;
  DataContainer.Update;
end;

procedure TfrmDataExplorer.actDiscardChangesExecute(Sender: TObject);
begin
  DataContainer.RejectChanges;
end;

end.
