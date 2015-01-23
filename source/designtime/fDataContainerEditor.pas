(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Data container editor form (design-time)
(*
(*  Copyright (c) 2005-2009, Context Software LLC
(*
(******************************************************************************)
unit fDataContainerEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, ImgList, ToolWin, ComCtrls, StdCtrls, ExtCtrls, StdActns, Menus,
  CtxDataTypes, CtxDBIntf, CtxData, CtxDataSet, CtxProfiles, CtxGridView, CtxPropView
  {$IFDEF VER240},System.Actions{$ENDIF};

type
  TfrmDataContainerEditor = class(TForm)
    PageControl: TPageControl;
    Images: TImageList;
    Actions: TActionList;
    actAddTable: TAction;
    actDeleteTable: TAction;
    actAddColumn: TAction;
    actDeleteColumn: TAction;
    actClose: TAction;
    tsRelations: TTabSheet;
    actAddRelation: TAction;
    actDeleteRelation: TAction;
    popTables: TPopupMenu;
    AddTable1: TMenuItem;
    DeleteTable1: TMenuItem;
    popColumns: TPopupMenu;
    AddColumn1: TMenuItem;
    DeleteColumn1: TMenuItem;
    popRelations: TPopupMenu;
    AddRelation1: TMenuItem;
    DeleteRelation1: TMenuItem;
    MainMenu: TMainMenu;
    DataObjects1: TMenuItem;
    AddTable2: TMenuItem;
    File1: TMenuItem;
    SavetoFile1: TMenuItem;
    LoadFromFile1: TMenuItem;
    actEditRelation: TAction;
    actTableWizard: TAction;
    EditRelation1: TMenuItem;
    ableWizard1: TMenuItem;
    N1: TMenuItem;
    actOpen: TAction;
    actSave: TAction;
    N2: TMenuItem;
    Close1: TMenuItem;
    Panel2: TPanel;
    grdRelations: TCtxCollectionView;
    actRenameTable: TAction;
    RenameTable1: TMenuItem;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton4: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    actToggleKey: TAction;
    oggleKeyColumn1: TMenuItem;
    N3: TMenuItem;
    tsTables: TTabSheet;
    Panel3: TPanel;
    pnlTreeView: TPanel;
    lvTables: TListView;
    Splitter: TSplitter;
    Panel1: TPanel;
    grdColumns: TCtxCollectionView;
    pnlTableHeader: TPanel;
    ToolBarColumns: TToolBar;
    ToolButton9: TToolButton;
    ToolButton11: TToolButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    actRefresh: TAction;
    Refresh1: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    Refresh2: TMenuItem;
    AddRelation2: TMenuItem;
    actTest: TAction;
    estActivation1: TMenuItem;
    CtxFormSettings: TCtxFormSettings;
    AppProfile: TCtxAppProfile;
    actConfigureAdapter: TAction;
    ConfigureDataAdapter1: TMenuItem;
    UpdateCommands1: TMenuItem;
    N6: TMenuItem;
    procedure actCloseExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actAddTableExecute(Sender: TObject);
    procedure actAddColumnExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actAddRelationExecute(Sender: TObject);
    procedure actDeleteTableExecute(Sender: TObject);
    procedure actDeleteColumnExecute(Sender: TObject);
    procedure actDeleteRelationExecute(Sender: TObject);
    procedure grdRelationsDblClick(Sender: TObject);
    procedure actTableWizardExecute(Sender: TObject);
    procedure lvTablesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure actToggleKeyExecute(Sender: TObject);
    procedure lvTablesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lvTablesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure actRenameTableExecute(Sender: TObject);
    procedure lvTablesEdited(Sender: TObject; Item: TListItem;
      var S: String);
    procedure actSaveExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actRefreshExecute(Sender: TObject);
    procedure actTestExecute(Sender: TObject);
    procedure actConfigureAdapterExecute(Sender: TObject);
    procedure UpdateCommands1Click(Sender: TObject);
    function grdColumnsCellState(Sender: TObject; ACol,
      ARow: Integer): TCtxCellStates;
    procedure grdColumnsDrawMarker(Sender: TObject; ARow: Integer;
      ARect: TRect);
    procedure grdRelationsDrawMarker(Sender: TObject; ARow: Integer;
      ARect: TRect);
    procedure grdColumnsCellProps(Sender: TObject; ACol, ARow: Integer;
      var AAlignment: TAlignment; var AColor: TColor; AFont: TFont);
  private
    { Private declarations }
    FDataContainer: TCtxDataContainer;
    FCurrentTable: TCtxDataTable;
    FDesigner: TCtxDataContainerDesigner;
    FLastTableName: String;
    // function GetCurrentTable: TCtxDataTable;
    procedure DrawObjectIcon(Canvas: TCanvas; Rect: TRect; Collection: TCollection; Idx: Integer);
  public
    { Public declarations }
    property DataContainer: TCtxDataContainer read FDataContainer;
    property Designer: TCtxDataContainerDesigner read FDesigner;
    procedure NotifyEvent(Context: TObject; DataEvent: TCtxDataEventType);
    procedure ReloadTables;
    procedure ReloadTableDetails(Table: TCtxDataTable);

    property CurrentTable: TCtxDataTable read FCurrentTable;
  end;

  TCtxCustomDataContainerDesigner = class (TCtxDataContainerDesigner)
  protected
    FDataContainerEditor: TfrmDataContainerEditor;
    FOwner: TComponent;
    procedure NotifyEvent(Context: TObject; DataEvent: TCtxDataEventType); override;
  public
    constructor Create(ADataContainer: TCtxDataContainer);
    destructor Destroy; override;
    procedure Show; override;
    procedure GetDataObjects(List: TStringList); override;
  end;

var
  frmDataContainerEditor: TfrmDataContainerEditor;

  procedure DesignDataContainer(ADataContainer: TCtxDataContainer; AOwner: TComponent = nil);

resourcestring
  SConfirmDeleteTable = 'Are you sure you want to delete selected table?';
  SConfirmDeleteColumn = 'Are you sure you want to delete selected column?';
  SConfirmDeleteRelation = 'Are you sure you want to delete selected relation?';
  STableNotSelected = ' Table: (not selected)';
  STableFormat = ' Table: %s';
  SActivationSuccess = 'Data container is activated successfully!';
  SConfirmOverwriteContainer = 'Are you sure you want to overwrite current data container definition? '
    +'This operation cannot be undone.';
  SConfirmConfigureAdapter = 'Are you sure you want to automatically configure connected data adapter?'+#13+#10+
    'This operation will overwrite all current adapter settings and cannot be undone.';
  SAdapterNotAssigned = 'The data adapter (TCtxDBAdapter component) is not assigned to this data container';
  SNoTableSelected = 'Please select a table to update commands';

implementation

uses TypInfo, fDataRelationEditor, fDataTableWizard, CtxDBAdapter,
  fSelectCommandTypes;

{$R *.dfm}

procedure DesignDataContainer(ADataContainer: TCtxDataContainer; AOwner: TComponent = nil);
begin
  if ADataContainer.Designer = nil then
  begin
    TCtxCustomDataContainerDesigner.Create(ADataContainer);
    if AOwner <> nil then
      TCtxCustomDataContainerDesigner(ADataContainer.Designer).FOwner := AOwner;
  end else ADataContainer.Designer.Show;
end;

{ TCtxCustomDataContainerDesigner }

constructor TCtxCustomDataContainerDesigner.Create(
  ADataContainer: TCtxDataContainer);
begin
  inherited Create(ADataContainer);
  FOwner := FDataContainer.Owner;
  FDataContainerEditor := TfrmDataContainerEditor.Create(nil);
  FDataContainerEditor.FDataContainer := DataContainer;
  FDataContainerEditor.FDesigner := Self;
  FDataContainerEditor.Show;
end;

destructor TCtxCustomDataContainerDesigner.Destroy;
begin
  FDataContainerEditor.FDesigner := nil;
  FDataContainerEditor.Free;
  inherited Destroy;
end;

procedure TCtxCustomDataContainerDesigner.GetDataObjects(List: TStringList);
var
  I: Integer;
  DB: ICtxDatabase;
  // C: TComponent;
begin
  // Implement in descendants
  for I := 0 to CtxDataTypes.DBDatabases.Count - 1 do
  begin
    if TObject(DBDatabases[I]).GetInterface(ICtxDatabase, DB) then
      List.AddObject(DB.DatabaseName, TObject(CtxDataTypes.DBDatabases[I]));
  end;

  (*
  if FOwner <> nil then
  for I := 0 to FOwner.ComponentCount - 1 do
  begin
    C := FOwner.Components[I];
    if (C.InheritsFrom(TDataSet))
      or AnsiSameText(C.ClassName, 'TDatabaseSchema')
      or C.GetInterface(ICtxDatabase, DB)
    then
      List.AddObject(C.Name, C);
  end;
  *)
end;

procedure TCtxCustomDataContainerDesigner.NotifyEvent(Context: TObject;
  DataEvent: TCtxDataEventType);
begin
  // update interface depending on chages
  FDataContainerEditor.NotifyEvent(Context, DataEvent);
end;

procedure TCtxCustomDataContainerDesigner.Show;
begin
  FDataContainerEditor.Visible := True;
  FDataContainerEditor.BringToFront;
end;

{ TfrmDataContainerEditor }

procedure TfrmDataContainerEditor.FormCreate(Sender: TObject);
begin
  PageControl.ActivePageIndex := 0;
end;

procedure TfrmDataContainerEditor.FormShow(Sender: TObject);
begin
  DataContainer.Active := False;
  grdRelations.Collection := DataContainer.Relations;
  ReloadTables;
end;

procedure TfrmDataContainerEditor.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmDataContainerEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Action = caFree then
    FDesigner.Free;
end;

procedure TfrmDataContainerEditor.DrawObjectIcon(Canvas: TCanvas; Rect: TRect; Collection: TCollection; Idx: Integer);
var
  ImageIdx: Integer;
begin
  if Collection = nil then exit;
  if Idx < Collection.Count then
  begin
    ImageIdx := -1;
    with Collection.Items[Idx] do
    if InheritsFrom(TCtxDataTable) then
      ImageIdx := 6
    else if InheritsFrom(TCtxDataRelation) then
      ImageIdx := 5
    else if InheritsFrom(TCtxDataColumn) then
    begin
      if TCtxDataColumn(Collection.Items[Idx]).PrimaryKey then
        ImageIdx := 11
      else if TCtxDataColumn(Collection.Items[Idx]).DataType = cdtReference then
        ImageIdx := 5
      else ImageIdx := 2;
    end;
    if ImageIdx >= 0 then
      Images.Draw(Canvas, Rect.Left + 1, Rect.Top + 1, ImageIdx);
  end;
end;

procedure TfrmDataContainerEditor.NotifyEvent(Context: TObject;
  DataEvent: TCtxDataEventType);
begin
  if Context = nil then
    Exit;
  case DataEvent of
    cdeContainerChanged: ReloadTables;
    cdeTableListChanged: ReloadTables;
    cdeRelationListChanged: begin
      if grdRelations.Collection <> nil then
        grdRelations.Refresh;
      ReloadTableDetails(CurrentTable);
    end;
    cdeTableChanged:
      if (lvTables.Selected = nil) or (lvTables.Selected.Data <> Context) then
        ReloadTables
      else ReloadTableDetails(CurrentTable);
    cdeColumnListChanged:
      if TCtxDataTable(Context) = CurrentTable then
        grdColumns.Refresh;
  end;
end;

procedure TfrmDataContainerEditor.ReloadTables;
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
  if grdRelations.Collection <> nil then
    grdRelations.Refresh;
  if grdColumns.Collection <> nil then
    grdColumns.Refresh;
end;

procedure TfrmDataContainerEditor.ReloadTableDetails(Table: TCtxDataTable);
var
  I: TCtxDataType;
  J: Integer;
begin
  // Reload relations & columns for the given table FCurrentTable
  grdColumns.ApplyEdits;

  // grdColumns.Editor.Hide;

  if Table = nil then
  begin
    grdColumns.Collection := nil;
    FLastTableName := '';
    pnlTableHeader.Caption := STableNotSelected;
  end else begin
    grdColumns.Collection := Table.Columns;
    if Table.Columns.Count > 0 then
      grdColumns.CurrentItem := Table.Columns[0];
    FLastTableName := Table.Name;
    pnlTableHeader.Caption := Format(STableFormat, [Table.Name]);
  end;

  if CurrentTable <> nil then
  begin
    with grdColumns.Columns.Find('DataTypeName') do
    begin
      ValueList.Clear;
      for I := Low(TCtxDataType) to High(TCtxDataType) do
      if I <> cdtReference then
        ValueList.Add(DataTypeNames[I]);

      TStringList(ValueList).Sort; // +++ valueList Sorted

      with DataContainer do
      for J := 0 to Relations.Count - 1 do
        if AnsiSameText(Relations[J].ChildTableName, CurrentTable.Name) then
          ValueList.Add('->' + Relations[J].Name);
    end;
  end;

  grdColumns.Invalidate;
end;

procedure TfrmDataContainerEditor.ActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actRenameTable.Enabled := CurrentTable <> nil;
  actDeleteTable.Enabled := CurrentTable <> nil;
  actAddColumn.Enabled := CurrentTable <> nil;
  actDeleteColumn.Enabled := grdColumns.CurrentItem <> nil;
  actDeleteRelation.Enabled := grdRelations.CurrentItem <> nil;
  actToggleKey.Enabled := grdColumns.CurrentItem <> nil;
  actConfigureAdapter.Enabled := Assigned(DataContainer.DataAdapter) and
    DataContainer.DataAdapter.InheritsFrom(TCtxDBAdapter);
end;

procedure TfrmDataContainerEditor.actAddTableExecute(Sender: TObject);
begin
  DataContainer.Tables.BeginUpdate;
  try
    FLastTableName := DataContainer.Tables.GetAutoName('Table', 1);
    DataContainer.Tables.Add.Name := FLastTableName;
  finally
    DataContainer.Tables.EndUpdate;
  end;
end;

procedure TfrmDataContainerEditor.actAddColumnExecute(Sender: TObject);
var
  NewItem: TCtxDataColumn;
begin
  if CurrentTable = nil then exit;

  NewItem := grdColumns.InsertItem(True) as TCtxDataColumn;
  NewItem.Name := CurrentTable.Columns.GetAutoName('Column', 1);
  NewItem.DataType := cdtString;
  grdColumns.Refresh;
  grdColumns.Editor.SelectAll;
end;

procedure TfrmDataContainerEditor.actAddRelationExecute(Sender: TObject);
var
  NewItem: TCtxDataRelation;
begin
  NewItem := DataContainer.Relations.Add;
  NewItem.Name := DataContainer.Relations.GetAutoName('Relation', 1);
  if not EditDataRelation(NewItem) then
    NewItem.Free;
  grdRelations.Refresh;
  ReloadTableDetails(CurrentTable);
end;

procedure TfrmDataContainerEditor.actDeleteTableExecute(Sender: TObject);
begin
  if MessageDlg(SConfirmDeleteTable, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    FreeAndNil(FCurrentTable);
end;

procedure TfrmDataContainerEditor.actDeleteColumnExecute(Sender: TObject);
begin
  if MessageDlg(SConfirmDeleteColumn, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    grdColumns.DeleteItem;
end;

procedure TfrmDataContainerEditor.actDeleteRelationExecute(
  Sender: TObject);
begin
  if MessageDlg(SConfirmDeleteRelation, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    grdRelations.DeleteItem;
end;

procedure TfrmDataContainerEditor.grdRelationsDblClick(Sender: TObject);
begin
  if grdRelations.CurrentItem <> nil then
    if EditDataRelation(TCtxDataRelation(grdRelations.CurrentItem)) then
    begin
      grdRelations.Refresh;
      ReloadTableDetails(CurrentTable);
    end;
end;

procedure TfrmDataContainerEditor.lvTablesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if (Item <> nil) and Selected then
  begin
    FCurrentTable := TCtxDataTable(Item.Data);
    ReloadTableDetails(FCurrentTable);
  end;
end;

procedure TfrmDataContainerEditor.actToggleKeyExecute(Sender: TObject);
begin
  if grdColumns.CurrentItem <> nil then
    with TCtxDataColumn(grdColumns.CurrentItem) do
    begin
      PrimaryKey := not PrimaryKey;
      if PrimaryKey then
        Required := True;
      grdColumns.Invalidate;
    end;
end;

procedure TfrmDataContainerEditor.lvTablesDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = lvTables) and (FCurrentTable <> nil);
end;

procedure TfrmDataContainerEditor.lvTablesDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  LI: TListItem;
begin
  LI := lvTables.GetItemAt(X, Y);
  if LI <> nil then
    FCurrentTable.Index := LI.Index
  else FCurrentTable.Index := DataContainer.Tables.Count - 1;
  ReloadTables;
end;

procedure TfrmDataContainerEditor.actRenameTableExecute(Sender: TObject);
begin
  if lvTables.Selected <> nil then
    lvTables.Selected.EditCaption;
end;

procedure TfrmDataContainerEditor.lvTablesEdited(Sender: TObject;
  Item: TListItem; var S: String);
begin
  TCtxDataTable(Item.Data).Name := S;
end;

procedure TfrmDataContainerEditor.actTableWizardExecute(Sender: TObject);
var
  DataObjects: TStringList;
begin
  DataObjects := TStringList.Create;
  try
    if FDesigner <> nil then
      FDesigner.GetDataObjects(DataObjects);
    NewDataTable(DataContainer, DataObjects);
  finally
    DataObjects.Free;
  end;
end;

procedure TfrmDataContainerEditor.actSaveExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
    DataContainer.SaveToFile(SaveDialog.FileName);
end;

procedure TfrmDataContainerEditor.actOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then
    if ((DataContainer.Tables.Count = 0) and (DataContainer.Relations.Count = 0))
      or (MessageDlg(SConfirmOverwriteContainer, mtWarning, [mbYes, mbNo], 0) = mrYes)
    then
      DataContainer.LoadFromFile(OpenDialog.FileName);
end;

procedure TfrmDataContainerEditor.actRefreshExecute(Sender: TObject);
begin
  if CurrentTable <> nil then
    FLastTableName := CurrentTable.Name;
  ReloadTables;
end;

procedure TfrmDataContainerEditor.actTestExecute(Sender: TObject);
begin
  // This will attempt to activate data container and raise
  DataContainer.Active := True;
  MessageDlg(SActivationSuccess, mtInformation, [mbOK], 0);
  DataContainer.Active := False;
end;

procedure TfrmDataContainerEditor.actConfigureAdapterExecute(
  Sender: TObject);
begin
  if not Assigned(DataContainer.DataAdapter) or not
    DataContainer.DataAdapter.InheritsFrom(TCtxDBAdapter) then exit;

  if (TCtxDBAdapter(DataContainer.DataAdapter).Commands.Count = 0) or
    (MessageDlg(SConfirmConfigureAdapter, mtConfirmation, [mbOK, mbCancel], 0) = mrOk)
  then
    TCtxDBAdapter(DataContainer.DataAdapter).AutoConfigure(DataContainer);
end;

procedure TfrmDataContainerEditor.UpdateCommands1Click(Sender: TObject);
var
  ACommandTypes: TCtxDBCommandItemTypes;
begin
  if not Assigned(DataContainer.DataAdapter) or
    not DataContainer.DataAdapter.InheritsFrom(TCtxDBAdapter)
  then
    raise Exception.Create(SAdapterNotAssigned);

  if CurrentTable = nil then
    raise Exception.Create(SNoTableSelected);

  ACommandTypes := TCtxDBAdapter(DataContainer.DataAdapter).GetTableCommandTypes(CurrentTable);
  Exclude(ACommandTypes, citProcedure);
  if SelectCommandTypes(ACommandTypes) and (ACommandTypes <> []) then
    TCtxDBAdapter(DataContainer.DataAdapter).ConfigureTableCommands(CurrentTable, ACommandTypes);
end;

(*
procedure TfrmDataContainerEditor.grdColumnsCellDraw(Sender: TObject;
  Canvas: TCanvas; Rect: TRect; ACol, ARow: Integer;
  const CellText: String; var HandledDrawing: Boolean);
begin
  with grdColumns do
  if (ARow >= 0) and (ACol = 0) and (Collection <> nil) and (ARow < Collection.Count) then
  begin
    DrawObjectIcon(Canvas, Rect, Collection, ARow);
    HandledDrawing := True;
  end;
end;
*)

function TfrmDataContainerEditor.grdColumnsCellState(Sender: TObject; ACol,
  ARow: Integer): TCtxCellStates;
begin
  Result := [];
  with grdColumns do
  if AnsiSameText(Columns[ACol].Name, 'DataLength') and (Collection <> nil)
    and not (TCtxDataColumn(Collection.Items[ARow]).DataType in [cdtString, cdtWideString])
  then
    Result := [csEmpty, csDisabled, csReadOnly];
end;

procedure TfrmDataContainerEditor.grdColumnsDrawMarker(Sender: TObject;
  ARow: Integer; ARect: TRect);
begin
  DrawObjectIcon(grdColumns.Canvas, ARect, grdColumns.Collection, ARow);
end;

procedure TfrmDataContainerEditor.grdRelationsDrawMarker(Sender: TObject;
  ARow: Integer; ARect: TRect);
begin
  DrawObjectIcon(grdRelations.Canvas, ARect, grdRelations.Collection, ARow);
end;

procedure TfrmDataContainerEditor.grdColumnsCellProps(Sender: TObject;
  ACol, ARow: Integer; var AAlignment: TAlignment; var AColor: TColor;
  AFont: TFont);
begin
  with grdColumns do
  if AnsiSameText(Columns[ACol].Name, 'DataLength') and (Collection <> nil)
    and not (TCtxDataColumn(Collection.Items[ARow]).DataType in [cdtString, cdtWideString])
  then
    AColor := $00efefef;
end;

end.


