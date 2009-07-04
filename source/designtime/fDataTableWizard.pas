(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Data Container Table wizard form
(*
(*  Copyright (c) 2005-2009, Context Software LLC
(*
(******************************************************************************)
unit fDataTableWizard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, CtxDataTypes, CtxData, dbSchema, 
  CtxDBIntf, DB, CheckLst, ImgList, CtxDataSet, CtxDBAdapter;

type
  TfrmDataTableWizard = class(TForm)
    btnNext: TButton;
    btnCancel: TButton;
    Notebook: TNotebook;
    btnBack: TButton;
    rgSource: TRadioGroup;
    lblSource: TLabel;
    cbxSourceObject: TComboBox;
    lvTables: TListView;
    Label7: TLabel;
    Label8: TLabel;
    lvRelations: TListView;
    Image1: TImage;
    Image2: TImage;
    Bevel1: TBevel;
    Images: TImageList;
    btnSelectAllRelations: TButton;
    btnSelectNoneRelations: TButton;
    procedure btnBackClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure rgSourceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NotebookPageChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvTablesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure btnSelectAllRelationsClick(Sender: TObject);
    procedure btnSelectNoneRelationsClick(Sender: TObject);
    procedure lvRelationsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    { Private declarations }
    FDataObjects: TStringList;
    FDataContainer: TCtxDataContainer;
    FTableNames: TStringList;
    FTempContainer: TCtxDataContainer;
    FCtxDatabase: ICtxDatabase;
    FSchema: TDatabaseSchema;
    FAdapter: TCtxDBAdapter;
    // FDataProvider: ICtxDataProvider; +++
    // FSchemaContainer: TCtxDataContainer;

    procedure FillDataComboboxes;
    procedure UpdateButtons;
    procedure GetTablesFromSchema;
    procedure GetTablesFromConnection;
    procedure GetTablesFromDBAdapter;
    procedure GetRelationsFromSchema;
    procedure ImportDataSet;
    procedure ImportTablesFromConnection;
    procedure ImportTablesFromSchema;
    procedure ImportTablesFromDBAdapter;
    procedure FillTables;
    procedure MergeToResultContainer;
    procedure DoImportDataSet(D: TDataSet; const TableName: String = '');
    function TableSelected(const TableName: String): Boolean;
  public
    { Public declarations }
  end;

var
  frmDataTableWizard: TfrmDataTableWizard;

  function NewDataTable(DataContainer: TCtxDataContainer; DataObjects: TStringList = nil): Boolean;

resourcestring
  SSelectDatabaseSchema = 'Select database schema:';
  SSelectDatabaseConnection = 'Select database connection:';
  SSelectDatabaseAdapter = 'Select database adapter:';
  SSelectDataSetComponents = 'Select TDatSet component:';
  SItemsFound = ' (%d items found)';
  SInvalidConnectionInterface = 'Invalid connection %s';
  STableExists = 'Table %s already exists in the data container. Would you like to overwrite it?';

implementation

{$R *.dfm}

function NewDataTable(DataContainer: TCtxDataContainer; DataObjects: TStringList = nil): Boolean;
begin
  with TfrmDataTableWizard.Create(nil) do
  try
    FDataObjects := DataObjects;
    FDataContainer := DataContainer;
    Result := ShowModal = mrOK;
  finally
    Free;
  end;
end;

function CheckedCount(ListView: TListView): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ListView.Items.Count - 1 do
    if ListView.Items[I].Checked then
      Inc(Result);
end;

procedure CheckAllItems(ListView: TListView; Value: Boolean);
var
  I: Integer;
begin
  for I := 0 to ListView.Items.Count - 1 do
    ListView.Items[I].Checked := Value;
end;

procedure TfrmDataTableWizard.FormShow(Sender: TObject);
begin
  Notebook.PageIndex := 0;
  rgSourceClick(nil);
  UpdateButtons;
end;

procedure TfrmDataTableWizard.FillDataComboboxes;
var
  I: Integer;
  DB: ICtxDatabase;
begin
  // Fill database schemas
  cbxSourceObject.Items.BeginUpdate;
  try
    cbxSourceObject.Items.Clear;
    if FDataObjects <> nil then
    case rgSource.ItemIndex of
      0: begin
        // Fill database schemas
        for I := 0 to FDataObjects.Count - 1 do
          if (FDataObjects.Objects[I] <> nil) and FDataObjects.Objects[I].InheritsFrom(TDatabaseSchema) then
            cbxSourceObject.Items.AddObject(FDataObjects[I], FDataObjects.Objects[I]);
        lblSource.Caption := SSelectDatabaseSchema;
      end;
      1: begin
        // Fill databases
        for I := 0 to FDataObjects.Count - 1 do
          if (FDataObjects.Objects[I] <> nil) and FDataObjects.Objects[I].GetInterface(ICtxDatabase, DB) then
            cbxSourceObject.Items.AddObject(FDataObjects[I], FDataObjects.Objects[I]);
        lblSource.Caption := SSelectDatabaseConnection;
      end;
        (* +++
      2: begin
        // Fill database connections
        for I := 0 to FDataObjects.Count - 1 do
          if (FDataObjects.Objects[I] <> nil) and FDataObjects.Objects[I].InheritsFrom(TCtxDBConnection) then
            cbxSourceObject.Items.AddObject(FDataObjects[I], FDataObjects.Objects[I]);
        lblSource.Caption := SSelectDatabaseConnection;
      end;
        *)
      2: begin
        // Fill database adapters
        for I := 0 to FDataObjects.Count - 1 do
          if (FDataObjects.Objects[I] <> nil) and FDataObjects.Objects[I].InheritsFrom(TCtxDBAdapter) then
            cbxSourceObject.Items.AddObject(FDataObjects[I], FDataObjects.Objects[I]);
        lblSource.Caption := SSelectDatabaseAdapter;
      end;
      3: begin
        // Fill data sets
        for I := 0 to FDataObjects.Count - 1 do
          if (FDataObjects.Objects[I] <> nil) and FDataObjects.Objects[I].InheritsFrom(TDataSet) then
            cbxSourceObject.Items.AddObject(FDataObjects[I], FDataObjects.Objects[I]);
        lblSource.Caption := SSelectDataSetComponents;
      end;
    end;

    cbxSourceObject.Enabled := cbxSourceObject.Items.Count > 0;
    if cbxSourceObject.Items.Count > 0 then
      cbxSourceObject.ItemIndex := 0;

    lblSource.Caption := lblSource.Caption + Format(SItemsFound, [cbxSourceObject.Items.Count]);
  finally
    cbxSourceObject.Items.EndUpdate;
  end;
end;

procedure TfrmDataTableWizard.btnBackClick(Sender: TObject);
begin
  // Go back
  if Notebook.PageIndex > 0 then
    Notebook.PageIndex := Notebook.PageIndex - 1;
end;

procedure TfrmDataTableWizard.btnNextClick(Sender: TObject);
begin
  // Go forward
  case Notebook.PageIndex of
    0: begin
      case rgSource.ItemIndex of
        0: GetTablesFromSchema;
        1: GetTablesFromConnection;
        2: GetTablesFromDBAdapter;
        3: ImportDataSet;
      end;
    end;
    1: begin
      case rgSource.ItemIndex of
        0: GetRelationsFromSchema;
        1: ImportTablesFromConnection;
        2: ImportTablesFromDBAdapter;
      end;
    end;
    2: begin
      case rgSource.ItemIndex of
        0: ImportTablesFromSchema;
      end;
    end;
  end;
end;

procedure TfrmDataTableWizard.rgSourceClick(Sender: TObject);
begin
  // Setup cbxSourceObject and buttons
  FillDataComboboxes;
  UpdateButtons;
end;

procedure TfrmDataTableWizard.UpdateButtons;
begin
  case Notebook.PageIndex of
    0: begin
      btnBack.Visible := False;
      if rgSource.ItemIndex = 3 then
        btnNext.Caption := '&Finish'
      else btnNext.Caption := '&Next';
      btnNext.Enabled := cbxSourceObject.ItemIndex >= 0;
    end;
    1: begin
      btnBack.Visible := True;
      if rgSource.ItemIndex = 1 then
        btnNext.Caption := '&Finish'
      else btnNext.Caption := '&Next';
      btnNext.Enabled := CheckedCount(lvTables) > 0;
    end;
    2: begin
      btnBack.Visible := True;
      btnNext.Caption := '&Finish';
      // btnNext.Enabled := CheckedCount(lvRelations) > 0;
    end;
  end;
end;

procedure TfrmDataTableWizard.FillTables;
var
  I: Integer;
begin
  // Fill tables list view
  lvTables.Items.BeginUpdate;
  try
    lvTables.Items.Clear;
    for I := 0 to FTableNames.Count - 1 do
    with lvTables.Items.Add do
    begin
      Caption := FTableNames[I];
      ImageIndex := 1;
    end;
  finally
    lvTables.Items.EndUpdate;
  end;
end;

function TfrmDataTableWizard.TableSelected(const TableName: String): Boolean;
var
  LI: TListItem;
begin
  LI := lvTables.FindCaption(0, TableName, False, True, False);
  Result := (LI <> nil) and LI.Checked;
end;

procedure TfrmDataTableWizard.GetTablesFromConnection;
begin
  with cbxSourceObject do
    if not Items.Objects[ItemIndex].GetInterface(ICtxDatabase, FCtxDatabase) then
      raise Exception.CreateFmt(SInvalidConnectionInterface, [Items[ItemIndex]]);

  FTempContainer.ClearStructure;
  FTableNames.Clear;
  FCtxDatabase.Connected := True;
  FCtxDatabase.GetTableNames(FTableNames);
  FillTables;

  Notebook.PageIndex := 1;
end;

procedure TfrmDataTableWizard.GetTablesFromSchema;
begin
  with cbxSourceObject do
    FSchema := Items.Objects[ItemIndex] as TDatabaseSchema;
  FTempContainer.ClearStructure;
  FTableNames.Clear;
  dbSchema.GetValues(FSchema.TableDefs, FTableNames);
  FillTables;
  Notebook.PageIndex := 1;
end;

procedure TfrmDataTableWizard.GetTablesFromDBAdapter;
var
  I: Integer;
begin
  with cbxSourceObject do
    FAdapter := Items.Objects[ItemIndex] as TCtxDBAdapter;
  FTableNames.Clear;
  for I := 0 to FAdapter.Commands.Count - 1 do
  if FAdapter.Commands[I].CommandType = citSelect then
    FTableNames.Add(FAdapter.Commands[I].Name);
  FillTables;
  Notebook.PageIndex := 1;
end;

procedure TfrmDataTableWizard.GetRelationsFromSchema;
var
  I: Integer;
  MasterPresent, DetailPresent: Boolean;
begin
  // Fill tables list view
  lvRelations.Items.BeginUpdate;
  try
    lvRelations.Items.Clear;
    for I := 0 to FSchema.Relationships.Count - 1 do
    with FSchema.Relationships[I] do
    begin
      MasterPresent := TableSelected(MasterTableName);
      DetailPresent := TableSelected(DetailTableName);
      if MasterPresent and DetailPresent
        or (MasterPresent and (FDataContainer.Tables.Find(DetailTableName) <> nil))
        or (DetailPresent and (FDataContainer.Tables.Find(MasterTableName) <> nil))
      then
      with lvRelations.Items.Add do
      begin
        Caption := Name;
        SubItems.Add(DetailTableName + '('+DetailKeyFields+')');
        SubItems.Add(MasterTableName + '('+MasterKeyFields+')');
        ImageIndex := 0;
        Checked := True;
      end;
    end;
  finally
    lvRelations.Items.EndUpdate;
  end;

  Notebook.PageIndex := 2;
end;

procedure TfrmDataTableWizard.DoImportDataSet(D: TDataSet; const TableName: String = '');
var
  I: Integer;
  T: TCtxDataTable;
  TN: String;
begin
  T := FTempContainer.Tables.Add;
  TN := TableName;
  if TN = '' then
    TN := D.Name;
  if TN = '' then
    TN := FTempContainer.Tables.GetAutoName('Table', 1);

  T.Name := TN;
  if D.FieldCount > 0 then
  begin
    for I := 0 to D.FieldCount - 1 do
    with T.Columns.Add do
    begin
      ColumnName := D.Fields[I].FieldName;
      DataType := GetCtxDataType(D.Fields[I].DataType);
      if DataType in [cdtString, cdtWideString] then
        DataLength := D.Fields[I].DataSize;
      Required := D.Fields[I].Required;
      DisplayLabel := D.Fields[I].DisplayLabel;
    end;
  end else
  begin
    D.FieldDefs.Update;
    for I := 0 to D.FieldDefs.Count - 1 do
    with T.Columns.Add do
    begin
      ColumnName := D.FieldDefs[I].Name;
      DataType := GetCtxDataType(D.FieldDefs[I].DataType);
      if DataType in [cdtString, cdtWideString] then
        DataLength := D.FieldDefs[I].Size;
      Required := D.FieldDefs[I].Required;
    end;
  end;
end;

procedure TfrmDataTableWizard.ImportDataSet;
begin
  FTempContainer.ClearStructure;
  with cbxSourceObject do
    DoImportDataSet(Items.Objects[ItemIndex] as TDataSet);

  MergeToResultContainer;
  ModalResult := mrOK;
end;

procedure TfrmDataTableWizard.ImportTablesFromConnection;
var
  I: Integer;
  D: TDataSet;
begin
  FCtxDatabase.Connected := True;
  for I := 0 to lvTables.Items.Count - 1 do
  if lvTables.Items[I].Checked then
  begin
    D := FCtxDatabase.CreateTable(lvTables.Items[I].Caption);
    try
      DoImportDataSet(D, lvTables.Items[I].Caption);
    finally
      D.Free;
    end;
  end;
  MergeToResultContainer;
  ModalResult := mrOK;
end;

procedure TfrmDataTableWizard.ImportTablesFromSchema;
var
  T: TCtxDataTable;
  R: TCtxDataRelation;
  SchemaTable: TTableDefinition;
  SchemaRel: TRelationship;
  I, J: Integer;
begin
  FTempContainer.ClearStructure;

  for I := 0 to lvTables.Items.Count - 1 do
  if lvTables.Items[I].Checked then
  begin
    SchemaTable := FSchema.GetTableDef(lvTables.Items[I].Caption);

    T := FTempContainer.Tables.Add;
    T.Name := SchemaTable.TableName;
    for J := 0 to SchemaTable.FieldDefs.Count - 1 do
    with T.Columns.Add do
    begin
      ColumnName := SchemaTable.FieldDefs[J].Name;
      DataType := GetCtxDataType(FieldDataTypeToVCL[SchemaTable.FieldDefs[J].DataType]);
      if DataType in [cdtString, cdtWideString] then
        DataLength := SchemaTable.FieldDefs[J].Size;
      Required := SchemaTable.FieldDefs[J].Required;
      PrimaryKey := SchemaTable.FieldDefs[J].IsPrimaryKey;
      DisplayLabel := SchemaTable.FieldDefs[J].DisplayLabel;
    end;
  end;

  for I := 0 to lvRelations.Items.Count - 1 do
  if lvRelations.Items[I].Checked then
  begin
    SchemaRel := FSchema.Relationships.Find(lvRelations.Items[I].Caption);
    if SchemaRel = nil then continue;
    R := FTempContainer.Relations.Add;
    R.Name := SchemaRel.Name;
    R.ChildTableName := SchemaRel.DetailTableName;
    R.ParentTableName := SchemaRel.MasterTableName;
    R.ChildColumnNames := SchemaRel.DetailKeyFields;
    R.ParentColumnNames := SchemaRel.MasterKeyFields;
    R.OwnRows := SchemaRel.MasterOwnsDetails;
  end;

  MergeToResultContainer;
  ModalResult := mrOK;
end;

procedure TfrmDataTableWizard.ImportTablesFromDBAdapter;
var
  I: Integer;
  T: TCtxDataTable;
  CI: TCtxDBCommandItem;
begin
  FTempContainer.ClearStructure;

  for I := 0 to lvTables.Items.Count - 1 do
  if lvTables.Items[I].Checked then
  begin
    CI := FAdapter.Commands.Find(lvTables.Items[I].Caption) as TCtxDBCommandItem;
    CI.Command.Prepared := True;
    T := FTempContainer.Tables.Add;
    T.Name := CI.SourceTableName;
    CI.UpdateTableStructure(T);
    (*
    for F := 0 to CI.Command.Fields.Count - 1 do
    with CI.Command.Fields[F] do
      T.Columns.AddColumn(Name, DataType, False, DataSize);
    *)
  end;

  MergeToResultContainer;
  ModalResult := mrOK;
end;

procedure TfrmDataTableWizard.NotebookPageChanged(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TfrmDataTableWizard.FormCreate(Sender: TObject);
begin
  FTableNames := TStringList.Create;
  FTableNames.Duplicates := dupIgnore;
  FTableNames.Sorted := True;
  FTempContainer := TCtxDataContainer.Create(Self);
end;

procedure TfrmDataTableWizard.FormDestroy(Sender: TObject);
begin
  FTableNames.Free;
  FCtxDatabase := nil;
  FSchema := nil;
end;

procedure TfrmDataTableWizard.lvTablesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdateButtons;
end;

procedure TfrmDataTableWizard.btnSelectAllRelationsClick(Sender: TObject);
begin
  CheckAllItems(lvRelations, True);
end;

procedure TfrmDataTableWizard.btnSelectNoneRelationsClick(Sender: TObject);
begin
  CheckAllItems(lvRelations, False);
end;

procedure TfrmDataTableWizard.lvRelationsChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  UpdateButtons;
end;

procedure TfrmDataTableWizard.MergeToResultContainer;
var
  I: Integer;
  DestTable: TCtxDataTable;
  DestRelation: TCtxDataRelation;
begin
  FDataContainer.BeginUpdate;
  try
    with FTempContainer do
    for I := 0 to Tables.Count - 1 do
    begin
      DestTable := FDataContainer.Tables.Find(Tables[I].Name);
      if (DestTable = nil) or
        (MessageDlg(Format(STableExists, [Tables[I].Name]), mtConfirmation, [mbYes, mbNo], 0) <> mrYes)
      then
        DestTable := FDataContainer.Tables.Add;
      DestTable.Assign(Tables[I]);
    end;

    with FTempContainer do
    for I := 0 to Relations.Count - 1 do
    begin
      DestRelation := FDataContainer.Relations.Find(Relations[I].Name);
      if (DestRelation <> nil)
        and (
          (DestRelation.ChildTableName <> Relations[I].ChildTableName)
          or (DestRelation.ParentTableName <> Relations[I].ParentTableName))
      then DestRelation := nil;

      if DestRelation = nil then
        DestRelation := FDataContainer.Relations.Add;

      DestRelation.Assign(Relations[I]);
    end;
  finally
    FDataContainer.EndUpdate;
  end;
end;


end.
