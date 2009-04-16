unit fCommandBuilder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DB,
  CtxDBIntf, CtxDataTypes, CtxData, CtxDBAdapter, CtxGridView, CtxPropView;

type
  TfrmCommandBuilder = class(TForm)
    pnlGenerate: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Bevel2: TBevel;
    cbxTableName: TComboBox;
    cbSelect: TCheckBox;
    cbInsert: TCheckBox;
    cbDelete: TCheckBox;
    cbUpdate: TCheckBox;
    grdColumns: TCtxCollectionView;
    Panel1: TPanel;
    Splitter1: TSplitter;
    btnOK: TButton;
    btnCancel: TButton;
    cbRefresh: TCheckBox;
    Bevel1: TBevel;
    Label4: TLabel;
    cbxDatabase: TComboBox;
    TempContainer: TCtxDataContainer;
    Label5: TLabel;
    edtSourceTable: TEdit;
    procedure FormShow(Sender: TObject);
    procedure cbxTableNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cbxDatabaseChange(Sender: TObject);
  private
    { Private declarations }
    FDBAdapter: TCtxDBAdapter;
    FColumnItems: TOwnedCollection;
  public
    { Public declarations }
    procedure FillTables(List: TStrings);
    procedure FillDatabases(List: TStrings);
    procedure GenerateCommands;
  end;

  TColumnItem = class (TCollectionItem)
  private
    FUpdate: Boolean;
    FSelect: Boolean;
    FKey: Boolean;
    FDataLength: Integer;
    FColumnName: String;
    FColumnType: TCtxDataType;
  published
    property Select: Boolean read FSelect write FSelect;
    property Update: Boolean read FUpdate write FUpdate;
    property Key: Boolean read FKey write FKey;
    property ColumnName: String read FColumnName write FColumnName;
    property ColumnType: TCtxDataType read FColumnType write FColumnType;
    property DataLength: Integer read FDataLength write FDataLength;
  end;


var
  frmCommandBuilder: TfrmCommandBuilder;

  function ExecuteCommandBuilder(ADBAdapter: TCtxDBAdapter): Boolean;

resourcestring
  SNoColumnsSelected = 'Please select one or more columns';

implementation

{$R *.dfm}

function ExecuteCommandBuilder(ADBAdapter: TCtxDBAdapter): Boolean;
begin
  ADBAdapter.CheckDataProvider;
  with TfrmCommandBuilder.Create(nil) do
  try
    FDBAdapter := ADBAdapter;
    Result := ShowModal = mrOK;
  finally
    Free;
  end;
end;

{ TfrmCommandBuilder }

procedure TfrmCommandBuilder.FormCreate(Sender: TObject);
begin
  FColumnItems := TOwnedCollection.Create(Self, TColumnItem);
  grdColumns.Collection := FColumnItems;
end;

procedure TfrmCommandBuilder.FormDestroy(Sender: TObject);
begin
  grdColumns.Collection := nil;
  FColumnItems.Free;
end;

procedure TfrmCommandBuilder.FormShow(Sender: TObject);
begin
  FillDatabases(cbxDatabase.Items);
  cbxDatabase.ItemIndex := 0;
  cbxDatabaseChange(nil);
end;

procedure TfrmCommandBuilder.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOK then
    GenerateCommands;
end;

procedure TfrmCommandBuilder.cbxDatabaseChange(Sender: TObject);
begin
  // database changed
  FillTables(cbxTableName.Items);
  with cbxTableName do
    if Items.Count > 0 then
      ItemIndex := 0;
  cbxTableNameChange(nil);
end;

procedure TfrmCommandBuilder.FillDatabases(List: TStrings);
begin
  List.Clear;
  List.Add('Connection');
end;

function Capitalize(const Value: String): String;
begin
  Result := AnsiLowerCase(Value);
  if Result <> '' then
    Result[1] := AnsiUpperCase(Result[1])[1];
end;

procedure TfrmCommandBuilder.FillTables(List: TStrings);
var
  DB: ICtxDatabase;
begin
  List.BeginUpdate;
  try
    List.Clear;
    if Supports(FDBAdapter.DataProviderObject, ICtxDatabase, DB) then
      DB.GetTableNames(List);
  finally
    List.EndUpdate;
  end;
end;

procedure TfrmCommandBuilder.cbxTableNameChange(Sender: TObject);
var
  I: Integer;
  DB: ICtxDatabase;
  DataSet: TDataSet;
begin
  // Display columns
  FColumnItems.Clear;

  if Supports(FDBAdapter.DataProviderObject, ICtxDatabase, DB) then
  begin
    DataSet := DB.CreateTable(cbxTableName.Text);
    try
      DataSet.FieldDefs.Update;
      for I := 0 to DataSet.FieldDefs.Count - 1 do
      with TColumnItem(FColumnItems.Add) do
      begin
        Select := True;
        Key := False;
        Update := True;
        ColumnName := DataSet.FieldDefs[I].Name;
        ColumnType := GetCtxDataType(DataSet.FieldDefs[I].DataType);
      end;
    finally
      DataSet.Free;
    end;
  end;

  grdColumns.Refresh;
  edtSourceTable.Text := Capitalize(cbxTableName.Text);
end;

procedure TfrmCommandBuilder.GenerateCommands;
var
  I: Integer;
  T: TCtxDataTable;
  Column: TCtxDataColumn;
  CommandTypes: TCtxDBCommandItemTypes;
begin
  CommandTypes := [];
  if cbSelect.Checked then
    Include(CommandTypes, citSelect);
  if cbRefresh.Checked then
    Include(CommandTypes, citRefresh);
  if cbInsert.Checked then
    Include(CommandTypes, citInsert);
  if cbDelete.Checked then
    Include(CommandTypes, citDelete);
  if cbUpdate.Checked then
    Include(CommandTypes, citUpdate);
  if CommandTypes = [] then exit;

  // Generate commands based on selected settings directly into FDBAdapter
  TempContainer.ClearStructure;

  T := TempContainer.Tables.Add;
  T.Name := edtSourceTable.Text;
  for I := 0 to FColumnItems.Count - 1 do
  with TColumnItem(FColumnItems.Items[I]) do
  if Select then
  begin
    Column := T.Columns.AddColumn(ColumnName, ColumnType, False, DataLength);
    Column.PrimaryKey := Key;
    Column.ReadOnly := not Update;
  end;

  if T.Columns.Count = 0 then
    raise Exception.Create(SNoColumnsSelected);

  FDBAdapter.ConfigureTableCommands(T, CommandTypes);
end;

end.
