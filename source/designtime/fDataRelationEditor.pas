unit fDataRelationEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ActnList, Menus,
  CtxData, CtxGridView, CtxPropView;

type
  TfrmDataRelationEditor = class(TForm)
    Label1: TLabel;
    cbxChildTable: TComboBox;
    Label3: TLabel;
    cbxParentTable: TComboBox;
    Label5: TLabel;
    cbOwner: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    Bevel1: TBevel;
    edtName: TEdit;
    grdColumns: TCtxCollectionView;
    popColumns: TPopupMenu;
    Add1: TMenuItem;
    Remove1: TMenuItem;
    Actions: TActionList;
    actAddColumnPair: TAction;
    actRemoveColumnPair: TAction;
    Label2: TLabel;
    cbxUpdateAction: TComboBox;
    cbxDeleteAction: TComboBox;
    Label4: TLabel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actAddColumnPairExecute(Sender: TObject);
    procedure actRemoveColumnPairExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxChildTableChange(Sender: TObject);
    procedure cbxParentTableChange(Sender: TObject);
    procedure grdColumnsDrawMarker(Sender: TObject; ARow: Integer;
      ARect: TRect);
  private
    { Private declarations }
    FColumns: TOwnedCollection;
    FRelation: TCtxDataRelation;
  public
    { Public declarations }
    procedure DataToForm;
    procedure FormToData;
    procedure UpdateColumnList(const TableName: String; List: TStrings);
  end;

  TRelationColumns = class (TCollectionItem)
  private
    FParentColumn: String;
    FChildColumn: String;
  published
    property ChildColumn: String read FChildColumn write FChildColumn;
    property ParentColumn: String read FParentColumn write FParentColumn;
  end;

var
  frmDataRelationEditor: TfrmDataRelationEditor;

  function EditDataRelation(ARelation: TCtxDataRelation): Boolean;

resourcestring
  SRelationNameEmpty = 'Please specify relation name';
  SDuplicateRelationName = 'Duplicate relation name: %s';
  SColumnNameEmpty = 'Please enter non empty column name';

implementation

{$R *.dfm}

uses Math;

function EditDataRelation(ARelation: TCtxDataRelation): Boolean;
begin
  with TfrmDataRelationEditor.Create(nil) do
  try
    FRelation := ARelation;
    DataToForm;
    Result := ShowModal = mrOK;
  finally
    Free;
  end;
end;

{ TfrmDataRelationEditor }

procedure TfrmDataRelationEditor.FormCreate(Sender: TObject);
begin
  FColumns := TOwnedCollection.Create(Self, TRelationColumns);
end;

procedure TfrmDataRelationEditor.FormDestroy(Sender: TObject);
begin
  FColumns.Free;
end;

procedure TfrmDataRelationEditor.FormShow(Sender: TObject);
begin
  if FColumns.Count = 0 then
    FColumns.Add;
  grdColumns.Collection := FColumns;
end;

procedure TfrmDataRelationEditor.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOK then
    FormToData;
end;

procedure TfrmDataRelationEditor.DataToForm;
var
  I: Integer;
  AParentTable, AChildTable: TCtxDataTable;
  AParentColumns, AChildColumns: TCtxColumnArray;
begin
  with FRelation do
  begin
    edtName.Text := Name;
    cbOwner.Checked := OwnRows;

    DataContainer.Tables.GetNames(cbxChildTable.Items);
    cbxParentTable.Items := cbxChildTable.Items;

    with cbxChildTable do
      ItemIndex := Items.IndexOf(ChildTableName);
    with cbxParentTable do
      ItemIndex := Items.IndexOf(ParentTableName);

    AParentTable := DataContainer.Tables.Find(ParentTableName);
    if AParentTable <> nil then
      AParentColumns := AParentTable.Columns.GetColumnArray(ParentColumnNames)
    else SetLength(AParentColumns, 0);

    AChildTable := DataContainer.Tables.Find(ChildTableName);
    if AChildTable <> nil then
      AChildColumns := AChildTable.Columns.GetColumnArray(ChildColumnNames)
    else SetLength(AChildColumns, 0);

    for I := 0 to Max(Length(AChildColumns), Length(AParentColumns)) - 1 do
    with TRelationColumns(FColumns.Add) do
    begin
      if I < Length(AParentColumns) then
        ParentColumn := AParentColumns[I].ColumnName;
      if I < Length(AChildColumns) then
        ChildColumn := AChildColumns[I].ColumnName;
    end;

    UpdateColumnList(cbxParentTable.Text, grdColumns.Columns.Find('ParentColumn').ValueList);
    UpdateColumnList(cbxChildTable.Text, grdColumns.Columns.Find('ChildColumn').ValueList);

    cbxDeleteAction.ItemIndex := Integer(DeleteAction);
    cbxUpdateAction.ItemIndex := Integer(UpdateAction);
  end;
end;

procedure TfrmDataRelationEditor.FormToData;
var
  I: Integer;
  MT, DT: TCtxDataTable;
  TempName: String;
begin
  with FRelation do
  begin
    TempName := Trim(edtName.Text);
    if TempName = '' then
      raise Exception.Create(SRelationNameEmpty);

    if not DataContainer.Relations.IsUnique(TempName, FRelation) then
      raise Exception.Create(SDuplicateRelationName);


    Name := TempName;
    OwnRows := cbOwner.Checked;

    DT := DataContainer.Tables.Get(cbxChildTable.Text);
    MT := DataContainer.Tables.Get(cbxParentTable.Text);

    ChildTableName := cbxChildTable.Text;
    ParentTableName := cbxParentTable.Text;

    ChildColumnNames := '';
    ParentColumnNames := '';


    for I := 0 to FColumns.Count - 1 do
    with TRelationColumns(FColumns.Items[I]) do
    begin
      if (ChildColumn = '') and (ParentColumn = '') then continue;

      if (ChildColumn = '') or (ParentColumn = '') then
        raise Exception.Create(SColumnNameEmpty);

      DT.Columns.Get(ChildColumn);
      MT.Columns.Get(ParentColumn);

      if ChildColumnNames <> '' then
        ChildColumnNames := ChildColumnNames + ';';
      ChildColumnNames := ChildColumnNames + ChildColumn;
      if ParentColumnNames <> '' then
        ParentColumnNames := ParentColumnNames + ';';
      ParentColumnNames := ParentColumnNames + ParentColumn;
    end;

    DeleteAction := TCtxDataRelationAction(cbxDeleteAction.ItemIndex);
    UpdateAction := TCtxDataRelationAction(cbxUpdateAction.ItemIndex);
  end;
end;

procedure TfrmDataRelationEditor.actAddColumnPairExecute(Sender: TObject);
begin
  grdColumns.InsertItem(True);
end;

procedure TfrmDataRelationEditor.actRemoveColumnPairExecute(
  Sender: TObject);
begin
  grdColumns.DeleteItem;
end;

procedure TfrmDataRelationEditor.UpdateColumnList(const TableName: String;
  List: TStrings);
var
  T: TCtxDataTable;
begin
  T := FRelation.DataContainer.Tables.Find(TableName);
  if T <> nil then
    T.Columns.GetNames(List)
  else List.Clear;
end;

procedure TfrmDataRelationEditor.cbxChildTableChange(Sender: TObject);
begin
  UpdateColumnList(cbxChildTable.Text, grdColumns.Columns.Find('ChildColumn').ValueList);
end;

procedure TfrmDataRelationEditor.cbxParentTableChange(Sender: TObject);
var
  J, I: Integer;
  T: TCtxDataTable;
begin
  T := FRelation.DataContainer.Tables.Find(cbxParentTable.Text);
  UpdateColumnList(cbxParentTable.Text, grdColumns.Columns.Find('ParentColumn').ValueList);
  if (FColumns.Count = 0) or (TRelationColumns(FColumns.Items[0]).ParentColumn = '') and (T <> nil) then
  begin
    J := 0;
    for I := 0 to T.Columns.Count - 1 do
    if T.Columns[I].PrimaryKey then
    begin
      if J >= FColumns.Count then FColumns.Add;
      TRelationColumns(FColumns.Items[J]).ParentColumn := T.Columns[I].ColumnName;
    end;
  end;
  grdColumns.Refresh;
end;

procedure TfrmDataRelationEditor.grdColumnsDrawMarker(Sender: TObject;
  ARow: Integer; ARect: TRect);
begin
  //
end;

end.
