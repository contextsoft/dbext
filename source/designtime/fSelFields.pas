(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Fields Selection Dialog (TfrmSelectFields)
(*
(*  Copyright (c) 2005-2009, Context Software LLC
(*
(******************************************************************************)
unit fSelFields;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs, Controls, StdCtrls,
  dbSchema, Buttons;

type
  TfrmSelectFields = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    SrcList: TListBox;
    DstList: TListBox;
    SrcLabel: TLabel;
    DstLabel: TLabel;
    IncludeBtn: TSpeedButton;
    IncAllBtn: TSpeedButton;
    ExcludeBtn: TSpeedButton;
    ExAllBtn: TSpeedButton;
    Label1: TLabel;
    cbxIndex: TComboBox;
    procedure IncludeBtnClick(Sender: TObject);
    procedure ExcludeBtnClick(Sender: TObject);
    procedure IncAllBtnClick(Sender: TObject);
    procedure ExcAllBtnClick(Sender: TObject);
    procedure cbxIndexChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function GetSelectedFields(List: TStrings): String;
    procedure SetSelectedFields(List: TStrings; const Value: String);
    { Private declarations }
  public
    { Public declarations }
    TableDef: TTableDefinition;

    procedure MoveSelected(FromList, ToList: TCustomListBox);
    procedure SetItem(List: TListBox; Index: Integer);
    function GetFirstSelection(List: TCustomListBox): Integer;
    procedure SetButtons;
    procedure SetupAvailableFields(Index: Integer);
    procedure SetupAvailableIndexes;

    property SelectedFields[List: TStrings]: String read GetSelectedFields write SetSelectedFields;
  end;

var
  frmSelectFields: TfrmSelectFields;

  function SelectFields(ATableDef: TTableDefinition; var Fields: String): Boolean;

implementation

{$R *.DFM}

function SelectFields(ATableDef: TTableDefinition; var Fields: String): Boolean;
begin
  with TfrmSelectFields.Create(nil) do
  try
    TableDef := ATableDef;
    SelectedFields[DstList.Items] := Fields;
    Result := ShowModal = mrOK;
    if Result then
      Fields := SelectedFields[DstList.Items];
  finally
    Free;
  end;
end;

procedure TfrmSelectFields.IncludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(SrcList);
  MoveSelected(SrcList, DstList);
  SetItem(SrcList, Index);
end;

procedure TfrmSelectFields.ExcludeBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(DstList);
  MoveSelected(DstList, SrcList);
  SetItem(DstList, Index);
end;

procedure TfrmSelectFields.IncAllBtnClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to SrcList.Items.Count - 1 do
    DstList.Items.AddObject(SrcList.Items[I], SrcList.Items.Objects[I]);
  SetItem(SrcList, 0);
end;

procedure TfrmSelectFields.ExcAllBtnClick(Sender: TObject);
begin
  DstList.Items.Clear;
  SetItem(DstList, 0);
end;

procedure TfrmSelectFields.MoveSelected(FromList, ToList: TCustomListBox);
var
  I: Integer;
begin
  for I := FromList.Items.Count - 1 downto 0 do
    if FromList.Selected[I] then
    begin
      if ToList <> SrcList then
        ToList.Items.AddObject(FromList.Items[I], FromList.Items.Objects[I]);
      if FromList <> SrcList then
        FromList.Items.Delete(I);
    end;
end;

procedure TfrmSelectFields.SetButtons;
var
  SrcEmpty, DstEmpty: Boolean;
begin
  SrcEmpty := SrcList.Items.Count = 0;
  DstEmpty := DstList.Items.Count = 0;
  IncludeBtn.Enabled := not SrcEmpty;
  IncAllBtn.Enabled := not SrcEmpty;
  ExcludeBtn.Enabled := not DstEmpty;
  ExAllBtn.Enabled := not DstEmpty;
end;

function TfrmSelectFields.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count - 1 do
    if List.Selected[Result] then Exit;
  Result := LB_ERR;
end;

procedure TfrmSelectFields.SetItem(List: TListBox; Index: Integer);
var
  MaxIndex: Integer;
begin
  with List do
  begin
    SetFocus;
    MaxIndex := List.Items.Count - 1;
    if Index = LB_ERR then Index := 0
    else if Index > MaxIndex then Index := MaxIndex;
    Selected[Index] := True;
  end;
  SetButtons;
end;

function TfrmSelectFields.GetSelectedFields(List: TStrings): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to List.Count - 1 do
    Result := Result + List[I] + ';';
  System.Delete(Result, Length(Result), 1);
end;

procedure TfrmSelectFields.SetSelectedFields(List: TStrings; const Value: String);
var
  I: Integer;
  TempStr: String;
begin
  List.Clear;
  I := 1;
  repeat
    TempStr := NextToken(Value, ';', I);
    if TempStr <> '' then
      List.Add(TempStr);
  until I > Length(Value);
end;

procedure TfrmSelectFields.cbxIndexChange(Sender: TObject);
begin
  SetupAvailableFields(cbxIndex.ItemIndex - 1);
end;

procedure TfrmSelectFields.SetupAvailableFields(Index: Integer);
var
  I: Integer;
begin
  SrcList.Items.Clear;
  if TableDef <> nil then
  begin
    if Index < 0 then
    begin
      for I := 0 to TableDef.FieldDefs.Count - 1 do
        if TableDef.FieldDefs[I].Name <> '' then
          SrcList.Items.Add(TableDef.FieldDefs[I].Name);
    end else begin
      SetSelectedFields(SrcList.Items, TableDef.IndexDefs[Index].Fields);
    end;
  end;
  SetButtons;
end;

procedure TfrmSelectFields.SetupAvailableIndexes;
var
  I: Integer;
begin
  if TableDef = nil then exit;
  cbxIndex.Items.Clear;
  cbxIndex.Items.Add('<All Fields>');
  for I := 0 to TableDef.IndexDefs.Count - 1 do
    cbxIndex.Items.Add(TableDef.IndexDefs[I].DisplayName);
end;

procedure TfrmSelectFields.FormShow(Sender: TObject);
begin
  SetupAvailableIndexes;
  cbxIndex.ItemIndex := 0;
  SetupAvailableFields(cbxIndex.ItemIndex - 1);
end;

end.
