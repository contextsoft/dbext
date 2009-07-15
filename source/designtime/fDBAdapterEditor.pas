(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Data Adapter editor
(*
(*  Copyright (c) 2005-2009, Context Software LLC
(*
(******************************************************************************)
unit fDBAdapterEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, ToolWin, ImgList, ActnList, Menus,
  CtxDataTypes, CtxData, CtxDBAdapter, CtxProfiles, CtxGridView, CtxPropView;

type
  TfrmDBAdapterEditor = class(TForm)
    Actions: TActionList;
    Images: TImageList;
    actSave: TAction;
    actLoad: TAction;
    actExecute: TAction;
    actAddParam: TAction;
    actDeleteParam: TAction;
    actAddCommand: TAction;
    pnlMain: TPanel;
    Splitter1: TSplitter;
    popCommands: TPopupMenu;
    pnlCommand: TPanel;
    Splitter2: TSplitter;
    pnlCommandOptions: TPanel;
    pnlLeft: TPanel;
    tvCommands: TTreeView;
    pnlViewBy: TPanel;
    cbxGroupBy: TComboBox;
    Label1: TLabel;
    lblParentTable: TLabel;
    cbxSourceTable: TComboBox;
    cbxParentTable: TComboBox;
    Label3: TLabel;
    cbxCommandType: TComboBox;
    popParams: TPopupMenu;
    edtCommandName: TEdit;
    Label4: TLabel;
    actClose: TAction;
    actBuildCommand: TAction;
    actConfigureFromDataContainer: TAction;
    actDeleteCommand: TAction;
    ToolBar: TToolBar;
    ToolButton4: TToolButton;
    ToolButton1: TToolButton;
    ToolButton7: TToolButton;
    ToolButton9: TToolButton;
    pcParams: TPageControl;
    tsParameters: TTabSheet;
    tsCommandSettings: TTabSheet;
    edtSQL: TMemo;
    ParamsToolBar: TToolBar;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton16: TToolButton;
    ToolButton13: TToolButton;
    gbUpdateOptions: TGroupBox;
    cbUpdateSourceRow: TCheckBox;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    AddCommand1: TMenuItem;
    actDeleteCommand1: TMenuItem;
    BuildSQLCommands1: TMenuItem;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    ToolButton5: TToolButton;
    rgResultType: TRadioGroup;
    actAddSelect: TAction;
    actAddRefresh: TAction;
    actAddInsert: TAction;
    actAddDelete: TAction;
    actAddUpdate: TAction;
    actProcedure: TAction;
    popAddCommands: TPopupMenu;
    AddSelectCommand1: TMenuItem;
    AddRefreshCommand1: TMenuItem;
    AddInsertCommand1: TMenuItem;
    AddUpdateCommand1: TMenuItem;
    AddDeleteCommand1: TMenuItem;
    AddProcedure1: TMenuItem;
    AppProfile: TCtxAppProfile;
    CtxFormSettings: TCtxFormSettings;
    Label2: TLabel;
    cbxDataProvider: TComboBox;
    Label5: TLabel;
    grdParams: TCtxCollectionView;
    ToolButton6: TToolButton;
    actEditCommand: TAction;
    ToolButton8: TToolButton;
    N1: TMenuItem;
    actCopyCommand: TAction;
    actPasteCommand: TAction;
    CopyCommand1: TMenuItem;
    PasteCommand1: TMenuItem;
    actCutCommand: TAction;
    CutCommand1: TMenuItem;
    procedure actCloseExecute(Sender: TObject);
    procedure actConfigureFromDataContainerExecute(Sender: TObject);
    procedure tvCommandsChange(Sender: TObject; Node: TTreeNode);
    procedure cbxGroupByChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbxCommandTypeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actAddCommandExecute(Sender: TObject);
    procedure actDeleteCommandExecute(Sender: TObject);
    procedure ActionsUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actAddParamExecute(Sender: TObject);
    procedure actDeleteParamExecute(Sender: TObject);
    procedure tvCommandsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvCommandsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure cbxSourceTableExit(Sender: TObject);
    procedure actBuildCommandExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure grdParamsDrawMarker(Sender: TObject; ARow: Integer;
      ARect: TRect);
    procedure actEditCommandExecute(Sender: TObject);
    procedure actCopyCommandExecute(Sender: TObject);
    procedure actPasteCommandExecute(Sender: TObject);
    procedure actCutCommandExecute(Sender: TObject);
  private
    { Private declarations }
    FDBAdapter: TCtxDBAdapter;
    FCurrentCommand: TCtxDBCommandItem;
    procedure DrawObjectIcon(Canvas: TCanvas; Rect: TRect; Collection: TCollection; Idx: Integer);
  public
    { Public declarations }
    procedure FillTables(List: TStrings);
    procedure FillCommands;
    procedure UpdateCommandCaptions;
    procedure SetupCommand(Command: TCtxDBCommandItem);

    procedure FormToCommand;
    procedure CommandToForm;

    procedure FillCommandTypes;
  end;

var
  frmDBAdapterEditor: TfrmDBAdapterEditor;

  procedure EditDBAdapter(ADBAdapter: TCtxDBAdapter);

resourcestring
  SConfirmDeleteCommand = 'Are you sure you want to delete command %s?';
  SConfirmDeleteParam = 'Are you sure you want to delete parameter "%s"?';

implementation

uses fCommandBuilder, Math, Clipbrd;

{$R *.dfm}

procedure EditDBAdapter(ADBAdapter: TCtxDBAdapter);
begin
  with TfrmDBAdapterEditor.Create(nil) do
  try
    FDBAdapter := ADBAdapter;
    ShowModal;
  finally
    Free;
  end;
end;


procedure TfrmDBAdapterEditor.FormCreate(Sender: TObject);
begin
  FillCommandTypes;
end;

procedure TfrmDBAdapterEditor.FormShow(Sender: TObject);
begin
  pcParams.ActivePageIndex := 0;
  FCurrentCommand := nil;
  FillCommands;
  FillTables(cbxSourceTable.Items);
  FillTables(cbxParentTable.Items);
  GetCtxDataProviders(cbxDataProvider.Items);
end;

procedure TfrmDBAdapterEditor.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmDBAdapterEditor.actConfigureFromDataContainerExecute(
  Sender: TObject);
begin
  // Select data container
end;

procedure TfrmDBAdapterEditor.FillCommands;

var
  CurNode: TTreeNode;

  function AddCommand(Node: TTreeNode; const ACaption: String;
    ACommand: TCtxDBCommandItem; ImageIndex: Integer): TTreeNode;
  begin
    Result := tvCommands.Items.AddChildObject(Node, ACaption, ACommand);
    Result.ImageIndex := ImageIndex;
    Result.SelectedIndex := ImageIndex;
    if (CurNode = nil) and ((ACommand = FCurrentCommand) or (FCurrentCommand = nil)) then
      CurNode := Result;
  end;

var
  I: Integer;
  J: Integer;
  T: TCtxDBCommandItemType;
  Node: TTreeNode;
  Tables: TStringList;
begin
  // keep selected command
  tvCommands.Items.BeginUpdate;
  try
    tvCommands.Items.Clear;
    CurNode := nil;
    if cbxGroupBy.ItemIndex = 0 then
    begin
      // Group by tables
      Tables := TStringList.Create;
      try
        FillTables(Tables);
        for I := 0 to Tables.Count - 1 do
        begin
          Node := tvCommands.Items.Add(nil, Tables[I]);
          Node.ImageIndex := 0;
          Node.SelectedIndex := 0;
          with FDBAdapter do
          for J := 0 to Commands.Count - 1 do
          if AnsiSameText(Commands[J].SourceTableName, Tables[I]) then
            AddCommand(Node, Commands[J].Name, Commands[J], Ord(Commands[J].CommandType) + 2);
        end;
      finally
        Tables.Free;
      end;
    end else
    begin
      // Group by commands
      for T := Low(TCtxDBCommandItemType) to High(TCtxDBCommandItemType) do
      begin
        Node := tvCommands.Items.Add(nil, SCommandItemTypes[T]);
        Node.ImageIndex := Ord(T) + 2;
        Node.SelectedIndex := Ord(T) + 2;
        with FDBAdapter do
        for J := 0 to Commands.Count - 1 do
        if Commands[J].CommandType = T then
          AddCommand(Node, Commands[J].Name, Commands[J], 0);
      end;
    end;

    tvCommands.FullExpand;

    if CurNode <> nil then
      tvCommands.Selected := CurNode;
  finally
    tvCommands.Items.EndUpdate;
  end;
  if tvCommands.Selected <> nil then
    SetupCommand(tvCommands.Selected.Data)
  else SetupCommand(nil);
end;

procedure TfrmDBAdapterEditor.FillTables(List: TStrings);
var
  I: Integer;
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.Duplicates := dupIgnore;
    L.Sorted := True;
    with FDBAdapter do
    for I := 0 to Commands.Count - 1 do
    if Commands[I].SourceTableName <> '' then
      L.Add(Commands[I].SourceTableName);
    List.Assign(L);
  finally
    L.Free;
  end;
end;

procedure TfrmDBAdapterEditor.FormToCommand;
begin
  if FCurrentCommand = nil then exit;

  FCurrentCommand.ParentTableName := cbxParentTable.Text;
  FCurrentCommand.CommandType := TCtxDBCommandItemType(cbxCommandType.ItemIndex);
  FCurrentCommand.SourceTableName := cbxSourceTable.Text;
  FCurrentCommand.DataProviderName := cbxDataProvider.Text;
  FCurrentCommand.CommandText := edtSQL.Text;
  FCurrentCommand.Name := edtCommandName.Text;
  FCurrentCommand.UpdateSourceRow := cbUpdateSourceRow.Checked;
  FCurrentCommand.ResultType := TCtxDBResultType(rgResultType.ItemIndex);

  // UpdateCommandCaptions;
  FillTables(cbxSourceTable.Items);
  FillTables(cbxParentTable.Items);
end;

procedure TfrmDBAdapterEditor.CommandToForm;
begin
  if FCurrentCommand = nil then
  begin
    pnlCommand.Enabled := False;
    pnlCommandOptions.Enabled := False;
    edtSQL.Enabled := False;
    pcParams.Enabled := False;

    cbxParentTable.Text := '';
    cbxCommandType.ItemIndex := -1;
    cbxSourceTable.Text := '';
    cbxDataProvider.Text := '';
    edtSQL.Text := '';
    edtCommandName.Text := '';
    cbUpdateSourceRow.Checked := False;
    rgResultType.ItemIndex := 0;
    rgResultType.Enabled := False;
    grdParams.Collection := nil;
  end else
  begin
    pnlCommand.Enabled := True;
    pnlCommandOptions.Enabled := True;
    edtSQL.Enabled := True;
    pcParams.Enabled := True;
    rgResultType.Enabled := True;

    cbxParentTable.Text := FCurrentCommand.ParentTableName;
    cbxCommandType.ItemIndex := Ord(FCurrentCommand.CommandType);
    cbxSourceTable.Text := FCurrentCommand.SourceTableName;
    cbxDataProvider.Text := FCurrentCommand.DataProviderName;
    edtSQL.Text := FCurrentCommand.CommandText;
    edtCommandName.Text := FCurrentCommand.Name;
    cbUpdateSourceRow.Checked := FCurrentCommand.UpdateSourceRow;
    rgResultType.ItemIndex := Ord(FCurrentCommand.ResultType);

    grdParams.Collection := FCurrentCommand.Params;
    cbxCommandTypeChange(nil);
  end;
  grdParams.Invalidate;
end;

procedure TfrmDBAdapterEditor.SetupCommand(Command: TCtxDBCommandItem);
begin
  if Command <> FCurrentCommand then
  begin
    // Update current command from controls
    FormToCommand;
    FCurrentCommand := Command;
    CommandToForm;
  end;
end;

procedure TfrmDBAdapterEditor.tvCommandsChange(Sender: TObject;
  Node: TTreeNode);
begin
  if Node.Data <> nil then
    SetupCommand(TCtxDBCommandItem(Node.Data));
end;

procedure TfrmDBAdapterEditor.cbxGroupByChange(Sender: TObject);
begin
  FillCommands; 
end;

procedure TfrmDBAdapterEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FormToCommand;
end;

procedure TfrmDBAdapterEditor.cbxCommandTypeChange(Sender: TObject);
begin
  lblParentTable.Enabled := cbxCommandType.ItemIndex = Ord(citSelect);
  cbxParentTable.Enabled := lblParentTable.Enabled;
  if not cbxParentTable.Enabled then
    cbxParentTable.ItemIndex := -1;
end;

procedure TfrmDBAdapterEditor.FillCommandTypes;
var
  I: TCtxDBCommandItemType;
begin
  with cbxCommandType.Items do
  begin
    BeginUpdate;
    try
      for I := Low(TCtxDBCommandItemType) to High(TCtxDBCommandItemType) do
        Add(SCommandItemTypes[I]);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TfrmDBAdapterEditor.actAddCommandExecute(Sender: TObject);
var
  LastTable: String;
  CommandType: TCtxDBCommandItemType;
begin
  // Comand builder
  FormToCommand;
  if FCurrentCommand <> nil then
    LastTable := FCurrentCommand.SourceTableName
  else LastTable := cbxSourceTable.Text;

  if Assigned(Sender) and Sender.InheritsFrom(TComponent) then
    CommandType := TCtxDBCommandItemType(TComponent(Sender).Tag)
  else CommandType := citSelect;

  SetupCommand(FDBAdapter.Commands.AddCommand(LastTable, CommandType));
  (*
  cbxSourceTable.Text := LastTable;
  edtCommandName.Text :=
    FDBAdapter.Commands.GetAutoName(LastTable + '.' +
      SCommandItemTypes[TCtxDBCommandItemType(cbxCommandType.ItemIndex)]);
  FormToCommand;
  *)
  FillCommands;
end;

procedure TfrmDBAdapterEditor.actDeleteCommandExecute(Sender: TObject);
begin
  if FCurrentCommand <> nil then
  begin
    if (MessageDlg(Format(SConfirmDeleteCommand, [FCurrentCommand.Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      FCurrentCommand.Free;
      FCurrentCommand := nil;
      CommandToForm;
      FillCommands;
    end;
  end;
end;

procedure TfrmDBAdapterEditor.ActionsUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  actDeleteCommand.Enabled := FCurrentCommand <> nil;
  actDeleteParam.Enabled := (grdParams.Collection <> nil) and (grdParams.Collection.Count > 0);
  actEditCommand.Enabled := (FCurrentCommand <> nil) and (FindCommandBuilder(FCurrentCommand) <> nil);
  actCopyCommand.Enabled := FCurrentCommand <> nil;
end;

procedure TfrmDBAdapterEditor.actAddParamExecute(Sender: TObject);
begin
  grdParams.InsertItem(True {Append});
end;

procedure TfrmDBAdapterEditor.actDeleteParamExecute(Sender: TObject);
begin
  if grdParams.CurrentItem = nil then exit;
  if (MessageDlg(Format(SConfirmDeleteParam, [TCtxParameter(grdParams.CurrentItem).Name]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    grdParams.DeleteItem;
end;

procedure TfrmDBAdapterEditor.DrawObjectIcon(Canvas: TCanvas; Rect: TRect; Collection: TCollection; Idx: Integer);
begin
  if (Collection <> nil) and (Idx < Collection.Count) then
    Images.Draw(Canvas, Rect.Left, Rect.Top, 15);
end;

procedure TfrmDBAdapterEditor.tvCommandsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  N: TTreeNode;
begin
  Accept := (Source = tvCommands) and (tvCommands.Selected.Data <> nil);
  if Accept then
  begin
    N := tvCommands.GetNodeAt(X, Y);
    Accept := (N <> nil) and (N.Data <> nil);
  end;
end;

procedure TfrmDBAdapterEditor.tvCommandsDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  N: TTreeNode;
begin
  if (Source = tvCommands) and (tvCommands.Selected.Data <> nil) then
  begin
    N := tvCommands.GetNodeAt(X, Y);
    if (N <> nil) and (N.Data <> nil) then
    begin
      TCtxDBCommandItem(tvCommands.Selected.Data).Index := TCtxDBCommandItem(N.Data).Index;
      FillCommands;
    end;
  end;
end;

procedure TfrmDBAdapterEditor.UpdateCommandCaptions;
var
  I: Integer;
begin
  // Update captions for all commands
  with tvCommands do
  for I := 0 to Items.Count - 1 do
    if Items[I].Data <> nil then
      Items[I].Text := TCtxDBCommandItem(Items[I].Data).Name;
end;

procedure TfrmDBAdapterEditor.cbxSourceTableExit(Sender: TObject);
begin
  FormToCommand;
  FillCommands;
end;

procedure TfrmDBAdapterEditor.actBuildCommandExecute(Sender: TObject);
begin
  // Run command builder
  FormToCommand;
  if ExecuteCommandBuilder(FDBAdapter) then
    FillCommands;
end;

procedure TfrmDBAdapterEditor.actExecuteExecute(Sender: TObject);
begin
  FDBAdapter.CheckDataProvider;
  // Execute selected command
  if FCurrentCommand <> nil then
  begin
    FormToCommand;
    with FCurrentCommand.CreateCommand do
    try
      Prepared := True;
      Execute;
    finally
      Free;
    end;
    MessageDlg('Command executed succesfully!', mtInformation, [mbOK], 0);
  end;
end;

procedure TfrmDBAdapterEditor.grdParamsDrawMarker(Sender: TObject;
  ARow: Integer; ARect: TRect);
begin
  DrawObjectIcon(grdParams.Canvas, ARect, grdParams.Collection, ARow)
end;

procedure TfrmDBAdapterEditor.actEditCommandExecute(Sender: TObject);
begin
  if FCurrentCommand = nil then exit;

  FormToCommand;
  if FCurrentCommand.EditCommandItem then
  begin
    CommandToForm;
    UpdateCommandCaptions;
  end;
end;

procedure TfrmDBAdapterEditor.actCopyCommandExecute(Sender: TObject);
var
  TempAdapter: TCtxDBAdapter;
begin
  if FCurrentCommand = nil then exit;
  FormToCommand;
  TempAdapter := TCtxDBAdapter.Create(nil);
  try
    TempAdapter.Commands.Add.Assign(FCurrentCommand);
    Clipboard.SetComponent(TempAdapter);
  finally
    TempAdapter.Free;
  end;
end;

procedure TfrmDBAdapterEditor.actPasteCommandExecute(Sender: TObject);
var
  TempAdapter: TCtxDBAdapter;
  I: Integer;
begin
  FormToCommand;
  if not Clipboard.HasFormat(CF_COMPONENT) then exit;
  TempAdapter := TCtxDBAdapter(Clipboard.GetComponent(nil, nil));
  if (TempAdapter <> nil) and TempAdapter.InheritsFrom(TCtxDBAdapter) then
  try
    for I := 0 to TempAdapter.Commands.Count - 1 do
      FDBAdapter.Commands.Add.Assign(TempAdapter.Commands[I]);
  finally
    TempAdapter.Free;
  end;
  FillCommands;
end;

procedure TfrmDBAdapterEditor.actCutCommandExecute(Sender: TObject);
begin
  actCopyCommand.Execute;
  actDeleteCommand.Execute;
end;

end.



