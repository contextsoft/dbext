(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Data provider selection dialog
(*
(*  Copyright (c) 2005-2010, Context Software LLC
(*
(******************************************************************************)
unit fSelectDataProvider;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, CtxDataTypes, ImgList;

type
  TfrmSelectDataProvider = class(TForm)
    ProvidersTreeView: TTreeView;
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    pnlDescription: TPanel;
    lblDescription: TLabel;
    Images: TImageList;
    procedure ProvidersTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ProvidersTreeViewDblClick(Sender: TObject);
  private
    { Private declarations }
    FDataProviderName: String;
  public
    { Public declarations }
    procedure FillProvidersTreeView;
  end;

var
  frmSelectDataProvider: TfrmSelectDataProvider;

  function SelectDataProvider(var DataProviderName: String): Boolean;

implementation

{$R *.dfm}

function SelectDataProvider(var DataProviderName: String): Boolean;
begin
  with TfrmSelectDataProvider.Create(nil) do
  try
    FDataProviderName := DataProviderName;
    FillProvidersTreeView;
    Result := ShowModal = mrOK;
    if Result then
      DataProviderName := ProvidersTreeView.Selected.Text;
  finally
    Free;
  end;
end;

procedure TfrmSelectDataProvider.ProvidersTreeViewChange(Sender: TObject;
  Node: TTreeNode);
begin
  if (Node <> nil) and (Node.Data <> nil) then
    lblDescription.Caption := Node.Text // TCtxReportProvider(Node.Data).GetDescription
  else lblDescription.Caption := '';
end;

procedure TfrmSelectDataProvider.FillProvidersTreeView;

var
  SelectNode: TTreeNode;
  Categories: TStringList;
  C: Integer;

  procedure AddCategories;
  var
    I: Integer;
    DataProvider: ICtxDataProvider;
  begin
    for I := 0 to DBDatabases.Count - 1 do
    if Supports(TObject(DBDatabases[I]), ICtxDataProvider, DataProvider) then
      Categories.Add(DataProvider.DriverName);
  end;

  procedure AddCategoryProviders(const Category: String);
  var
    I: Integer;
    ParentNode, N: TTreeNode;
    DataProvider: ICtxDataProvider;
  begin
    if Category = '' then
      ParentNode := nil
    else begin
      ParentNode := ProvidersTreeView.Items.Add(nil, Category);
      ParentNode.ImageIndex := 0;
      ParentNode.SelectedIndex := ParentNode.ImageIndex;
    end;

    for I := 0 to DBDatabases.Count - 1 do
    if Supports(TObject(DBDatabases[I]), ICtxDataProvider, DataProvider) then
    begin
      if AnsiSameText(DataProvider.DriverName, Category) then
      begin
        N := ProvidersTreeView.Items.AddChildObject(ParentNode, DataProvider.DatabaseName, DBDatabases[I]);
        N.ImageIndex := 1;
        N.SelectedIndex := N.ImageIndex;
        if SelectNode = nil then
          SelectNode := N;
        if AnsiSameText(FDataProviderName, DataProvider.DatabaseName) then
          SelectNode := N;
      end;
    end;
  end;

begin
  Categories := TStringList.Create;
  try
    Categories.Duplicates := dupIgnore;
    Categories.Sorted := True;
    ProvidersTreeView.Items.BeginUpdate;
    try
      ProvidersTreeView.Items.Clear;
      SelectNode := nil;
      AddCategories;
      for C := 0 to Categories.Count - 1 do
        AddCategoryProviders(Categories[C]);
      ProvidersTreeView.FullExpand;
      ProvidersTreeView.Selected := SelectNode;
    finally
      ProvidersTreeView.Items.EndUpdate;
    end;
  finally
    Categories.Free;
  end;
end;

procedure TfrmSelectDataProvider.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOK then
    CanClose := (ProvidersTreeView.Selected <> nil) and (ProvidersTreeView.Selected.Data <> nil);
end;

procedure TfrmSelectDataProvider.ProvidersTreeViewDblClick(Sender: TObject);
begin
  if (ProvidersTreeView.Selected <> nil) and (ProvidersTreeView.Selected.Data <> nil) then
    ModalResult := mrOK;
end;

end.
