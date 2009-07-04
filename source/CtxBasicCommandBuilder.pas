(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Simple command builder.
(*
(*  Copyright (c) 2005-2009, Context Software LLC
(*
(******************************************************************************)
unit CtxBasicCommandBuilder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CtxDBAdapter;

type
  TfrmCtxBasicCommandBuilder = class(TForm)
    Label1: TLabel;
    edtCommandName: TEdit;
    edtCommandText: TMemo;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TCtxBasicCommandBuilder = class (TCtxCommandBuilder)
  public
    function CanEdit(CommandItem: TCtxDBCommandItem): Boolean; override;
    function EditCommandItem(CommandItem: TCtxDBCommandItem): Boolean; override;
    function GetDisplayName: String; override;
  end;

var
  frmCtxBasicCommandBuilder: TfrmCtxBasicCommandBuilder;

resourcestring
  SEmptyCommandName = 'Command name cannot be empty';

implementation

uses CtxData;

{$R *.dfm}

{ TCtxBasicCommandBuilder }

function TCtxBasicCommandBuilder.CanEdit(
  CommandItem: TCtxDBCommandItem): Boolean;
begin
  Result := True;
end;

function TCtxBasicCommandBuilder.EditCommandItem(
  CommandItem: TCtxDBCommandItem): Boolean;
begin
  with TfrmCtxBasicCommandBuilder.Create(nil) do
  try
    edtCommandName.Text := CommandItem.Name;
    edtCommandText.Lines.Text := CommandItem.CommandText;
    Result := ShowModal = mrOK;
    if Result then
    begin
      CommandItem.Name := edtCommandName.Text;
      CommandItem.CommandText := edtCommandText.Lines.Text;
    end;
  finally
    Free;
  end;
end;

function TCtxBasicCommandBuilder.GetDisplayName: String;
begin
  Result := 'Basic Command Builder';
end;

procedure TfrmCtxBasicCommandBuilder.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOK then
  begin
    if Trim(edtCommandName.Text) = '' then
      raise Exception.Create(SEmptyCommandName);
  end;
end;

initialization
  RegisterCommandBuilder(TCtxBasicCommandBuilder.Create(nil));
end.
