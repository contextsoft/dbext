(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Command type selection dialog
(*
(*  Copyright (c) 2005-2009, Context Software LLC
(*
(******************************************************************************)
unit fSelectCommandTypes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CtxDBAdapter;

type
  TfrmSelectCommandTypes = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    gbCommandTypes: TGroupBox;
    cbAll: TCheckBox;
    cbSelect: TCheckBox;
    cbRefresh: TCheckBox;
    cbInsert: TCheckBox;
    cbDelete: TCheckBox;
    cbUpdate: TCheckBox;
    procedure cbInsertClick(Sender: TObject);
    procedure cbAllClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSelectCommandTypes: TfrmSelectCommandTypes;

  function SelectCommandTypes(var CommandTypes: TCtxDBCommandItemTypes): Boolean;

implementation

{$R *.dfm}

function SelectCommandTypes(var CommandTypes: TCtxDBCommandItemTypes): Boolean;
begin
  with TfrmSelectCommandTypes.Create(nil) do
  try
    cbUpdate.Checked := citUpdate in CommandTypes;
    cbSelect.Checked := citSelect in CommandTypes;
    cbRefresh.Checked := citRefresh in CommandTypes;
    cbInsert.Checked := citInsert in CommandTypes;
    cbDelete.Checked := citDelete in CommandTypes;
    cbAll.Checked := cbUpdate.Checked and cbSelect.Checked and cbRefresh.Checked and cbInsert.Checked and cbDelete.Checked;
    Result := ShowModal = mrOK;
    if Result then
    begin
      CommandTypes := [];
      if cbAll.Checked then
        CommandTypes := [citSelect, citRefresh, citInsert, citDelete, citUpdate]
      else begin
        if cbSelect.Checked then Include(CommandTypes, citSelect);
        if cbRefresh.Checked then Include(CommandTypes, citRefresh);
        if cbInsert.Checked then Include(CommandTypes, citInsert);
        if cbDelete.Checked then Include(CommandTypes, citDelete);
        if cbUpdate.Checked then Include(CommandTypes, citUpdate);
      end;
    end;
  finally
    Free;
  end;
end;

procedure TfrmSelectCommandTypes.cbInsertClick(Sender: TObject);
begin
  if not (Sender as TCheckBox).Checked then
    cbAll.Checked := False;
end;

procedure TfrmSelectCommandTypes.cbAllClick(Sender: TObject);
begin
  if cbAll.Checked then
  begin
    cbUpdate.Checked := True;
    cbSelect.Checked := True;
    cbRefresh.Checked := True;
    cbInsert.Checked := True;
    cbDelete.Checked := True;
  end;
end;

end.

