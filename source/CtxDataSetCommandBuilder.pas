unit CtxDataSetCommandBuilder;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CtxDBAdapter;

type
  TfrmCtxDataSetCommandBuilder = class(TForm)
    Label1: TLabel;
    edtCommandName: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    rgFetchRows: TRadioGroup;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TCtxDataSetCommandBuilder = class (TCtxCommandBuilder)
  public
    constructor Create(AOwner: TComponent); override;

    function EditCommandItem(CommandItem: TCtxDBCommandItem): Boolean; override;
    function CanEdit(CommandItem: TCtxDBCommandItem): Boolean; override;
    function GetDisplayName: String; override;
  end;

var
  frmCtxDataSetCommandBuilder: TfrmCtxDataSetCommandBuilder;

resourcestring
  SEmptyCommandName = 'Command name cannot be empty';
  
implementation

uses CtxDataSetProvider;

{$R *.dfm}

{ TCtxDataSetCommandBuilder }

constructor TCtxDataSetCommandBuilder.Create(AOwner: TComponent);
begin
  inherited;
  RegisterCommandBuilder(Self);
end;

function TCtxDataSetCommandBuilder.CanEdit(
  CommandItem: TCtxDBCommandItem): Boolean;

  function IsDataSetProvider(Value: TObject): Boolean;
  begin
    Result := (Value <> nil) and Value.InheritsFrom(TCtxDataSetProvider);
  end;

begin
  Result := (CommandItem.DataProvider <> nil)
    and (
      IsDataSetProvider(CommandItem.DataProviderObject)
      or (
        (CommandItem.DataProviderObject = nil)
        and IsDataSetProvider(CommandItem.Adapter.DataProviderObject)
      )
    );
end;

function TCtxDataSetCommandBuilder.GetDisplayName: String;
begin
  Result := 'DataSet Command Builder';
end;

function TCtxDataSetCommandBuilder.EditCommandItem(
  CommandItem: TCtxDBCommandItem): Boolean;
begin
  with TfrmCtxDataSetCommandBuilder.Create(nil) do
  try
    edtCommandName.Text := CommandItem.Name;
    if AnsiSameText(Trim(CommandItem.CommandText), 'CURRENT') then
      rgFetchRows.ItemIndex := 1
    else rgFetchRows.ItemIndex := 0;
    Result := ShowModal = mrOK;
    if Result then
    begin
      if rgFetchRows.ItemIndex = 1 then
        CommandItem.CommandText := 'CURRENT'
      else CommandItem.CommandText := '';
      CommandItem.Name := edtCommandName.Text;
    end;
  finally
    Free;
  end;
end;


procedure TfrmCtxDataSetCommandBuilder.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOK then
  begin
    if Trim(edtCommandName.Text) = '' then
      raise Exception.Create(SEmptyCommandName);
  end;
end;

end.
