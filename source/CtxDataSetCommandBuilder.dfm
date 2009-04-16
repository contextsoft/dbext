object frmCtxDataSetCommandBuilder: TfrmCtxDataSetCommandBuilder
  Left = 462
  Top = 366
  BorderStyle = bsDialog
  Caption = 'Data Set Provider'
  ClientHeight = 155
  ClientWidth = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    312
    155)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 12
    Width = 81
    Height = 13
    Caption = 'Command Name:'
  end
  object edtCommandName: TEdit
    Left = 104
    Top = 8
    Width = 193
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 135
    Top = 120
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 215
    Top = 120
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object rgFetchRows: TRadioGroup
    Left = 16
    Top = 40
    Width = 280
    Height = 66
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Options '
    Items.Strings = (
      'Fetch all records'
      'Fetch current record only')
    TabOrder = 1
  end
end
