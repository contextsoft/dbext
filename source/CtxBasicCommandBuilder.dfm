object frmCtxBasicCommandBuilder: TfrmCtxBasicCommandBuilder
  Left = 428
  Top = 302
  Width = 464
  Height = 314
  Caption = 'Edit Command'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    456
    280)
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
    Width = 336
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object edtCommandText: TMemo
    Left = 16
    Top = 40
    Width = 424
    Height = 189
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 279
    Top = 244
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 359
    Top = 244
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
