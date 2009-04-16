object frmSelectCommandTypes: TfrmSelectCommandTypes
  Left = 524
  Top = 431
  Width = 242
  Height = 178
  Caption = 'Command Types'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 40
    Top = 104
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 128
    Top = 104
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object gbCommandTypes: TGroupBox
    Left = 8
    Top = 8
    Width = 217
    Height = 81
    Caption = ' Select which table commands to update: '
    TabOrder = 0
    object cbAll: TCheckBox
      Left = 12
      Top = 21
      Width = 97
      Height = 17
      Caption = '&All commands'
      TabOrder = 0
      OnClick = cbAllClick
    end
    object cbSelect: TCheckBox
      Left = 12
      Top = 37
      Width = 97
      Height = 17
      Caption = '&Select'
      TabOrder = 1
      OnClick = cbInsertClick
    end
    object cbRefresh: TCheckBox
      Left = 12
      Top = 53
      Width = 97
      Height = 17
      Caption = '&Refresh'
      TabOrder = 2
      OnClick = cbInsertClick
    end
    object cbInsert: TCheckBox
      Left = 124
      Top = 21
      Width = 80
      Height = 17
      Caption = '&Insert'
      TabOrder = 3
      OnClick = cbInsertClick
    end
    object cbDelete: TCheckBox
      Left = 124
      Top = 37
      Width = 80
      Height = 17
      Caption = '&Delete'
      TabOrder = 4
      OnClick = cbInsertClick
    end
    object cbUpdate: TCheckBox
      Left = 124
      Top = 53
      Width = 80
      Height = 17
      Caption = '&Update'
      TabOrder = 5
      OnClick = cbInsertClick
    end
  end
end
