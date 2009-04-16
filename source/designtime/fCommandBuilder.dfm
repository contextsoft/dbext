object frmCommandBuilder: TfrmCommandBuilder
  Left = 335
  Top = 248
  Width = 613
  Height = 428
  BorderWidth = 4
  Caption = 'Command Builder'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 225
    Top = 0
    Height = 346
  end
  object pnlGenerate: TPanel
    Left = 0
    Top = 0
    Width = 225
    Height = 346
    Align = alLeft
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    DesignSize = (
      225
      346)
    object Label1: TLabel
      Left = 11
      Top = 56
      Width = 84
      Height = 13
      Caption = 'Database Tables:'
    end
    object Label2: TLabel
      Left = 11
      Top = 110
      Width = 102
      Height = 13
      Caption = 'Generate Commands:'
    end
    object Bevel2: TBevel
      Left = 11
      Top = 79
      Width = 204
      Height = 2
      Anchors = [akLeft, akTop, akRight]
    end
    object Bevel1: TBevel
      Left = 11
      Top = 239
      Width = 204
      Height = 2
      Anchors = [akLeft, akTop, akRight]
      Visible = False
    end
    object Label4: TLabel
      Left = 11
      Top = 8
      Width = 99
      Height = 13
      Caption = 'Database \ Schema:'
    end
    object Label5: TLabel
      Left = 11
      Top = 246
      Width = 98
      Height = 13
      Caption = 'Source Table Name:'
    end
    object cbxTableName: TComboBox
      Left = 11
      Top = 73
      Width = 205
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 1
      OnChange = cbxTableNameChange
    end
    object cbSelect: TCheckBox
      Left = 16
      Top = 136
      Width = 65
      Height = 17
      Caption = '&Select'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object cbInsert: TCheckBox
      Left = 16
      Top = 168
      Width = 65
      Height = 17
      Caption = '&Insert'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object cbDelete: TCheckBox
      Left = 16
      Top = 200
      Width = 65
      Height = 17
      Caption = '&Delete'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object cbUpdate: TCheckBox
      Left = 16
      Top = 184
      Width = 65
      Height = 17
      Caption = '&Update'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object cbRefresh: TCheckBox
      Left = 16
      Top = 152
      Width = 65
      Height = 17
      Caption = '&Refresh'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object cbxDatabase: TComboBox
      Left = 11
      Top = 25
      Width = 205
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbxDatabaseChange
    end
    object edtSourceTable: TEdit
      Left = 11
      Top = 265
      Width = 203
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 7
    end
  end
  object grdColumns: TCtxCollectionView
    Left = 228
    Top = 0
    Width = 369
    Height = 346
    Alignment = taLeftJustify
    Align = alClient
    Borders = bsSingle
    ShowOptions = [soVerticalGrid, soHorizontalGrid, soHeader, soAlwaysShowEditor, soAutoWidth]
    TabOrder = 1
    AutoAddRow = False
    ReadOnly = False
    Columns.Sorted = False
    Columns = <
      item
        Custom = False
        Alignment = taCenter
        TextAlignment = taLeftJustify
        DefaultColors = False
        Caption = 'Select'
        Color = clWindow
        EditType = etCheck
        Name = 'Select'
        ReadOnly = False
        Empty = False
        Disabled = False
        TextColor = clWindowText
        Visible = True
        AutoWidth = False
        SortOption = isoNone
        Width = 50
      end
      item
        Custom = False
        Alignment = taCenter
        TextAlignment = taLeftJustify
        DefaultColors = False
        Caption = 'Update'
        Color = clWindow
        EditType = etCheck
        Name = 'Update'
        ReadOnly = False
        Empty = False
        Disabled = False
        TextColor = clWindowText
        Visible = True
        AutoWidth = False
        SortOption = isoNone
        Width = 50
      end
      item
        Custom = False
        Alignment = taCenter
        TextAlignment = taLeftJustify
        DefaultColors = False
        Caption = 'Key'
        Color = clWindow
        EditType = etCheck
        Name = 'Key'
        ReadOnly = False
        Empty = False
        Disabled = False
        TextColor = clWindowText
        Visible = True
        AutoWidth = False
        SortOption = isoNone
        Width = 50
      end
      item
        Custom = False
        Alignment = taLeftJustify
        TextAlignment = taLeftJustify
        DefaultColors = False
        Caption = 'Column'
        Color = clWindow
        EditType = etNone
        Name = 'ColumnName'
        ReadOnly = False
        Empty = False
        Disabled = False
        TextColor = clWindowText
        Visible = True
        AutoWidth = True
        SortOption = isoNone
        Width = 215
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 346
    Width = 597
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      597
      40)
    object btnOK: TButton
      Left = 436
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 516
      Top = 10
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cacel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object TempContainer: TCtxDataContainer
    Tables = <>
    Relations = <>
    Active = False
    Params = <>
    Left = 304
    Top = 200
  end
end
