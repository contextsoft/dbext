object frmDataRelationEditor: TfrmDataRelationEditor
  Left = 447
  Top = 334
  BorderStyle = bsDialog
  Caption = 'Relation Properties'
  ClientHeight = 342
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 42
    Width = 56
    Height = 13
    Caption = 'Child Table:'
  end
  object Label3: TLabel
    Left = 192
    Top = 42
    Width = 64
    Height = 13
    Caption = 'Parent Table:'
  end
  object Label5: TLabel
    Left = 16
    Top = 21
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Bevel1: TBevel
    Left = 16
    Top = 291
    Width = 345
    Height = 2
  end
  object Label2: TLabel
    Left = 16
    Top = 244
    Width = 71
    Height = 13
    Caption = 'Update Action:'
  end
  object Label4: TLabel
    Left = 16
    Top = 268
    Width = 67
    Height = 13
    Caption = 'Delete Action:'
  end
  object cbxChildTable: TComboBox
    Left = 16
    Top = 58
    Width = 161
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = cbxChildTableChange
  end
  object cbxParentTable: TComboBox
    Left = 192
    Top = 58
    Width = 169
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = cbxParentTableChange
  end
  object cbOwner: TCheckBox
    Left = 16
    Top = 216
    Width = 249
    Height = 17
    Caption = 'Parent table owns rows in the child table'
    TabOrder = 4
  end
  object btnOK: TButton
    Left = 208
    Top = 304
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 7
  end
  object btnCancel: TButton
    Left = 288
    Top = 304
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 8
  end
  object edtName: TEdit
    Left = 56
    Top = 16
    Width = 305
    Height = 21
    TabOrder = 0
  end
  object grdColumns: TCtxCollectionView
    Left = 16
    Top = 90
    Width = 345
    Height = 121
    Alignment = taLeftJustify
    Borders = bsSingle
    ShowOptions = [soVerticalGrid, soHorizontalGrid, soHeader, soMarker, soAlwaysShowEditor, soAutoWidth, soRowMoved]
    TabOrder = 3
    AutoAddRow = False
    ReadOnly = False
    Columns.Sorted = False
    Columns = <
      item
        Custom = False
        Alignment = taLeftJustify
        TextAlignment = taLeftJustify
        DefaultColors = False
        Caption = 'Child Key Columns'
        Color = clWindow
        EditType = etValueList
        Name = 'ChildColumn'
        ReadOnly = False
        Empty = False
        Disabled = False
        TextColor = clWindowText
        Visible = True
        ValueListSorted = False
        AutoWidth = False
        SortOption = isoNone
        Width = 140
      end
      item
        Custom = False
        Alignment = taLeftJustify
        TextAlignment = taLeftJustify
        DefaultColors = False
        Caption = 'Parent Key Columns'
        Color = clWindow
        EditType = etValueList
        Name = 'ParentColumn'
        ReadOnly = False
        Empty = False
        Disabled = False
        TextColor = clWindowText
        Visible = True
        ValueListSorted = False
        AutoWidth = True
        SortOption = isoNone
        Width = 183
      end>
    PopupMenu = popColumns
    OnDrawMarker = grdColumnsDrawMarker
  end
  object cbxUpdateAction: TComboBox
    Left = 96
    Top = 240
    Width = 114
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    Items.Strings = (
      'Ignore'
      'Error'
      'Cascade'
      'Nullify')
  end
  object cbxDeleteAction: TComboBox
    Left = 96
    Top = 264
    Width = 114
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 6
    Items.Strings = (
      'Ignore'
      'Error'
      'Cascade'
      'Nullify')
  end
  object popColumns: TPopupMenu
    Left = 60
    Top = 168
    object Add1: TMenuItem
      Action = actAddColumnPair
      ShortCut = 16429
    end
    object Remove1: TMenuItem
      Action = actRemoveColumnPair
      ShortCut = 16430
    end
  end
  object Actions: TActionList
    Left = 104
    Top = 168
    object actAddColumnPair: TAction
      Caption = '&Add...'
      ImageIndex = 18
      OnExecute = actAddColumnPairExecute
    end
    object actRemoveColumnPair: TAction
      Caption = '&Remove'
      ImageIndex = 13
      OnExecute = actRemoveColumnPairExecute
    end
  end
end
