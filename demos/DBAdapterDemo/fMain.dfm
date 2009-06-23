object Form1: TForm1
  Left = 419
  Top = 181
  Caption = 'Form1'
  ClientHeight = 415
  ClientWidth = 696
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 436
    Top = 36
    Width = 75
    Height = 25
    Caption = 'Edit'
    TabOrder = 0
    OnClick = Button1Click
  end
  object DBGrid1: TDBGrid
    Left = 16
    Top = 88
    Width = 625
    Height = 281
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object CtxDataContainer1: TCtxDataContainer
    Tables = <
      item
        Columns = <
          item
            DataType = cdtString
            DataLength = 20
            Name = 'Column1'
            Required = True
            ReadOnly = False
            PrimaryKey = True
            Calculated = False
          end
          item
            DataType = cdtWideString
            DataLength = 20
            Name = 'Column2'
            Required = False
            ReadOnly = False
            PrimaryKey = False
            Calculated = False
          end
          item
            DataType = cdtString
            DataLength = 20
            Name = 'Column3'
            Required = False
            ReadOnly = False
            PrimaryKey = False
            Calculated = False
          end>
        Name = 'Table1'
      end>
    Relations = <>
    Active = True
    Params = <>
    DataAdapter = CtxDBAdapter1
    Left = 252
    Top = 28
  end
  object ADOConnectionExt1: TADOConnectionExt
    SystemTableName = 'SysTable'
    DatabaseName = 'Main'
    Left = 80
    Top = 28
  end
  object CtxDBAdapter1: TCtxDBAdapter
    DataProviderName = 'Main'
    Commands = <
      item
        Name = 'Table1.Select'
        SourceTableName = 'Table1'
        Params = <
          item
            Name = 'Column1'
            DataType = cdtUnknown
            ParamType = cptInput
            DataSize = 0
            SourceColumn = 'Column1'
            RowVersion = drvCurrent
          end>
        UpdateSourceRow = False
        ResultType = crtResultSet
        CommandType = citSelect
        CommandText = 
          'SELECT '#13#10'  Column1, '#13#10'  Column2, '#13#10'  Column3'#13#10'FROM Table1'#13#10'WHERE' +
          ' '#13#10'  Column1 = :Column1'
      end
      item
        Name = 'Table1.Refresh'
        SourceTableName = 'Table1'
        Params = <
          item
            Name = 'OLD_Column1'
            DataType = cdtUnknown
            ParamType = cptInput
            DataSize = 0
            SourceColumn = 'Column1'
            RowVersion = drvOriginal
          end>
        UpdateSourceRow = True
        ResultType = crtResultSet
        CommandType = citRefresh
        CommandText = 
          'SELECT '#13#10'  Column1, '#13#10'  Column2, '#13#10'  Column3'#13#10'FROM Table1'#13#10'WHERE' +
          ' '#13#10'  Column1 = :OLD_Column1'
      end
      item
        Name = 'Table1.Insert'
        SourceTableName = 'Table1'
        Params = <
          item
            Name = 'Column1'
            DataType = cdtUnknown
            ParamType = cptInput
            DataSize = 0
            SourceColumn = 'Column1'
            RowVersion = drvCurrent
          end
          item
            Name = 'Column2'
            DataType = cdtUnknown
            ParamType = cptInput
            DataSize = 0
            SourceColumn = 'Column2'
            RowVersion = drvCurrent
          end
          item
            Name = 'Column3'
            DataType = cdtUnknown
            ParamType = cptInput
            DataSize = 0
            SourceColumn = 'Column3'
            RowVersion = drvCurrent
          end>
        UpdateSourceRow = True
        ResultType = crtParameters
        CommandType = citInsert
        CommandText = 
          'INSERT INTO Table1('#13#10'  Column1, '#13#10'  Column2, '#13#10'  Column3'#13#10') VALU' +
          'ES ('#13#10'  :Column1, '#13#10'  :Column2, '#13#10'  :Column3'#13#10')'
      end
      item
        Name = 'Table1.Update'
        SourceTableName = 'Table1'
        Params = <
          item
            Name = 'Column1'
            DataType = cdtUnknown
            ParamType = cptInput
            DataSize = 0
            SourceColumn = 'Column1'
            RowVersion = drvCurrent
          end
          item
            Name = 'OLD_Column1'
            DataType = cdtUnknown
            ParamType = cptInput
            DataSize = 0
            SourceColumn = 'Column1'
            RowVersion = drvOriginal
          end
          item
            Name = 'Column2'
            DataType = cdtUnknown
            ParamType = cptInput
            DataSize = 0
            SourceColumn = 'Column2'
            RowVersion = drvCurrent
          end
          item
            Name = 'Column3'
            DataType = cdtUnknown
            ParamType = cptInput
            DataSize = 0
            SourceColumn = 'Column3'
            RowVersion = drvCurrent
          end>
        UpdateSourceRow = True
        ResultType = crtParameters
        CommandType = citUpdate
        CommandText = 
          'UPDATE Table1 SET '#13#10'  Column1 = :Column1, '#13#10'  Column2 = :Column2' +
          ', '#13#10'  Column3 = :Column3'#13#10'WHERE '#13#10'  Column1 = :OLD_Column1'
      end
      item
        Name = 'Table1.Delete'
        SourceTableName = 'Table1'
        Params = <
          item
            Name = 'OLD_Column1'
            DataType = cdtUnknown
            ParamType = cptInput
            DataSize = 0
            SourceColumn = 'Column1'
            RowVersion = drvOriginal
          end>
        UpdateSourceRow = False
        ResultType = crtNone
        CommandType = citDelete
        CommandText = 'DELETE FROM Table1'#13#10'WHERE '#13#10'  Column1 = :OLD_Column1'
      end>
    FetchAll = False
    ParamPrefix = ':'
    Left = 168
    Top = 28
  end
  object CtxDataSet1: TCtxDataSet
    DataContainer = CtxDataContainer1
    DataTableName = 'Table1'
    FieldDefs = <
      item
        Name = 'Column1'
        Attributes = [faRequired]
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Column2'
        DataType = ftWideString
        Size = 20
      end
      item
        Name = 'Column3'
        DataType = ftString
        Size = 20
      end>
    Active = True
    Left = 336
    Top = 32
  end
  object DataSource1: TDataSource
    DataSet = CtxDataSet1
    Left = 384
    Top = 136
  end
end
