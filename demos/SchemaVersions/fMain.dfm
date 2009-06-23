object frmVersionDemo: TfrmVersionDemo
  Left = 316
  Top = 286
  Caption = 'Schema Versions Demo'
  ClientHeight = 432
  ClientWidth = 604
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    604
    432)
  PixelsPerInch = 96
  TextHeight = 13
  object lblStatus: TLabel
    Left = 8
    Top = 265
    Width = 40
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'lblStatus'
  end
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 49
    Height = 13
    Caption = 'Database:'
  end
  object Label2: TLabel
    Left = 127
    Top = 60
    Width = 463
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Press "Open Database" to open an existing database from the path' +
      ' above. If the database does not exist or has old structure, it ' +
      'will be updated automatically.'
    Color = clInfoBk
    ParentColor = False
    WordWrap = True
  end
  object lblVersion: TLabel
    Left = 8
    Top = 40
    Width = 3
    Height = 13
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 94
    Width = 583
    Height = 166
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object ProgressBar: TProgressBar
    Left = 8
    Top = 281
    Width = 584
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
  end
  object edtLog: TMemo
    Left = 8
    Top = 306
    Width = 584
    Height = 113
    Anchors = [akLeft, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object Button1: TButton
    Left = 525
    Top = 10
    Width = 65
    Height = 23
    Anchors = [akTop, akRight]
    Caption = 'Select...'
    TabOrder = 1
    OnClick = Button1Click
  end
  object edtDatabase: TEdit
    Left = 64
    Top = 11
    Width = 454
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object Button2: TButton
    Left = 8
    Top = 62
    Width = 113
    Height = 25
    Caption = 'Open Database...'
    TabOrder = 5
    OnClick = Button2Click
  end
  object DatabaseSchema: TDatabaseSchema
    TargetDB = 'Paradox'
    Updates = <
      item
        Description = 'Creating table Customers'
        SQLScript = 
          '-- ## TargetDB: Paradox; Delimiter: ";"; Comments: "/*,*/";'#13#10#13#10'C' +
          'REATE TABLE Customers ('#13#10'  CustomerID AUTOINC,'#13#10'  CompanyName VA' +
          'RCHAR(40),'#13#10'  LastName VARCHAR(40),'#13#10'  FirstName VARCHAR(40),'#13#10' ' +
          ' Initial VARCHAR(5),'#13#10'  StreetNo VARCHAR(20),'#13#10'  Street VARCHAR(' +
          '60),'#13#10'  City VARCHAR(80),'#13#10'  State VARCHAR(15),'#13#10'  Zip VARCHAR(1' +
          '2),'#13#10'  HomePhone VARCHAR(20),'#13#10'  WorkPhone VARCHAR(20),'#13#10'  PRIMA' +
          'RY KEY (CustomerID)'#13#10');'#13#10#13#10'CREATE INDEX ByName ON Customers(Last' +
          'Name,FirstName);'#13#10
        Iterate = False
        VersionLabel = '1.01'
        ItemID = 17
      end
      item
        Description = 'Altering table Customers'
        SQLScript = 
          '-- ## TargetDB: Paradox; Delimiter: ";"; Comments: "/*,*/";'#13#10#13#10'A' +
          'LTER TABLE Customers '#13#10'  ADD CareOf VARCHAR(40),'#13#10'  ADD MobilePh' +
          'one VARCHAR(20),'#13#10'  ADD Fax VARCHAR(20);'#13#10
        Iterate = False
        VersionLabel = '1.02'
        ItemID = 21
      end>
    TableDefs = <
      item
        Category = 'References'
        Name = 'Customers'
        Description = 'Customers'
        FieldDefs = <
          item
            Name = 'CustomerID'
            Description = 'Customer #'
            Required = True
            SQLFieldType = 'AUTOINC'
            Attributes = [faRequired]
            DataType = ftAutoInc
            Size = 0
            DefaultExpression = '0'
            Identity = True
            ItemID = 1
          end
          item
            Name = 'CompanyName'
            Description = 'Company'
            Required = True
            SQLFieldType = 'VARCHAR'
            Attributes = [faRequired]
            DataType = ftString
            Size = 40
            ItemID = 2
          end
          item
            Name = 'LastName'
            Description = 'Last Name'
            Required = True
            SQLFieldType = 'VARCHAR'
            Attributes = [faRequired]
            DataType = ftString
            Size = 40
            ItemID = 3
          end
          item
            Name = 'FirstName'
            Description = 'First Name'
            SQLFieldType = 'VARCHAR'
            DataType = ftString
            Size = 40
            ItemID = 4
          end
          item
            Name = 'Initial'
            Description = 'Initial'
            SQLFieldType = 'VARCHAR'
            DataType = ftString
            Size = 5
            ItemID = 5
          end
          item
            Name = 'StreetNo'
            Description = 'Street #'
            SQLFieldType = 'VARCHAR'
            DataType = ftString
            Size = 20
            ItemID = 6
          end
          item
            Name = 'Street'
            Description = 'Street'
            SQLFieldType = 'VARCHAR'
            DataType = ftString
            Size = 60
            ItemID = 7
          end
          item
            Name = 'City'
            Description = 'City'
            SQLFieldType = 'VARCHAR'
            DataType = ftString
            Size = 80
            ItemID = 8
          end
          item
            Name = 'State'
            Description = 'State'
            SQLFieldType = 'VARCHAR'
            DataType = ftString
            Size = 15
            ItemID = 9
          end
          item
            Name = 'Zip'
            Description = 'Zip'
            SQLFieldType = 'VARCHAR'
            DataType = ftString
            Size = 12
            ItemID = 10
          end
          item
            Name = 'HomePhone'
            Description = 'HomePhone'
            SQLFieldType = 'VARCHAR'
            DataType = ftString
            Size = 20
            ItemID = 11
          end
          item
            Name = 'WorkPhone'
            Description = 'WorkPhone'
            SQLFieldType = 'VARCHAR'
            DataType = ftString
            Size = 20
            ItemID = 12
          end
          item
            Name = 'CareOf'
            Description = 'C/O'
            SQLFieldType = 'VARCHAR'
            DataType = ftString
            Size = 40
            ItemID = 18
          end
          item
            Name = 'MobilePhone'
            Description = 'MobilePhone'
            SQLFieldType = 'VARCHAR'
            DataType = ftString
            Size = 20
            ItemID = 19
          end
          item
            Name = 'Fax'
            Description = 'Fax'
            SQLFieldType = 'VARCHAR'
            DataType = ftString
            Size = 20
            ItemID = 20
          end>
        IndexDefs = <
          item
            IndexFields = <
              item
                Name = 'CustomerID'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            Options = [ixPrimary, ixUnique]
            ItemID = 13
          end
          item
            Name = 'ByName'
            IndexFields = <
              item
                Name = 'LastName'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end
              item
                Name = 'FirstName'
                Descending = False
                CaseInsensitive = False
                ItemID = 2
              end>
            Options = [ixCaseInsensitive]
            ItemID = 14
          end>
        Triggers = <
          item
            Name = 'OnInsert'
            TriggerWhen = [ctInserted]
            TriggerType = ttAfter
            Definition = 
              'CREATE TRIGGER "OnInsert"'#13#10'update customers set'#13#10'LastName = '#39'A'#39#13 +
              #10'where (:CustomerID = CustomerID) or '#13#10'(:CustomerID = 12) '
            ItemID = 15
          end>
        ObjectType = 'Customers222'
        TypePrefix = 'T'
        IsView = False
        ItemID = 16
      end
      item
        Name = 'Table1'
        FieldDefs = <
          item
            Name = 'Field1'
            Description = #207#224#248#224' '#232' '#239#229#242#255
            Required = True
            SQLFieldType = 'VARCHAR'
            Attributes = [faRequired]
            DataType = ftString
            Size = 20
            ItemID = 23
          end>
        IndexDefs = <
          item
            Name = 'PK_Table1'
            IndexFields = <
              item
                Name = 'Field1'
                Descending = False
                CaseInsensitive = False
                ItemID = 1
              end>
            Options = [ixPrimary, ixUnique]
            ItemID = 24
          end>
        IsView = False
        ItemID = 22
      end>
    SchemaName = 'Database'
    LibVersion = 301
    DSDFileName = 'Database.dsd'
    DefaultValues.Strings = (
      'Enumeration.'
      'Domain.'
      'Sequence.'
      'Table.'
      'Field.'
      'Index.'
      'IndexField.'
      'Trigger.'
      'Constraint.'
      'View.'
      'Relationship.'
      'StoredProc.')
    Left = 64
    Top = 168
    SchemaGUID = '{7736A6A6-3CD0-43AE-99FD-F62F0896EDD7}'
  end
  object DatabaseExt: TDatabaseExt
    DatabaseName = 'MainDB'
    DriverName = 'STANDARD'
    Params.Strings = (
      'PATH=Data')
    SessionName = 'Default'
    Schema = DatabaseSchema
    SystemTableName = 'SysTable'
    Left = 144
    Top = 168
  end
  object DataSource1: TDataSource
    DataSet = Query1
    Left = 352
    Top = 176
  end
  object Query1: TQuery
    DatabaseName = 'MainDB'
    SQL.Strings = (
      'select * from Customers')
    Left = 336
    Top = 168
  end
end
