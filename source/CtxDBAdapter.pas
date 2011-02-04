(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  TCtxDBAdapter - a generic final implementation of data adapter, which works
(*                with data providers, via ICtxDataProvider interface.
(*
(*  Copyright (c) 2005-2010, Context Software LLC
(*
(******************************************************************************)
unit CtxDBAdapter;

{ Contains objects to combine CtxData namespace with DB namespace. }

interface

uses Classes, SysUtils, Variants, CtxData, CtxDataTypes;

type
  TCtxDBAdapter = class;
  TCtxDBCommandItem = class;
  TCtxDBCommandItems = class;
  TCtxDBResultType = (crtNone, crtResultSet, crtParameters);
  TCtxDBCommandItemType = (citSelect, citRefresh, citInsert, citDelete, citUpdate, citProcedure);
  TCtxDBCommandItemTypes = set of TCtxDBCommandItemType;

  TCtxCommandBuilder = class (TComponent)
  public
    destructor Destroy; override;

    function CreateCommandData: TObject; virtual;
    procedure ReadCommandData(Stream: TStream; CommandData: TObject); virtual;
    procedure WriteCommandData(Stream: TStream; CommandData: TObject); virtual;

    procedure CheckCommandData(CommandItem: TCtxDBCommandItem); virtual;
    function CanEdit(CommandItem: TCtxDBCommandItem): Boolean; virtual;
    function EditCommandItem(CommandItem: TCtxDBCommandItem): Boolean; virtual; abstract;
    function GetDisplayName: String; virtual;
    procedure PrepareCommand(CommandItem: TCtxDBCommandItem); virtual;
    procedure UpdateTableStructure(CommandItem: TCtxDBCommandItem; DataTable: TCtxDataTable); virtual;

    procedure GetRelatedCommands(CommandItem: TCtxDBCommandItem; RelatedCommandItems: TCtxDBCommandItems); virtual;

    // function GetCommandDataParams(CommandData: TObject): TObject;
    // function EditCommandItem(CommandItem: TCtxCommandItem): Boolean;
    // procedure InitCommand(CommandData: TObject; Command: TCtxDataCommand);
  end;
  
  {:$ TCtxDBCommandItems - is a collection of command items conatined in data adapter. }
  TCtxDBCommandItems = class (TCtxDataCollection)
  protected
    function GetItem(Index: Integer): TCtxDBCommandItem;
    procedure SetItem(Index: Integer; Value: TCtxDBCommandItem);
  public
    {:$ Creates and adds a new TCtxDBCommandItem item to the TCtxDBCommandItems collection. }
    function Add: TCtxDBCommandItem;
    {:$ Finds TCtxDBCommandItem item by name in the TCtxDBCommandItems collection. }
    {:$ Raises exception if the command item is not found. }
    function Get(const AName: string): TCtxDBCommandItem;
    {:$ Finds TCtxDBCommandItem item by name in the TCtxDBCommandItems collection. }
    function Find(const AName: string): TCtxDBCommandItem;

    {:$ Creates and adds command item of a specific type to the collection. }
    function AddCommand(const TableName: String; ACommandType: TCtxDBCommandItemType): TCtxDBCommandItem;
    {:$ Provides access to the TCtxDBCommandItem items in the collection by index. }
    property Items[Index: Integer]: TCtxDBCommandItem read GetItem write SetItem; default;
  end;

  {:$ TCtxDBCommandItem collection item represent a command element of a certain type }
  {:$ (select, refresh, insert, update, delete) assigned to a certain source table, which }
  {:$ will be affected (populated or updated) by execution of this command. }
  {:$ Each table can have any number of commands of each type. }
  TCtxDBCommandItem = class (TCtxDataCollectionItem)
  protected
    FCommandText: String;
    FSourceTableName: String;
    FParentTableName: String;
    FParams: TCtxParameters;
    FCommandType: TCtxDBCommandItemType;
    FUpdateSourceRow: Boolean;
    FResultType: TCtxDBResultType;
    FCommand: TCtxDataCommand;
    FDataProviderObject: TComponent;
    FDataProvider: ICtxDataProvider;
    FDataProviderName: String;
    FCommandData: TObject;
    FCommandParamsAssigned: Boolean;

    function GetPrepared: Boolean;
    procedure SetPrepared(const Value: Boolean);
    function GetCommand: TCtxDataCommand;
    procedure UpdateCommandParams(ACommand: TCtxDataCommand);
    function GetEffectiveDataProviderName: String;
    procedure SetCommandData(const Value: TObject);
    procedure SetCommandText(const Value: String);
    procedure SetCommandType(const Value: TCtxDBCommandItemType);
    procedure SetResultType(const Value: TCtxDBResultType);
    procedure SetParams(const Value: TCtxParameters);
    function GetDisplayName: String; override;
    procedure DoUpdateSourceRow(Command: TCtxDataCommand; ARow: TCtxDataRow);
    procedure SetDataProviderName(const Value: String);
    procedure SetDataProviderObject(const Value: TComponent);
    function GetAdapter: TCtxDBAdapter;

    procedure InternalPrepareCommand;
    procedure InternalExecuteCommand(const AParams: Variant); overload;
    procedure InternalExecuteCommand(ARow: TCtxDataRow; AParams: TCtxParameters); overload;
    procedure InternalFetchRows(DataTable: TCtxDataTable; AcceptChanges: Boolean);
    procedure InternalRefreshRow(Command: TCtxDataCommand; ARow: TCtxDataRow);
    function GetDataProvider: ICtxDataProvider;

    procedure SelectParentRows(Row: TCtxDataRow);
    procedure SelectIntoTable(DataTable: TCtxDataTable; ParentRow: TCtxDataRow = nil);
    procedure FetchRows(DataTable: TCtxDataTable; const AParams: Variant);

    procedure WriteCommandData(Stream: TStream);
    procedure ReadCommandData(Stream: TStream);
    procedure DefineProperties(Filer: TFiler); override;
    function CopyCommandData(Source: TCtxDBCommandItem): TObject;
    procedure OnParamsChange(Sender: TObject);
  public
    {:$ Creates an item of TCtxDBCommandItem class. }
    constructor Create(Collection: TCollection); override;
    {:$ Destroys the item of TCtxDBCommandItem class. }
    destructor Destroy; override;
    {:$ Assigns one item of TCtxDBCommandItem class to another. }
    procedure Assign(Source: TPersistent); override;

    {:$ Creates a physical command object using data provider. }
    function CreateCommand: TCtxDataCommand; virtual;
    {:$ Release physical command object created by the data provider and cached }
    {:$ temporarily inside this command item. }
    procedure ReleaseCommand;
    {:$ Update table strcuture using Command Builder if exists. Otherwise, UpdateTableStructureFromResultSet }
    {:$ is used to update table's structure from prepared result set. }
    procedure UpdateTableStructure(DataTable: TCtxDataTable);
    {:$ Update table strcuture from the meta-data of this command (usually select or refresh type of command item) }
    procedure UpdateTableStructureFromResultSet(DataTable: TCtxDataTable);
    {:$ Update primary key columns. }
    procedure UpdateTablePrimaryKey(DataTable: TCtxDataTable);

    procedure UpdateCommandName;

    {:$ Execute this command item as a stored procedure with given paramters. In order to pass }
    {:$ more then one parameter, use variant array. }
    function ExecuteProc(AParams: Variant): Variant; overload;
    {:$ Execute a procedure that updates source row. }
    procedure ExecuteProc(Row: TCtxDataRow; AParams: Variant); overload;
    {:$ Execute an update command for the given row. }
    procedure ExecuteUpdateRow(Row: TCtxDataRow);
    {:$ Execute a refresh command for the given row. }
    function ExecuteRefreshRow(Row: TCtxDataRow; DeleteIfNotFound: Boolean = False): Boolean;
    {:$ Execute a refresh command for each row in the given table. }
    function ExecuteRefreshRows(DataTable: TCtxDataTable; AllRows, DeleteIfNotFound: Boolean): Integer;

    {:$ Locates Command builder and uses it to edit self. }
    function EditCommandItem: Boolean;

    {:$ Called internally to update list of command parameters from command statement if necessary. }
    procedure ParseCommandParams;

    {:$ Returns reference to the TCtxDataCommand created by the provider. }
    property Command: TCtxDataCommand read GetCommand;
    {:$ Returns reference to the component, implementing ICtxDataProvider interface. }
    property DataProviderObject: TComponent read FDataProviderObject write SetDataProviderObject;
    {:$ Returns reference to the ICtxDataProvider interface used to perform all operations }
    {:$ associated with this command item. }
    property DataProvider: ICtxDataProvider read GetDataProvider;
    {:$ Returns reference to the adapter component, this command item belongs to. }
    property Adapter: TCtxDBAdapter read GetAdapter;
    {:$ CommandData references an object, which Command Item maintains on behalf of Command Builder. }
    {:$ The type of this object is determined solely by the nature of command builder, which }
    {:$ is also responsible for its serialization. }
    property CommandData: TObject read FCommandData write SetCommandData;
    {:$ Returns effective data provider to be used by this command item. }
    {:$ In case DataProviderName property is empty, the adapter's (default) data provider }
    {:$ name is returned. }
    property EffectiveDataProviderName: String read GetEffectiveDataProviderName;

    property Prepared: Boolean read GetPrepared write SetPrepared;
  published
    {:$ Sepcifies the name of this command item. }
    property Name;
    {:$ Sepcifies the name of the source table for this command item. }
    property SourceTableName: String read FSourceTableName write FSourceTableName;
    {:$ Sepcifies the collection of parameters for this command item. }
    property Params: TCtxParameters read FParams write SetParams;
    {:$ Sepcifies whether the source row must be updated by the result of command execution. }
    property UpdateSourceRow: Boolean read FUpdateSourceRow write FUpdateSourceRow;
    {:$ Sepcifies the type of result returned by execution of this command item. }
    property ResultType: TCtxDBResultType read FResultType write SetResultType;
    {:$ Sepcifies the type of command item. }
    property CommandType: TCtxDBCommandItemType read FCommandType write SetCommandType;
    {:$ Sepcifies the text for the command. This text should normally contain parameterized }
    {:$ SQL statement for the SQl databases or table name if direct access to the table is used. }
    property CommandText: String read FCommandText write SetCommandText;
    {:$ Specifies the parent table name for this command. In case this property is assigned, the
    {:$ select command will be executed for each record in the parent table with }
    {:$ parent table's row as source of parameters. }
    property ParentTableName: String read FParentTableName write FParentTableName;
    {:$ Specifies data provider name for this command item. If empty, the main data provider }
    {:$ assigned to the data adapter will be used instead. }
    property DataProviderName: String read FDataProviderName write SetDataProviderName;
  end;

  {:$ TCtxDBAdapter is a generic final implementation of data adapter, which works with }
  {:$ data providers, via ICtxDataProvider interface. }
  TCtxDBAdapter = class (TCtxDataAdapter)
  protected
    FCommands: TCtxDBCommandItems;
    FDataProviderObject: TComponent;
    FDataProvider: ICtxDataProvider;
    FDataProviderName: String;
    FFetchAll: Boolean;
    FEncloseIdentifiers: String;
    FParamPrefix: String;

    FFillingCounter: Integer;
    FFetchingAll: Boolean;

    function GetDataProvider: ICtxDataProvider;
    procedure SetCommands(const Value: TCtxDBCommandItems);
    procedure SetDataProviderObject(Value: TComponent);
    procedure SetDataProviderName(const Value: String);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AfterFill(DataContainer: TCtxDataContainer); virtual;
    procedure BeforeFill(DataContainer: TCtxDataContainer); virtual;

    procedure InternalUpdateRow(Row: TCtxDataRow; UpdateCommands: TList = nil);
    procedure InternalUpdateTable(DataTable: TCtxDataTable; ParentRow: TCtxDataRow = nil);
    procedure InternalFillTable(ACommand: TCtxDBCommandItem; DataTable: TCtxDataTable; ParentRow: TCtxDataRow = nil);
    procedure InternalFillChildRows(ARow: TCtxDataRow);
  public
    {:$ Creates an instance of TCtxDBAdapter component. }
    constructor Create(AOwner: TComponent); override;
    {:$ Destroys the instance of TCtxDBAdapter component. }
    destructor Destroy; override;

    {:$ Raises exception if data provider interface is not assigned. }
    procedure CheckDataProvider;
    {:$ Release all commands chached by command items in this data adapter. }
    procedure ReleaseCommands;

    {:$ Assigns the command items of one data adapter to another. }
    procedure Assign(Source: TPersistent); override;

    {:$ Begin batch filling operation. }
    procedure BeginFilling(DataContainer: TCtxDataContainer);
    {:$ End batch filling operation on a data container. }
    procedure EndFilling(DataContainer: TCtxDataContainer);
    {:$ Returns true if batch filling operation on a data container is in progress. }
    function IsFilling: Boolean;


    {:$ Auto configure this data adapter based on data container's structure. }
    {:$ This will generate all SQL select, refresh, insert, update and delete statements }
    {:$ required to fill and update this data container. }
    procedure AutoConfigure(DataContainer: TCtxDataContainer);

    {:$ Auto configure command items for a particular data table. }
    {:$ This will generate all SQL select, refresh, insert, update and delete statements }
    {:$ required to fill and update this data table. }
    procedure ConfigureTableCommands(DataTable: TCtxDataTable;
      CommandTypes: TCtxDBCommandItemTypes; Replace: Boolean = True);

    {:$ Returns the set of command types alsready present for a partiulcar data table. }
    function GetTableCommandTypes(DataTable: TCtxDataTable): TCtxDBCommandItemTypes;

    {:$ Returns True if the table already has command items of the given types. }
    function HasCommands(const TableName: String; CommandTypes: TCtxDBCommandItemTypes): Boolean;
    {:$ Returns the list of command items of the given types for the specified table. }
    function GetCommands(const TableName: String; CommandTypes: TCtxDBCommandItemTypes): TList;
    {:$ Returns the first command item of the given type for the specified table. }
    function FindCommand(const TableName: String; CommandTypes: TCtxDBCommandItemTypes = [citSelect]): TCtxDBCommandItem;

    {:$ Execute command item by name with specified parameters. }
    function Execute(const CommandName: String; Params: Variant): Variant; overload;
    {:$ Execute command item by name. }
    function Execute(const CommandName: String): Variant; overload;

    { Container level operations }

    {:$ Fills tables in data container from the data provider by executing select command items. }
    procedure Fill(DataContainer: TCtxDataContainer); override;
    {:$ Updates tables in data container to the data provider by executing insert/update/delete command items. }
    procedure Update(DataContainer: TCtxDataContainer); override;

    {:$ Begins batch update operation. }
    procedure BeginUpdate; override;
    {:$ Ends batch update operation. }
    procedure EndUpdate; override;

    { Table level operations }
    {:$ Fills the table using select commands. ParentRow is used to provide value for the }
    {:$ parameters in case this table is a child table and has to be filled with ranges }
    {:$ of information, related to Parent record. }
    procedure FillTable(DataTable: TCtxDataTable; ParentRow: TCtxDataRow = nil); override;
    {:$ Refreshes the table using refresh commands. }
    procedure RefreshTable(DataTable: TCtxDataTable); override;
    {:$ Updates the table using insert\update\delete commands. }
    procedure UpdateTable(DataTable: TCtxDataTable; ParentRow: TCtxDataRow = nil); override;
    {:$ Fetches rows into the table by executing a select or procedure command. }
    procedure FetchRows(DataTable: TCtxDataTable; const ACommandName: String; const AParams: Variant);

    { Row level operations }
    {:$ Refreshes a given row from database or other source of data.  }
    procedure RefreshRow(Row: TCtxDataRow); override;
    {:$ Updates changes made to the given row into the database or other source of data.  }
    procedure UpdateRow(Row: TCtxDataRow); override;

    {:: Only refresh rows, with flag NeedRefresh. Returns the number of rows refreshed. }
    function RefreshRows(DataTable: TCtxDataTable): Integer;

    {:$ Returns reference to the component, implementing ICtxDataProvider interface. }
    property DataProviderObject: TComponent read FDataProviderObject write SetDataProviderObject;
    {:$ Returns reference to the ICtxDataProvider interface. }
    property DataProvider: ICtxDataProvider read GetDataProvider;
  published
    {:$ Specifies data provider name to use by default by all command items. }
    property DataProviderName: String read FDataProviderName write SetDataProviderName;
    {:$ Contains collection of command item for this data adapter. }
    property Commands: TCtxDBCommandItems read FCommands write SetCommands;
    {:$ Specifies Data Adapter's display name. }
    property DisplayName;
    {:$ Specifies Data Adapter's description. }
    property Description;
    {:$ Specifies whether the data adapter must fetch all rows from the database. }
    {:$ Alternatively, the fetching of some rows (generally N to 1 references) may be delayed }
    {:$ until the data is actually requested. }
    property FetchAll: Boolean read FFetchAll write FFetchAll;
    {:$ Specifies whether the itentifiers must be encolsed in double quotes. }
    property EncloseIdentifiers: String read FEncloseIdentifiers write FEncloseIdentifiers;
    {:$ Specifies a profix to use when generating parameter names from column names. }
    property ParamPrefix: String read FParamPrefix write FParamPrefix;
  end;

  function FindCommandBuilder(CommandItem: TCtxDBCommandItem): TCtxCommandBuilder;
  procedure RegisterCommandBuilder(CommandBuilder: TCtxCommandBuilder);
  procedure UnRegisterCommandBuilder(CommandBuilder: TCtxCommandBuilder);
  procedure GetCommandBuilders(List: TList);

type
  TCtxColumnMap = array of TCtxDataColumn;

resourcestring
  SDatabaseIsNotAssigned = 'Database is not assigned';
  SConnectionIsNotAssigned = 'Database connection is not assigned';

const
  DBCommandTypes: array [TCtxDBCommandItemType] of TCtxCommandType = (
    ctSQLSelect, ctSQLSelect, ctSQLUpdate, ctSQLUpdate, ctSQLUpdate, ctSQLUpdate);

  SCommandItemTypes: array [TCtxDBCommandItemType] of String = (
    'Select', 'Refresh', 'Insert', 'Delete', 'Update', 'Procedure');

  citAllCommands = [citSelect, citRefresh, citInsert, citDelete, citUpdate, citProcedure];

var
  CommandBuilders: TList;

implementation

uses Math;

{ General routines }

function FindCommandBuilder(CommandItem: TCtxDBCommandItem): TCtxCommandBuilder;
var
  I: Integer;
begin
  if (CommandItem <> nil) and (CommandBuilders <> nil) then
  for I := CommandBuilders.Count - 1 downto 0 do
  begin
    Result := CommandBuilders[I];
    if Result.CanEdit(CommandItem) then exit;
  end;
  Result := nil;
end;

procedure RegisterCommandBuilder(CommandBuilder: TCtxCommandBuilder);
begin
  if CommandBuilders = nil then
    CommandBuilders := TList.Create;
  if CommandBuilders.IndexOf(CommandBuilder) < 0 then
    CommandBuilders.Add(CommandBuilder);
end;

procedure UnRegisterCommandBuilder(CommandBuilder: TCtxCommandBuilder);
begin
  if CommandBuilders <> nil then
    CommandBuilders.Remove(CommandBuilder);
end;

procedure GetCommandBuilders(List: TList);
begin
  List.Assign(CommandBuilders);
end;

procedure UnRegisterCommandBuilders;
var
  O: TCtxCommandBuilder;
begin
  if CommandBuilders <> nil then
  while CommandBuilders.Count > 0 do
  begin
    O := CommandBuilders[0];
    CommandBuilders.Delete(0);
    if (O <> nil) and (O.Owner = nil) then
      O.Free;
  end;
  FreeAndNil(CommandBuilders);
end;

function FormatIdentifier(const Identifier, QuoteChars: String): String;
begin
  if QuoteChars = '' then
    Result := Identifier
  else if Length(QuoteChars) = 1 then
    Result := AnsiQuotedStr(Identifier, QuoteChars[1])
  else
    Result := QuoteChars[1] + Identifier + QuoteChars[2];
end;

procedure InitTableParams(DataTable: TCtxDataTable; Params: TCtxParameters;
  KeysOnly, NewKeyValues, OldKeyValues: Boolean);
var
  I: Integer;
  Col: TCtxDataColumn;
begin
  Params.Clear;
  for I := 0 to DataTable.Columns.Count - 1 do
  begin
    Col := DataTable.Columns[I];
    // Do not include read-only columns
    if Col.ReadOnly then continue;

    if (KeysOnly and Col.PrimaryKey) or not KeysOnly then
    begin
      if not Col.PrimaryKey or NewKeyValues then
      with Params.Add do
      begin
        Name := Params.GetParamName(Col.ColumnName);
        SourceColumn := Col.ColumnName;
        ParamType := cptInput;
      end;
      if Col.PrimaryKey and OldKeyValues then
      with Params.Add do
      begin
        Name := Params.GetParamName('OLD_' + Col.ColumnName);
        SourceColumn := Col.ColumnName;
        ParamType := cptInput;
        RowVersion := drvOriginal;
      end;
    end;
  end;
end;

procedure InitRelationParams(Relation: TCtxDataRelation; Params: TCtxParameters;
  out WhereClause: String; const Prefix, QuoteChars: String);
var
  I: Integer;
  ParentCols, ChildCols: TCtxColumnArray;
begin
  ParentCols := Relation.ParentColumns;
  ChildCols := Relation.ChildColumns;
  Params.Clear;
  WhereClause := '';
  for I := Low(ParentCols) to High(ParentCols) do
    with Params.Add do
    begin
      Name := Params.GetParamName(ChildCols[I].ColumnName);
      SourceColumn := ParentCols[I].ColumnName;
      ParamType := cptInput;
      if WhereClause <> '' then
        WhereClause := WhereClause + ' AND ';
      WhereClause := WhereClause + FormatIdentifier(ChildCols[I].ColumnName, QuoteChars) + ' = ' + Prefix + Params[I].Name;
    end;
end;

function GetColumnNames(DataTable: TCtxDataTable; const QuoteChars: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to DataTable.Columns.Count - 1 do
  with DataTable.Columns[I] do
  if not (DataType in [cdtUnknown, cdtReference]) then
  begin
    if Result <> '' then
      Result := Result + ', ';
    Result := Result + #13#10'  ' + FormatIdentifier(ColumnName, QuoteChars);
  end;
end;

procedure GetColumnValues(DataTable: TCtxDataTable; Params: TCtxParameters;
  var Names, Values: String; const Prefix, QuoteChars: String);
var
  I: Integer;
begin
  Names := '';
  Values := '';
  for I := 0 to Params.Count - 1 do
  begin
    if Names <> '' then
      Names := Names + ', ';
    Names := Names + #13#10'  ' + FormatIdentifier(Params[I].GetSourceName, QuoteChars);

    if Values <> '' then
      Values := Values + ', ';
    Values := Values + #13#10'  ' + Prefix + Params[I].Name;
  end;
end;

function GetColumnAssign(DataTable: TCtxDataTable; Params: TCtxParameters;
  const Prefix, QuoteChars, Delimiter: String; OldValues: Boolean = False): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Params.Count - 1 do
  if (Params[I].RowVersion = drvOriginal) = OldValues then
  begin
    if Result <> '' then
      Result := Result + Delimiter;
    Result := Result + #13#10'  ' + FormatIdentifier(Params[I].GetSourceName, QuoteChars) + ' = ' + Prefix + Params[I].Name;
  end;
end;

function CreateColumnMap(DataTable: TCtxDataTable; Params: TCtxParameters): TCtxColumnMap;
var
  I: Integer;
begin
  SetLength(Result, Params.Count);
  for I := 0 to Params.Count - 1 do
    Result[I] := DataTable.Columns.Find(Params[I].Name);
end;

procedure AssignRowFromParams(Row: TCtxDataRow; Params: TCtxParameters; ColumnMap: TCtxColumnMap);
var
  I: Integer;
begin
  for I := 0 to Params.Count - 1 do
  if ColumnMap[I] <> nil then
    Row.Value[ColumnMap[I]] := Params[I].Value;
end;

procedure AssignRowFromOutputParams(Row: TCtxDataRow; Params: TCtxParameters);
var
  I: Integer;
  C: TCtxDataColumn;
begin
  for I := 0 to Params.Count - 1 do
  if Params[I].ParamType in [cptOutput, cptInputOutput, cptResult] then
  with Row.DataTable do
  begin
    C := Columns.Find(Params[I].GetSourceName);
    if C <> nil then
      Row.Value[C] := Params[I].Value;
  end;
end;

procedure AssignParams(Params: TCtxParameters; ParentRow: TCtxDataRow; ParentParams: TCtxParameters);

  function AssignFromRow(Param: TCtxParameter): Boolean;
  var
    C: TCtxDataColumn;
  begin
    Result := ParentRow <> nil;
    if Result then
    begin
      C := ParentRow.DataTable.Columns.Find(Param.GetSourceName);
      Result := C <> nil;
      if Result then
        if Param.RowVersion = drvCurrent then
          Param.Value := ParentRow.Value[C]
        else Param.Value := ParentRow.OriginalValue[C];
    end;
  end;

  function AssignFromParams(Param: TCtxParameter): Boolean;
  var
    SrcParam: TCtxParameter;
  begin
    SrcParam := ParentParams.Find(Param.GetSourceName);
    Result := SrcParam <> nil;
    if Result then
      Param.Value := SrcParam.Value;
  end;

var
  I: Integer;
begin
  for I := 0 to Params.Count - 1 do
  begin
    Params[I].Value := Null;
    if not AssignFromRow(Params[I]) then
      AssignFromParams(Params[I]);
  end;
end;

{ TCtxDBCommandItem }

constructor TCtxDBCommandItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParams := TCtxParameters.Create(Self, TCtxParameter);
  FParams.OnChange := OnParamsChange;
  FCommand := nil;
end;

destructor TCtxDBCommandItem.Destroy;
begin
  ReleaseCommand;
  DataProviderObject := nil;
  FParams.Free;
  FreeAndNil(FCommandData);
  inherited Destroy;
end;

procedure TCtxDBCommandItem.UpdateCommandParams(ACommand: TCtxDataCommand);
begin
  ACommand.Prepared := False;
  ACommand.CommandText := CommandText;
  ACommand.CommandType := DBCommandTypes[Self.CommandType];
  ACommand.Params := Params;
end;

function TCtxDBCommandItem.CreateCommand: TCtxDataCommand;
begin
  Result := DataProvider.CreateCommand;
  UpdateCommandParams(Result);
end;

function TCtxDBCommandItem.GetCommand: TCtxDataCommand;
begin
  if FCommand = nil then
    FCommand := CreateCommand
  else
    if not FCommandParamsAssigned then
      UpdateCommandParams(FCommand);
  FCommandParamsAssigned := True;
  Result := FCommand;
end;

procedure TCtxDBCommandItem.ReleaseCommand;
begin
  FreeAndNil(FCommand);
end;

function TCtxDBCommandItem.GetDisplayName: String;
begin
  Result := Name;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TCtxDBCommandItem.SetParams(const Value: TCtxParameters);
begin
  FParams.Assign(Value);
  FCommandParamsAssigned := False;
end;

procedure TCtxDBCommandItem.InternalExecuteCommand(const AParams: Variant);
begin
  // +++ Simplify parameters passing back and forth
  InternalPrepareCommand;
  Params.AssignValues(AParams);
  // Assign parameters to command
  Command.Params.AssignValues(Params);
  // Execute it
  Command.Execute;
  // Get output parameters back
  Params.AssignValues(Command.Params);
end;

procedure TCtxDBCommandItem.InternalExecuteCommand(ARow: TCtxDataRow; AParams: TCtxParameters);
begin
  // +++ Simplify parameters passing back and forth
  InternalPrepareCommand;
  AssignParams(Params, ARow, AParams);
  // Assign parameters to command
  Command.Params.AssignValues(Params);
  // Execute it
  Command.Execute;
  // Get output parameters back
  Params.AssignValues(Command.Params);
end;

procedure TCtxDBCommandItem.InternalFetchRows(DataTable: TCtxDataTable; AcceptChanges: Boolean);
var
  ColMap: TCtxColumnMap;
  Row, NewRow: TCtxDataRow;
begin
  ColMap := CreateColumnMap(DataTable, Command.Fields);
  Command.BeginIteration;
  try
    while not Command.EOF do
    begin
      // Command.CurrentRow --> DataTable.Insert
      NewRow := DataTable.New;
      Row := nil;
      try
        AssignRowFromParams(NewRow, Command.Fields, ColMap);
        if DataTable.PKExists then
          Row := DataTable.FindRow(NewRow);
          // Row := DataTable.FindRow(NewRow, DataTable.PKColumns, []);
        SelectParentRows(NewRow);
        if Row <> nil then
          Row.Assign(NewRow)
        else Row := DataTable.Insert(NewRow);

        if AcceptChanges then
          Row.AcceptChanges;
      finally
        if NewRow <> Row then
          FreeAndNil(NewRow);
      end;
      Command.Next;
    end;
  finally
    Command.EndIteration;
  end;
end;

procedure TCtxDBCommandItem.DoUpdateSourceRow(Command: TCtxDataCommand; ARow: TCtxDataRow);
var
  WasEditing: Boolean;
begin
  if (not UpdateSourceRow) or (ResultType = crtNone) then exit;

  WasEditing := ARow.Editing;
  if not WasEditing then
    ARow.BeginEdit;
  try
    if (ResultType = crtResultSet) and not Command.EOF then
    begin
      // Assign source row columns from fields
      AssignRowFromOutputParams(ARow, Command.Fields);
    end else
    begin
      // Assign source row columns from output params
      AssignRowFromOutputParams(ARow, Params);
    end;
    if not WasEditing then
      ARow.EndEdit;
  except
    if not WasEditing then
      ARow.CancelEdit;
    raise;
  end;
end;

procedure TCtxDBCommandItem.SelectParentRows(Row: TCtxDataRow);
var
  Relation: TCtxDataRelation;
  I: Integer;
  KeyValue: Variant;
begin
  // Insert keys into related tables
  for I := 0 to Row.DataTable.DataContainer.Relations.Count - 1 do
  begin
    Relation := Row.DataTable.DataContainer.Relations[I];
    // Requirement: Relation.ParentColumns == Relation.ParentTable.PKColumns
    if (Relation.ChildTable = Row.DataTable) and Relation.Identifying then
    begin
      KeyValue := Row.Values[Relation.ChildColumns];
      // We need to redo this.
      // 1. Locate ParentTable.Relation.ParentColumns(Row.ChildColumns)
      // 2. if not Exists and Relation.ParentColumns == Relation.ParentTable.PKColumns then InsertKey
      if not _VarIsNull(KeyValue) then
        Relation.ParentTable.InsertKey(KeyValue);
    end;
  end;
end;

procedure TCtxDBCommandItem.SelectIntoTable(DataTable: TCtxDataTable;
  ParentRow: TCtxDataRow = nil);
begin
  InternalExecuteCommand(ParentRow, DataTable.DataContainer.Params);
  InternalFetchRows(DataTable, True {AcceptChanges});
end;

procedure TCtxDBCommandItem.FetchRows(DataTable: TCtxDataTable; const AParams: Variant);
begin
  InternalExecuteCommand(AParams);
  InternalFetchRows(DataTable, False {AcceptChanges});
end;

procedure TCtxDBCommandItem.InternalRefreshRow(Command: TCtxDataCommand; ARow: TCtxDataRow);
begin
  // Delete childs ? Refresh details +++
  DoUpdateSourceRow(Command, ARow);
  // How do we control that we need to accept changes
  ARow.AcceptChanges;
  // Select linked parent rows
  SelectParentRows(ARow);
  // Select detail rows
  Adapter.InternalFillChildRows(ARow);
end;

function TCtxDBCommandItem.ExecuteRefreshRow(Row: TCtxDataRow; DeleteIfNotFound: Boolean = False): Boolean;
begin
  Row.NeedRefresh := False; // MB: 11/20/2008

  // 2011-02-01 MB: Added DeleteIfNotFound parameter so now we can use this method in ExecuteRefreshRows

  InternalExecuteCommand(Row, Row.DataContainer.Params);
  // What if we have output parameters?
  // NOTE: we do not correctly support refreshing row via return of
  // output parameters, only select statement is supported correctly!
  Result := not Command.EOF;
  if Result then
    InternalRefreshRow(Command, Row)
  else
    with Row.DataTable do
    if DeleteIfNotFound and CanDeleteRow(Row) then
      Delete(Row);
end;

function TCtxDBCommandItem.ExecuteRefreshRows(DataTable: TCtxDataTable;
  AllRows, DeleteIfNotFound: Boolean): Integer;
var
  R: Integer;
  Row: TCtxDataRow;
  DataTableWasUpdating: Boolean;
begin
  Result := 0;
  DataTableWasUpdating := DataTable.IsUpdating;
  try
    // After preparing refresh command, we will iterate this table
    R := 0;
    with DataTable do
      while R < RowCount do
      begin
        Row := Rows[R];
        if AllRows or Row.NeedRefresh then
        begin
          if not DataTable.IsUpdating then
            DataTable.BeginUpdate;

          if ExecuteRefreshRow(Row, DeleteIfNotFound) then
            Inc(Result);
        end;
        if not Row.Deleted then
          Inc(R);
      end;
  finally
    if not DataTableWasUpdating and DataTable.IsUpdating then
      DataTable.EndUpdate;
  end;
end;

procedure TCtxDBCommandItem.ExecuteUpdateRow(Row: TCtxDataRow);
begin
  InternalExecuteCommand(Row, Row.DataContainer.Params);
  DoUpdateSourceRow(Command, Row);
end;

function TCtxDBCommandItem.ExecuteProc(AParams: Variant): Variant;
begin
  InternalExecuteCommand(AParams);
  if ResultType = crtParameters then
    Result := Command.Params.GetOutputValues
  else if (ResultType = crtResultSet) and not Command.EOF then
    Result := Command.Fields.GetOutputValues
  else Result := NULL;
end;

procedure TCtxDBCommandItem.ExecuteProc(Row: TCtxDataRow; AParams: Variant); 
begin
  InternalExecuteCommand(Row, Params);
  DoUpdateSourceRow(Command, Row);
end;

procedure TCtxDBCommandItem.SetCommandText(const Value: String);
begin
  if FCommandText <> Value then
  begin
    // ReleaseCommand;
    FCommandText := Value;
    FCommandParamsAssigned := False;
  end;
end;

procedure TCtxDBCommandItem.SetCommandType(
  const Value: TCtxDBCommandItemType);
begin
  if FCommandType <> Value then
  begin
    // ReleaseCommand;
    FCommandType := Value;
    FCommandParamsAssigned := False;
  end;
end;

procedure TCtxDBCommandItem.SetResultType(const Value: TCtxDBResultType);
begin
  if FResultType <> Value then
  begin
    // ReleaseCommand;
    FResultType := Value;
    FCommandParamsAssigned := False;
  end;
end;

function TCtxDBCommandItem.GetDataProvider: ICtxDataProvider;
begin
  Result := FDataProvider;
  if (Result = nil) and (DataProviderName <> '') then
  begin
    DataProviderObject := FindCtxDataProvider(FDataProviderName);
    Result := FDataProvider;
  end;
  if Result = nil then
  begin
    Adapter.CheckDataProvider;
    Result := Adapter.DataProvider;
  end;
end;

procedure TCtxDBCommandItem.SetDataProviderName(const Value: String);
begin
  if FDataProviderName <> Value then
  begin
    ReleaseCommand;
    FDataProviderName := Value;
    DataProviderObject := FindCtxDataProvider(FDataProviderName);
  end;
end;

procedure TCtxDBCommandItem.SetDataProviderObject(const Value: TComponent);
begin
  if FDataProviderObject <> Value then
  begin
    ReleaseCommand;
    FDataProviderObject := Value;
    FDataProvider := nil;
    if FDataProviderObject <> nil then
      FDataProviderObject.FreeNotification(Adapter);
    if FDataProviderObject <> nil then
      Supports(FDataProviderObject, ICtxDataProvider, FDataProvider);
  end;
end;

function TCtxDBCommandItem.GetAdapter: TCtxDBAdapter;
begin
  Result := TCtxDBAdapter(Collection.Owner);
end;

procedure TCtxDBCommandItem.UpdateTableStructureFromResultSet(DataTable: TCtxDataTable);
var
  Column: TCtxDataColumn;
  F: TCtxParameter;
  I: Integer;
begin
  try
    Command.Prepared := True;
  except
    // Ignore errors here
  end;

  DataTable.DataContainer.Active := False;
  I := 0;
  // Delete data columns not present in result set
  while I < DataTable.Columns.Count do
  begin
    if (Command.Fields.Find(DataTable.Columns[I].ColumnName) = nil)
      and not DataTable.Columns[I].Calculated
    then
      DataTable.Columns[I].Free
    else
      Inc(I);
  end;

  // Create or update columns from the result set
  for I := 0 to Command.Fields.Count - 1 do
  begin
    F := Command.Fields[I];
    Column := DataTable.Columns.Find(F.Name);
    if Column = nil then
      Column := DataTable.Columns.AddColumn(F.Name, F.DataType, False, F.DataSize);
    Column.DataType := F.DataType;
    Column.DataLength := F.DataSize;
  end;
end;

procedure TCtxDBCommandItem.UpdateTableStructure(DataTable: TCtxDataTable);
var
  B: TCtxCommandBuilder;
begin
  B := FindCommandBuilder(Self);
  if B = nil then
    UpdateTableStructureFromResultSet(DataTable)
  else B.UpdateTableStructure(Self, DataTable);
end;

procedure TCtxDBCommandItem.UpdateTablePrimaryKey(DataTable: TCtxDataTable);
var
  I: Integer;
  ColName: String;
  Col: TCtxDataColumn;
begin
  if DataTable.HasPKColumns then exit;
  for I := 0 to Params.Count - 1 do
  begin
    ColName := Params[I].SourceColumn;
    if ColName = '' then
      ColName := Params[I].Name;
    if ColName = '' then continue;

    Col := DataTable.Columns.Find(ColName);
    if Col <> nil then
      Col.PrimaryKey := True;
  end;
end;

function TCtxDBCommandItem.CopyCommandData(Source: TCtxDBCommandItem): TObject;
begin
  if Source.CommandData = nil then
    Result := nil
  else begin
    Result := TMemoryStream.Create;
    Source.WriteCommandData(Result as TMemoryStream);
    TMemoryStream(Result).Position := 0;
  end;
end;

procedure TCtxDBCommandItem.Assign(Source: TPersistent);
begin
  if Source is TCtxDBCommandItem then
  begin
    Name := TCtxDBCommandItem(Source).FName;
    DataProviderName := TCtxDBCommandItem(Source).DataProviderName;
    SourceTableName := TCtxDBCommandItem(Source).SourceTableName;
    Params := TCtxDBCommandItem(Source).Params;
    UpdateSourceRow := TCtxDBCommandItem(Source).UpdateSourceRow;
    ResultType := TCtxDBCommandItem(Source).ResultType;
    CommandType := TCtxDBCommandItem(Source).CommandType;
    CommandText := TCtxDBCommandItem(Source).CommandText;
    CommandData := CopyCommandData(TCtxDBCommandItem(Source));
    ParentTableName := TCtxDBCommandItem(Source).ParentTableName;
  end else
    inherited;
end;

procedure TCtxDBCommandItem.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('CommandData', ReadCommandData, WriteCommandData, FCommandData <> nil);
end;

procedure TCtxDBCommandItem.ReadCommandData(Stream: TStream);
begin
  if Stream.Size = 0 then
    CommandData := nil
  else
  begin
    CommandData := TMemoryStream.Create;
    TMemoryStream(FCommandData).Size := Stream.Size;
    Stream.Read(TMemoryStream(FCommandData).Memory^, Stream.Size);
  end;
  (*
var
  B: TCtxCommandBuilder;

  FreeAndNil(FCommandData);
  if Stream.Size = 0 then exit;

  B := FindCommandBuilder(Self);
  if B <> nil then
  begin
    FCommandData := B.CreateCommandData;
    if FCommandData <> nil then
      B.ReadCommandData(Stream, FCommandData);
  end else
  begin
    FCommandData := TMemoryStream.Create;
    TMemoryStream(FCommandData).Size := Stream.Size;
    Stream.Read(TMemoryStream(FCommandData).Memory^, Stream.Size);
  end;
  *)
end;

procedure TCtxDBCommandItem.WriteCommandData(Stream: TStream);
var
  B: TCtxCommandBuilder;
begin
  if FCommandData = nil then exit;
  if FCommandData is TMemoryStream then
    Stream.Write(TMemoryStream(FCommandData).Memory^, TMemoryStream(FCommandData).Size)
  else
  begin
    B := FindCommandBuilder(Self);
    if B <> nil then
      B.WriteCommandData(Stream, FCommandData);
  end;
end;

function TCtxDBCommandItem.EditCommandItem: Boolean;
var
  B: TCtxCommandBuilder;
begin
  B := FindCommandBuilder(Self);
  if B <> nil then
    Result := B.EditCommandItem(Self)
  else Result := False;

  if Result then
    ParseCommandParams;
end;

procedure TCtxDBCommandItem.SetCommandData(const Value: TObject);
begin
  if FCommandData <> Value then
  begin
    if FCommandData <> nil then
      FCommandData.Free;
    FCommandData := Value;
  end;
end;

function TCtxDBCommandItem.GetEffectiveDataProviderName: String;
begin
  Result := FDataProviderName;
  if Result = '' then
    Result := Adapter.DataProviderName;
end;

procedure TCtxDBCommandItem.OnParamsChange(Sender: TObject);
begin
  FCommandParamsAssigned := False;
end;

procedure TCtxDBCommandItem.ParseCommandParams;
var
  I: Integer;
  P: TCtxParameter;
  TempParams: TCtxParameters;
begin
  if CommandText <> '' then
  begin
    TempParams := TCtxParameters.Create(Self, TCtxParameter);
    try
      TempParams.Assign(Params);
      Params.Clear;
      Command.Prepared := False;
      Command.Prepared := True;
    except
      // Ignore errors
    end;
    Params.Assign(TempParams);
    FreeAndNil(TempParams);

    // Iterate parameters and if not found in own collection
    // assign them from command parameters
    with Command do
    for I := 0 to Params.Count - 1 do
    begin
      P := Self.Params.Find(Params[I].Name);
      if P = nil then
        Self.Params.Add.Assign(Params[I]);
    end;
  end;
end;

procedure TCtxDBCommandItem.UpdateCommandName;
begin
  if Name = '' then
    Name := Collection.GetAutoName(SourceTableName + '.' + SCommandItemTypes[CommandType]);
end;

procedure TCtxDBCommandItem.InternalPrepareCommand;
var
  B: TCtxCommandBuilder;
begin
  if not Command.Prepared then
  begin
    // Give command builder a chance to enter parameters
    B := FindCommandBuilder(Self);
    if B <> nil then
      B.PrepareCommand(Self);
    Command.Prepared := True;
  end;
end;

function TCtxDBCommandItem.GetPrepared: Boolean;
begin
  Result := (FCommand <> nil) and FCommand.Prepared;
end;

procedure TCtxDBCommandItem.SetPrepared(const Value: Boolean);
begin
  if Prepared <> Value then
  begin
    if Value then
      InternalPrepareCommand
    else
      if FCommand <> nil then
        FCommand.Prepared := False;
  end;
end;

{ TCtxDBCommandItems }

function TCtxDBCommandItems.Add: TCtxDBCommandItem;
begin
  Result := TCtxDBCommandItem(inherited Add);
end;

function TCtxDBCommandItems.AddCommand(const TableName: String; ACommandType: TCtxDBCommandItemType): TCtxDBCommandItem;
begin
  Result := Add;
  Result.SourceTableName := TableName;
  Result.CommandType := ACommandType;
  Result.Name := Self.GetAutoName(TableName + '.' + SCommandItemTypes[ACommandType]);
  Result.UpdateSourceRow := not (ACommandType in [citSelect, citDelete]);
  if ACommandType in [citSelect, citRefresh] then
    Result.ResultType := crtResultSet
  else Result.ResultType := crtParameters;
end;

function TCtxDBCommandItems.Find(const AName: string): TCtxDBCommandItem;
begin
  Result := TCtxDBCommandItem(inherited Find(AName));
end;

function TCtxDBCommandItems.Get(const AName: string): TCtxDBCommandItem;
begin
  Result := TCtxDBCommandItem(inherited Get(AName));
end;

function TCtxDBCommandItems.GetItem(Index: Integer): TCtxDBCommandItem;
begin
  Result := TCtxDBCommandItem(inherited GetItem(Index));
end;

procedure TCtxDBCommandItems.SetItem(Index: Integer;
  Value: TCtxDBCommandItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCtxDBAdapter }

constructor TCtxDBAdapter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFetchingAll := False;
  FCommands := TCtxDBCommandItems.Create(Self, TCtxDBCommandItem);
  FParamPrefix := ':';
  FFillingCounter := 0;
  FUpdateCounter := 0;
end;

destructor TCtxDBAdapter.Destroy;
begin
  DataProviderObject := nil;
  FCommands.Free;
  inherited Destroy;
end;

procedure TCtxDBAdapter.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  if Operation = opRemove then
  begin
    if FDataProviderObject = AComponent then
      DataProviderObject := nil;
    for I := 0 to Commands.Count - 1 do
      if Commands[I].DataProviderObject = AComponent then
        Commands[I].DataProviderObject := nil;
  end;
  inherited;
end;

procedure TCtxDBAdapter.CheckDataProvider;
begin
  if (FDataProvider = nil) and (FDataProviderName <> '') then
    DataProviderObject := FindCtxDataProvider(FDataProviderName);
  if FDataProvider = nil then
    raise Exception.Create(SConnectionIsNotAssigned);
end;

procedure TCtxDBAdapter.SetCommands(const Value: TCtxDBCommandItems);
begin
  FCommands.Assign(Value);
end;

function TCtxDBAdapter.HasCommands(const TableName: String; CommandTypes: TCtxDBCommandItemTypes): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Commands.Count - 1 do
    if AnsiSameText(Commands[I].SourceTableName, TableName)
      and (Commands[I].CommandType in CommandTypes)
    then exit;
  Result := False;
end;

function TCtxDBAdapter.GetCommands(const TableName: String; CommandTypes: TCtxDBCommandItemTypes): TList;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Commands.Count - 1 do
    if AnsiSameText(Commands[I].SourceTableName, TableName)
      and (Commands[I].CommandType in CommandTypes)
    then begin
      if Result = nil then Result := TList.Create;
      Result.Add(Commands[I]);
    end;
end;

function TCtxDBAdapter.FindCommand(const TableName: String; CommandTypes: TCtxDBCommandItemTypes): TCtxDBCommandItem;
var
  I: Integer;
begin
  for I := 0 to Commands.Count - 1 do
  begin
    Result := Commands[I];
    if AnsiSameText(Result.SourceTableName, TableName) and (Result.CommandType in CommandTypes) then
      exit;
  end;
  Result := nil;
end;

procedure TCtxDBAdapter.BeginFilling(DataContainer: TCtxDataContainer);
begin
  if FFillingCounter = 0 then
    BeforeFill(DataContainer);
  Inc(FFillingCounter);
end;

procedure TCtxDBAdapter.EndFilling(DataContainer: TCtxDataContainer);
begin
  Dec(FFillingCounter);
  if FFillingCounter = 0 then
    AfterFill(DataContainer);
end;

function TCtxDBAdapter.IsFilling: Boolean;
begin
  Result := FFillingCounter > 0;
end;

procedure TCtxDBAdapter.BeforeFill(DataContainer: TCtxDataContainer); 
var
  I: Integer;
begin
  // Unprepare all command items before fill
  for I := 0 to Commands.Count - 1 do
    Commands[I].Prepared := False;
end;

procedure TCtxDBAdapter.AfterFill(DataContainer: TCtxDataContainer);
var
  Refreshed, I: Integer;
begin
  if IsFilling or (DataContainer = nil) then exit;

  // In FetchAll mode we will have to iterate all tables and attemopt to
  // refresh them until all rows that need refresh are refreshed.
  // This can be improved by having NeedRefresh flag on table level
  // set by setting NeedRefresh flag for one of the rows.
  if FetchAll and not FFetchingAll then
  begin
    FFetchingAll := True;
    try
      repeat
        Refreshed := 0;
        // for each table that needs refreshing do refresh
        for I := 0 to DataContainer.Tables.Count - 1 do
          Inc(Refreshed, RefreshRows(DataContainer.Tables[I]));
      until Refreshed = 0;
    finally
      FFetchingAll := False;
    end;
  end;
end;

procedure TCtxDBAdapter.FetchRows(DataTable: TCtxDataTable; const ACommandName: String; const AParams: Variant);
var
  I: Integer;
begin
  BeginFilling(DataTable.DataContainer);
  try
    // DataTable.BeginUpdate;
    try
      TCtxDBCommandItem(Commands.Get(ACommandName)).FetchRows(DataTable, AParams);

      // Load detail rows for each owned relation
      with DataTable do
      for I := 0 to RowCount - 1 do
        InternalFillChildRows(Rows[I]);
    finally
      // DataTable.EndUpdate;
    end;
  finally
    EndFilling(DataTable.DataContainer);
  end;
end;

procedure TCtxDBAdapter.InternalFillChildRows(ARow: TCtxDataRow);
var
  I: Integer;
begin
  (*
  Relation: TCtxDataRelation;
  with ARow.DataContainer do
  for I := 0 to Relations.Count - 1 do
  begin
    Relation := Relations[I];
    if (Relation.ParentTable = ARow.DataTable) and Relation.OwnRows then
    begin
      // If child rows are empty
      if Relation.ChildTable.FindRow(ARow, Relation.ChildColumns, Relation.ParentColumns, []) = nil then
        FillTable(Relation.ChildTable, ARow);
    end;
  end;
  *)
  for I := 0 to Commands.Count - 1 do
  with Commands[I] do
    if (CommandType = citSelect)
      and not AnsiSameText(SourceTableName, ARow.DataTable.Name)
      and AnsiSameText(ParentTableName, ARow.DataTable.Name)
    then
      InternalFillTable(Commands[I], ARow.DataContainer.Tables.Get(SourceTableName), ARow);
end;

procedure TCtxDBAdapter.InternalFillTable(ACommand: TCtxDBCommandItem;
  DataTable: TCtxDataTable; ParentRow: TCtxDataRow = nil);
var
  I: Integer;
begin
  // Ignore nil data table - this is a possiblity here and is not a problem.
  if (DataTable = nil) or (ACommand = nil) then exit;
  DataTable.BeginUpdate;
  try
    // Load table rows
    ACommand.SelectIntoTable(DataTable, ParentRow);
    // Load detail rows for each owned relation
    with DataTable do
    for I := 0 to RowCount - 1 do
      InternalFillChildRows(DataTable.Rows[I]);
  finally
    DataTable.EndUpdate;
  end;
end;

procedure TCtxDBAdapter.FillTable(DataTable: TCtxDataTable; ParentRow: TCtxDataRow = nil);
var
  I: Integer;
begin
  BeginFilling(DataTable.DataContainer);
  try
    for I := 0 to Commands.Count - 1 do
    with Commands[I] do
      if (CommandType = citSelect)
        and AnsiSameText(SourceTableName, DataTable.Name)
        and ((ParentTableName = '') or ((ParentRow <> nil) and AnsiSameText(ParentRow.DataTable.Name, ParentTableName)))
      then
        InternalFillTable(Commands[I], DataTable, ParentRow);
  finally
    EndFilling(DataTable.DataContainer);
  end;
end;

procedure TCtxDBAdapter.RefreshTable(DataTable: TCtxDataTable);
var
  RefreshCommands: TList;
  CommandItem: TCtxDBCommandItem;
  R, I: Integer;
  ParentTable: TCtxDataTable;
begin
  RefreshCommands := nil;
  BeginFilling(DataTable.DataContainer);
  try
    DataTable.BeginUpdate;
    try
      RefreshCommands := GetCommands(DataTable.Name, [citRefresh]);
      if RefreshCommands <> nil then
      begin
        CommandItem := TCtxDBCommandItem(RefreshCommands.First);
        // Execute refresh via refresh command
        CommandItem.ExecuteRefreshRows(DataTable, True, True);
      end else
      begin
        RefreshCommands := GetCommands(DataTable.Name, [citSelect]);
        if RefreshCommands = nil then exit;

        // Execute refresh via select
        // 1. Delete all records that we can delete
        with DataTable do
        for R := 0 to RowCount - 1 do
          if CanDeleteRow(Rows[R]) then
            Delete(Rows[R]);

        // 2. Select new records for each select command
        for I := 0 to RefreshCommands.Count - 1 do
        begin
          CommandItem := TCtxDBCommandItem(RefreshCommands[I]);
          if CommandItem.ParentTableName <> '' then
          begin
            // 2.1. Select range for each record in parent table
            ParentTable := DataTable.DataContainer.Tables.Get(CommandItem.ParentTableName);
            for R := 0 to ParentTable.RowCount - 1 do
              CommandItem.SelectIntoTable(DataTable, ParentTable.Rows[R]);
          end else
            // 2.2. Select all records from command items
            CommandItem.SelectIntoTable(DataTable);
        end;
      end;
    finally
      RefreshCommands.Free;
      DataTable.EndUpdate;
    end;
  finally
    EndFilling(DataTable.DataContainer);
  end;
end;

procedure TCtxDBAdapter.RefreshRow(Row: TCtxDataRow);
var
  I: Integer;
begin
  if Row.Deleted then exit;
  BeginFilling(Row.DataContainer);
  try
    for I := 0 to Commands.Count - 1 do
    if AnsiSameText(Commands[I].SourceTableName, Row.DataTable.Name) and
      (Commands[I].CommandType = citRefresh)
    then
    begin
      // Only execute first refresh operation
      Commands[I].ExecuteRefreshRow(Row, True {Delete if not found}); // 2011-02-01 MB
      break;
    end;
  finally
    EndFilling(Row.DataContainer);
  end;
end;

function TCtxDBAdapter.RefreshRows(DataTable: TCtxDataTable): Integer;
var
  I: Integer;
  CommandItem: TCtxDBCommandItem;
begin
  Result := 0;
  BeginFilling(DataTable.DataContainer);
  try
    for I := 0 to Commands.Count - 1 do
    begin
      CommandItem := Commands[I];
      if AnsiSameText(CommandItem.SourceTableName, DataTable.Name) and
        (CommandItem.CommandType = citRefresh)
      then
      begin
        Inc(Result, CommandItem.ExecuteRefreshRows(DataTable, False, True));
        break;
      end;
    end;
  finally
    EndFilling(DataTable.DataContainer);
  end;
end;

procedure TCtxDBAdapter.InternalUpdateRow(Row: TCtxDataRow; UpdateCommands: TList = nil);
var
  I: Integer;
  OwnCommands: Boolean;
begin
  if not (Row.Inserted or Row.Updated or Row.Deleted) then exit;

  OwnCommands := UpdateCommands = nil;
  if OwnCommands then
    UpdateCommands := GetCommands(Row.DataTable.Name, [citInsert, citUpdate, citDelete]);
  if UpdateCommands = nil then exit;
  try
    // Update details? +++
    with Row do
      for I := 0 to UpdateCommands.Count - 1 do
      with TCtxDBCommandItem(UpdateCommands[I]) do
        case CommandType of
          citInsert: if Inserted and not Deleted then
            ExecuteUpdateRow(Row);
          citDelete: if Deleted and not Inserted then
            ExecuteUpdateRow(Row);
          citUpdate: if Updated and not (Inserted or Deleted) then
            ExecuteUpdateRow(Row);
        end;
  finally
    if OwnCommands then
      UpdateCommands.Free;
  end;
end;

procedure TCtxDBAdapter.InternalUpdateTable(DataTable: TCtxDataTable; ParentRow: TCtxDataRow = nil);
var
  R: Integer;
  UpdateCommands: TList;
begin
  // 1. Do we have update/delete/insert commands
  UpdateCommands := GetCommands(DataTable.Name, [citInsert, citUpdate, citDelete]);
  if UpdateCommands = nil then exit;
  with DataTable do
  try
    // First we execute deletes
    for R := RowCount to PhysicalRowCount - 1 do
      InternalUpdateRow(Rows[R], UpdateCommands);
    // Then we execute updates and inserts - this is to avoid key violations on duplicate rows
    for R := 0 to RowCount - 1 do
      InternalUpdateRow(Rows[R], UpdateCommands);
    AcceptChanges;
  finally
    UpdateCommands.Free;
  end;
end;

procedure TCtxDBAdapter.UpdateRow(Row: TCtxDataRow);
begin
  BeginUpdate;
  try
    InternalUpdateRow(Row);
    Row.AcceptChanges;
  finally
    EndUpdate;
  end;
end;

procedure TCtxDBAdapter.UpdateTable(DataTable: TCtxDataTable; ParentRow: TCtxDataRow = nil);
begin
  BeginUpdate;
  try
    InternalUpdateTable(DataTable);
  finally
    EndUpdate;
  end;
end;

procedure TCtxDBAdapter.Fill(DataContainer: TCtxDataContainer);
var
  I: Integer;
begin
  CheckDataProvider;
  BeginFilling(DataContainer);
  try
    // Fill all tables using select commands in order they appear in adapter
    // Only execute commands which has no parent table requirement.
    for I := 0 to Commands.Count - 1 do
    with Commands[I] do
      if (CommandType = citSelect) and (ParentTableName = '') then
        InternalFillTable(Commands[I], DataContainer.Tables.Find(SourceTableName));
    (*
    // Fill all tables in the order they appear in the container.
    // Only fill tables that are not owned.
    for I := 0 to DataContainer.Tables.Count - 1 do
      if not DataContainer.Tables[I].HasOwnerTable then
        FillTable(DataContainer.Tables[I]);
    *)
  finally
    EndFilling(DataContainer);
  end;
end;

procedure TCtxDBAdapter.BeginUpdate;
begin
  CheckDataProvider;
  if FUpdateCounter = 0 then
    FDataProvider.StartTransaction;
  Inc(FUpdateCounter);
end;

procedure TCtxDBAdapter.EndUpdate;
begin
  Dec(FUpdateCounter);
  if FUpdateCounter = 0 then
  begin
    if ExceptObject = nil then
      FDataProvider.Commit
    else FDataProvider.Rollback;
  end;
end;

procedure TCtxDBAdapter.Update(DataContainer: TCtxDataContainer);
var
  I: Integer;
begin
  BeginUpdate;
  try
    // Updates are executed in the order of tables in container
    for I := 0 to DataContainer.Tables.Count - 1 do
      InternalUpdateTable(DataContainer.Tables[I]);
  finally
    EndUpdate;
  end;
end;

procedure TCtxDBAdapter.ConfigureTableCommands(DataTable: TCtxDataTable;
  CommandTypes: TCtxDBCommandItemTypes; Replace: Boolean = True);
var
  I: Integer;
  J: Integer;
  HasParentRelations: Boolean;
  CmdItem: TCtxDBCommandItem;
  ColNames, ColValues, WhereClause: String;
begin
  // Activate container to make sure that it is consistent
  DataTable.DataContainer.Active := True;
  if Replace then
  begin
    // Erase all commands, except procedures
    I := Commands.Count - 1;
    while I >= 0 do
    begin
      if AnsiSameText(Commands[I].SourceTableName, DataTable.Name) and
        (Commands[I].CommandType in CommandTypes - [citProcedure])
      then
        Commands[I].Free;
      Dec(I);
    end;
  end;

  // build select command from parent relations
  if citSelect in CommandTypes then
  begin
    HasParentRelations := False;
    with DataTable.DataContainer do
    for J := 0 to Relations.Count - 1 do
      if (Relations[J].ChildTable = DataTable) and Relations[J].OwnRows then
      begin
        CmdItem := Commands.AddCommand(DataTable.Name, citSelect);
        CmdItem.ParentTableName := Relations[J].ParentTableName;
        CmdItem.ResultType := crtResultSet;
        WhereClause := '';

        InitRelationParams(Relations[J], CmdItem.Params, WhereClause, ParamPrefix, EncloseIdentifiers);
        CmdItem.CommandText :=
          'SELECT ' + GetColumnNames(DataTable, EncloseIdentifiers) + #13#10 +
          'FROM ' + FormatIdentifier(DataTable.Name, EncloseIdentifiers);
        if WhereClause <> '' then
          CmdItem.CommandText := CmdItem.CommandText + #13#10'WHERE ' + WhereClause;
        HasParentRelations := True;
        break;
      end;

    if not HasParentRelations then
    begin
      CmdItem := Commands.AddCommand(DataTable.Name, citSelect);
      CmdItem.ResultType := crtResultSet;
      InitTableParams(DataTable, CmdItem.Params, True {keys only}, True {new key values}, False {old key values});

      CmdItem.CommandText :=
        'SELECT '+ GetColumnNames(DataTable, EncloseIdentifiers) + #13#10 +
        'FROM ' + FormatIdentifier(DataTable.Name, EncloseIdentifiers);
      if DataTable.PKExists then
        CmdItem.CommandText := CmdItem.CommandText + #13#10 +
          'WHERE ' + GetColumnAssign(DataTable, CmdItem.Params, ParamPrefix, EncloseIdentifiers, ' AND ', False);
    end;
  end;

  // build refresh command
  if (citRefresh in CommandTypes) and DataTable.PKExists then
  begin
    CmdItem := Commands.AddCommand(DataTable.Name, citRefresh);
    CmdItem.ResultType := crtResultSet;
    InitTableParams(DataTable, CmdItem.Params, True {keys only}, False {new key values}, True {old key values});
    CmdItem.CommandText :=
      'SELECT '+ GetColumnNames(DataTable, EncloseIdentifiers) + #13#10 +
      'FROM ' + FormatIdentifier(DataTable.Name, EncloseIdentifiers) + #13#10 +
      'WHERE ' + GetColumnAssign(DataTable, CmdItem.Params, ParamPrefix, EncloseIdentifiers, ' AND ', True);
  end;

  if citInsert in CommandTypes then
  begin
    // build insert command
    CmdItem := Commands.AddCommand(DataTable.Name, citInsert);
    CmdItem.ResultType := crtParameters;
    InitTableParams(DataTable, CmdItem.Params, False {keys only}, True {new key values}, False {old key values});
    GetColumnValues(DataTable, CmdItem.Params, ColNames, ColValues, ParamPrefix, EncloseIdentifiers);
    CmdItem.CommandText :=
      'INSERT INTO ' + FormatIdentifier(DataTable.Name, EncloseIdentifiers) + '(' +
        ColNames + #13#10') VALUES (' +
        ColValues + #13#10')';
  end;

  if (citUpdate in CommandTypes) and DataTable.PKExists then
  begin
    // build update command
    CmdItem := Commands.AddCommand(DataTable.Name, citUpdate);
    CmdItem.ResultType := crtParameters;
    InitTableParams(DataTable, CmdItem.Params, False {keys only}, True {new key values}, True {old key values});
    CmdItem.CommandText :=
      'UPDATE ' + FormatIdentifier(DataTable.Name, EncloseIdentifiers) +
      ' SET ' + GetColumnAssign(DataTable, CmdItem.Params, ParamPrefix, EncloseIdentifiers, ', ', False) + #13#10+
      'WHERE ' + GetColumnAssign(DataTable, CmdItem.Params, ParamPrefix, EncloseIdentifiers, ' AND ', True);
  end;

  if (citDelete in CommandTypes) and DataTable.PKExists then
  begin
    // build delete command
    CmdItem := Commands.AddCommand(DataTable.Name, citDelete);
    CmdItem.ResultType := crtNone;
    InitTableParams(DataTable, CmdItem.Params, True {keys only}, False {new key values}, True {old key values});
    CmdItem.CommandText :=
      'DELETE FROM ' + FormatIdentifier(DataTable.Name, EncloseIdentifiers) + #13#10 +
      'WHERE ' + GetColumnAssign(DataTable, CmdItem.Params, ParamPrefix, EncloseIdentifiers, ' AND ', True);
  end;
end;

function TCtxDBAdapter.GetTableCommandTypes(DataTable: TCtxDataTable): TCtxDBCommandItemTypes;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to Commands.Count - 1 do
  if AnsiSameText(Commands[I].SourceTableName, DataTable.Name) then
    Result := Result + [Commands[I].CommandType];
end;

procedure TCtxDBAdapter.AutoConfigure(DataContainer: TCtxDataContainer);
var
  I: Integer;
begin
  // Activate container to make sure that it is consistent
  DataContainer.Active := True;
  // Add commands for all tables
  for I := 0 to DataContainer.Tables.Count - 1 do
    ConfigureTableCommands(DataContainer.Tables[I], citAllCommands);
end;

function TCtxDBAdapter.Execute(const CommandName: String; Params: Variant): Variant;
begin
  BeginUpdate;
  try
    Result := TCtxDBCommandItem(Commands.Get(CommandName)).ExecuteProc(Params);
  finally
    EndUpdate;
  end;
end;

function TCtxDBAdapter.Execute(const CommandName: String): Variant;
begin
  BeginUpdate;
  try
    Result := TCtxDBCommandItem(Commands.Get(CommandName)).ExecuteProc(Unassigned);
  finally
    EndUpdate;
  end;
end;

procedure TCtxDBAdapter.ReleaseCommands;
var
  I: Integer;
begin
  for I := 0 to Commands.Count - 1 do
    Commands[I].ReleaseCommand;
end;

procedure TCtxDBAdapter.SetDataProviderName(const Value: String);
begin
  if FDataProviderName <> Value then
  begin
    ReleaseCommands;
    FDataProviderName := Value;
    DataProviderObject := FindCtxDataProvider(FDataProviderName);
  end;
end;

procedure TCtxDBAdapter.SetDataProviderObject(Value: TComponent);
begin
  if FDataProviderObject <> Value then
  begin
    ReleaseCommands;
    if FDataProviderObject <> nil then
      FDataProviderObject.RemoveFreeNotification(Self);
    FDataProviderObject := Value;
    FDataProvider := nil;
    if FDataProviderObject <> nil then
    begin
      FDataProviderObject.FreeNotification(Self);
      Supports(FDataProviderObject, ICtxDataProvider, FDataProvider);
    end;
  end;
end;

procedure TCtxDBAdapter.Assign(Source: TPersistent);
begin
  if Source is TCtxDBAdapter then
  begin
    DataProviderName := TCtxDBAdapter(Source).DataProviderName;
    Commands := TCtxDBAdapter(Source).Commands;
    DisplayName := TCtxDBAdapter(Source).DisplayName;
    Description := TCtxDBAdapter(Source).Description;
    FetchAll := TCtxDBAdapter(Source).FetchAll;
    EncloseIdentifiers := TCtxDBAdapter(Source).EncloseIdentifiers;
    ParamPrefix := TCtxDBAdapter(Source).ParamPrefix;
  end else
    inherited;
end;

function TCtxDBAdapter.GetDataProvider: ICtxDataProvider;
begin
  if (FDataProvider = nil) and (FDataProviderName <> '') then
    DataProviderObject := FindCtxDataProvider(FDataProviderName);
  Result := FDataProvider;
end;

{ TCtxCommandBuilder }

function TCtxCommandBuilder.CanEdit(
  CommandItem: TCtxDBCommandItem): Boolean;
begin
  Result := False;
end;

destructor TCtxCommandBuilder.Destroy;
begin
  UnRegisterCommandBuilder(Self);
  inherited;
end;

function TCtxCommandBuilder.GetDisplayName: String;
begin
  Result := Name;
end;

function TCtxCommandBuilder.CreateCommandData: TObject;
begin
  // Implement in descendants to store additional data with command items
  Result := nil;
end;

procedure TCtxCommandBuilder.ReadCommandData(Stream: TStream;
  CommandData: TObject);
begin
  // Do nothing. Implement in descendants if they also implement CreateCommandData
end;

procedure TCtxCommandBuilder.WriteCommandData(Stream: TStream;
  CommandData: TObject);
begin
  // Do nothing. Implement in descendants if they also implement CreateCommandData
end;

procedure TCtxCommandBuilder.CheckCommandData(CommandItem: TCtxDBCommandItem);
var
  NewCommandData: TObject;
begin
  if CommandItem.CommandData = nil then
    CommandItem.CommandData := CreateCommandData
  else if CommandItem.CommandData is TMemoryStream then
  begin
    NewCommandData := CreateCommandData;
    try
      TMemoryStream(CommandItem.CommandData).Position := 0;
      if NewCommandData <> nil then
        ReadCommandData(TMemoryStream(CommandItem.CommandData), NewCommandData);
      CommandItem.CommandData := NewCommandData;
    except
      NewCommandData.Free;
      raise;
    end;
  end;
end;

procedure TCtxCommandBuilder.PrepareCommand(
  CommandItem: TCtxDBCommandItem);
begin
  CheckCommandData(CommandItem);
end;

procedure TCtxCommandBuilder.UpdateTableStructure(CommandItem: TCtxDBCommandItem; DataTable: TCtxDataTable);
begin
  // Implement in descendants
  CommandItem.UpdateTableStructureFromResultSet(DataTable);
end;

procedure TCtxCommandBuilder.GetRelatedCommands( CommandItem: TCtxDBCommandItem;
  RelatedCommandItems: TCtxDBCommandItems);
begin
  // Implement in descendants. Fill the list of commands with potentiall references
  RelatedCommandItems.Clear;
end;

initialization
  CommandBuilders := nil;
finalization
  UnRegisterCommandBuilders;
end.


