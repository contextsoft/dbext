(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  ICtxDataProvider - common interface implemented by all database adapters
(*  TCtxDataCommand - abstract ancestor for all provider specific command objects
(*  TCtxParameters - generic collection of named parameters or values
(*
(*  Copyright (c) 2005-2017, Context Software LLC
(*
(******************************************************************************)
unit CtxDataTypes;

{$I CtxVer.inc}

interface

uses SysUtils, {$IFDEF DXE4_ORLATER}Types,{$ENDIF} Classes {$IFnDEF VER130}, Variants{$ENDIF};

type
  TCtxDataCommand = class;

  TCtxCommandType = (ctTableDirect, ctSQLSelect, ctSQLUpdate);
  TCtxParamType = (cptUnknown, cptInput, cptOutput, cptInputOutput, cptResult);
  TCtxDataType = (cdtUnknown, cdtSmallInt, cdtLargeInt, cdtBoolean, cdtInteger,
    cdtFloat, cdtDateTime, cdtDate, cdtTime, cdtString, cdtWideString, cdtGuid,
    cdtMemo, cdtBlob, cdtReference, cdtBCD, cdtSQLTimeStamp);
  TCtxDataRowVersion = (drvCurrent, drvOriginal);

  {:$ ICtxDataProvider defines common interface that should be implemented by all }
  {:$ data providers to support most simple command-based disconnected mode operation. }
  ICtxDataProvider = interface (IUnknown)
    ['{031E3CEE-C3C0-4218-B081-E907AC04C447}']

    { Property handlers }
    procedure SetConnected(Value: Boolean);
    function GetConnected: Boolean;
    function GetDatabaseName: String;
    procedure SetDatabaseName(const Value: String);
    function GetDriverName: String;
    function GetDatabaseURL: String;
    procedure SetDatabaseURL(const Value: String);

    { Transactions }
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function GetInTransaction: Boolean;

    { Disconnected mode methods }
    function CreateCommand: TCtxDataCommand;

    { Properties }
    property Connected: Boolean read GetConnected write SetConnected;
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
    property DatabaseURL: String read GetDatabaseURL write SetDatabaseURL;
    property DriverName: String read GetDriverName;
    property InTransaction: Boolean read GetInTransaction;
  end;

  {:$ TCtxParameter collection item represents a single parameter in a TCtxParameters parameters collection.}
  TCtxParameter = class (TCollectionItem)
  protected
    FName: String;
    FDataType: TCtxDataType;
    FParamType: TCtxParamType;
    FDataSize: Integer;
    FSourceColumn: String;
    FRowVersion: TCtxDataRowVersion;
    // FIsMacro: Boolean;
    // FLookupContext: String;
  public
    FValue: Variant;
    {:$ Copies the contents from another TCtxParameter object. }
    procedure Assign(Source: TPersistent); override;
    {:$ Specifies the name of the TCtxParameter as it appears in Object Inspector. }
    function GetDisplayName: string; override;
    {:$ Returns the effective name of the source column. If SourceColumn property }
    {:$ is empty, then the parameter name is assumed instead. }
    function GetSourceName: string;
    // procedure GetLookupList(List: TStrings); virtual; abstract;
  published
    {:$ Specifies the name of the parameter. }
    property Name: String read FName write FName;
    {:$ Specifies the value of the parameter. }
    property Value: Variant read FValue write FValue;
    {:$ Specifies the data type of the parameter. }
    property DataType: TCtxDataType read FDataType write FDataType;
    {:$ Specifies the parameter type (i.e. input, output, etc. see TCtxParamType enumeration). }
    property ParamType: TCtxParamType read FParamType write FParamType;
    {:$ Specifies the data size for the parameter when appicable. }
    property DataSize: Integer read FDataSize write FDataSize;
    {:$ Specifies the source column, this parameter is linked to. }
    property SourceColumn: String read FSourceColumn write FSourceColumn;
    {:$ Specifies the version of row, this parameter should take its value from. }
    property RowVersion: TCtxDataRowVersion read FRowVersion write FRowVersion;
    // property IsMacro: Boolean read FIsMacro write FIsMacro default False;
    // property IsArray: Boolean read GetIsArray write SetIsArray default False;
    // property LookupContext: String read FLookupContext write FLookupContext;
  end;

  {:$ TCtxParameters collection contains a list of TCtxParameter items.}
  TCtxParameters = class (TOwnedCollection)
  protected
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TCtxParameter;
    procedure SetItem(Index: Integer; Value: TCtxParameter);
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    {:$ Creates and adds a new TCtxParameter item to the TCtxParameters collection. }
    function Add: TCtxParameter;
    {:$ Creates and adds a new TCtxParameter item to the TCtxParameters collection. }
    function AddParameter(const Name: String; ParamType: TCtxParamType;
      DataType: TCtxDataType = cdtUnknown; DataSize: Integer = 0): TCtxParameter;
    {:$ Finds a parameter by name in parameters collection. }
    function Find(const Name: String): TCtxParameter;
    {:$ Finds a parameter by name in parameters collection. Rasies exception if }
    {:$ the parameter is not found. }
    function Get(const Name: String): TCtxParameter;
    {:$ Assigns value to a parameter by name. }
    procedure AssignByName(const Name: String; Value: Variant);
    {:$ Clear all parameter values. }
    procedure ClearValues;
    {:$ Returns variant-array containing all parameter values from parameters of output or result type. }
    function GetOutputValues: Variant;
    {:$ Returns a valid parameter name by replacing invalid characters with underscores. }
    function GetParamName(const ProposedName: String): String;
    {:$ Generate unique parameter name based on a proposed name. }
    function GetAutoName(const ProposedName: String; Counter: Integer = 0): String;
    {:$ Assign parameter values by name from a different set of parameters. }
    procedure AssignValues(Source: TCtxParameters); overload;
    {:$ Assign parameter values by order from a variant or variant array. }
    procedure AssignValues(Values: Variant); overload;

    // procedure GetLookupList(const LookupContext: String; List: TStrings); virtual; abstract;
    // function GetLookupElement(const LookupContext, ElementKey: String): String; virtual; abstract;

    {:$ Provides array-like access to the TCtxParameter items in the collection by index. }
    property Items[Index: Integer]: TCtxParameter read GetItem write SetItem; default;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {:$ TCtxDataCommand is an abstract ancestor for all provider specific command objects. }
  TCtxDataCommand = class (TPersistent)
  protected
    FCommandType: TCtxCommandType;
    FCommandText: String;
    FParams: TCtxParameters;
    FFields: TCtxParameters;
    FDataProvider: ICtxDataProvider;
    FDataProviderObject: TObject;

    procedure SetParams(const Value: TCtxParameters);
    procedure SetCommandType(Value: TCtxCommandType); virtual;
    procedure SetCommandText(const Value: String); virtual;
    function GetRowsAffected: Int64; virtual;

    function GetPrepared: Boolean; virtual; abstract;
    procedure SetPrepared(Value: Boolean); virtual; abstract;
  public
    {:$ Creates an instance of TCtxDataCommand object. Takes a reference to the object, }
    {:$ implementing data provider interface (ICtxDataProvider) }
    {:$ Generally, you should never create TCtxDataCommand or its descendants directly. }
    {:$ It is done for you by corresponding data providers. }
    constructor Create(ADataProviderObject: TObject);
    {:$ Destroys the instance of TCtxDataCommand object. }
    destructor Destroy; override;

    {:$ Checks to make sure that the DataProvider interface is assigned and Connected. }
    {:$ Raises exception in case DataProvider interface is not assigned. }
    procedure CheckProviderConnected;
    {:$ Checks if the command is prepared, otherwise raises an exception. }
    procedure CheckPrepared;

    procedure BeginIteration; virtual;
    procedure EndIteration; virtual;
    { Navigational methods}
    {:$ Executes command. The results are returned in Fields and Params collections. }
    {:$ Use Next and EOF methods for forward-only navigation through the returned record set. }
    {:$ The values for fields are contained in the Fields collection. }
    procedure Execute; virtual; abstract;
    {:$ Moves to the next record in returned record set.  }
    {:$ The values for fields are contained in the Fields collection. }
    procedure Next; virtual; abstract;
    {:$ Returns True if the cursor riches past the end of the recordset. }
    function EOF: Boolean; virtual; abstract;

    { Properties }
    {:$ Returns reference to the object, implementing data provider interface (ICtxDataProvider). }
    property DataProviderObject: TObject read FDataProviderObject;
    {:$ Returns True if the cursor riches past the end of the recordset. }
    property DataProvider: ICtxDataProvider read FDataProvider;
    {:$ True if the command is prepared. }
    property Prepared: Boolean read GetPrepared write SetPrepared;
    {:$ Determines the type of command. }
    property CommandType: TCtxCommandType read FCommandType write SetCommandType;
    {:$ Contains the text of command. Normally, for most providers, this would be }
    {:$ either a table name or SQL statement. }
    property CommandText: String read FCommandText write SetCommandText;
    {:$ Contains list of input and output parameters for the command. }
    {:$ Input parameters should be assigned before command execution. }
    {:$ The result of execution of procedures is normally returned in output parameters. }
    {:$ See also TCtxParamType. }
    property Params: TCtxParameters read FParams write SetParams;
    {:$ Contains list of fields and their value for the current record in the }
    {:$ returned result set. Check EOF method before accessing fields values. }
    property Fields: TCtxParameters read FFields;
    {:$ Returns number of rows affected or returned after the command is executed. }
    property RowsAffected: Int64 read GetRowsAffected;
  end;

  {:$ Registers a component, implementing ICtxDataProvider interface. }
  procedure RegisterCtxDataProvider(Database: TComponent);
  {:$ Unregisters a component, previously registered by RegisterCtxDataProvider method. }
  procedure UnRegisterCtxDataProvider(Database: TComponent);
  {:$ Finds data provider by its DatabaseName. }
  function FindCtxDataProvider(const DatabaseName: String): TComponent;
  {:$ Returns list of DatabaseName properties of all registered data providers. }
  {:$ Each instance is queried for ICtxDataProvider interface and its DatabaseName property }
  {:$ is added to List parameter.<br> }
  {:$ Note: the List will be cleared before adding data provider names. }
  procedure GetCtxDataProviders(List: TStrings);

resourcestring
  SParameterNotFound = 'Field or parameter not found: %s';
  SCommandNotPrepared = 'Unable to perform this operation. Command is not prepared';
  SInvalidDataProviderObject = 'Invalid data provider object assigned';
  SProviderNotAssigned = 'Data provider object is not assigned';

var
  DBDatabases: TList;

implementation

uses Math;

{$I CtxD2009.inc}

const
  { Databse Object Classes }
  docTable      = 1;
  docView       = 2;
  docField      = 3;
  docIndex      = 4;
  docConstraint = 5;
  docDomain     = 6;
  docStoredProc = 7;
  docTrigger    = 8;
  docUser       = 9;

function FindCtxDataProvider(const DatabaseName: String): TComponent;
var
  I: Integer;
  DB: ICtxDataProvider;
begin
  Result := nil;
  if DBDatabases = nil then exit;
  for I := 0 to DBDatabases.Count - 1 do
  if TObject(DBDatabases[I]).GetInterface(ICtxDataProvider, DB) then
  begin
    if AnsiCompareText(DB.DatabaseName, DatabaseName) = 0
    then begin
      Result := TComponent(DBDatabases[I]);
      exit;
    end;
  end;
end;

procedure RegisterCtxDataProvider(Database: TComponent);
var
  DB: ICtxDataProvider;
begin
  if (DBDatabases = nil) or (Database = nil) then exit;
  if not Database.GetInterface(ICtxDataProvider, DB) then exit;
  if DB.DatabaseName <> '' then
  begin
    if DBDatabases.IndexOf(Database) < 0 then
      DBDatabases.Add(Database);
  end else begin
    if DBDatabases <> nil then
      DBDatabases.Remove(Database);
  end;
end;

procedure UnRegisterCtxDataProvider(Database: TComponent);
begin
  if DBDatabases <> nil then
    DBDatabases.Remove(Database);
end;

procedure GetCtxDataProviders(List: TStrings);
var
  I: Integer;
  DB: ICtxDataProvider;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := 0 to DBDatabases.Count - 1 do
    if Supports(TObject(DBDatabases[I]), ICtxDataProvider, DB) then
      List.Add(DB.DatabaseName)
  finally
    List.EndUpdate;
  end;
end;

{ TCtxParameter }

function TCtxParameter.GetDisplayName: String;
begin
  Result := Name;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TCtxParameter.Assign(Source: TPersistent);
begin
  if Source is TCtxParameter then
  begin
    FName := TCtxParameter(Source).FName;
    FValue := TCtxParameter(Source).FValue;
    FDataType := TCtxParameter(Source).FDataType;
    FParamType := TCtxParameter(Source).FParamType;
    FDataSize := TCtxParameter(Source).FDataSize;
    FSourceColumn := TCtxParameter(Source).FSourceColumn;
    FRowVersion := TCtxParameter(Source).FRowVersion;
  end else
    inherited Assign(Source);
end;

function TCtxParameter.GetSourceName: string;
begin
  Result := SourceColumn;
  if Result = '' then
    Result := Name;
end;

{ TCtxParameters }

function TCtxParameters.Add: TCtxParameter;
begin
  Result := TCtxParameter(inherited Add);
end;

function TCtxParameters.AddParameter(const Name: String; ParamType: TCtxParamType;
  DataType: TCtxDataType = cdtUnknown; DataSize: Integer = 0): TCtxParameter;
begin
  Result := Add;
  Result.Name := Name;
  Result.ParamType := ParamType;
  Result.DataType := DataType;
  Result.DataSize := DataSize;
end;

procedure TCtxParameters.AssignByName(const Name: String; Value: Variant);
var
  Param: TCtxParameter;
begin
  Param := Find(Name);
  if Param <> nil then
    Param.Value := Value;
end;

procedure TCtxParameters.AssignValues(Source: TCtxParameters);
var
  I: Integer;
begin
  for I := 0 to Source.Count - 1 do
    AssignByName(Source[I].Name, Source[I].Value);
end;

procedure TCtxParameters.AssignValues(Values: Variant);
var
  I: Integer;
begin
  if VarIsEmpty(Values) then exit;

  for I := 0 to Count - 1 do
    Items[I].Value := Null;

  if VarIsArray(Values) then
  begin
    for I := 0 to Min(Count - 1, VarArrayHighBound(Values, 1)) do
      Items[I].Value := Values[I];
  end else
  begin
    if Count > 0 then
      Items[0].Value := Values;
  end;
end;

procedure TCtxParameters.ClearValues;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Value := NULL;
end;

function TCtxParameters.Find(const Name: String): TCtxParameter;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if AnsiSameText(Result.Name, Name) then
      exit;
  end;
  Result := nil;
end;

function TCtxParameters.Get(const Name: String): TCtxParameter;
begin
  Result := Find(Name);
  if Result = nil then
    raise Exception.CreateFmt(SParameterNotFound, [Name]);
end;

function TCtxParameters.GetAutoName(const ProposedName: String;
  Counter: Integer): String;
begin
  Result := ProposedName;
  if Counter > 0 then
    Result := Result + IntToStr(Counter);
  while Find(Result) <> nil do
  begin
    Inc(Counter);
    Result := ProposedName + IntToStr(Counter);
  end;
end;

function TCtxParameters.GetItem(Index: Integer): TCtxParameter;
begin
  Result := TCtxParameter(inherited GetItem(Index));
end;

function TCtxParameters.GetOutputValues: Variant;
var
  I, C: Integer;
begin
  // +++ return variant array containing all values of type output
  Result := NULL;
  C := 0;
  for I := 0 to Count - 1 do
  if Items[I].ParamType in [cptOutput, cptInputOutput, cptResult] then
    Inc(C);
  if C > 1 then
  begin
    Result := VarArrayCreate([0, C - 1], varVariant);
    C := 0;
    for I := 0 to Count - 1 do
      if Items[I].ParamType in [cptOutput, cptInputOutput, cptResult] then
      begin
        Result[C] := Items[I].Value;
        Inc(C);
      end;
  end else if C = 1 then
  begin
    for I := 0 to Count - 1 do
      if Items[I].ParamType in [cptOutput, cptInputOutput, cptResult] then
      begin
        Result := Items[I].Value;
        break;
      end;
  end;
end;

function TCtxParameters.GetParamName(const ProposedName: String): String;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  I: Integer;
begin
  // Replace all invalid identifiers with undescores
  Result := ProposedName;
  if Result = '' then
    Result := 'Param'
  else
  begin
    // classes, sysutils
    if not CharInSet(Result[1], Alpha) then
      Result := '_' + Result;
    for I := 1 to Length(Result) do
      if not CharInSet(Result[I], AlphaNumeric) then
        Result[I] := '_';
  end;
  Result := Result; // GetAutoName(Result);
end;

procedure TCtxParameters.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCtxParameters.SetItem(Index: Integer; Value: TCtxParameter);
begin
  inherited SetItem(Index, Value);
end;

{ TCtxDataCommand }

procedure TCtxDataCommand.CheckPrepared;
begin
  if not Prepared then
    raise Exception.Create(SCommandNotPrepared);
end;

procedure TCtxDataCommand.CheckProviderConnected;
begin
  if FDataProvider = nil then
    raise Exception.Create(SProviderNotAssigned);
  FDataProvider.Connected := True;
end;

constructor TCtxDataCommand.Create(ADataProviderObject: TObject);
begin
  inherited Create;
  FDataProviderObject := ADataProviderObject;
  if not Supports(FDataProviderObject, ICtxDataProvider, FDataProvider) then
    raise Exception.Create(SInvalidDataProviderObject);
  FParams := TCtxParameters.Create(Self, TCtxParameter);
  FFields := TCtxParameters.Create(Self, TCtxParameter);
end;

destructor TCtxDataCommand.Destroy;
begin
  Prepared := False;
  FDataProvider := nil;
  FParams.Free;
  FFields.Free;
  inherited;
end;

procedure TCtxDataCommand.BeginIteration;
begin
  // Implement in descendants
end;

procedure TCtxDataCommand.EndIteration;
begin
  // Implement in descendants
end;

function TCtxDataCommand.GetRowsAffected: Int64;
begin
  Result := -1;
end;

procedure TCtxDataCommand.SetCommandText(const Value: String);
begin
  FCommandText := Value;
end;

procedure TCtxDataCommand.SetCommandType(Value: TCtxCommandType);
begin
  FCommandType := Value;
end;

procedure TCtxDataCommand.SetParams(const Value: TCtxParameters);
begin
  FParams.Assign(Value);
end;

initialization
  DBDatabases := TList.Create;
finalization
  FreeAndNil(DBDatabases);
end.
