(******************************************************************************)
(*
(*  Context Database Extensions Suite (BDE)
(*
(*  Contains: TDatabaseExt component.
(*
(*  Copyright (c) 2005-2007 Michael Baytalsky
(*
(*  ------------------------------------------------------------
(*  FILE        : BDEExt.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 2.15
(*  DELPHI\BCB  : Delphi 5,6,7,2005,2006, 2007; C++Builder 6.0, 2006,
(*
(******************************************************************************)
unit BDEExt;

interface

uses
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, CtxDataTypes, CtxDataSetCommand, CtxDBIntf, dbSchema;

type
  TDatabaseExt = class;

  {:$ TBDERangeCursor object is an internal helper object, that facilitates }
  {:$ the process of iterating a table for a given range of key values. }
  TBDERangeCursor = class (TDBRangeCursor)
  public
    constructor Create(Database: TDatabaseExt; Relation: TRelation; KeyValues: Variant);
    constructor CreateExt(Database: TDatabaseExt; const TableName, KeyFields, TableFilter: AnsiString;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: AnsiString = '');
    destructor Destroy; override;
  end;

  {:$ Represents a BDE database connection. }
  TDatabaseExt = class(TDatabase, ISchemaDatabase, ICtxDataProvider, ICtxDatabase)
  protected
    { Protected declarations }
    FSchema: TDatabaseSchema;
    FSystemTableName: AnsiString;
    FVersionStr: AnsiString;

    function GetDatabaseName: AnsiString;
    procedure SetDatabaseName(const Value: AnsiString);
    function GetVersionLabel: AnsiString;
    procedure SetVersionLabel(const Value: AnsiString);

    { Replication support }
    function GetSchema: TDatabaseSchema;
    procedure SetSchema(Value: TDatabaseSchema);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetDatabaseURL: AnsiString;
    procedure SetDatabaseURL(const Value: AnsiString);

    function GetDriverName: AnsiString;

    { Checks if the database is connected }
    procedure CheckActive;
    { Checks if schema is assigned }
    procedure CheckSchema;
    procedure DoConnect; override;
  public
    {:: Creates an instance of a TDatabaseExt component.}
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of a TDatabaseExt component.}
    destructor Destroy; override;

    {:: Creates TTableExt component, corresponding to TableName. }
    function CreateTable(const TableName: AnsiString): TDataSet; virtual;

    {:: Creates TQueryExt component, corresponding to Statement. }
    function CreateQuery(const Statement: AnsiString): TDataSet;

    { Schema related methods. Schema must be assigned for them to work }
    {:$ Updates database schema from the physical database tables. }
    {:: Schema property must be assigned and refer to the Schema component }
    {:: that will be updated. }
    procedure ReverseEngineer;

    {:$ Checks whether the database version is *equal* or Newer }
    {:$ then the current Schema's Version. }
    function IsVersionCurrent(AllowNewer: Boolean = False): Boolean;
    function GetVersion: TSchemaVersion;
    procedure SetVersion(const Value: TSchemaVersion);

    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function GetInTransaction: Boolean;

    function FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;

    function GetRangeCursor(const TableName, KeyFields, TableFilter: AnsiString;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: AnsiString = ''): TDBRangeCursor; overload;
    function GetRangeCursor(Relation: TRelation; KeyValues: Variant): TDBRangeCursor; overload;

    { Returns assignable SQL property. }
    function GetQuerySQL(Query: TDataSet): AnsiString;
    procedure SetQuerySQL(Query: TDataSet; const Statement: AnsiString);
    { Returns assignable Params property. }
    procedure GetQueryParams(Query: TDataSet; Params: TParams);
    procedure SetQueryParams(Query: TDataSet; Params: TParams);
    { Executes query that does not return result set. }
    procedure ExecSQL(Query: TDataSet);
    { Executes statement that may return result set. }
    function ExecuteStatement(SQL: AnsiString; ResultSet: Pointer = nil): Integer;

    function CreateCommand: TCtxDataCommand;

    { Parent object is always a table or schema. }
    procedure GetTableNames(List: TStrings; SystemTables: Boolean = False);
    function GetIndexDefs(DataSet: TDataSet): TIndexDefs;
    function GetSystemTableName: AnsiString;
  published
    { Published properties }
    {:$ Reference to a TDatabaseSchema component. }
    {:: Schema may contain information about the database structure as weel as some }
    {:: additional information like referential integrity constraints, triggers and more. }
    property Schema: TDatabaseSchema read FSchema write SetSchema;
    {:$ Text presentation of the database version. }
    {:: This property is effectively read-only. Any text assigned to it will be ignored. }
    property VersionLabel: AnsiString read GetVersionLabel write SetVersionLabel stored False;
    {:$ The name of the System table. }
    {:: The default value of this property is 'system'. }
    property SystemTableName: AnsiString read GetSystemTableName write FSystemTableName;
    {:$ Specifies the uniform path to the database for both local and C/S types of access. }
    {:: This parameter is usefull for displaying or storing database and session parameters }
    {:: for the specific database. It may also be used with TBDEOpenDatabase dialog.<br> }
    {:: Attention! Assigning DatabaseURL may change the parameters of the session component }
    {:: the database is attached to. }
    property DatabaseURL: AnsiString read GetDatabaseURL write SetDatabaseURL stored false;
    property DatabaseName: AnsiString read GetDatabaseName write SetDatabaseName;
  end;

  {:$ Retrieves database URL from the TDatabase component and the connected Session component. }
  function GetDatabaseURL(BDEDatabase: TDatabase): AnsiString;
  {:$ Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(BDEDatabase: TDatabase; DatabaseURL: AnsiString);

  function GetBDEKeyParam(ParamList: TStrings): AnsiString;

  procedure Register;

resourcestring
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SDatabaseSchemaIsNotAssigned = 'Database Schema is not assigned';
  SInvalidRangeCursor = 'Invalid parameters supplied to range cursor';

implementation

const
  defSysTableName = 'SysTable';

{ General Helper Rountines }

procedure Register;
begin
  RegisterComponents('Database Extensions', [TDatabaseExt]);
end;

function GetBDEKeyParam(ParamList: TStrings): AnsiString;
const
  KeyParamNames: array [1..4] of AnsiString = ('ODBC DSN', 'PATH', 'DATABASE NAME', 'SERVER NAME');
var
  I: Integer;
begin
  for I := Low(KeyParamNames) to High(KeyParamNames) do
    if ContainsParam(KeyParamNames[I], ParamList) then
    begin
      Result := KeyParamNames[I];
      break;
    end;
end;

function GetDatabaseURL(BDEDatabase: TDatabase): AnsiString;
var
  ConnectionType, RemoteHost, DatabaseName: AnsiString;
begin
  if BDEDatabase.AliasName <> '' then
  begin
    ConnectionType := 'BDE';
    RemoteHost := 'ALIAS';
    DatabaseName := BDEDatabase.AliasName;
  end else begin
    ConnectionType := BDEDatabase.DriverName;
    RemoteHost := GetBDEKeyParam(BDEDatabase.Params);
    if RemoteHost <> '' then
      DatabaseName := BDEDatabase.Params.Values[RemoteHost];
  end;
  Result := EncodeDatabaseURL(ConnectionType, RemoteHost, DatabaseName);
end;

procedure SetDatabaseURL(BDEDatabase: TDatabase; DatabaseURL: AnsiString);
var
  ConnectionType, RemoteHost, DatabaseName: AnsiString;
begin
  DecodeDatabaseURL(DatabaseURL, ConnectionType, RemoteHost, DatabaseName);
  if AnsiSameText(ConnectionType, 'BDE') then
  begin
    BDEDatabase.AliasName := DatabaseName;
  end else begin
    BDEDatabase.DriverName := ConnectionType;
    BDEDatabase.Params.Values[RemoteHost] := DatabaseName;
  end;
end;

{ TBDERangeCursor }

constructor TBDERangeCursor.Create(Database: TDatabaseExt; Relation: TRelation; KeyValues: Variant);
begin
  if (Relation.ForeignTable = '') or (Relation.ForeignKeyFields = '') then
    DatabaseError(SInvalidRangeCursor);
  CreateExt(Database, Relation.ForeignTable, Relation.ForeignKeyFields, Relation.ForeignCondition, Relation.CaseInsensitive, KeyValues);
end;

constructor TBDERangeCursor.CreateExt(Database: TDatabaseExt;
  const TableName, KeyFields, TableFilter: AnsiString;
  CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: AnsiString = '');
var
  IdxDef: TIndexDef;
  RangeFilter: AnsiString;
  Table: TTable;
begin
  if (TableName = '') then
    DatabaseError(SInvalidRangeCursor);

  inherited Create;
  KeyFieldsList := TList.Create;

  Table := Database.CreateTable(TableName) as TTable;
  DataSet := Table; // Assign inherited field

  // Do not set any range if no KeyFields specified
  if KeyFields = '' then exit;

  Table.GetFieldList(KeyFieldsList, KeyFields);
  if ExtraKeyFields = '' then
    IdxDef := Table.IndexDefs.GetIndexForFields(KeyFields, CaseInsensitive)
  else IdxDef := Table.IndexDefs.GetIndexForFields(KeyFields + ';' + ExtraKeyFields, CaseInsensitive);

  with Table do
    if IdxDef = nil then
    begin
      // Create filter for fields
      RangeFilter := CreateRangeFilter(KeyFieldsList, KeyValues);
      if TableFilter <> '' then
        RangeFilter := '(' + TableFilter + ') and (' + RangeFilter + ')';
      Filter := RangeFilter;
      if CaseInsensitive then
        FilterOptions := [foCaseInsensitive]
      else FilterOptions := [];
      Filtered := True;
      CancelRange;
    end else begin
      // Setup range
      IndexName := IdxDef.Name;
      SetRangeStart;
      KeyFieldCount := KeyFieldsList.Count;
      AssignKeyFields(KeyFieldsList, KeyValues);
      SetRangeEnd;
      KeyFieldCount := KeyFieldsList.Count;
      AssignKeyFields(KeyFieldsList, KeyValues);
      ApplyRange;
      if TableFilter <> Filter then
        Filter := TableFilter;
      Filtered := Filter <> '';
    end;
end;

destructor TBDERangeCursor.Destroy;
begin
  KeyFieldsList.Free;
  inherited Destroy;
end;

{ TDatabaseExt }

constructor TDatabaseExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchema := nil;
  FSystemTableName := defSysTableName;
  FVersionStr := '';
end;

destructor TDatabaseExt.Destroy;
begin
  UnRegisterCtxDataProvider(Self);
  SystemTableName := '';
  inherited Destroy;
end;

procedure TDatabaseExt.CheckActive;
begin
  if (Handle = nil) then
    DatabaseError(SDatabaseClosed);
end;

procedure TDatabaseExt.CheckSchema;
begin
  if Schema = nil then
    DatabaseError(sDatabaseSchemaIsNotAssigned);
end;

function TDatabaseExt.GetVersion: TSchemaVersion;
begin
  CheckActive;
  Result := GetDatabaseVersion(Self);
end;

procedure TDatabaseExt.SetVersion(const Value: TSchemaVersion);
begin
  CheckActive;
  SetDatabaseVersion(Self, Value);
end;

function TDatabaseExt.GetVersionLabel: AnsiString;
begin
  if Connected then
  begin
    if FVersionStr = '' then
      FVersionStr := VersionToStr(GetVersion);
    Result := FVersionStr;
  end else
    Result := VersionToStr(Undefined);
end;

function TDatabaseExt.IsVersionCurrent(AllowNewer: Boolean): Boolean;
begin
  CheckSchema;
  CheckActive;
  Result := VersionIsOkey(Schema.Version, GetVersion, AllowNewer);
end;

procedure TDatabaseExt.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSchema) then
    Schema := nil;
  inherited;
end;

procedure TDatabaseExt.SetSchema(Value: TDatabaseSchema);
begin
  FSchema := Value;
end;

procedure TDatabaseExt.SetVersionLabel(const Value: AnsiString);
begin
  // Ignore this.
end;

procedure TDatabaseExt.StartTransaction;
begin
  inherited StartTransaction;
end;

procedure TDatabaseExt.Commit;
begin
  inherited Commit;
end;

procedure TDatabaseExt.Rollback;
begin
  inherited Rollback;
end;

function TDatabaseExt.GetInTransaction: Boolean;
begin
  Result := InTransaction;
end;

function TDatabaseExt.GetDriverName: AnsiString;
begin
  Result := 'BDE';
end;

function TDatabaseExt.GetDatabaseURL: AnsiString;
begin
  Result := BDEExt.GetDatabaseURL(Self);
end;

procedure TDatabaseExt.SetDatabaseURL(const Value: AnsiString);
begin
  BDEExt.SetDatabaseURL(Self, Value);
end;

function TDatabaseExt.GetRangeCursor(const TableName, KeyFields,
  TableFilter: AnsiString; CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: AnsiString): TDBRangeCursor;
begin
  Result := TBDERangeCursor.CreateExt(Self, TableName, KeyFields,
    TableFilter, CaseInsensitive, KeyValues, ExtraKeyFields);
end;

function TDatabaseExt.GetRangeCursor(Relation: TRelation;
  KeyValues: Variant): TDBRangeCursor;
begin
  Result := TBDERangeCursor.Create(Self, Relation, KeyValues);
end;

function TDatabaseExt.GetSchema: TDatabaseSchema;
begin
  Result := FSchema;
end;

function TDatabaseExt.GetDatabaseName: AnsiString;
begin
  Result := inherited DatabaseName;
end;

procedure TDatabaseExt.SetDatabaseName(const Value: AnsiString);
begin
  inherited DatabaseName := Value;
  RegisterCtxDataProvider(Self)
end;

function TDatabaseExt.CreateTable(const TableName: AnsiString): TDataSet;
begin
  Result := TTable.Create(nil);
  try
    TTable(Result).DatabaseName := DatabaseName;
    TTable(Result).SessionName := SessionName;
    TTable(Result).TableName := TableName;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TDatabaseExt.CreateQuery(const Statement: AnsiString): TDataSet;
begin
  Result := TQuery.Create(nil);
  with TQuery(Result) do
  try
    DatabaseName := Self.DatabaseName;
    SessionName := Self.SessionName;
    // Assign SQL statement
    SQL.Text := Statement;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TDatabaseExt.ExecSQL(Query: TDataSet);
begin
  TQuery(Query).ExecSQL;
end;

procedure TDatabaseExt.GetQueryParams(Query: TDataSet; Params: TParams);
begin
  Params.Assign(TQuery(Query).Params);
end;

procedure TDatabaseExt.SetQueryParams(Query: TDataSet; Params: TParams);
begin
  TQuery(Query).Params.AssignValues(Params);
end;

function TDatabaseExt.GetQuerySQL(Query: TDataSet): AnsiString;
begin
  Result := TQuery(Query).SQL.Text;
end;

procedure TDatabaseExt.SetQuerySQL(Query: TDataSet;
  const Statement: AnsiString);
begin
  TQuery(Query).SQL.Text := Statement;
end;

function TDatabaseExt.FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;
begin
  Result := (Table as TTable).FindKey(KeyValues);
end;

procedure TDatabaseExt.GetTableNames(List: TStrings; SystemTables: Boolean = False);
begin
  Session.GetTableNames(Self.DatabaseName, '', True, SystemTables, List);
end;

function TDatabaseExt.GetIndexDefs(DataSet: TDataSet): TIndexDefs;
begin
  Result := (DataSet as TTable).IndexDefs;
end;

{ DDL functions: ReverseEngineering}

procedure TDatabaseExt.ReverseEngineer;
var
  I, J, Idx, DefIdx: Integer;
  Table: TTable;
  Tables: TStringList;
  FileExt, LogicalTableName: AnsiString;
begin
  CheckActive;
  CheckSchema;
  // Update schema from the physical tables
  Table := CreateTable('') as TTable;
  Tables := TStringList.Create;
  try
    Session.GetTableNames(Self.DatabaseName, '', True, False, Tables);
    // Processing tables
    // Pass #1. Creating new and updating existing definitions
    for I := 0 to Tables.Count - 1 do
    begin
      FileExt := ExtractFileExt(Tables[I]);
      if AnsiSameText(FileExt, '.db') or AnsiSameText(FileExt, '.dbf') then
        LogicalTableName := ChangeFileExt(Tables[I], '')
      else LogicalTableName := Tables[I];

      if AnsiSameText(LogicalTableName, FSystemTableName) then continue;

      Table.TableName := Tables[I];

      try
        Table.FieldDefs.Update;
        Table.IndexDefs.Update;
      except
        // Ignore possible errors here...
      end;

      Idx := Schema.TableDefs.IndexOf(LogicalTableName);
      if Idx < 0 then begin
        // Create table defs
        with Schema.TableDefs.Add do
        begin
          TableName := LogicalTableName;
          FieldDefs.Assign(Table.FieldDefs);
          IndexDefs.Assign(Table.IndexDefs);
        end;
      end else begin
        with Schema.TableDefs[Idx] do
        begin
          // Processing Field Definitions
          // Pass #1: Removing fields defs, that has been deleted from table
          // and updating the ones that remain
          J := 0;
          while J < FieldDefs.Count do
          begin
            DefIdx := Table.FieldDefs.IndexOf(FieldDefs[J].Name);
            if DefIdx < 0 then
              FieldDefs[J].Free
            else begin
              FieldDefs[J].Assign(Table.FieldDefs[DefIdx]);
              Inc(J);
            end;
          end;
          // Pass #2: Adding fields defs, that has been added to a table
          for J := 0 to Table.FieldDefs.Count - 1 do
            if FieldDefs.IndexOf(Table.FieldDefs[J].Name) < 0 then
              FieldDefs.Add.Assign(Table.FieldDefs[J]);

          // Processing Index Definitions
          // Pass #1: Removing index defs, that has been deleted from table
          // and updating the ones that remain
          J := 0;
          while J < IndexDefs.Count do
          begin
            DefIdx := Table.IndexDefs.IndexOf(IndexDefs[J].Name);
            if DefIdx < 0 then
              IndexDefs[J].Free
            else begin
              IndexDefs[J].Assign(Table.IndexDefs[DefIdx]);
              Inc(J);
            end;
          end;
          // Pass #2: Adding fields defs, that has been added to a table
          for J := 0 to Table.IndexDefs.Count - 1 do
            if IndexDefs.IndexOf(Table.IndexDefs[J].Name) < 0 then
              IndexDefs.Add.Assign(Table.IndexDefs[J]);
        end;
      end;
    end;
    // Remove table file extensions
    for I := 0 to Tables.Count - 1 do
    begin
      FileExt := ExtractFileExt(Tables[I]);
      if AnsiSameText(FileExt, '.db') or AnsiSameText(FileExt, '.dbf') then
        Tables[I] := ChangeFileExt(Tables[I], '');
    end;
    // Pass #2. Removing definitions for dropped tables
    with Schema do
    begin
      // Process definitions
      I := 0;
      while I < TableDefs.Count do
      begin
        if Tables.IndexOf(TableDefs[I].TableName) < 0 then
          TableDefs[I].Free
        else Inc(I);
      end;
    end;
  finally
    Tables.Free;
    Table.Free;
  end;
end;

function TDatabaseExt.GetSystemTableName: AnsiString;
begin
  if Trim(FSystemTableName) = '' then
    Result := defSysTableName else
    Result := FSystemTableName;
end;

procedure TDatabaseExt.DoConnect;
begin
  inherited;
  FVersionStr := '';
end;

function TDatabaseExt.ExecuteStatement(SQL: AnsiString;
  ResultSet: Pointer): Integer;
var
  Q: TQuery;
  AParams: TParams;
begin
  Q := TQuery(CreateQuery(SQL));
  AParams := TParams.Create;
  try
    Result := IProviderSupport(Q).PSExecuteStatement(SQL, AParams, ResultSet);
  finally
    AParams.Free;
    Q.Free;
  end;
end;

function TDatabaseExt.CreateCommand: TCtxDataCommand;
begin
  Result := TCtxDataSetCommand.Create(Self);
end;

end.




