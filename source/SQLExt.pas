(******************************************************************************)
(*
(*  Context Database Extensions Suite (DBX)
(*
(*  Contains: TSQLConnectionExt components.
(*
(*  Copyright (c) 2005-2007 Michael Baytalsky
(*
(*  ------------------------------------------------------------
(*  FILE        : SQLExt.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 2.15
(*  DELPHI\BCB  : Delphi 5,6,7,2005,2006, 2007; C++Builder 6.0, 2006
(*
(******************************************************************************)
unit SQLExt;

interface

uses
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, CtxDataTypes, CtxDataSetCommand, CtxDBIntf, dbSchema, DBCommon, DBXpress, SQLExpr;

type
  TSQLConnectionExt = class;

  {:$ Represents a DBX database connection. }
  TSQLConnectionExt = class(TSQLConnection, ISchemaDatabase, ICtxDatabase, ICtxDataProvider)
  protected
    { Protected declarations }
    FSchema: TDatabaseSchema;
    FSystemTableName: AnsiString;
    FDatabaseName: AnsiString;
    FTransactionDesc: TTransactionDesc;
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
    {:: Creates an instance of a TSQLConnectionExt component.}
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of a TSQLConnectionExt component.}
    destructor Destroy; override;

    {:: Creates TSQLTable component, corresponding to TableName. }
    function CreateTable(const TableName: AnsiString): TDataSet;

    {:: Creates TSQLQuery component, corresponding to Statement. }
    function CreateQuery(const Statement: AnsiString): TDataSet;

    { Schema related methods. Schema must be assigned for them to work }
    {:$ Updates database schema from the physical database tables. }
    {:: Schema property must be assigned and refer to the Schema component }
    {:: that will be updated. }
    procedure ReverseEngineer;

    {:$ Checks whether the database version is *equal* or Newer }
    {:$ then the current Schema's Version. }
    function IsVersionCurrent(AllowNewer: Boolean = False): Boolean;

    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function GetInTransaction: Boolean;

    function GetVersion: TSchemaVersion;
    procedure SetVersion(const Value: TSchemaVersion);

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
    function ExecuteStatement(SQL: AnsiString; ResultSet: Pointer): Integer;

    { Parent object is always a table or schema. }
    function GetIndexDefs(DataSet: TDataSet): TIndexDefs;
    function GetSystemTableName: AnsiString;

    function CreateCommand: TCtxDataCommand;

    //property DriverRegistryFile;
    {:$ Transaction Descriptor Record }
    property TransactionDesc: TTransactionDesc read FTransactionDesc write FTransactionDesc;
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
    {:$ Specifies the uniform path to the database. }
    property DatabaseURL: AnsiString read GetDatabaseURL write SetDatabaseURL stored false;
    property DatabaseName: AnsiString read GetDatabaseName write SetDatabaseName;
  end;

  {:$ Retrieves database URL from the TDatabase component and the connected Session component. }
  function GetDatabaseURL(SQLConnection: TSQLConnection): AnsiString;
  {:$ Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(SQLConnection: TSQLConnection; DatabaseURL: AnsiString);

  procedure Register;

resourcestring
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SDatabaseSchemaIsNotAssigned = 'Database Schema is not assigned';

implementation

uses
  SqlConst, IniFiles;

const
  defSysTableName = 'System';

procedure Register;
begin
  RegisterComponents('Database Extensions', [TSQLConnectionExt]);
end;

{ General Helper Rountines }

function GetProfileString(Section, Setting, IniFileName: AnsiString): AnsiString;
var
  IniFile: TMemIniFile;
  List: TStrings;
begin
  List := TStringList.Create;
  try
    IniFile := TMemIniFile.Create(IniFileName);
    IniFile.ReadSectionValues(Section, List);
    try
      Result := List.Values[ Setting ];
    finally
      IniFile.Free;
    end;
  finally
    List.Free;
  end;
end;

function GetDatabaseURL(SQLConnection: TSQLConnection): AnsiString;
var
  ConnectionType, RemoteHost, DatabaseName: AnsiString;
begin
  ConnectionType := SQLConnection.DriverName;
  RemoteHost := SQLConnection.Params.Values['HostName'];
  if RemoteHost = '' then
    RemoteHost := 'LocalHost';
  DatabaseName := SQLConnection.Params.Values['Database'];
  Result := EncodeDatabaseURL(ConnectionType, RemoteHost, DatabaseName);
end;

procedure SetDatabaseURL(SQLConnection: TSQLConnection; DatabaseURL: AnsiString);
var
  ConnectionType, RemoteHost, DatabaseName: AnsiString;
begin
  DecodeDatabaseURL(DatabaseURL, ConnectionType, RemoteHost, DatabaseName);
  // if RemoteHost = 'LocalHost' then RemoteHost := '';
  SQLConnection.DriverName := ConnectionType;
  with SQLConnection as TSQLConnectionExt do
  begin
    VendorLib := GetProfileString(DriverName, VENDORLIB_KEY, GetDriverRegistryFile);
    LibraryName := GetProfileString(DriverName, DLLLIB_KEY, GetDriverRegistryFile);
    GetDriverFunc := GetProfileString(DriverName, GETDRIVERFUNC_KEY, GetDriverRegistryFile);
  end;
  SQLConnection.Params.Values['HostName'] := RemoteHost;
  SQLConnection.Params.Values['Database'] := DatabaseName;
end;

{ TSQLConnectionExt }

constructor TSQLConnectionExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchema := nil;
  FSystemTableName := defSysTableName;
  FDatabaseName := '';
  FTransactionDesc.TransactionID := 1;
  FTransactionDesc.IsolationLevel := xilREADCOMMITTED;
  FVersionStr := '';
end;

destructor TSQLConnectionExt.Destroy;
begin
  UnRegisterCtxDataProvider(Self);
  SystemTableName := '';
  inherited Destroy;
end;

procedure TSQLConnectionExt.CheckActive;
begin
  if not Connected then
    DatabaseError(SDatabaseClosed);
end;

procedure TSQLConnectionExt.CheckSchema;
begin
  if Schema = nil then
    DatabaseError(sDatabaseSchemaIsNotAssigned);
end;

function TSQLConnectionExt.GetVersion: TSchemaVersion;
begin
  CheckActive;
  Result := GetDatabaseVersion(Self);
end;

procedure TSQLConnectionExt.SetVersion(const Value: TSchemaVersion);
begin
  CheckActive;
  SetDatabaseVersion(Self, Value);
end;

function TSQLConnectionExt.GetVersionLabel: AnsiString;
begin
  if Connected then
  begin
    if FVersionStr = '' then
      FVersionStr := VersionToStr(GetVersion);
    Result := FVersionStr;
  end else
    Result := VersionToStr(Undefined);
end;

procedure TSQLConnectionExt.SetVersionLabel(const Value: AnsiString);
begin
  // Ignore this.
end;

function TSQLConnectionExt.IsVersionCurrent(AllowNewer: Boolean): Boolean;
begin
  CheckSchema;
  CheckActive;
  Result := VersionIsOkey(Schema.Version, Self.GetVersion, AllowNewer);
end;

procedure TSQLConnectionExt.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSchema) then
    Schema := nil;
  inherited;
end;

procedure TSQLConnectionExt.SetSchema(Value: TDatabaseSchema);
begin
  FSchema := Value;
end;

procedure TSQLConnectionExt.StartTransaction;
begin
  inherited StartTransaction(FTransactionDesc);
end;

procedure TSQLConnectionExt.Commit;
begin
  inherited Commit(FTransactionDesc);
end;

procedure TSQLConnectionExt.Rollback;
begin
  inherited Rollback(FTransactionDesc);
end;

function TSQLConnectionExt.GetInTransaction: Boolean;
begin
  Result := InTransaction;
end;

function TSQLConnectionExt.GetDriverName: AnsiString;
begin
  Result := 'DBX';
end;

function TSQLConnectionExt.GetDatabaseURL: AnsiString;
begin
  Result := SQLExt.GetDatabaseURL(Self);
end;

procedure TSQLConnectionExt.SetDatabaseURL(const Value: AnsiString);
begin
  SQLExt.SetDatabaseURL(Self, Value);
end;

function TSQLConnectionExt.GetRangeCursor(const TableName, KeyFields,
  TableFilter: AnsiString; CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: AnsiString): TDBRangeCursor;
begin
  DatabaseError(SCapabilityNotSupported); Result := nil;
end;

function TSQLConnectionExt.GetRangeCursor(Relation: TRelation;
  KeyValues: Variant): TDBRangeCursor;
begin
  DatabaseError(SCapabilityNotSupported); Result := nil;
end;

function TSQLConnectionExt.GetSchema: TDatabaseSchema;
begin
  Result := FSchema;
end;

function TSQLConnectionExt.GetDatabaseName: AnsiString;
begin
  Result := FDatabaseName;
end;

procedure TSQLConnectionExt.SetDatabaseName(const Value: AnsiString);
begin
  // ConnectionName := Value;
  FDatabaseName := Value;
  RegisterCtxDataProvider(Self);
end;

function TSQLConnectionExt.CreateTable(const TableName: AnsiString): TDataSet;
begin
  Result := TSQLTable.Create(nil);
  TSQLTable(Result).SQLConnection := Self;
  TSQLTable(Result).TableName := TableName;
end;

function TSQLConnectionExt.CreateQuery(const Statement: AnsiString): TDataSet;
begin
  Result := TSQLQuery.Create(nil);
  with TSQLQuery(Result) do
  try
    SQLConnection := Self;
    SQL.Text := Statement;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TSQLConnectionExt.ExecSQL(Query: TDataSet);
begin
  TSQLQuery(Query).ExecSQL;
end;

function TSQLConnectionExt.ExecuteStatement(SQL: AnsiString;
  ResultSet: Pointer): Integer;
var
  Q: TSQLQuery;
  AParams: TParams;
begin
  Q := TSQLQuery(CreateQuery(SQL));
  AParams := TParams.Create;
  try
    Result := IProviderSupport(Q).PSExecuteStatement(SQL, AParams, ResultSet);
  finally
    AParams.Free;
    Q.Free;
  end;
end;

procedure TSQLConnectionExt.GetQueryParams(Query: TDataSet; Params: TParams);
begin
  Params.Assign(TSQLQuery(Query).Params);
end;

procedure TSQLConnectionExt.SetQueryParams(Query: TDataSet; Params: TParams);
begin
  TSQLQuery(Query).Params.AssignValues(Params);
end;

function TSQLConnectionExt.GetQuerySQL(Query: TDataSet): AnsiString;
begin
  Result := TSQLQuery(Query).SQL.Text;
end;

procedure TSQLConnectionExt.SetQuerySQL(Query: TDataSet;
  const Statement: AnsiString);
begin
  TSQLQuery(Query).SQL.Text := Statement;
end;

function TSQLConnectionExt.GetIndexDefs(DataSet: TDataSet): TIndexDefs;
begin
  Result := (DataSet as TSQLTable).IndexDefs;
end;

{ DDL functions: ReverseEngineering }

procedure TSQLConnectionExt.ReverseEngineer;
var
  I, J, Idx, DefIdx: Integer;
  Table: TSQLTable;
  Tables: TStringList;
  LogicalTableName: AnsiString;
begin
  CheckActive;
  CheckSchema;
  // Update schema from the physical tables
  Table := CreateTable('') as TSQLTable;
  Tables := TStringList.Create;
  try
    GetTableNames(Tables);
    // Processing tables
    // Pass #1. Creating new and updating existing definitions
    for I := 0 to Tables.Count - 1 do
    begin
      LogicalTableName := ChangeFileExt(Tables[I], '');

      if AnsiSameText(LogicalTableName, FSystemTableName) then continue;

      Table.TableName := Tables[I];
      Table.FieldDefs.Update;
      Table.IndexDefs.Update;
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
    // Pass #2. Removing definitions for dropped tables
    with Schema do
    begin
      // Remove table file extensions
      for I := 0 to Tables.Count - 1 do
        Tables[I] := ChangeFileExt(Tables[I], '');
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

function TSQLConnectionExt.FindKey(Table: TDataSet;
  const KeyValues: array of const): Boolean;
begin
  Result := False;
  DatabaseError(SCapabilityNotSupported);
end;

function TSQLConnectionExt.GetSystemTableName: AnsiString;
begin
  if Trim(FSystemTableName) = '' then
    Result := defSysTableName else
    Result := FSystemTableName;
end;

procedure TSQLConnectionExt.DoConnect;
begin
  inherited;
  FVersionStr := '';
end;

function TSQLConnectionExt.CreateCommand: TCtxDataCommand;
begin
  Result := TCtxDataSetCommand.Create(Self);
end;

end.




