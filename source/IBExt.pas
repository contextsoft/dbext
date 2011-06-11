(******************************************************************************)
(*
(*  Context Database Extensions Suite (Interbase)
(*
(*  Contains: TIBDatabaseExt components.
(*
(*  Copyright (c) 2005-2011, Context Software LLC
(*
(*  ------------------------------------------------------------
(*  FILE        : IBExt.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.25
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009, 2010, XE
(*
(******************************************************************************)
unit IBExt;

interface

uses
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, IBTable, IBQuery, IBDatabase, IBSQL,
  CtxDataTypes, CtxDataSetCommand, CtxDBIntf, dbSchema;

type
  TIBDatabaseExt = class;

  {:$ TIBRangeCursor object is an internal helper object, that facilitates }
  {:$ the process of iterating a table for a given range of key values. }
  TIBRangeCursor = class (TDBRangeCursor)
  public
    constructor Create(Database: TIBDatabaseExt; Relation: TRelation; KeyValues: Variant);
    constructor CreateExt(Database: TIBDatabaseExt; const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = '');
    destructor Destroy; override;
  end;

  {:$ Represents a IB database connection. }
  TIBDatabaseExt = class(TIBDatabase, ISchemaDatabase, ICtxDataProvider, ICtxDatabase)
  protected
    { Protected declarations }
    FSchema: TDatabaseSchema;
    FSystemTableName: String;
    FVersionStr: String;

    function GetDatabaseName: String;
    procedure SetDatabaseName(const Value: String);
    function GetVersionLabel: String;
    procedure SetVersionLabel(const Value: String);

    { Replication support }
    function GetSchema: TDatabaseSchema;
    procedure SetSchema(Value: TDatabaseSchema);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetDatabaseURL: String;
    procedure SetDatabaseURL(const Value: String);

    function GetDriverName: String;

    { Checks if the database is connected }
    procedure CheckActive;
    { Checks if schema is assigned }
    procedure CheckSchema;
    procedure DoConnect; override;
  public
    {:: Creates an instance of a TIBDatabaseExt component.}
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of a TIBDatabaseExt component.}
    destructor Destroy; override;

    {:: Creates TTableExt component, corresponding to TableName. }
    function CreateTable(const TableName: String): TDataSet; virtual;

    {:: Creates TQueryExt component, corresponding to Statement. }
    function CreateQuery(const Statement: String): TDataSet;

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

    function GetRangeCursor(const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = ''): TDBRangeCursor; overload;
    function GetRangeCursor(Relation: TRelation; KeyValues: Variant): TDBRangeCursor; overload;

    { Returns assignable SQL property. }
    function GetQuerySQL(Query: TDataSet): String;
    procedure SetQuerySQL(Query: TDataSet; const Statement: String);
    { Returns assignable Params property. }
    procedure GetQueryParams(Query: TDataSet; Params: TParams);
    procedure SetQueryParams(Query: TDataSet; Params: TParams);
    { Executes query that does not return result set. }
    procedure ExecSQL(Query: TDataSet);
    function ExecuteStatement(SQL: String; ResultSet: Pointer): Integer;

    function CreateCommand: TCtxDataCommand;

    { Parent object is always a table or schema. }
    function GetIndexDefs(DataSet: TDataSet): TIndexDefs;
    function GetSystemTableName: String;
  published
    { Published properties }
    {:$ Reference to a TDatabaseSchema component. }
    {:: Schema may contain information about the database structure as weel as some }
    {:: additional information like referential integrity constraints, triggers and more. }
    property Schema: TDatabaseSchema read FSchema write SetSchema;
    {:$ Text presentation of the database version. }
    {:: This property is effectively read-only. Any text assigned to it will be ignored. }
    property VersionLabel: String read GetVersionLabel write SetVersionLabel stored False;
    {:$ The name of the System table. }
    {:: The default value of this property is 'system'. }
    property SystemTableName: String read GetSystemTableName write FSystemTableName;
    {:$ Specifies the uniform path to the database. }
    property DatabaseURL: String read GetDatabaseURL write SetDatabaseURL stored false;
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
  end;

  {:$ Retrieves database URL from the TDatabase component and the connected Session component. }
  function GetDatabaseURL(IBDatabase: TIBDatabase): String;
  {:$ Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(IBDatabase: TIBDatabase; DatabaseURL: String);

resourcestring
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SDatabaseSchemaIsNotAssigned = 'Database Schema is not assigned';
  SInvalidRangeCursor = 'Invalid parameters supplied to range cursor';

  procedure Register;

implementation

uses IBCustomDataSet, Math;

procedure Register;
begin
  RegisterComponents('Database Extensions', [TIBDatabaseExt]);
end;

{ General Helper Rountines }

function GetDatabaseURL(IBDatabase: TIBDatabase): String;
begin
  Result := IBDatabase.DatabaseName;
end;

procedure SetDatabaseURL(IBDatabase: TIBDatabase; DatabaseURL: String);
begin
  IBDatabase.DatabaseName := DatabaseURL;
end;

{ TIBRangeCursor }

constructor TIBRangeCursor.Create(Database: TIBDatabaseExt; Relation: TRelation; KeyValues: Variant);
begin
  if (Relation.ForeignTable = '') or (Relation.ForeignKeyFields = '') then
    DatabaseError(SInvalidRangeCursor);
  CreateExt(Database, Relation.ForeignTable, Relation.ForeignKeyFields, Relation.ForeignCondition, Relation.CaseInsensitive, KeyValues);
end;

constructor TIBRangeCursor.CreateExt(Database: TIBDatabaseExt;
  const TableName, KeyFields, TableFilter: String;
  CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String = '');
var
  RangeFilter: String;
  Table: TIBTable;
begin
  if (TableName = '') then
    DatabaseError(SInvalidRangeCursor);

  inherited Create;
  KeyFieldsList := TList.Create;

  Table := Database.CreateTable(TableName) as TIBTable;
  DataSet := Table; // Assign inherited field

  // Do not set any range if no KeyFields specified
  if KeyFields = '' then exit;

  Table.GetFieldList(KeyFieldsList, KeyFields);

  with Table do
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
    end;
end;

destructor TIBRangeCursor.Destroy;
begin
  KeyFieldsList.Free;
  inherited Destroy;
end;

{ TIBDatabaseExt }

constructor TIBDatabaseExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchema := nil;
  FSystemTableName := defSysTableName;
  FVersionStr := '';
end;

destructor TIBDatabaseExt.Destroy;
begin
  UnRegisterCtxDataProvider(Self);
  SystemTableName := '';
  inherited Destroy;
end;

procedure TIBDatabaseExt.CheckActive;
begin
  if (Handle = nil) then
    DatabaseError(SDatabaseClosed);
end;

procedure TIBDatabaseExt.CheckSchema;
begin
  if Schema = nil then
    DatabaseError(sDatabaseSchemaIsNotAssigned);
end;

function TIBDatabaseExt.GetVersion: TSchemaVersion;
begin
  CheckActive;
  Result := GetDatabaseVersion(Self);
end;

procedure TIBDatabaseExt.SetVersion(const Value: TSchemaVersion);
begin
  CheckActive;
  SetDatabaseVersion(Self, Value);
end;

function TIBDatabaseExt.GetVersionLabel: String;
begin
  if Connected then
  begin
    if FVersionStr = '' then
      FVersionStr := VersionToStr(GetVersion);
    Result := FVersionStr;
  end else
    Result := VersionToStr(Undefined);
end;

function TIBDatabaseExt.IsVersionCurrent(AllowNewer: Boolean): Boolean;
begin
  CheckSchema;
  CheckActive;
  Result := VersionIsOkey(Schema.Version, GetVersion, AllowNewer);
end;

procedure TIBDatabaseExt.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSchema) then
    Schema := nil;
  inherited;
end;

procedure TIBDatabaseExt.SetSchema(Value: TDatabaseSchema);
begin
  FSchema := Value;
end;

procedure TIBDatabaseExt.SetVersionLabel(const Value: String);
begin
  // Ignore this.
end;

procedure TIBDatabaseExt.StartTransaction;
begin
  DefaultTransaction.StartTransaction;
end;

procedure TIBDatabaseExt.Commit;
begin
  DefaultTransaction.Commit;
end;

procedure TIBDatabaseExt.Rollback;
begin
  DefaultTransaction.Rollback;
end;

function TIBDatabaseExt.GetInTransaction: Boolean;
begin
  Result := DefaultTransaction.InTransaction;
end;

function TIBDatabaseExt.GetDriverName: String;
begin
  Result := 'IB\FB';
end;

function TIBDatabaseExt.GetDatabaseURL: String;
begin
  Result := IBExt.GetDatabaseURL(Self);
end;

procedure TIBDatabaseExt.SetDatabaseURL(const Value: String);
begin
  IBExt.SetDatabaseURL(Self, Value);
end;

function TIBDatabaseExt.GetRangeCursor(const TableName, KeyFields,
  TableFilter: String; CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String): TDBRangeCursor;
begin
  Result := TIBRangeCursor.CreateExt(Self, TableName, KeyFields,
    TableFilter, CaseInsensitive, KeyValues, ExtraKeyFields);
end;

function TIBDatabaseExt.GetRangeCursor(Relation: TRelation;
  KeyValues: Variant): TDBRangeCursor;
begin
  Result := TIBRangeCursor.Create(Self, Relation, KeyValues);
end;

function TIBDatabaseExt.GetSchema: TDatabaseSchema;
begin
  Result := FSchema;
end;

function TIBDatabaseExt.GetDatabaseName: String;
begin
  Result := inherited DatabaseName;
end;

procedure TIBDatabaseExt.SetDatabaseName(const Value: String);
begin
  inherited DatabaseName := Value;
  RegisterCtxDataProvider(Self);
end;

function TIBDatabaseExt.CreateTable(const TableName: String): TDataSet;
begin
  Result := TIBTable.Create(nil);
  try
    TIBTable(Result).Database := Self;
    TIBTable(Result).Transaction := DefaultTransaction;
    if TIBTable(Result).Transaction = nil then
      TIBTable(Result).Transaction := InternalTransaction;
    TIBTable(Result).TableName := TableName;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TIBDatabaseExt.CreateQuery(const Statement: String): TDataSet;
begin
  Result := TIBQuery.Create(nil);
  with TIBQuery(Result) do
  try
    Database := Self;
    Transaction := DefaultTransaction;
    if Transaction = nil then
      Transaction := InternalTransaction;
    // Assign SQL statement
    SQL.Text := Statement;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TIBDatabaseExt.ExecSQL(Query: TDataSet);
begin
  TIBQuery(Query).ExecSQL;
  if TIBQuery(Query).Transaction.InTransaction then
    TIBQuery(Query).Transaction.CommitRetaining;
end;

function TIBDatabaseExt.ExecuteStatement(SQL: String;
  ResultSet: Pointer): Integer;
var
  Q: TIBQuery;
begin
  if ResultSet <> nil then
    TDataSet(ResultSet^) := nil;
  Q := TIBQuery(CreateQuery(''));
  try
    Q.ParamCheck := False;
    SetQuerySQL(Q, SQL);
    Q.Prepare;
    if Q.StatementType = IBSQL.SQLSelect then
    begin
      Q.Active := True;
      if ResultSet <> nil then
        TDataSet(ResultSet^) := Q;
    end else
      ExecSQL(Q);
    Result := Q.RowsAffected;
  finally
    if (ResultSet = nil) or (TDataSet(ResultSet^) = nil) then
      Q.Free;
  end;
end;

procedure TIBDatabaseExt.GetQueryParams(Query: TDataSet; Params: TParams);
begin
  Params.Assign(TIBQuery(Query).Params);
end;

procedure TIBDatabaseExt.SetQueryParams(Query: TDataSet; Params: TParams);
begin
  TIBQuery(Query).Params.AssignValues(Params);
end;

function TIBDatabaseExt.GetQuerySQL(Query: TDataSet): String;
begin
  Result := TIBQuery(Query).SQL.Text;
end;

procedure TIBDatabaseExt.SetQuerySQL(Query: TDataSet;
  const Statement: String);
begin
  TIBQuery(Query).SQL.Text := Statement;
end;

function TIBDatabaseExt.FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;
begin
  Result := False;
  DatabaseError(SCapabilityNotSupported);
end;

function TIBDatabaseExt.GetIndexDefs(DataSet: TDataSet): TIndexDefs;
begin
  Result := (DataSet as TIBTable).IndexDefs;
end;

{ DDL functions: ReverseEngineering}

procedure TIBDatabaseExt.ReverseEngineer;
var
  I, J, Idx, DefIdx: Integer;
  Table: TIBTable;
  Tables: TStringList;
  LogicalTableName: String;
  STable: string;
begin
  CheckActive;
  CheckSchema;

  if Self.GetSchema <> nil then
    STable := Self.GetSchema.SystemTableName else
    STable := FSystemTableName;

  if Trim(STable) = '' then
    STable := defSysTableName;

  // Update schema from the physical tables
  Table := CreateTable('') as TIBTable;
  Tables := TStringList.Create;
  try
    GetTableNames(Tables, False);
    // Processing tables
    // Pass #1. Creating new and updating existing definitions
    for I := 0 to Tables.Count - 1 do
    begin
      LogicalTableName := ChangeFileExt(Tables[I], '');

      if AnsiSameText(LogicalTableName, STable) then continue;

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

function TIBDatabaseExt.GetSystemTableName: String;
begin
  if Trim(FSystemTableName) = '' then
    Result := defSysTableName else
    Result := FSystemTableName;
end;

procedure TIBDatabaseExt.DoConnect;
begin
  inherited;
  FVersionStr := '';
end;

function TIBDatabaseExt.CreateCommand: TCtxDataCommand;
begin
  Result := TCtxDataSetCommand.Create(Self);
end;

end.




