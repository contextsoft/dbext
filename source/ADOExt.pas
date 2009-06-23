(******************************************************************************)
(*
(*  Context Database Extensions Suite (ADO)
(*
(*  Contains: TADOConnectionExt component.
(*
(*  Copyright (c) 2005-2007 Michael Baytalsky
(*
(*  ------------------------------------------------------------
(*  FILE        : ADOExt.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 2.15
(*  DELPHI\BCB  : Delphi 5,6,7,2005,2006, 2007; C++Builder 6.0, 2006, 
(*
(******************************************************************************)
unit ADOExt;

interface

uses
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, ADODB, ADOInt, OleDB, CtxDataTypes, CtxDataSetCommand, CtxDBIntf, dbSchema;

type
  TADOConnectionExt = class;

  {:$ TADORangeCursor object is an internal helper object, that facilitates }
  {:$ the process of iterating a table for a given range of key values. }
  TADORangeCursor = class (TDBRangeCursor)
  public
    constructor Create(Database: TADOConnectionExt; Relation: TRelation; KeyValues: Variant);
    constructor CreateExt(Database: TADOConnectionExt; const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = '');
    destructor Destroy; override;
  end;

  {:$ Represents a ADO database connection. }
  TADOConnectionExt = class(TADOConnection, ISchemaDatabase, ICtxDataProvider, ICtxDatabase)
  protected
    { Protected declarations }
    FSchema: TDatabaseSchema;
    FSystemTableName: String;
    FDatabaseName: String;
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
    {:: Creates an instance of a TADOConnectionExt component.}
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of a TADOConnectionExt component.}
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
    {:: Returns current version of physical database structure, stored in system table. }
    function GetVersion: TSchemaVersion;
    {:: Set current version of physical database structure, stored in system table, }
    {:: to the value, specified in Value parameter. }
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
    { Executes statement that may return result set. }
    function ExecuteStatement(SQL: String; ResultSet: Pointer = nil): Integer;

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
    property DatabaseName: String read FDatabaseName write SetDatabaseName;
  end;

  {:$ Retrieves database URL from the TADOConnection component and the connected Session component. }
  function GetDatabaseURL(ADOConnection: TADOConnection): String;
  {:$ Assigned database and session parameters accoriding to the provided database URL. }
  procedure SetDatabaseURL(ADOConnection: TADOConnection; DatabaseURL: String);

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
  RegisterComponents('Database Extensions', [TADOConnectionExt]);
end;

function GetDatabaseURL(ADOConnection: TADOConnection): String;
begin
  Result := ADOConnection.ConnectionString;
end;

procedure SetDatabaseURL(ADOConnection: TADOConnection; DatabaseURL: String);
begin
  ADOConnection.ConnectionString := DatabaseURL
end;

{ TADORangeCursor }

constructor TADORangeCursor.Create(Database: TADOConnectionExt; Relation: TRelation; KeyValues: Variant);
begin
  if (Relation.ForeignTable = '') or (Relation.ForeignKeyFields = '') then
    DatabaseError(SInvalidRangeCursor);
  CreateExt(Database, Relation.ForeignTable, Relation.ForeignKeyFields, Relation.ForeignCondition, Relation.CaseInsensitive, KeyValues);
end;

constructor TADORangeCursor.CreateExt(Database: TADOConnectionExt;
  const TableName, KeyFields, TableFilter: String;
  CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String = '');
var
  RangeFilter: String;
  Table: TADOTable;
begin
  if (TableName = '') then
    DatabaseError(SInvalidRangeCursor);

  inherited Create;
  KeyFieldsList := TList.Create;

  Table := Database.CreateTable(TableName) as TADOTable;
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

destructor TADORangeCursor.Destroy;
begin
  KeyFieldsList.Free;
  inherited Destroy;
end;

{ TADOConnectionExt }

constructor TADOConnectionExt.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSchema := nil;
  FSystemTableName := defSysTableName;
  FVersionStr := '';
end;

destructor TADOConnectionExt.Destroy;
begin
  UnRegisterCtxDataProvider(Self);
  SystemTableName := '';
  inherited Destroy;
end;

procedure TADOConnectionExt.CheckActive;
begin
  if not Connected then
    DatabaseError(SDatabaseClosed);
end;

procedure TADOConnectionExt.CheckSchema;
begin
  if Schema = nil then
    DatabaseError(sDatabaseSchemaIsNotAssigned);
end;

function TADOConnectionExt.GetVersion: TSchemaVersion;
begin
  CheckActive;
  Result := GetDatabaseVersion(Self);
end;

procedure TADOConnectionExt.SetVersion(const Value: TSchemaVersion);
begin
  CheckActive;
  SetDatabaseVersion(Self, Value);
end;

function TADOConnectionExt.GetVersionLabel: String;
begin
  if Connected then
  begin
    if FVersionStr = '' then
      FVersionStr := VersionToStr(GetVersion);
    Result := FVersionStr;
  end else
    Result := VersionToStr(Undefined);
end;

function TADOConnectionExt.IsVersionCurrent(AllowNewer: Boolean): Boolean;
begin
  CheckSchema;
  CheckActive;
  Result := VersionIsOkey(Schema.Version, GetVersion, AllowNewer);
end;

procedure TADOConnectionExt.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSchema) then
    Schema := nil;
  inherited;
end;

procedure TADOConnectionExt.SetSchema(Value: TDatabaseSchema);
begin
  FSchema := Value;
end;

procedure TADOConnectionExt.SetVersionLabel(const Value: String);
begin
  // Ignore this.
end;

procedure TADOConnectionExt.StartTransaction;
begin
  inherited BeginTrans;
end;

procedure TADOConnectionExt.Commit;
begin
  inherited CommitTrans;
end;

procedure TADOConnectionExt.Rollback;
begin
  inherited RollbackTrans;
end;

function TADOConnectionExt.GetInTransaction: Boolean;
begin
  Result := InTransaction;
end;

function TADOConnectionExt.GetDriverName: String;
begin
  Result := 'ADO';
end;

function TADOConnectionExt.GetDatabaseURL: String;
begin
  Result := ADOExt.GetDatabaseURL(Self);
end;

procedure TADOConnectionExt.SetDatabaseURL(const Value: String);
begin
  ADOExt.SetDatabaseURL(Self, Value);
end;

function TADOConnectionExt.GetRangeCursor(const TableName, KeyFields,
  TableFilter: String; CaseInsensitive: Boolean; KeyValues: Variant;
  const ExtraKeyFields: String): TDBRangeCursor;
begin
  Result := TADORangeCursor.CreateExt(Self, TableName, KeyFields,
    TableFilter, CaseInsensitive, KeyValues, ExtraKeyFields);
end;

function TADOConnectionExt.GetRangeCursor(Relation: TRelation;
  KeyValues: Variant): TDBRangeCursor;
begin
  Result := TADORangeCursor.Create(Self, Relation, KeyValues);
end;

function TADOConnectionExt.GetSchema: TDatabaseSchema;
begin
  Result := FSchema;
end;

function TADOConnectionExt.GetDatabaseName: String;
begin
  Result := FDatabaseName;
end;

procedure TADOConnectionExt.SetDatabaseName(const Value: String);
begin
  FDatabaseName := Value;
  RegisterCtxDataProvider(Self);
end;

function TADOConnectionExt.CreateTable(const TableName: String): TDataSet;
begin
  Result := TADOTable.Create(nil);
  try
    TADOTable(Result).Connection := Self;
    TADOTable(Result).TableName := TableName;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TADOConnectionExt.CreateQuery(const Statement: String): TDataSet;
begin
  Result := TADOQuery.Create(nil);
  try
    TADOQuery(Result).Connection := Self;
    TADOQuery(Result).CommandTimeout := Self.CommandTimeout;
    // Assign SQL statement
    TADOQuery(Result).SQL.Text := Statement;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TADOConnectionExt.ExecSQL(Query: TDataSet);
begin
  TADOQuery(Query).ExecSQL;
end;

function TADOConnectionExt.ExecuteStatement(SQL: String; ResultSet: Pointer = nil): Integer;
var
  RS: _RecordSet;
  Cmd: TADOCommand;
begin
  Cmd := TADOCommand.Create(Self);
  try
    Cmd.CommandObject._Set_ActiveConnection(ConnectionObject);
    Cmd.ParamCheck := False;
    Cmd.CommandText := SQL;
    if Assigned(ResultSet) then
    begin
      RS := Cmd.Execute;
      if (RS <> nil) and (RS.State and adStateOpen <> 0) then
      begin
        TDataSet(ResultSet^) := TADODataSet.Create(nil);
        TADODataSet(ResultSet^).RecordSet := RS;
      end;
    end else
    begin
      Cmd.ExecuteOptions := [eoExecuteNoRecords];
      Cmd.Execute(Result, EmptyParam);
    end
  finally
    Cmd.Free;
  end;
end;

procedure TADOConnectionExt.GetQueryParams(Query: TDataSet; Params: TParams);
var
  I: Integer;
  Param: TParam;
begin
  with TADOQuery(Query) do
  for I := 0 to Parameters.Count - 1 do
  begin
    Param := Params.CreateParam(Parameters[I].DataType,
      Parameters[I].Name, TParamType(Parameters[I].Direction));
    Param.Value := Parameters[I].Value;
  end;
end;

procedure TADOConnectionExt.SetQueryParams(Query: TDataSet; Params: TParams);
var
  I: Integer;
begin
  for I := 0 to Params.Count - 1 do
    TADOQuery(Query).Parameters.ParamByName(Params[I].Name).Value := Params[I].Value;
end;

function TADOConnectionExt.GetQuerySQL(Query: TDataSet): String;
begin
  Result := TADOQuery(Query).SQL.Text;
end;

procedure TADOConnectionExt.SetQuerySQL(Query: TDataSet;
  const Statement: String);
begin
  TADOQuery(Query).SQL.Text := Statement;
end;

function TADOConnectionExt.FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;
begin
  Result := False;
  DatabaseError(SCapabilityNotSupported);
end;

function TADOConnectionExt.GetIndexDefs(DataSet: TDataSet): TIndexDefs;
begin
  Result := (DataSet as TADOTable).IndexDefs;
end;


(*
procedure TADOConnectionExt.ReverseEngineer;
var
  I, J, Idx, DefIdx: Integer;
  Table: TADOTable;
  Tables: TStringList;
  LogicalTableName: String;
begin
  CheckActive;
  CheckSchema;
  // Update schema from the physical tables
  Table := CreateTable('') as TADOTable;
  Tables := TStringList.Create;
  try
    GetTableNames(Tables, False);
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
*)

function ADOTypeToFieldType(const ADOType: DataTypeEnum): TFieldDataType;
begin
  case ADOType of
    adEmpty: Result := ftUnknown;
    adTinyInt, adSmallInt: Result := ftSmallint;
    adError, adInteger, adUnsignedInt: Result := ftInteger;
    adBigInt, adUnsignedBigInt: Result := ftLargeInt;
    adUnsignedTinyInt, adUnsignedSmallInt: Result := ftWord;
    adSingle, adDouble: Result := ftFloat;
    adCurrency: Result := ftBCD;
    adBoolean: Result := ftBoolean;
    adDBDate: Result := ftDate;
    adDBTime: Result := ftTime;
    adDate, adDBTimeStamp, adFileTime, adDBFileTime: Result := ftDateTime;
    adChar: Result := ftFixedChar;
    adVarChar: Result := ftString;
    adBSTR, adWChar, adVarWChar: Result := ftWideString;
    adLongVarChar, adLongVarWChar: Result := ftMemo;
    adLongVarBinary: Result := ftBlob;
    adBinary: Result := ftVarBytes;
    adVarBinary: Result := ftVarBytes;
    adChapter: Result := ftDataSet;
    adPropVariant, adVariant: Result := ftVariant;
    adIUnknown: Result := ftInterface;
    adIDispatch: Result := ftIDispatch;
    adGUID: Result := ftGUID;
    adDecimal, adNumeric, adVarNumeric: Result := ftBCD;
  else
    Result := ftUnknown;
  end;
end;

{ DDL functions: ReverseEngineering}
procedure TADOConnectionExt.ReverseEngineer;
var
  DS : TADODataSet;
  DT : TADOTable;
  TD: TTableDefinition;
  TI: TIndexDefinition;
  TC: TTableConstraint;
  TR: TRelationship;
  Temp: String;
  AdoDataType: DataTypeEnum;
  AdoDataSize: Integer;

  Restrict: OleVariant;

  function FindCheck(const AName: String): TTableConstraint;
  var
    I: integer;
  begin
    for I := 0 to Schema.TableDefs.Count-1 do
    begin
      Result := Schema.TableDefs[I].Constraints.Find(AName);
      if Result <> nil then
        Exit;
    end;
    Result := nil;    
  end;

  function AdoRuleToAction(const ARule: String): TRelationAction;
  begin
    Result := raIgnore;
    if AnsiSameText(ARule, 'CASCADE') then
      Result := raCascade else
    if AnsiSameText(ARule, 'SET NULL') then
      Result := raNullify else
    if AnsiSameText(ARule, 'SET DEFAULT') then
      Result := raSetDefault else
    if AnsiSameText(ARule, 'NO ACTION') then
      Result := raError;
  end;

  procedure CleanupEmptyChecks;
  var
    I, J: Integer;
  begin
    for I := 0 to Schema.TableDefs.Count - 1 do
    with Schema.TableDefs[I] do
    begin
      J := Constraints.Count - 1;
      while J >= 0 do
      begin
        if Constraints[J].Check = '' then
          Constraints[J].Free;
        Dec(J);
      end;
    end;
  end;

  procedure CleanupRelationIndexes;
  var
    I, J: Integer;
    IdxDef: TIndexDefinition;
  begin
    for I := 0 to Schema.TableDefs.Count - 1 do
    with Schema.TableDefs[I] do
      for J := 0 to Relations.Count - 1 do
      begin
        IdxDef := IndexDefs.Find(Relations[J].Name);
        if IdxDef <> nil then
          IdxDef.Free;
      end;
  end;

  procedure UpdateAutoIncFields;
  var
    I, J: Integer;
    F: TFieldDefinition;
  begin
    // Update AutoInc fields
    DT := TADOTable.Create(nil);
    try
      DT.Connection := Self;
      for I := 0 to Schema.TableDefs.Count - 1 do
      begin
        DT.Active := False;
        try
          DT.TableName := Schema.TableDefs[I].TableName;
          DT.FieldDefs.Update;
          for J := 0 to DT.FieldDefs.Count - 1 do
          if DT.FieldDefs[J].DataType = DB.ftAutoInc then
          begin
            F := Schema.TableDefs[I].FieldDefs.Find(DT.FieldDefs[J].Name);
            if F <> nil then
            begin
              F.SQLFieldType := '';
              F.DataType := dbSchema.ftAutoInc;
            end;
          end;
        except
          // Ignore all errors
        end;
      end;
    finally
      DT.Free;
    end;
  end;

begin
  CheckActive;
  CheckSchema;
  Schema.Clear;
  // Update schema from the physical tables
  DS := TADODataSet.Create(nil);
  try
    DS.Connection := Self;
    DS.CursorLocation := clUseClient;

    //Restrict := EmptyParam;
    Restrict := VarArrayOf([NULL, 'DATA']);
    try
    OpenSchema(siTables, Restrict, EmptyParam, DS);
    DS.Active := True;
    while not DS.Eof do
    begin
      if not AnsiSameText(DS.FieldByName('TABLE_NAME').AsString, GetSystemTableName) then
        if AnsiSameText(DS.FieldByName('TABLE_TYPE').AsString, 'TABLE') then
        try
          with Schema.TableDefs.Add do
          begin
            Name := DS.FieldByName('TABLE_NAME').AsString;
            Description := DS.FieldByName('DESCRIPTION').AsString;
          end;
        except
        end;
      DS.Next;
    end;
    except
      Application.HandleException(Self);
      //ShowMessage('Tables Error');
    end;
    DS.Active := False;


    Restrict := VarArrayOf([NULL, 'DATA']);

    try
    OpenSchema(siColumns, Restrict, EmptyParam, DS);
    DS.Active := True;
    DS.Sort := 'TABLE_NAME, ORDINAL_POSITION';
    while not DS.Eof do
    begin
      TD := Schema.TableDefs.Find(DS.FieldByName('TABLE_NAME').AsString);
      if TD <> nil then
      try
        with TD.FieldDefs.Add do
        begin
          Name := DS.FieldByName('COLUMN_NAME').AsString;
          AdoDataType := DS.FieldByName('DATA_TYPE').AsInteger;
          AdoDataSize := DS.FieldByName('CHARACTER_MAXIMUM_LENGTH').AsInteger;
          if (AdoDataType in [adBSTR, adWChar, adVarWChar]) and (AdoDataSize = 0) then
            AdoDataType := adLongVarWChar
          else if (AdoDataType = adChar) and (AdoDataSize = 0) then
            AdoDataType := adLongVarChar;

          DataType := ADOTypeToFieldType(AdoDataType);
          if DS.FieldByName('COLUMN_HASDEFAULT').AsBoolean then
            DefaultExpression := DS.FieldByName('COLUMN_DEFAULT').AsString;
          Required := not DS.FieldByName('IS_NULLABLE').AsBoolean;
          Size := AdoDataSize;
          Precision := DS.FieldByName('NUMERIC_PRECISION').AsInteger;
          SetPropValue('Scale', DS.FieldByName('NUMERIC_SCALE').AsString);
          Description := DS.FieldByName('DESCRIPTION').AsString;
        end;
      except
      end;
      DS.Next;
    end;
    except
      ShowMessage('Fields Error')
    end;
    DS.Active := False;


    Restrict := VarArrayOf([NULL, 'DATA']);
    //UpdateAutoIncFields;
    try
      OpenSchema(siIndexes, Restrict, EmptyParam, DS);
      DS.Active := True;
      DS.Sort := 'TABLE_NAME, INDEX_NAME, ORDINAL_POSITION';

      while not DS.Eof do
      begin
        TD := Schema.TableDefs.Find(DS.FieldByName('TABLE_NAME').AsString);
        if TD <> nil then
        begin
          Temp := DS.FieldByName('INDEX_NAME').AsString;
          TI := TD.IndexDefs.Find(Temp);
          if TI = nil then
          begin
            TI := TD.IndexDefs.Add;
            TI.Name := DS.FieldByName('INDEX_NAME').AsString;
            TI.PrimaryKey := DS.FieldByName('PRIMARY_KEY').AsBoolean;
            TI.Unique := DS.FieldByName('UNIQUE').AsBoolean;
            TI.SetPropValue('Clustered', DS.FieldByName('CLUSTERED').AsString);
          end;
          try
            with TI.IndexFields.Add do
            begin
              Name := DS.FieldByName('COLUMN_NAME').AsString;
              Descending := DS.FieldByName('COLLATION').AsInteger = DB_COLLATION_DESC;
            end;
          except
          end;
        end;
        DS.Next;
      end;
    except
    end;
    DS.Active := False;


    try
    OpenSchema(siViews, Restrict, EmptyParam, DS);
    DS.Active := True;
    DS.Sort := '';
    while not DS.Eof do
    begin
      Temp := DS.FieldByName('TABLE_SCHEMA').AsString;
      if not AnsiSameText(Temp, 'INFORMATION_SCHEMA') and not AnsiSameText(Temp, 'sys') then
      try
        with Schema.ViewDefs.Add do
        begin
          Name := DS.FieldByName('TABLE_NAME').AsString;
          Temp := Trim(DS.FieldByName('VIEW_DEFINITION').AsString);
          while (Temp <> '') and (Temp[Length(Temp)] = ';') do
            System.Delete(Temp, Length(Temp), 1);
          if Temp <> '' then
            Definition.Text := 'CREATE VIEW ' + FormatName(Name, 'b') + ' AS ' + Temp;
          Description := DS.FieldByName('DESCRIPTION').AsString;
        end;
      except
      end;
      DS.Next;
    end;
    except
    end;
    DS.Active := False;



    (* -- skip procedures, nothing but name is returned
    OpenSchema(siProcedures, EmptyParam, EmptyParam, DS);
    DS.Active := True;
    DS.Sort := '';
    while not DS.Eof do
    begin
      Temp := DS.FieldByName('PROCEDURE_SCHEMA').AsString;
      if not AnsiSameText(Temp, 'sys') then
        if DS.FieldByName('PROCEDURE_TYPE').AsInteger in [DB_PT_PROCEDURE, DB_PT_FUNCTION] then
        with Schema.StoredProcs.Add do
        begin
          Name := DS.FieldByName('PROCEDURE_NAME').AsString;
          IsFunction := DS.FieldByName('PROCEDURE_TYPE').AsInteger = DB_PT_FUNCTION;
          Temp := Trim(DS.FieldByName('PROCEDURE_DEFINITION').AsString);
          if Temp <> '' then
            if IsFunction then
              Definition.Text := 'CREATE FUNCTION ' + Name + ' AS ' + Temp else
              Definition.Text := 'CREATE PROCEDURE ' + Name + ' AS ' + Temp;
          Description := DS.FieldByName('DESCRIPTION').AsString;
        end;
      DS.Next;
    end;
    DS.Active := False;
    *)

    try
    OpenSchema(siTableConstraints, Restrict, EmptyParam, DS);
    DS.Active := True;
    DS.Sort := '';
    while not DS.Eof do
    begin
      if AnsiSameText(DS.FieldByName('CONSTRAINT_TYPE').AsString, 'CHECK') then
      begin
        TD := Schema.TableDefs.Find(DS.FieldByName('TABLE_NAME').AsString);
        if TD <> nil then
      try
        with TD.Constraints.Add do
        begin
          Name := DS.FieldByName('CONSTRAINT_NAME').AsString;
          Description := DS.FieldByName('DESCRIPTION').AsString;
        end;
      except
      end;
      end;
      DS.Next;
    end;
    except
    end;
    DS.Active := False;

    try
      OpenSchema(siCheckConstraints, Restrict, EmptyParam, DS);
      DS.Active := True;
      DS.Sort := '';
      while not DS.Eof do
      begin
        TC := FindCheck(DS.FieldByName('CONSTRAINT_NAME').AsString);
        if TC <> nil then
          TC.Check := DS.FieldByName('CHECK_CLAUSE').AsString;
        DS.Next;
      end;
    except
    end;
    DS.Active := False;

    try
    OpenSchema(siForeignKeys, Restrict, EmptyParam, DS);
    DS.Active := True;
    DS.Sort := 'FK_NAME, ORDINAL';
    while not DS.Eof do
    begin
      Temp := DS.FieldByName('FK_NAME').AsString;
      TR := Schema.Relationships.Find(Temp);
      try
        if TR = nil then
        begin
          TR := Schema.Relationships.Add;
          TR.Name := DS.FieldByName('FK_NAME').AsString;
          TR.DetailRelationName := TR.Name;
          TR.MasterTableName := DS.FieldByName('PK_TABLE_NAME').AsString;
          TR.DetailTableName := DS.FieldByName('FK_TABLE_NAME').AsString;
          TR.MasterKeyFields := DS.FieldByName('PK_COLUMN_NAME').AsString;
          TR.DetailKeyFields := DS.FieldByName('FK_COLUMN_NAME').AsString;
          TR.UpdateAction := AdoRuleToAction(DS.FieldByName('UPDATE_RULE').AsString);
          TR.DeleteAction := AdoRuleToAction(DS.FieldByName('DELETE_RULE').AsString);
        end else
        begin
          TR.MasterKeyFields := TR.MasterKeyFields + ';' + DS.FieldByName('PK_COLUMN_NAME').AsString;
          TR.DetailKeyFields := TR.DetailKeyFields + ';' + DS.FieldByName('FK_COLUMN_NAME').AsString;
        end;
      except
      end;
      DS.Next;
    end;
    except
    end;
    DS.Active := False;


    Schema.UpdateRelationships;

    CleanupRelationIndexes;
    CleanupEmptyChecks;

    DS.Active := False;
  finally
    DS.Free;
  end;
end;

function TADOConnectionExt.GetSystemTableName: String;
begin
  if Trim(FSystemTableName) = '' then
    Result := defSysTableName else
    Result := FSystemTableName;
end;

procedure TADOConnectionExt.DoConnect;
begin
  inherited;
  FVersionStr := '';
end;

function TADOConnectionExt.CreateCommand: TCtxDataCommand;
begin
  Result := TCtxDataSetCommand.Create(Self);
end;

end.


