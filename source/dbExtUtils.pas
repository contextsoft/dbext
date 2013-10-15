(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Common database utilities
(*
(*  Copyright (c) 2005-2011, Context Software LLC
(*  ------------------------------------------------------------
(*  FILE        : dbExtUtils.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.32
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009, 2010, XE
(*
(******************************************************************************)
unit dbExtUtils;

{$I CtxVer.inc}

interface

uses Classes, SysUtils,
{$IFnDEF VER130}
  Variants,
  FmtBcd,
{$ENDIF}
  dbSchema, DB, CtxDBIntf, dbExtParser;


type
{$IFnDEF D2009_ORLATER}
  TRecordBuffer = PChar;
{$ENDIF}


  (* This interface is implemented to support client-side operations. *)
  IDatabaseExt = interface (ISchemaDatabase)
    ['{D5E345B5-63A8-42EE-8A4A-0969A9ABF99A}']

    function CheckObjectsTable(Create: Boolean = False): Boolean;
    function CheckSystemTable(Create: Boolean = False): Boolean;

    // procedure ReverseEngineer;
    procedure CreateNewDatabase(OverwriteTables: Boolean = True;
      CreateSystemTable: Boolean = True; CreateObjectsTable: Boolean = True);
    function UpdateDatabase: Boolean;

    procedure LockDatabase;
    procedure UnLockDatabase;

    procedure BeginReplicating;
    procedure EndReplicating;
    function GetReplicating: Boolean;
    procedure StartTransactionCount;

    function GetObjectsTable: TObjectsTable;
    function GetSystemTable: TSystemTable;

    function GetUpdateOptions: TUpdateOptions;
    // function GetSchema: TDatabaseSchema;
    // procedure SetSchema(Value: TDatabaseSchema);
    function GetActiveTransaction: TActiveTransaction;
    function GetEnabledTriggers: TSelectTriggers;

    function AddVirtualKey(const TableName, KeyFields: String; KeyValues: Variant; CaseInsensitive: Boolean = False): Integer;
    function VirtualKeyExists(const VirtualKey: String): Boolean;
    procedure DeleteVirtualKey(VirtualKeyID: Integer);

    procedure DoProgress(const Operation: String; PercentDone: Byte; var Abort: Boolean);

    function GetReplicationID: Integer;
    procedure SetReplicationID(Value: Integer);
    function GetSnapshotID: Integer;
    procedure SetSnapshotID(Value: Integer);
    function GetUserName: String;
    procedure SetUserName(const Value: String);
    function GetObjectsTableName: String;
    function GetSystemTableName: String;

    { Public properties - a matter of convinience }
    property ActiveTransaction: TActiveTransaction read GetActiveTransaction;
    property ObjectsTable: TObjectsTable read GetObjectsTable;
    property SystemTable: TSystemTable read GetSystemTable;

    {:$ The name of the current user. }
    property UserName: String read GetUserName write SetUserName;
  end;

  TDataSetContext = class (TExpressionContext)
    class function GetItem(context: pointer; const name: string): variant; override;
  end;

  TDataSetContextOldVal = class (TExpressionContext)
    class function GetItem(context: pointer; const name: string): variant; override;
  end;

type
  {:: Contains information about current procedure and helps calculate the percentage of progress. }
  TProgressCounter = record
    PercentDone: Byte;
    MinValue: Integer;
    MaxValue: Integer;
    Value: Integer;
    Abort: Boolean;
  end;

  procedure Init(var Counter: TProgressCounter);
  procedure InitMinMax(var Counter: TProgressCounter; AMinValue, AMaxValue: Integer);
  function Progress(var Counter: TProgressCounter; Increment: Integer = 1): Boolean;

type
  {:$ TMacro represents a query macro. }
  TMacro = class (TCollectionItem)
  protected
    FValue: String;
    FName: String;
    procedure SetName(const Value: String);
    procedure SetValue(const Value: String);
    {:$ Specifies the name of the TMacro as it appears in Object Inspector. }
    function GetDisplayName: String; override;
  public
    {:$ Creates an instance of TMacro object. }
    {:: This method should never be used directly. }
    {:: Use Add method of TMacros collection instead. }
    constructor Create(Collection: TCollection); override;
    {:$ Destroys the instance of TMacro object. }
    destructor Destroy; override;
    {:$ Assigns instance of TMacro object from the Source object. }
    procedure Assign(Source: TPersistent); override;
    {:$ Returns string value of this macro. }
    {:! This property is provided for compatibility with RX query supporting macros. }
    property AsString: String read FValue write SetValue;
  published
    {:$ Specifies the name of this macro. }
    property Name: String read FName write SetName;
    {:$ Returns string value of this macro. }
    property Value: String read FValue write SetValue;
  end;

  {:$ TMacros represents a collection of query macros. }
  TMacros = class (TOwnedCollection)
  private
    function GetItem(Index: Integer): TMacro;
    procedure SetItem(Index: Integer; const Value: TMacro);

    function AddMacro(const MacroName: String; Data: Pointer): String;
    function ExpandMacro(const MacroName: String; Data: Pointer): String;
  public
    {:$ Assign values from the macros collection provided in Source parameter. }
    procedure AssignValues(Source: TMacros);
    {:$ Expands template, replacing macro names with their values. }
    function ExpandMacros(const Template, MacroBegin, MacroEnd: String): String;
    {:$ Parses template and extracts macro names. }
    procedure ParseTemplate(const Template, MacroBegin, MacroEnd: String);
    {:$ Update names of macros from new template keeping values of the unchanged parameters. }
    procedure UpdateMacros(const Template, MacroBegin, MacroEnd: String);
    {:$ Finds macro by name. Returns nil if no macro with this name found. }
    function Find(const Name: String): TMacro;
    {:$ Provides array-like access to contained macros. }
    property Items[Index: Integer]: TMacro read GetItem write SetItem; default;
  end;

  {:$ Reassign new values to ItemNo field. }
  function UpdateItemNo(ItemNoField: TField): Integer;
  {:$ Move document's item up (Dist < 0) or down (Dist > 0) in list. }
  function MoveItem(ItemNoField: TField; Dist: Integer = 1): Boolean;

  function EvaluateFilter(DataSet: TDataSet; const FilterExpr: String): Boolean;
  procedure EvaluateCondition(Table: TDataSet; const Condition: String; var Value, OldValue: Boolean);

  function ExecuteSQL(Database: IDatabaseExt; const Statement: String;
    ReturnSet: Boolean = True; ReportProgress: Boolean = False): TDataSet;
  function ExecuteSQLParam(Database: IDatabaseExt; const Statement,
    ParamNames: String; ParamValues: Variant; ParamDataSet: TDataSet; ReturnSet,
    ReportProgress: Boolean): TDataSet;

  procedure SetPrivateCalcBuffer(DataSet: TDataSet; Buffer: TRecordBuffer);
  function CalcSum(Fld: TField): Double;
  function VarDiff(NewValue, OldValue: Variant): Variant;

  {:$ Updates field properties from the specified Database component. }
  procedure UpdateFieldsProperties(DataSet: TDataSet; Database: TObject);
  {:$ Updates field properties from the specified Schema component. }
  procedure UpdateFieldsFromSchema(DataSet: TDataSet; Schema: TDatabaseSchema);
  {:$ Returns full path to a directory (Dir) relative to current application
  {:$ directory the application's exe is located }
  function GetRelativePath(const Dir: String): String;

  {:$ Executes Error constraints from schema. }
  procedure ExecuteErrorConstraints(Database: IDatabaseExt; Table: TDataSet; ChangeType: TChangeType; TableDef: TTableDefinition);
  {:$ Executes Cascade constraints from schema. }
  procedure ExecuteCascadeOperations(Database: IDatabaseExt; Table: TDataSet; ChangeType: TChangeType; TableDef: TTableDefinition);
  {:$ Executes aggregated fields (sum and count). }
  procedure ExecuteAggregates(Database: IDatabaseExt; Table: TDataSet; ChangeType: TChangeType; TableDef: TTableDefinition);
  {:$ Executes Table Triggers from schema. }
  procedure ExecuteTableTriggers(Database: IDatabaseExt; Table: TDataSet; ChangeType: TChangeType; TriggerType: TTriggerType; TableDef: TTableDefinition); overload;
  {:$ Executes Table Triggers from schema. }
  procedure ExecuteTableTriggers(Database: IDatabaseExt; ObjectKey: Variant; ChangeType: TChangeType; TriggerType: TTriggerType; TableDef: TTableDefinition); overload;
  {:$ Makes entry into 'objects' table. }
  procedure ObjectChanged(Database: IDatabaseExt; Table: TDataSet; ChangeType: TChangeType; TableDef: TTableDefinition); overload;
  {:$ Executes all triggers & constraints for a given table. }
  function ExecuteTriggers(Database: IDatabaseExt; DataSet: TDataSet; ChangeType: TChangeType; UpdateOptions: TUpdateOptions): TTableDefinition;
  {:$ Update calculated (aggregated) fields after one of the corresponding records has been changed. }
  {:: This method is called internally at the end of transaction. }
  procedure ExecuteScheduledAggregates(Database: IDatabaseExt);
  procedure WriteObjectChanges(Database: IDatabaseExt);
  procedure CalculateFieldsExt(Buffer: TRecordBuffer; DataSet: TDataSet);

  // procedure ExecuteUpdate(Database: IDatabaseExt;
  //  DatabaseUpdate: TDatabaseUpdate; var Abort: Boolean);
  // function UpdateDatabase(Database: IDatabaseExt): Boolean;

  procedure SetVersion(Database: IDatabaseExt; const Value: TSchemaVersion);
  function GetVersion(Database: IDatabaseExt): TSchemaVersion;

  {:$ Writes field (Field) value into stream using Writer. }
  procedure WriteFieldValue(Writer: TWriter; Field: TField);
  {:$ Reads field (Field) value from the stream using Reader. }
  procedure ReadFieldValue(Reader: TReader; Field: TField);
  {:$ Skips field (Field) value stored the stream using Reader. }
  procedure SkipFieldValue(Reader: TReader);

  function ExecuteVerbExt(Databases: TList; DatabaseID, Verb: Integer;
    var Data: OleVariant): Integer;

const
  { Published properties - do not localize }
  propDisplayLabel = 'DisplayLabel';
  propDisplayWidth = 'DisplayWidth';
  propDisplayFormat = 'DisplayFormat';
  propDefaultExpression = 'DefaultExpression';
  propEditMask = 'EditMask';

resourcestring
  SUnsupportedDataType = 'Unsupported data type';
  SInvalidAggregateExpression = 'Invalid aggregate expression. Table: %s, Field: %s';
  SDatabaseSchemaIsNotAssigned = 'Database Schema is not assigned';
  SObjectsTableNotFound = 'Objects table not found. It is required for change tracking and replications.';
  SCapabilityNotSupported = 'Capability not supported';

  SExecutingQuery = 'Executing Query...';
  SCreatingDatabase = 'Creating Database...';
  SSynchronizing = 'Synchronizing Database...';
  SUpdatingObjectsTable = 'Updating Objects Table...';
  SExportingData = 'Exporting data...';
  SImportingData = 'Importing data...';
  SOperationAbortedByUser = 'Operation aborted by user';
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SUnableToLockDatabase = 'Unable to lock the database.';
  SInvalidRelationFields = 'Invalid fields specification. Relation: %s';
  SInvalidRelationSpecs = 'Invalid relation specification. Relation: %s';
  STableDefintionNotFound = 'Table definition not found: %s';
  SInvalidRangeCursor = 'Invalid parameters supplied to range cursor';
  SStreamReadError = 'Error reading object from stream';
  SNoFieldmapFoundForTable = 'No field name found. Table: %s';
  SInvalidFieldMap = 'Invalid field map. Table: %s';
  SErrorCreatingTable = 'Error creating table %s. Error: %s';
  SMacroNotFound = 'Macro not found: %s';

  STypeNotSupported = 'This field type is not supported in calculated expressions';

  SSynchronizationNotApplicable = 'Synchronization is only applicable for snapshots';
  SCanNotSynchronizeWithAnotherSnapshot = 'Cannot synchronize with another snapshot';
  SDestDatabaseDoesnotSupportSynchronization = 'Destination database does not support synchronizations';
  SOperationIsNotSupport = '%s is not supported in your system';
  SExportingChanges = 'Exporting changes';
  SImportingChanges = 'Importing changes';
  SInvalidFileFormat = 'Invalid file format';
  SInvalidFileVersion = 'Invalid file version';
  SInvalidSchemaName = 'Schema referred to by the import file differs from the current database schema';

  SDatabaseMustBeAssigned = 'Database must be assigned.';
  SSystemTableNotFound = 'System table not found. It is required for change tracking and replications.';

  SOperationNotSupported = 'Selected operation is not supported by %s adapter';
  SInvalidDatabaseHandle = 'The database is not found or inactive';

implementation

uses TypInfo;

{ Common DB Extensions Routines }


function UpdateItemNo(ItemNoField: TField): Integer;
var
  B: TDataSetBookmark;
  IsCurrent: Boolean;
begin
  Result := 1;
  if (ItemNoField = nil) or (ItemNoField.DataSet = nil) or
    (not ItemNoField.DataSet.Active) then exit;
  with ItemNoField.DataSet do
  begin
    B := Bookmark;
    DisableControls;
    try
      First;
      while not EOF do
      begin
        if ItemNoField.AsInteger <> Result then
        begin
          IsCurrent := B = Bookmark;
          Edit;
          try
            ItemNoField.AsInteger := Result;
            Post;
          except
            Cancel;
            raise;
          end;
          if IsCurrent then
            B := Bookmark;
        end;
        Next;
        Result := Result + 1;
      end;
    finally
      Bookmark := B;
      EnableControls;
    end;
  end;
end;

function MoveItem(ItemNoField: TField; Dist: Integer = 1): Boolean;
var
  ItemNo: Integer;
begin
  Result := False;
  with ItemNoField.DataSet do
  begin
    DisableControls;
    try
      ItemNo := ItemNoField.AsInteger;
      if (ItemNo + Dist < 1) or (ItemNo + Dist > RecordCount) then exit;

      Result := True;
      Edit;
      try
        ItemNoField.AsInteger := -1;
        Post;
      except
        Cancel;
        raise;
      end;
      if Locate(ItemNoField.FieldName, ItemNo + Dist, []) then
      begin
        Edit;
        try
          ItemNoField.AsInteger := ItemNo;
          Post;
        except
          Cancel;
          raise;
        end;
      end;
      if Locate(ItemNoField.FieldName, -1, []) then
      begin
        Edit;
        try
          ItemNoField.AsInteger := ItemNo + Dist;
          Post;
        except
          Cancel;
          raise;
        end;
      end;
    finally
      EnableControls;
    end;
  end;
end;

function EvaluateFilter(DataSet: TDataSet; const FilterExpr: String): Boolean;
begin
  Result := dbExtParser.EvaluateExpression(FilterExpr, DataSet, TDataSetContext);
end;

procedure EvaluateCondition(Table: TDataSet; const Condition: String; var Value, OldValue: Boolean);
var
  ConditionExprToken: TEvaluator;
begin
  if Condition <> '' then
  begin
    { this could be optimized by caching TEvaluator in relation... }
    ConditionExprToken := TEvaluator.Create(Condition, Table, TDataSetContext);
    try
      Value := ConditionExprToken.Evaluate;
      ConditionExprToken.ContextClass := TDataSetContextOldVal;
      OldValue := ConditionExprToken.Evaluate;
    finally
      ConditionExprToken.Free;
    end;
  end else begin
    Value := True;
    OldValue := True;
  end;
end;

function CalcSum(Fld: TField): Double;
var
  B: TDataSetBookmark;
begin
  Result := 0;
  if Fld = nil then exit;
  with Fld.DataSet do
  begin
    DisableControls;
    B := Bookmark;
    try
      First;
      if Fld.FieldKind = fkData then
        BlockReadSize := 200
      else BlockReadSize := 0;
      while not EOF do
      begin
        Result := Result + Fld.AsFloat;
        Next;
      end;
    finally
      if Fld.FieldKind = fkData then
        BlockReadSize := 0;
      Bookmark := B;
      EnableControls;
    end;
  end;
end;

function ExecuteSQL(Database: IDatabaseExt; const Statement: String;
  ReturnSet: Boolean = True; ReportProgress: Boolean = False): TDataSet;
begin
  Result := ExecuteSQLParam(Database, Statement, '', NULL, nil, ReturnSet, ReportProgress);
end;

function ExecuteSQLParam(Database: IDatabaseExt; const Statement,
  ParamNames: String; ParamValues: Variant; ParamDataSet: TDataSet; ReturnSet,
  ReportProgress: Boolean): TDataSet;
var
  ParamName: String;
  P, I: Integer;
  Fld: TField;
  Param: TParam;
  Params: TParams;
begin
  Result := Database.CreateQuery(Statement);
  with Result do
  try
    // Assign parameters
    Params := TParams.Create;
    try
      Database.GetQueryParams(Result, Params);
      if Params.Count > 0 then
      begin
        if ParamDataSet <> nil then
        begin
          for I := 0 to Params.Count - 1 do
          begin
            Fld := ParamDataSet.FindField(Params[I].Name);
            if Fld <> nil then
              Params[I].Value := Fld.Value;
          end;
        end else if ParamNames <> '' then
        begin
          if Pos(';', ParamNames) > 0 then
          begin
            P := 1;
            I := 0;
            repeat
              ParamName := NextToken(ParamNames, ';', P);
              if ParamName <> '' then
              begin
                Param := Params.FindParam(ParamName);
                if Param <> nil then
                  Param.Value := ParamValues[I];
              end;
              Inc(I);
            until P > Length(ParamNames);
          end else begin
            Param := Params.FindParam(ParamNames);
            if Param <> nil then
              Param.Value := ParamValues;
          end;
        end;
        Database.SetQueryParams(Result, Params);
      end;
    finally
      Params.Free;
    end;

    // if ReportProgress then
      // OnQueryProgress := DoQueryProgress;
    if ReturnSet then
      Active := True  // caller is responsible for destroying returned result set
    else begin
      Database.ExecSQL(Result);
      Free;
      Result := nil; // Return nil if set is not expected
    end;
  except
    Free;
    raise;
  end;
end;

procedure ExecuteErrorConstraints(Database: IDatabaseExt; Table: TDataSet;
  ChangeType: TChangeType; TableDef: TTableDefinition);
var
  I: Integer;
  OldKeyValues, NewKeyValues: Variant;
  KeyChanged: Boolean;
  OldConditionValue, ConditionValue: Boolean;
  Cursor: TDBRangeCursor;
  VirtualKey: String;
begin
  if TableDef = nil then exit;
  if not (ChangeType in [ctDeleted, ctInserted, ctModified]) then exit;

  for I := 0 to TableDef.Relations.Count - 1 do
  with TableDef.Relations[I] do
  if EnforceForeignKey then
  begin
    EvaluateCondition(Table, Condition, ConditionValue, OldConditionValue);
    // Only process records, that satisfy the condition
    if ConditionValue or OldConditionValue then
    begin
      if ConditionValue <> OldConditionValue then
      begin
        if ConditionValue then
          ChangeType := ctInserted
        else ChangeType := ctDeleted;
      end;
      if ((ChangeType = ctDeleted) and (EffectiveDeleteAction = raError)) or
       ((ChangeType = ctModified) and (EffectiveUpdateAction = raError)) then
      begin
        KeyChanged := GetKeyValues(Table, KeyFields, OldKeyValues, NewKeyValues);
        if (ChangeType = ctModified) and (not KeyChanged) then continue;
        if ChangeType = ctDeleted then
          OldKeyValues := NewKeyValues;
        if VarIsNullExt(OldKeyValues) then continue;
        Cursor := Database.GetRangeCursor(TableDef.Relations[I], OldKeyValues);
        try
          { Error if referenced record exists }
          if not Cursor.DataSet.EOF then
          begin
            if ChangeType = ctDeleted then DatabaseError(EffectiveDeleteErrorMessage)
            else DatabaseError(EffectiveUpdateErrorMessage);
          end;
        finally
          Cursor.Free;
        end;
      end else if (ChangeType in [ctModified, ctInserted]) and RequireOneRecord then
      begin
        // Here we process references, that require validation
        GetKeyValues(Table, KeyFields, OldKeyValues, NewKeyValues);
        if VarIsNullExt(NewKeyValues) then continue; { allow null reference }

        VirtualKey := GetVirtualKey(ForeignTable, ForeignKeyFields, NewKeyValues, CaseInsensitive);
        if Database.VirtualKeyExists(VirtualKey) then continue;

        Cursor := Database.GetRangeCursor(TableDef.Relations[I], NewKeyValues);
        try
          { Otherwise if it's not null, make sure, that referenced record exists }
          if Cursor.DataSet.EOF then
            DatabaseError(EffectiveRequireRecordErrorMessage);
        finally
          Cursor.Free;
        end;
      end;
    end; { if condition }
  end;
end;

procedure ExecuteCascadeOperations(Database: IDatabaseExt; Table: TDataSet;
  ChangeType: TChangeType; TableDef: TTableDefinition);
var
  I: Integer;
  OldKeyValues, NewKeyValues: Variant;
  KeyChanged: Boolean;
  Cursor: TDBRangeCursor;
  VirtualIdx: Integer;
  ConditionValue: Boolean;
  OldConditionValue: Boolean;
begin
  if TableDef = nil then exit;
  if not (ChangeType in [ctDeleted, ctModified]) then exit;

  // Process cascade constraints
  for I := 0 to TableDef.Relations.Count - 1 do
  with TableDef.Relations[I] do
  if EnforceForeignKey then
  begin
    EvaluateCondition(Table, Condition, ConditionValue, OldConditionValue);
    // Only process records, that satisfy the condition
    if ConditionValue or OldConditionValue then
    begin
      if ConditionValue <> OldConditionValue then
      begin
        if ConditionValue then
          ChangeType := ctInserted
        else ChangeType := ctDeleted;
      end;
      if ChangeType = ctModified then
      case EffectiveUpdateAction of
        raCascade:
          begin
            KeyChanged := GetKeyValues(Table, KeyFields, OldKeyValues, NewKeyValues);
            if (not KeyChanged) or VarIsNullExt(OldKeyValues) then continue;
            Cursor := Database.GetRangeCursor(TableDef.Relations[I], OldKeyValues);
            try
              if not Cursor.DataSet.EOF then
                if not Database.GetInTransaction then
                  Database.StartTransaction; {FIX:1.20}
              while not Cursor.DataSet.EOF do
              begin
                VirtualIdx := Database.AddVirtualKey(TableDef.TableName,
                  KeyFields, NewKeyValues, CaseInsensitive);
                try
                  { Record moves after assigning new key values }
                  AssignFields(Cursor.DataSet, Cursor.KeyFieldsList, NewKeyValues);
                finally
                  Database.DeleteVirtualKey(VirtualIdx);
                end;
              end;
            finally
              Cursor.Free;
            end;
          end;
        raNullify:
          begin
            KeyChanged := GetKeyValues(Table, KeyFields, OldKeyValues, NewKeyValues);
            if (not KeyChanged) or VarIsNullExt(OldKeyValues) then continue;
            Cursor := Database.GetRangeCursor(TableDef.Relations[I], OldKeyValues);
            try
              if not Cursor.DataSet.EOF then
                if not Database.GetInTransaction then
                  Database.StartTransaction; {FIX:1.20}
              while not Cursor.DataSet.EOF do
                ClearFields(Cursor.DataSet, Cursor.KeyFieldsList);
            finally
              Cursor.Free;
            end;
          end;
      end {case}
      else if ChangeType = ctDeleted then
      case EffectiveDeleteAction of
        raCascade:
          begin
            GetKeyValues(Table, KeyFields, OldKeyValues, NewKeyValues);
            if VarIsNullExt(NewKeyValues) then continue;
            Cursor := Database.GetRangeCursor(TableDef.Relations[I], NewKeyValues);
            try
              if not Cursor.DataSet.EOF then
                if not Database.GetInTransaction then
                  Database.StartTransaction; {FIX:1.20}
              while not Cursor.DataSet.EOF do
                Cursor.DataSet.Delete;
            finally
              Cursor.Free;
            end;
          end;
        raNullify:
          begin
            GetKeyValues(Table, KeyFields, OldKeyValues, NewKeyValues);
            if VarIsNullExt(NewKeyValues) then continue;
            Cursor := Database.GetRangeCursor(TableDef.Relations[I], NewKeyValues);
            try
              if not Cursor.DataSet.EOF then
                if not Database.GetInTransaction then
                  Database.StartTransaction; {FIX:1.20}
              while not Cursor.DataSet.EOF do
                ClearFields(Cursor.DataSet, Cursor.KeyFieldsList);
            finally
              Cursor.Free;
            end;
          end;
      end; { case }
    end; { if condition }
  end; { with }
end;

procedure ExecuteAggregates(Database: IDatabaseExt; Table: TDataSet;
  ChangeType: TChangeType; TableDef: TTableDefinition);
var
  I, VirtualIdx: Integer;
  OldKeyValues, NewKeyValues: Variant;
  ValueChange, Value, OldValue: Variant;
  ExprToken: TEvaluator;
  KeyChanged: Boolean;
  ConditionValue: Boolean;
  OldConditionValue: Boolean;

  procedure UpdateAggField(AggregateLink: TAggregateLink; KeyValues, Delta: Variant);
  var
    Cursor: TDBRangeCursor;
  begin
    if Delta = 0 then exit;

    if not Database.GetInTransaction then Database.StartTransaction;
    with Database do
    if GetReplicating then
    begin
      // Schedule update to be executed later
      GetActiveTransaction.GetAggregateLinkData(AggregateLink).AddKey(EncodeVariant(KeyValues));
    end else begin
      // Update value immediately
      Cursor := GetRangeCursor(AggregateLink.ForeignTable, AggregateLink.ForeignKeyFields,
        AggregateLink.ForeignCondition, AggregateLink.CaseInsensitive, KeyValues);
      // Cursor := GetRangeCursor(AggregateLink, KeyValues);
      try
        // ASSUMPTION: There's only one record!
        if Cursor.DataSet.EOF then
        begin
          Cursor.DataSet.Insert;
          AssignKeyFields(Cursor.KeyFieldsList, KeyValues);
        end else Cursor.DataSet.Edit;
        try
          with Cursor.DataSet.FieldByName(AggregateLink.ForeignFieldName) do
            Value := Value + Delta;
          Cursor.DataSet.Post;
        except
          Cursor.DataSet.Cancel;
          raise;
        end;
      finally
        Cursor.Free;
      end;
    end;
  end;

begin
  if TableDef = nil then exit;
  if not (ChangeType in [ctDeleted, ctInserted, ctModified]) then exit;

  // Process aggregated fields
  for I := 0 to TableDef.AggregateLinks.Count - 1 do
  with TableDef.AggregateLinks[I] do
  begin
    // Update foreign incremental aggregated [summary/count] field
    KeyChanged := GetKeyValues(Table, KeyFields, OldKeyValues, NewKeyValues);
    EvaluateCondition(Table, Condition, ConditionValue, OldConditionValue);
    if AggregateExpression <> '' then
    begin
      ExprToken := TEvaluator.Create(AggregateExpression, Table, TDataSetContext);
      try
        Value := ExprToken.Evaluate;
        ExprToken.ContextClass := TDataSetContextOldVal;
        OldValue := ExprToken.Evaluate;
      finally
        ExprToken.Free;
      end;
      if AggregateType = aCount then
      begin
        if VarIsNull(Value) then Value := 0 else Value := 1;
        if VarIsNull(OldValue) then OldValue := 0 else OldValue := 1;
      end;
    end else if AggregateType = aCount then
    begin
      { Allow empty aggregate expression for count - in this case it works like count(*) }
      Value := 1;
      OldValue := 1;
    end else
      DatabaseErrorFmt(SInvalidAggregateExpression, [ForeignTable, ForeignFieldName]);

    case ChangeType of
      ctInserted:
        if ConditionValue and not VarIsNull(Value) then
        begin
          // Pretend that this record is already posted
          VirtualIdx := Database.AddVirtualKey(TableDef.TableName, KeyFields, NewKeyValues, CaseInsensitive);
          try
            UpdateAggField(TableDef.AggregateLinks[I], NewKeyValues, Value);
          finally
            Database.DeleteVirtualKey(VirtualIdx);
          end;
        end;
      ctModified: begin
        if KeyChanged then
        begin
          if RelationType = rtOneToOne then
          begin
            // Sum is effectively transformed to copy
            // Key change should be handled by Cascade operation
            if ConditionValue then
            begin
              ValueChange := VarDiff(Value, OldValue);
              if (not VarIsNull(ValueChange)) and (ValueChange <> 0) then
              begin
                // Pretend that this record is already posted
                VirtualIdx := Database.AddVirtualKey(TableDef.TableName, KeyFields, NewKeyValues, CaseInsensitive);
                try
                  UpdateAggField(TableDef.AggregateLinks[I], NewKeyValues, ValueChange);
                finally
                  Database.DeleteVirtualKey(VirtualIdx);
                end;
              end;
            end;
          end else begin
            if OldConditionValue and not VarIsNull(OldValue) then
              UpdateAggField(TableDef.AggregateLinks[I], OldKeyValues, - OldValue);
            if ConditionValue and not VarIsNull(Value) then
              UpdateAggField(TableDef.AggregateLinks[I], NewKeyValues, Value);
          end;
        end else if ConditionValue <> OldConditionValue then
        begin
          if ConditionValue and not VarIsNull(Value) then
            UpdateAggField(TableDef.AggregateLinks[I], NewKeyValues, Value)
          else if (not VarIsNull(OldValue)) and OldConditionValue then
            UpdateAggField(TableDef.AggregateLinks[I], OldKeyValues, - OldValue);
        end else if ConditionValue then begin
          ValueChange := VarDiff(Value, OldValue);
          if (not VarIsNull(ValueChange)) and (ValueChange <> 0) then
            UpdateAggField(TableDef.AggregateLinks[I], NewKeyValues, ValueChange);
        end;
      end;
      ctDeleted:
        if (not VarIsNull(Value)) and ConditionValue then
          UpdateAggField(TableDef.AggregateLinks[I], NewKeyValues, - Value);
    end; { case }
  end;
end;

procedure ExecuteScheduledAggregates(Database: IDatabaseExt);
var
  I, K: Integer;
  UpdatedValue: Variant;
  Value, KeyValues: Variant;
  TExprToken: TEvaluator;
  ExprToken: PEvaluator;
  Cursor: TDBRangeCursor;
begin
  with Database do
  for I := 0 to GetActiveTransaction.Aggregates.Count - 1 do
  with TAggregateLinkData(GetActiveTransaction.Aggregates[I]) do
  begin
    if HasKeys then
    begin
      if not GetInTransaction then StartTransaction;
      for K := 0 to UpdateKeys.Count - 1 do
      begin
        // Update aggregated field
        KeyValues := DecodeVariant(UpdateKeys[K]);
        // Calculate value
        // ExprToken := nil;
        with AggregateLink do
          Cursor := GetRangeCursor(TableDef.TableName, KeyFields, Condition,
            CaseInsensitive, KeyValues);
        try
          // ASSUMPTION: There's only one record!
          UpdatedValue := 0;
          Cursor.DataSet.First;
          ExprToken := nil;
          if AggregateLink.AggregateExpression <> '' then
          begin
            TExprToken.Create(AggregateLink.AggregateExpression, Cursor.DataSet, TDataSetContext);
            ExprToken := @TExprToken;
          end;
          while not Cursor.DataSet.EOF do
          begin
            if ExprToken <> nil then
            begin
              Value := ExprToken.Evaluate;
              if AggregateLink.AggregateType = aCount then
              begin
                if VarIsNull(Value) then Value := 0 else Value := 1;
              end;
            end else
              Value := 1;
            UpdatedValue := UpdatedValue + Value;
            Cursor.DataSet.Next;
          end;
        finally
          Cursor.Free;
        end;

        // Locate record and assign value
        // Cursor := GetRangeCursor(AggregateLink, KeyValues);
        Cursor := GetRangeCursor(AggregateLink.ForeignTable, AggregateLink.ForeignKeyFields,
          AggregateLink.ForeignCondition, AggregateLink.CaseInsensitive, KeyValues);
        try
          // ASSUMPTION: There's only one record!
          if Cursor.DataSet.EOF then
          begin
            Cursor.DataSet.Insert;
            AssignKeyFields(Cursor.KeyFieldsList, KeyValues);
          end else Cursor.DataSet.Edit;
          try
            // Assign value
            Cursor.DataSet.FieldByName(AggregateLink.ForeignFieldName).Value := UpdatedValue;
            Cursor.DataSet.Post;
          except
            Cursor.DataSet.Cancel;
            raise;
          end;
        finally
          Cursor.Free;
        end;
      end;
      ClearKeys;
    end;
  end;
end;

procedure ObjectChanged(Database: IDatabaseExt; Table: TDataSet; ChangeType: TChangeType;
  TableDef: TTableDefinition);
var
  OldKeyValues, NewKeyValues: Variant;
  KeyChanged: Boolean;
  I: Integer;
  Cursor: TDBRangeCursor;
  FrgnTableDef: TTableDefinition;
  ChildChangeType: TChangeType;
  UpdateReferences: Boolean;
begin
  { if not ((TableDef <> nil) and TableDef.Replicate
    and CheckSystemTable and CheckObjectsTable) then exit; }
  if TableDef = nil then exit;

  UpdateReferences := True;

  with Database do
  if TableDef.MasterRecord and (TableDef.GetObjectType <> '') and (TableDef.ObjectKeyFields <> '') then
  begin
    if not GetInTransaction then StartTransaction;

    KeyChanged := GetKeyValues(Table, TableDef.ObjectKeyFields, OldKeyValues, NewKeyValues);

    // Mark original Key as 'deleted' if KeyValue is modified
    if (ChangeType = ctModified) and KeyChanged and (not VarIsNullExt(OldKeyValues)) then
      GetActiveTransaction.WriteChange(TableDef, OldKeyValues, ctDeleted);

    // Update references if the state of this object is changed
    UpdateReferences := GetActiveTransaction.WriteChange(TableDef, NewKeyValues, ChangeType);
  end;

  // Iterate relations and mark objects as ctModifiedContent or ctModifiedReference
  if UpdateReferences then
  for I := 0 to TableDef.Relations.Count - 1 do
  with TableDef.Relations[I] do
  if EnforceForeignKey then
    // Notify mandatory relations (parent or other) that referenced record has changed
    if RequireOneRecord or (RelationKind = rkParent) then
    begin
      GetKeyValues(Table, KeyFields, OldKeyValues, NewKeyValues);
      if VarIsNullExt(NewKeyValues) then continue;
      Cursor := Database.GetRangeCursor(TableDef.Relations[I], NewKeyValues);
      try
        if (RelationKind = rkParent) and (ChangeType <> ctModifiedReference) then
          ChildChangeType := ctModifiedContent
        else ChildChangeType := ctModifiedReference;
        FrgnTableDef := Schema.GetTableDef(Cursor.DataSet);
        while not Cursor.DataSet.EOF do
        begin
          ObjectChanged(Database, Cursor.DataSet, ChildChangeType, FrgnTableDef);
          Cursor.DataSet.Next;
        end;
      finally
        Cursor.Free;
      end;
    end;
end;

procedure WriteObjectChanges(Database: IDatabaseExt);
var
  I: Integer;
  ObjectTableDef: TTableDefinition;
  ActiveTrans: TActiveTransaction;
begin
  with Database do
  begin
    if Schema = nil then
      DatabaseError(SDatabaseSchemaIsNotAssigned);

    if not CheckObjectsTable then
      DatabaseError(SObjectsTableNotFound);

    ActiveTrans := GetActiveTransaction;
    if ActiveTrans <> nil then
    for I := 0 to ActiveTrans.ChangedObjects.Count - 1 do
    with TObjectState(ActiveTrans.ChangedObjects[I]) do
    begin
      ObjectTableDef := Schema.GetObjectTable(ObjectType);
      if Assigned(ObjectTableDef) and ObjectTableDef.Replicate then
      begin
        if not Database.FindKey(ObjectsTable.DataSet, [ObjectType, ObjectStrKey]) then
        begin
          ObjectsTable.DataSet.Insert;
          ObjectsTable.ObjectType.AsString := ObjectType;
          ObjectsTable.ObjectKey.AsString := ObjectStrKey;
        end else begin
          // if it has already been changed within the same replication
          // then don't write ModifiedReference
          if (ObjectsTable.ReplicationID.AsInteger = ActiveTrans.ReplicationID)
            and (ChangeType = ctModifiedReference) then continue;
          ObjectsTable.DataSet.Edit;
        end;
        try
          ObjectsTable.SnapshotID.AsInteger := SnapshotID;
          ObjectsTable.Timestamp.AsDateTime := ActiveTrans.TimeStamp;
          ObjectsTable.ReplicationID.AsInteger := ActiveTrans.ReplicationID;
          ObjectsTable.ChangeType.AsInteger := Integer(ChangeType);
          ObjectsTable.ChangeStatus.AsInteger := Integer(ChangeStatus);
          if UserName = '' then
            ObjectsTable.UserName.AsString := Database.UserName
          else ObjectsTable.UserName.AsString := UserName;
          ObjectsTable.DataSet.Post;
        except
          ObjectsTable.DataSet.Cancel;
          raise;
        end;
      end;
      if Assigned(ObjectTableDef) then
        ExecuteTableTriggers(Database, ObjectKey, ChangeType, ttUponCommit, ObjectTableDef);
    end;
  end;
end;

procedure ExecuteTableTriggers(Database: IDatabaseExt; Table: TDataSet;
  ChangeType: TChangeType; TriggerType: TTriggerType; TableDef: TTableDefinition);
var
  I: Integer;
  Trigger: TTriggerDefinition;
begin
  // Table triggers are executed before change is posted to a table
  // Table triggers can't have access to a referred Table other then
  // via Table reference passed here as a parameter
  if TableDef = nil then exit;
  for I := 0 to TableDef.Triggers.Count - 1 do
  begin
    Trigger := TableDef.Triggers[I];
    if (Trigger.TriggerActive in Database.GetEnabledTriggers)
      and (ChangeType in Trigger.TriggerWhen)
      and (Trigger.TriggerType = TriggerType)
    then with Database do begin
      if not GetInTransaction then StartTransaction; {FIX:1.20}
      ExecuteSQLParam(Database, Trigger.SQLScript, '', NULL, Table, False, False);
    end;
  end;
end;

procedure ExecuteTableTriggers(Database: IDatabaseExt; ObjectKey: Variant;
  ChangeType: TChangeType; TriggerType: TTriggerType; TableDef: TTableDefinition);
var
  I: Integer;
  Trigger: TTriggerDefinition;
begin
  // Table triggers are executed before change is posted to a table
  // Table triggers can't have access to a referred Table other then
  // via Table reference passed here as a parameter
  if TableDef = nil then exit;
  for I := 0 to TableDef.Triggers.Count - 1 do
  begin
    Trigger := TableDef.Triggers[I];
    if (Trigger.TriggerActive in Database.GetEnabledTriggers)
      and (ChangeType in Trigger.TriggerWhen)
      and (Trigger.TriggerType = TriggerType)
    then begin
      with Database do
        if not GetInTransaction then StartTransaction; {FIX:1.20}
      ExecuteSQLParam(Database, Trigger.SQLScript, TableDef.ObjectKeyFields, ObjectKey, nil, False, False);
    end;
  end;
end;

function ExecuteTriggers(Database: IDatabaseExt; DataSet: TDataSet; ChangeType: TChangeType; UpdateOptions: TUpdateOptions): TTableDefinition;
var
  UpdOpt: TUpdateOptions;
begin
  Result := nil;
  with Database do
  begin
    if Schema = nil then exit;
    UpdOpt := GetUpdateOptions * UpdateOptions;
    if UpdOpt = uoNone then exit;
    Result := Schema.GetTableDef(DataSet, '');
    if Result = nil then exit;

    // Make sure that schema is prepared
    Schema.Prepare; { if not prepared already }

    if uoEnableErrorConstraints in UpdOpt then
      ExecuteErrorConstraints(Database, DataSet, ChangeType, Result);
    if uoEnableCascadeConstraints in UpdOpt then
      ExecuteCascadeOperations(Database, DataSet, ChangeType, Result);
    if uoEnableTriggers in UpdOpt then
      ExecuteTableTriggers(Database, DataSet, ChangeType, ttBefore, Result);
    if uoEnableAggregates in UpdOpt then
      ExecuteAggregates(Database, DataSet, ChangeType, Result);

    // Only mark object as changed if it is not Insert
    // Otherwise we'll have to do it *after* post is complete
    if (ChangeType <> ctInserted) and
       ((uoEnableChangeTracking in UpdOpt)
       or (uoEnableTriggers in UpdOpt))
    then
      ObjectChanged(Database, DataSet, ChangeType, Result);
  end;
end;

(*
procedure ExecuteUpdate(Database: IDatabaseExt;
  DatabaseUpdate: TDatabaseUpdate; var Abort: Boolean);
begin
  Abort := False;
  with DatabaseUpdate do
  begin
    try
      if Trim(SQLScript) <> '' then
        ExecuteSQL(Database, SQLScript, False, False);
    except
      if not IgnoreSQLError then raise;
    end;
  end;
end;
*)

function GetVersion(Database: IDatabaseExt): TSchemaVersion;
begin
  Result := SchemaVersion(-1,-1);
  if not Database.Connected then exit;
  with Database do
  if CheckSystemTable then
    Result := SchemaVersion(SystemTable.MajorVersion.AsInteger, SystemTable.MinorVersion.AsInteger);
end;

procedure SetVersion(Database: IDatabaseExt; const Value: TSchemaVersion);
var
  B: TStream;
begin
  if Database.Schema = nil then
    DatabaseError(SDatabaseSchemaIsNotAssigned);
  if not Database.CheckSystemTable(True) then
    DatabaseError(SSystemTableNotFound);
  // Write version and other stuff to the 'system' table
  with Database.SystemTable do
  begin
    // DataSet.Refresh;
    DataSet.Edit;
    try
      SchemaName.AsString := Database.Schema.SchemaName;
      MajorVersion.AsInteger := Value.Major;
      MinorVersion.AsInteger := Value.Minor;
      if Database.Schema.Version.IntVer = Value.IntVer then
      begin
        B := DataSet.CreateBlobStream(Schema, bmWrite);
        try
          B.Size := 0;
          B.Position := 0;
          Database.Schema.SaveToStream(B);
        finally
          B.Free;
        end;
      end;
      DataSet.Post;
    except
      DataSet.Cancel;
      raise;
    end;
  end;
end;

(*
function UpdateDatabase(Database: IDatabaseExt): Boolean;
var
  I: Integer;
  Ver: TSchemaVersion;
  Counter: TProgressCounter;
begin
  Result := False;
  with Database do
  begin
    if Schema = nil then
      DatabaseError(SDatabaseSchemaIsNotAssigned);
    // Get current version
    Ver := SchemaVersion(-1, -1);
    if CheckSystemTable then
      Ver := SchemaVersion(Database.SystemTable.MajorVersion.AsInteger, Database.SystemTable.MinorVersion.AsInteger);
    // Lock system table
    LockDatabase;
    try
      InitMinMax(Counter, 0, Schema.Updates.Count - 1);
      DoProgress(SUpdatingDatabaseVersion, 0, Counter.Abort);
      try
        for I := 0 to Schema.Updates.Count - 1 do
        begin
          if Counter.Progress then
            DoProgress(sUpdatingDatabaseVersion, Counter.PercentDone, Counter.Abort);
          if Counter.Abort then exit;
          if CompareVersions(Schema.Updates[I].Version, Ver) > 0 then
          begin
            ExecuteUpdate(Database, Schema.Updates[I], Counter.Abort);
            Ver := Schema.Updates[I].Version;
          end;
        end;
        // Always update version to the Schema Version at the end
        Ver := Schema.Version;
        Result := True;
      finally
        // Report end of task progress
        DoProgress(sUpdatingDatabaseVersion, 100, Counter.Abort);
      end;
    finally
      // Write version to system table
      with Database.SystemTable do
      if CompareVersions(Ver, SchemaVersion(MajorVersion.Value, MinorVersion.Value)) > 0 then
        Database.SetVersion(Ver);
      // UnLock system table
      UnLockDatabase;
    end;
  end;
end;
*)

type
{$HINTS OFF}
  // MIGHT NEED TO BE CHANGED FOR DELPHI 2005 !!!
  TFriendlyDataSet = class(TComponent)
  private
    FFields: TFields;
    FAggFields: TFields;
    FFieldDefs: TFieldDefs;
    FFieldDefList: TFieldDefList;
    FFieldList: TFieldList;
    FDataSources: TList;
    FFirstDataLink: TDataLink;
    FBufferCount: Integer;
    FRecordCount: Integer;
    FActiveRecord: Integer;
    FCurrentRecord: Integer;
    FBuffers: TBufferList;
    FCalcBuffer: TRecordBuffer;
  end;
{$HINTS ON}

procedure SetPrivateCalcBuffer(DataSet: TDataSet; Buffer: TRecordBuffer);
begin
  TFriendlyDataSet(DataSet).FCalcBuffer := Buffer;
end;

procedure CalculateFieldsExt(Buffer: TRecordBuffer; DataSet: TDataSet);
var
  I: Integer;
  Idx: Integer;
  LookupsList: TStringList;
  FieldsList: TList;
  ResultValues: Variant;
  FirstField: TField;
  ResultFields, LookupID: String;
begin
  SetPrivateCalcBuffer(DataSet, Buffer);

  with DataSet do
  if State <> dsInternalCalc then
  begin
    // ClearCalcFields(CalcBuffer);
    LookupsList := TStringList.Create;
    try
      for I := 0 to Fields.Count - 1 do
      begin
        with Fields[I] do
          if FieldKind = fkLookup then
          begin
            // CalcLookupValue -- had to copy it here, because it's declared private (?)
            if LookupCache then
              Value := LookupList.ValueOfKey(DataSet.FieldValues[KeyFields])
            else if (LookupDataSet <> nil) and LookupDataSet.Active then
            begin
              LookupID := Format('%d|%s|%s', [Integer(LookupDataSet), LookupKeyFields, KeyFields]);
              Idx := LookupsList.IndexOf(LookupID);
              if Idx < 0 then
                Idx := LookupsList.AddObject(LookupID, TList.Create);
              TList(LookupsList.Objects[Idx]).Add(Fields[I]);
            end;
          end;
      end;
      // Process lookups
      for I := 0 to LookupsList.Count - 1 do
      begin
        FieldsList := TList(LookupsList.Objects[I]);
        if FieldsList.Count = 0 then continue; // This should never happen, but who knows...
        FirstField := TField(FieldsList[0]);
        ResultFields := FirstField.LookupResultField;
        with FirstField do
          if FieldsList.Count = 1 then
            Value := LookupDataSet.Lookup(LookupKeyFields, FieldValues[KeyFields], ResultFields)
          else begin
            for Idx := 1 to FieldsList.Count - 1 do
              ResultFields := ResultFields + ';' + TField(FieldsList[Idx]).LookupResultField;
            ResultValues := LookupDataSet.Lookup(LookupKeyFields, FieldValues[KeyFields], ResultFields);
            if not VarIsNull(ResultValues) then
              for Idx := 0 to FieldsList.Count - 1 do
                TField(FieldsList[Idx]).Value := ResultValues[Idx];
          end;
      end;
    finally
      for I := 0 to LookupsList.Count - 1 do
        LookupsList.Objects[I].Free;
      LookupsList.Free;
    end;
  end;
end;

procedure UpdateFieldsProperties(DataSet: TDataSet; Database: TObject);
begin
  if (Database <> nil) and IsPublishedProp(Database, 'Schema') then
    UpdateFieldsFromSchema(DataSet, TDatabaseSchema(GetOrdProp(Database, 'Schema')));
end;

procedure UpdateFieldsFromSchema(DataSet: TDataSet; Schema: TDatabaseSchema);
var
  I, P, TblIdx, FldIdx: Integer;
  OriginTable, OriginField: String;
  LastOriginTable, TempStr: String;
  FldDef: TFieldDefinition;
  Instance: TField;
begin
  if Schema = nil then exit;
  LastOriginTable := '';
  TblIdx := -1;
  for I := 0 to DataSet.FieldCount - 1 do
  begin
    Instance := DataSet.Fields[I];
    with Instance do
    begin
      P := 1;
      OriginTable := NextToken(Origin, '.', P);
      OriginField := NextToken(Origin, '.', P);
      if not AnsiSameText(OriginTable, LastOriginTable) then
      begin
        TblIdx := Schema.TableDefs.IndexOf(OriginTable);
        LastOriginTable := OriginTable;
      end;
      if TblIdx < 0 then continue;
      FldIdx := Schema.TableDefs[TblIdx].FieldDefs.IndexOf(OriginField);
      if FldIdx < 0 then continue;
      FldDef := Schema.TableDefs[TblIdx].FieldDefs[FldIdx];

      { Assign Display Label if it hasn't been assigned yet }
      if (FldDef.DisplayLabel <> '') and (not IsStoredProp(Instance, propDisplayLabel)) then
        DisplayLabel := FldDef.DisplayLabel;
      if (FldDef.DisplayWidth <> 0) and (not IsStoredProp(Instance, propDisplayWidth)) then
        DisplayWidth := FldDef.DisplayWidth;

      if FldDef.DisplayFormat <> '' then
        if IsPublishedProp(Instance, propDisplayFormat) then
        begin
          TempStr := GetStrProp(Instance, propDisplayFormat);
          if TempStr = '' then
            SetStrProp(Instance, propDisplayFormat, FldDef.DisplayFormat);
        end;

      if FldDef.DefaultExpression <> '' then
        if IsPublishedProp(Instance, propDefaultExpression) then
        begin
          TempStr := GetStrProp(Instance, propDefaultExpression);
          if TempStr = '' then
            SetStrProp(Instance, propDefaultExpression, FldDef.DefaultExpression);
        end;

      if FldDef.EditMask <> '' then
        if isPublishedProp(Instance, propEditMask) then
        begin
          TempStr := GetStrProp(Instance, propEditMask);
          if TempStr = '' then
            SetStrProp(Instance, propEditMask, FldDef.EditMask);
        end;
    end;
  end;
end;

function GetRelativePath(const Dir: String): String;
begin
  {$IFDEF VER130}
  Result := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0))) + Dir;
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + Dir;
  {$ENDIF}
end;

function VarDiff(NewValue, OldValue: Variant): Variant;
begin
  if VarIsNull(OldValue) then
  begin
    if not VarIsNull(NewValue) then
      Result := NewValue
    else Result := NULL;
  end else begin
    if VarIsNull(NewValue) then
      Result := -OldValue
    else Result := NewValue - OldValue;
  end;
end;

procedure WriteFieldValue(Writer: TWriter; Field: TField);
var
  Temp: Integer;
  BlobStream: TStringStream;
begin
  Temp := Integer(ftUnknown);
  if Field.IsNull then
    Writer.WriteInteger(Temp) // Type = ftUnknown for NULL fields
  else begin
    Temp := Integer(Field.DataType);
    Writer.WriteInteger(Temp);
    case Field.DataType of
      ftFmtMemo, ftParadoxOle, ftDBaseOle,
      ftTypedBinary, ftBlob, ftMemo, ftGraphic:
      begin
        BlobStream := TStringStream.Create('');
        try
          TBlobField(Field).SaveToStream(BlobStream);
          Writer.WriteString(BlobStream.DataString);
        finally
          BlobStream.Free;
        end;
      end;
      ftBoolean: Writer.WriteBoolean(Field.AsBoolean);
      ftSmallint, ftInteger, ftWord,
      ftAutoInc: Writer.WriteInteger(Field.AsInteger);
      ftFloat, ftCurrency: Writer.WriteFloat(Field.AsFloat);
      ftDate, ftTime, ftDateTime: Writer.WriteDate(Field.AsDateTime);
      else { ftString and others }
        Writer.WriteString(Field.AsString);
    end;
  end;
end;

procedure ReadFieldValue(Reader: TReader; Field: TField);
var
  Temp: Integer;
  BlobStream: TStringStream;
begin
  Temp := Reader.ReadInteger;
  if TFieldType(Temp) = ftUnknown then
    Field.Clear
  else
    case TFieldType(Temp) of
      ftFmtMemo, ftParadoxOle, ftDBaseOle,
      ftTypedBinary, ftBlob, ftMemo, ftGraphic:
      begin
        BlobStream := TStringStream.Create(Reader.ReadString);
        try
          TBlobField(Field).LoadFromStream(BlobStream);
        finally
          BlobStream.Free;
        end;
      end;
      ftBoolean: Field.AsBoolean := Reader.ReadBoolean;
      ftSmallint, ftInteger, ftWord,
      ftAutoInc: Field.AsInteger := Reader.ReadInteger;
      ftFloat, ftCurrency: Field.AsFloat := Reader.ReadFloat;
      ftDate, ftTime, ftDateTime: Field.AsDateTime := Reader.ReadDate;
      else { ftString, etc } Field.AsString := Reader.ReadString;
    end;
end;

procedure SkipFieldValue(Reader: TReader);
begin
  case TFieldType(Reader.ReadInteger) of
    ftUnknown:;
    ftBoolean: Reader.ReadBoolean;
    ftSmallint, ftInteger, ftWord, ftAutoInc: Reader.ReadInteger;
    ftFloat, ftCurrency: Reader.ReadFloat;
    ftDate, ftTime, ftDateTime: Reader.ReadDate;
    else Reader.ReadString;
  end;
end;

function ExecuteVerbExt(Databases: TList; DatabaseID, Verb: Integer;
  var Data: OleVariant): Integer;
const
  { Database Verbs }
  dvCreateDatabase = 1;
  dvReverseEngineer = 2;
  dvGetVersion = 3;
  dvSetVersion = 4;
  dvExecuteSQL = 5;
  dvGetSystemTableName = 6;
  dvSetSystemTableName = 7;

var
  Idx: Integer;
  ResultSet: TDataSet;
  DB: ISchemaDatabase;
begin
  if not Supports(TObject(DatabaseID), ISchemaDatabase, DB) then
    DatabaseError(SInvalidDatabaseHandle);

  Idx := Databases.IndexOf(TObject(DatabaseID));
  Result := 0;
  if (Idx < 0) or (DatabaseID = 0) then
    DatabaseError(SInvalidDatabaseHandle)
  else with DB do
  begin
    if Schema = nil then
      Schema := TDatabaseSchema.Create(TComponent(DatabaseID));

    Connected := True;
    case Verb of
      // dvCreateDatabase:; not used currently  dbSchema
      dvReverseEngineer: begin
        Schema.Clear;
        ReverseEngineer;
        Data := StrToVarArray(Schema.SaveToStr);
      end;
      dvGetSystemTableName: Data := Schema.SystemTableName;
      dvSetSystemTableName: Schema.SystemTableName := VarToStr(Data);
      dvGetVersion: Data := VersionToStr(GetVersion);
      dvSetVersion: SetVersion(StrToVersion(Data));
      dvExecuteSQL: begin
        ResultSet := nil;
        ExecuteStatement(Data, @ResultSet);
        Data := Integer(ResultSet);
      end;
      else
        DatabaseError(SOperationNotSupported);
    end;
  end;
end;

{ TDataSetContext }

class function TDataSetContext.GetItem(context: pointer;
  const name: string): variant;
begin
  Result := TDataSet(context).FieldByName(name).Value;
end;

{ TDataSetContextOldVal }

class function TDataSetContextOldVal.GetItem(context: pointer;
  const name: string): variant;
begin
  Result := TDataSet(context).FieldByName(name).OldValue;
end;

{ TProgressCounter }

procedure Init(var Counter: TProgressCounter);
begin
  with Counter do 
  begin
    PercentDone := 0;
    MinValue := 0;
    MaxValue := 100;
    Value := MinValue;
    Abort := False;
  end;
end;

procedure InitMinMax(var Counter: TProgressCounter; AMinValue, AMaxValue: Integer);
begin
  with Counter do 
  begin
    PercentDone := 0;
    MinValue := AMinValue;
    MaxValue := AMaxValue;
    if MaxValue <= MinValue then
      MaxValue := MinValue + 1;
    Value := MinValue;
    Abort := False;
  end;
end;

function Progress(var Counter: TProgressCounter; Increment: Integer = 1): Boolean;
var
  NewPercent: Byte;
begin
  with Counter do 
  begin
    Value := Value + Increment;
    NewPercent := Trunc(100 * (Value - MinValue) / (MaxValue - MinValue));
    Result := NewPercent > PercentDone;
    if Result then
      PercentDone := NewPercent;
  end;
end;



{ TMacros }

procedure TMacros.AssignValues(Source: TMacros);
var
  I: Integer;
  M: TMacro;
begin
  for I := 0 to Count - 1 do
  begin
    M := Source.Find(Items[I].Name);
    if M <> nil then
      Items[I].Value := M.Value;
  end;
end;

function TMacros.ExpandMacro(const MacroName: String; Data: Pointer): String;
var
  M: TMacro;
begin
  M := Find(MacroName);
  if M <> nil then
    Result := M.Value
  else Result := '';
end;

function TMacros.AddMacro(const MacroName: String; Data: Pointer): String;
begin
  Result := '';
  if Find(MacroName) = nil then
    TMacro(Add).Name := MacroName;
end;

function TMacros.ExpandMacros(const Template, MacroBegin,
  MacroEnd: String): String;
begin
  Result := ParseFields(Template, MacroBegin, MacroEnd, ExpandMacro, nil);
end;

function TMacros.Find(const Name: String): TMacro;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if AnsiCompareText(Items[I].Name, Name) = 0 then
    begin
      Result := Items[I];
      exit;
    end;
end;

function TMacros.GetItem(Index: Integer): TMacro;
begin
  Result := TMacro(inherited GetItem(Index));
end;

procedure TMacros.SetItem(Index: Integer; const Value: TMacro);
begin
  inherited SetItem(Index, Value);
end;

procedure TMacros.ParseTemplate(const Template, MacroBegin,
  MacroEnd: String);
begin
  Clear;
  ParseFields(Template, MacroBegin, MacroEnd, AddMacro, nil);
end;

procedure TMacros.UpdateMacros(const Template, MacroBegin,
  MacroEnd: String);
var
  TempMacros: TMacros;
begin
  TempMacros := TMacros.Create(GetOwner, TMacro);
  try
    TempMacros.Assign(Self); // Store values
    ParseTemplate(Template, MacroBegin, MacroEnd);
    AssignValues(TempMacros); // Re-store values
  finally
    TempMacros.Free;
  end;
end;

{ TMacro }

constructor TMacro.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := '';
  FValue := '';
end;

destructor TMacro.Destroy;
begin
  inherited Destroy;
end;

procedure TMacro.Assign(Source: TPersistent);
begin
  if Source is TMacro then
  begin
    Name := TMacro(Source).Name;
    Value := TMacro(Source).Value;
  end else
    inherited Assign(Source);
end;

function TMacro.GetDisplayName: String;
begin
  if Name <> '' then
    Result := Name
  else Result := inherited GetDisplayName;
end;

procedure TMacro.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TMacro.SetValue(const Value: String);
begin
  FValue := Value;
end;

end.
