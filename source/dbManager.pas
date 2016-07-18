(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Database manager component (TDBManager), responsible for functionality like
(*  database replication, reverse engineering, etc.
(*
(*  Copyright (c) 2005-2016, Context Software LLC
(*
(*  ------------------------------------------------------------
(*  FILE        : dbManager.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.40
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009, 2010, XE, XE2, XE3, XE4, 
(*                XE5, XE6, XE7, XE8, 10, 10.1
(*
(******************************************************************************)
unit dbManager;

interface

uses SysUtils, Classes, DB, Forms, dbExtUtils, CtxDBIntf,
  {$IFnDEF VER130}Variants,{$ENDIF} dbSchema;

type
  TDBManager = class;

  {:$ Represents an action, that should be performed on a given object during replication process. }
  TReplicationAction = (raUpload, raDownload, raConfirm, raSkip);

  {:$ Represents one of the possible ways of resolving replication conflict. }
  TReplicationConflictOptions = (rcoAcceptLaterVersion, rcoAcceptSnapshotVersion,
    rcoAcceptRemoteVersion, rcoLetUserResolveConflict);
  
  {:$ TReplicationConflict type is used by the database's OnReplicationConflict event. }
  TReplicationConflict = procedure (Sender: TObject; Source, Dest: IDatabaseExt; var Resolution: TReplicationAction) of object;

  {:$ TDatabaseOperationProgress type is used by the database's OnProgress event. }
  TDatabaseOperationProgress = procedure (Sender: TDBManager; const Operation: String; PercentDone: Byte; var Abort: Boolean) of object;

  {:: TDBManager }
  TDBManager = class (TComponent)
  protected
    FDatabaseExt: IDatabaseExt;
    FDatabase: TComponent;
    FStoredFieldmaps: TStringList;
    FReplicationConflictOptions: TReplicationConflictOptions;

    FOnProgress: TDatabaseOperationProgress;
    FOnReplicationConflict: TReplicationConflict;

    function GetDatabaseExt: IDatabaseExt;
    procedure SetDatabase(const Value: TComponent);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    { Object Synchronization }
    procedure UpdateObject(SrcDatabase, DestDatabase: IDatabaseExt; ObjectInfo: TObjectsTable = nil);
    procedure UpdateRecords(SrcDatabase, DestDatabase: IDatabaseExt; TableDef: TTableDefinition;
      const KeyFields: String; CaseInsensitive: Boolean; KeyValues: Variant);

    { Object Streaming }
    procedure WriteObjectState(Writer: TWriter);
    procedure WriteObject(Writer: TWriter; const ObjectType: String; ObjectKey: Variant);
    procedure WriteRecords(Writer: TWriter; TableDef: TTableDefinition;
      const ObjectKeyFields: String; CaseInsensitive: Boolean; KeyValues: Variant); overload;
    procedure WriteRecords(Writer: TWriter; const TableName, ObjectKeyFields: String;
      CaseInsensitive: Boolean; KeyValues: Variant); overload;
    function  ReadObjectState(Reader: TReader): TObjectState;
    procedure ReadObject(Reader: TReader);
    procedure SkipObject(Reader: TReader);
    procedure ReadFieldMap(Reader: TReader; const TableName: String; FieldMap: TList = nil; Table: TDataSet = nil);
    procedure ReadRecords(Reader: TReader);
    procedure SkipRecords(Reader: TReader);
  public
    {:: Creates an instance of a TDBManager component.}
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of a TDBManager component.}
    destructor Destroy; override;

    procedure CheckActive;
    procedure CheckSchema;

    { Schema related methods. Schema must be assigned for them to work }
    {:$ Updates database schema from the physical database tables. }
    {:: Schema property must be assigned and refer to the Schema component }
    {:: that will be updated. }
    procedure ReverseEngineer;
    {:$ Creates a new database based on the schema information. }
    {:: Schema property must be assigned and refer to the Schema component }
    {:: that will be used to create database tables. }
    {:: If the database already exists in the specified location, OwerwriteTables }
    {:: parameter controls whether the tables will be overwriten. }
    {:: CreateSystemTable parameter controls whether the System table should be created. }
    {:: CreateObjectsTable parameter controls whether the Objects table should be created. }
    {:! The name for the System table is stored in SystemTableName property. }
    procedure CreateNewDatabase(OverwriteTables: Boolean = True; CreateSystemTable: Boolean = True; CreateObjectsTable: Boolean = True);

    {:$ Synchronizes the content of the database with abother database. }
    procedure SynchronizeWith(DestDatabase: IDatabaseExt; UseObjectsTableOnly: Boolean = False);
    {:$ Writes information about all objects, contained in the database and }
    {:$ described in the database Schema into Objects table. }
    {:: This method will not overwrite any information, that is already contained }
    {:: in the Objects table and will only add information related to objects that
    {:: have never been modified.<br> }
    {:: This method is usefull when converting the database to a replicable form. }
    procedure UpdateObjectsTable;

    {:$ Exports Objects into the Stream. }
    {:: Optional ReplicationID parameter is used to determine which objects }
    {:: should be exported. If ReplicationID is set to any negative value only }
    {:: the objects modified within a current replication will be exported. }
    {:: Use ReplicationID = 0 in order to export all objects. }
    {:: If ReplicationID is a positive number this method will store all objects }
    {:: modified within that replication or later. }
    procedure ExportChanges(Stream: TStream; ReplicationID: Integer = -1); overload;
    {:$ Exports Objects into a file on disk. }
    {:: Optional ReplicationID parameter is used to determine which objects }
    {:: should be exported. If ReplicationID is set to any negative value only }
    {:: the objects modified within a current replication will be exported. }
    {:: Use ReplicationID = 0 in order to export all objects. }
    {:: If ReplicationID is a positive number this method will store all objects }
    {:: modified within that replication or later. }
    procedure ExportChanges(const FileName: String; ReplicationID: Integer = -1); overload;

    {:$ Imports Objects stored in the Stream. }
    procedure ImportChanges(Stream: TStream); overload;
    {:$ Imports Objects from a disk file. }
    procedure ImportChanges(const FileName: String); overload;

    {:$ Saves object selected in ObjectsTable into Stream. }
    procedure SaveObjectToStream(Stream: TStream); overload;
    procedure SaveObjectToStream(Database: IDatabaseExt; Stream: TStream); overload;
    {:$ Loads object from Stream and writes it into the database. }
    procedure LoadObjectFromStream(Stream: TStream);

    {:$ Saves table into Stream. }
    procedure SaveTableToStream(Stream: TStream; const TableName: String);
    {:$ Loads table from Stream and writes it into the database. }
    procedure LoadTableFromStream(Stream: TStream);
    {:$ Saves table to a file on disk }
    procedure SaveTableToFile(const FileName, TableName: String);
    {:$ Loads table from a disk file }
    procedure LoadTableFromFile(const FileName: String);
    {:$ Saves database to a file on disk }
    procedure SaveDatabaseToFile(const FileName: String);
    {:$ Loads database from a disk file }
    procedure LoadDatabaseFromFile(const FileName: String);

    {:: Invokes OnProgress event handler for the TDatabaseExt component. }
    procedure DoProgress(const Operation: String; PercentDone: Byte; var Abort: Boolean);

    property DatabaseExt: IDatabaseExt read GetDatabaseExt;
  published
    property Database: TComponent read FDatabase write SetDatabase;

    {:$ Occurs when database performs some long lasting operation or executes a query using ExecuteSQL or ExecuteSQLParam methods. }
    property OnProgress: TDatabaseOperationProgress read FOnProgress write FOnProgress;
    {:$ Occurs when replication conflict (i.e. same record has been modified within the same replication in both databases) is detected. }
    property OnReplicationConflict: TReplicationConflict read FOnReplicationConflict write FOnReplicationConflict;
    {:$ Specifies the way the database will try to resolve replication conflicts. }
    property ReplicationConflictOptions: TReplicationConflictOptions read FReplicationConflictOptions
      write FReplicationConflictOptions default rcoAcceptLaterVersion;
  end;

  procedure Register;

resourcestring
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SDatabaseSchemaIsNotAssigned = 'Database Schema is not assigned';
  SSynchronizing = 'Synchronizing Database...';
  SUpdatingObjectsTable = 'Updating Objects Table...';
  SExportingData = 'Exporting data...';
  SImportingData = 'Importing data...';
  SOperationAbortedByUser = 'Operation aborted by user';
  STableDefintionNotFound = 'Table definition not found: %s';
  SOperationIsNotSupport = '%s is not supported in your system';
  SExportingChanges = 'Exporting changes';
  SImportingChanges = 'Importing changes';
  SInvalidFileFormat = 'Invalid file format';
  SInvalidFileVersion = 'Invalid file version';
  SInvalidSchemaName = 'Schema referred to by the import file differs from the current database schema';
  SStreamReadError = 'Error reading object from stream';
  SNoFieldmapFoundForTable = 'No field name found. Table: %s';
  SInvalidFieldMap = 'Invalid field map. Table: %s';
  SErrorCreatingTable = 'Error creating table %s. Error: %s';
  SSynchronizationNotApplicable = 'Synchronization is only applicable for snapshots';
  SCanNotSynchronizeWithAnotherSnapshot = 'Cannot synchronize with another snapshot';
  SDestDatabaseDoesnotSupportSynchronization = 'Destination database does not support synchronizations';

implementation


const
  idxByReplicationID = 'byReplicationID';

  strFILE_SIGNATURE = '$RD$'; { Replication Data }
  strOBJECT_SIGNATURE = '$OBJECT$';

  intFILE_VERSION = 1;

  STREAM_BUFSIZE = 4096;


procedure Register;
begin
  RegisterComponents('Database Extensions', [TDBManager]);
end;


{ TDBManager }

constructor TDBManager.Create(AOwner: TComponent);
begin
  inherited;
  FDatabase := nil;
  FDatabaseExt := nil;
  FStoredFieldmaps := nil;
  FReplicationConflictOptions := rcoAcceptLaterVersion;
end;

destructor TDBManager.Destroy;
begin
  FDatabaseExt := nil;
  inherited;
end;

procedure TDBManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FDatabase) then
    Database := nil;
  inherited;
end;

procedure TDBManager.SetDatabase(const Value: TComponent);
begin
  if FDatabase <> Value then
  begin
    FDatabaseExt := nil;
    FDatabase := Value;
    if FDatabase <> nil then
    begin
      if not Value.GetInterface(IDatabaseExt, FDatabaseExt) then
        raise Exception.Create('Interface not supported');
      FDatabase.FreeNotification(Self);
    end;
  end;
end;

function TDBManager.GetDatabaseExt: IDatabaseExt;
begin
  Result := FDatabaseExt;
  if Result = nil then
    raise Exception.Create('Database is not assigned');
end;

procedure TDBManager.CheckActive;
begin
  if not DatabaseExt.Connected then
    DatabaseError(SDatabaseClosed);
end;

procedure TDBManager.CheckSchema;
begin
  if DatabaseExt.Schema = nil then
    DatabaseError(sDatabaseSchemaIsNotAssigned);
end;

procedure TDBManager.UpdateObjectsTable;
var
  I: Integer;
  TableDef: TTableDefinition;
  Table: TDataSet;
  ObjectKey: Variant;
  StrKey: String;
  StrObjType: String;
  ModificationTimestamp: TDateTime;
  SnapshotID, ReplicationID: Integer;
  Counter: TProgressCounter;
begin
  with DatabaseExt do
  begin
    CheckSchema;
    if not (CheckSystemTable and CheckObjectsTable) then exit;

    // Do through all objects in all tables and make sure,
    // that the corresponding record exists in the Objects table.
    // The primary purpose for this procedure is to facilitate
    // database import/conversion process, where a lot of information
    // is imported without tracking changes. It can also be used in order
    // to support upgrading of the existing database to make it replicable.
    SnapshotID := GetSnapshotID;
    ReplicationID := GetReplicationID;
    ModificationTimestamp := Now;
    ObjectsTable.Refresh;
    InitMinMax(Counter, 0, Schema.TableDefs.Count - 1);
    try
      for I := 0 to Schema.TableDefs.Count - 1 do
      begin
        // Report progress & process Abort parameter
        if Progress(Counter) then
          DoProgress(SUpdatingObjectsTable, Counter.PercentDone, Counter.Abort);
        if Counter.Abort then
          DatabaseError(SOperationAbortedByUser);

        TableDef := Schema.TableDefs[I];
        if TableDef.Replicate and (TableDef.ObjectType <> '') then
        begin
          Table := CreateTable(TableDef.TableName);
          try
            Table.Active := True;
            Table.First;
            Table.BlockReadSize := 100;
            while not Table.EOF do
            begin
              ObjectKey := Table.FieldValues[TableDef.ObjectKeyFields];
              StrKey := EncodeVariant(ObjectKey);
              if TableDef.ObjectKeyCaseInsensitive then
                StrKey := AnsiUpperCase(StrKey);
              StrObjType := AnsiUpperCase(TableDef.ObjectType);

              if not FindKey(ObjectsTable.DataSet, [StrObjType, StrKey]) then
              begin
                ObjectsTable.DataSet.Insert;
                try
                  ObjectsTable.ObjectType.AsString := StrObjType;
                  ObjectsTable.ObjectKey.AsString := StrKey;
                  ObjectsTable.SnapshotID.AsInteger := SnapshotID;
                  ObjectsTable.Timestamp.AsDateTime := ModificationTimeStamp;
                  ObjectsTable.ReplicationID.AsInteger := ReplicationID;
                  ObjectsTable.ChangeType.AsInteger := Integer(ctModified);
                  ObjectsTable.ChangeStatus.AsInteger := Integer(csActive);
                  ObjectsTable.UserName.AsString := UserName;
                  ObjectsTable.DataSet.Post;
                except
                  ObjectsTable.DataSet.Cancel;
                  raise;
                end;
              end;
              Table.Next;
            end;
          finally
            Table.BlockReadSize := 0;
            Table.Free;
          end;
        end;
      end;
    finally
      DoProgress(SUpdatingObjectsTable, 100, Counter.Abort);
    end;
  end;
end;

procedure TDBManager.UpdateRecords(SrcDatabase, DestDatabase: IDatabaseExt; TableDef: TTableDefinition;
  const KeyFields: String; CaseInsensitive: Boolean; KeyValues: Variant);
var
  I: Integer;
  ForeignKeyValues: Variant;
  ForeignTableDef: TTableDefinition;
  SrcCursor, DestCursor: TDBRangeCursor;
begin
  SrcCursor := SrcDatabase.GetRangeCursor(TableDef.TableName, KeyFields, '', CaseInsensitive, KeyValues);
  DestCursor := DestDatabase.GetRangeCursor(TableDef.TableName, KeyFields, '', CaseInsensitive, KeyValues);
  try
    // Update table records
    DestCursor.UpdateFrom(SrcCursor);
    // Update child records
    SrcCursor.DataSet.First;
    while not SrcCursor.DataSet.EOF do
    begin
      for I := 0 to TableDef.Relations.Count - 1 do
        with TableDef.Relations[I] do
          if RelationKind = rkChildren then
          begin
            // 3. Update records for table 'ForeignTable'
            ForeignTableDef := DatabaseExt.Schema.GetTableDef(nil, ForeignTable);
            if ForeignTableDef = nil then
              DatabaseErrorFmt(STableDefintionNotFound, [ForeignTable]);

            ForeignKeyValues := SrcCursor.DataSet.FieldValues[KeyFields];
            UpdateRecords(SrcDatabase, DestDatabase, ForeignTableDef, ForeignKeyFields, CaseInsensitive, ForeignKeyValues);
          end;
      SrcCursor.DataSet.Next;
    end;
  finally
    SrcCursor.Free;
    DestCursor.Free;
  end;
end;

procedure TDBManager.UpdateObject(SrcDatabase, DestDatabase: IDatabaseExt; ObjectInfo: TObjectsTable = nil);
var
  I: Integer;
  ObjectKeyValue: Variant;
  TableDef: TTableDefinition;
  ObjectType, ObjectKey: String;
  ChangeType: TChangeType;
  SnapshotID: Integer;
  UserName: String;
begin
  if ObjectInfo = nil then
    ObjectInfo := SrcDatabase.ObjectsTable;

  ObjectType := ObjectInfo.ObjectType.AsString;
  ObjectKey := ObjectInfo.ObjectKey.AsString;
  SnapshotID := ObjectInfo.SnapshotID.AsInteger;
  UserName := ObjectInfo.UserName.AsString;

  ChangeType := TChangeType(ObjectInfo.ChangeType.AsInteger);

  ObjectKeyValue := DecodeVariant(ObjectKey);
  TableDef := nil;
  with DatabaseExt do
  for I := 0 to Schema.TableDefs.Count - 1 do
  if AnsiSameText(Schema.TableDefs[I].ObjectType, ObjectType) then
  begin
    TableDef := Schema.TableDefs[I];
    UpdateRecords(SrcDatabase, DestDatabase,
      TableDef, TableDef.ObjectKeyFields, TableDef.ObjectKeyCaseInsensitive, ObjectKeyValue);
  end;
  // Manually record changes to the active transaction of the destination database
  if TableDef <> nil then
    DestDatabase.ActiveTransaction.WriteChange(TableDef, ObjectKeyValue, ChangeType, SnapshotID, UserName);
end;

procedure TDBManager.ExportChanges(Stream: TStream; ReplicationID: Integer = -1);
var
  ObjectType: String;
  ObjectKey: Variant;
  Writer: TWriter;
  Counter: TProgressCounter;
  ObjTable: TObjectsTable;
  SnapshotID: Integer;
begin
  CheckSchema;
  with DatabaseExt do
  begin
    if not CheckSystemTable(False) then
      DatabaseErrorFmt(SOperationIsNotSupport, [SExportingChanges]);
    CheckObjectsTable(True);

    // ReplicationID < 0 means that we take only the last replication
    if ReplicationID < 0 then
      ReplicationID := GetReplicationID;
    SnapshotID := GetSnapshotID;

    Writer := TWriter.Create(Stream, STREAM_BUFSIZE);
    FStoredFieldmaps := TStringList.Create;
    try
      FStoredFieldmaps.Duplicates := dupIgnore;
      FStoredFieldmaps.Sorted := True;

      // Write Export Header
      Writer.WriteString(strFILE_SIGNATURE);
      Writer.WriteInteger(intFILE_VERSION);
      Writer.WriteString(Schema.SchemaName); // Schema Name
      Writer.WriteString(UserName); // Who exported
      Writer.WriteDate(Now); // When exported
      Writer.WriteInteger(ReplicationID); // Which replication
      Writer.WriteInteger(SnapshotID); // Which replication
      Writer.WriteString(Application.Title); // Application name
      Writer.WriteString(ExtractFileName(Application.ExeName)); // Application exe file
      Writer.WriteString(''); // Additional info

      // Write Data
      Writer.WriteListBegin;
      ObjTable := TObjectsTable.Create(CreateQuery(Format('select * from %s where ReplicationID > %d',
        [GetObjectsTableName, ReplicationID])));
      try
        ObjTable.DataSet.First;
        InitMinMax(Counter, 0, ObjTable.DataSet.RecordCount);
        while not ObjTable.DataSet.EOF do
        begin
          if Progress(Counter) then
            DoProgress(SExportingData, Counter.PercentDone, Counter.Abort);
          if Counter.Abort then
            DatabaseError(SOperationAbortedByUser);
          WriteObjectState(Writer);
          ObjectType := ObjTable.ObjectType.AsString;
          ObjectKey := DecodeVariant(ObjTable.ObjectKey.AsString);
          WriteObject(Writer, ObjectType, ObjectKey);
          ObjTable.DataSet.Next;
        end;
      finally
        ObjTable.Free;
        Writer.WriteListEnd;
      end;
    finally
      FreeObjects(FStoredFieldmaps);
      FreeAndNil(FStoredFieldmaps);
      Writer.Free;
      DoProgress(SExportingData, 100, Counter.Abort);
    end;
  end;
end;

procedure TDBManager.ExportChanges(const FileName: String; ReplicationID: Integer = -1);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    ExportChanges(FileStream, ReplicationID);
  finally
    FileStream.Free;
  end;
end;

procedure TDBManager.ImportChanges(Stream: TStream);
var
  ObjectState: TObjectState;
  Reader: TReader;
  Counter: TProgressCounter;
  AllowReadObject: Boolean;
  SchemaName: String;
  CurrentChangeType: TChangeType;
begin
  CheckSchema;
  with DatabaseExt do
  begin
    if not CheckSystemTable(False) then
      DatabaseErrorFmt(SOperationIsNotSupport, [SImportingChanges]);
    CheckObjectsTable(True);

    BeginReplicating;

    Reader := TReader.Create(Stream, STREAM_BUFSIZE);
    FStoredFieldmaps := TStringList.Create;
    try
      FStoredFieldmaps.Duplicates := dupIgnore;
      FStoredFieldmaps.Sorted := True;
      StartTransaction;
      try
        GetActiveTransaction.ReplicationID := GetReplicationID;

        // Read Export Header
        if Reader.ReadString <> strFILE_SIGNATURE then
          DatabaseError(SInvalidFileFormat);
        if Reader.ReadInteger > intFILE_VERSION then
          DatabaseError(SInvalidFileVersion);

        SchemaName := Reader.ReadString;
        if (SchemaName <> '') and (not AnsiSameText(SchemaName, Schema.SchemaName)) then
          DatabaseError(SInvalidSchemaName);

        // Read other header parameters
        {User} Reader.ReadString;
        {Date} Reader.ReadDate;
        {ReplcaitionID} Reader.ReadInteger;
        {SnapshotID} Reader.ReadInteger;
        {Application.Title} Reader.ReadString;
        {Application.ExeName} Reader.ReadString;
        {Add. Info} Reader.ReadString;

        Reader.ReadListBegin;
        InitMinMax(Counter, 0, Stream.Size - Reader.Position);
        while not Reader.EndOfList do
        begin
          Counter.Value := Reader.Position;
          DoProgress(SImportingData, Counter.PercentDone, Counter.Abort);
          if Counter.Abort then
            DatabaseError(SOperationAbortedByUser);

          ObjectState := ReadObjectState(Reader);
          AllowReadObject := ObjectState.ChangeStatus in [csActive, csConfirmed];
          // Locate record in ObjectsTable
          if AllowReadObject and
            FindKey(ObjectsTable.DataSet, [ObjectState.ObjectType, ObjectState.ObjectStrKey]) then
          begin
            CurrentChangeType := TChangeType(ObjectsTable.ChangeType.AsInteger);
            // Note: generally we do not import Deleted records, because we cannot
            // enforce delete constraints, without knowing the replication
            // Delete synchronization is only supported via regular SynchronizeWith procedure
            AllowReadObject :=
              ((ObjectState.ChangeType = ctModifiedReference) and (CurrentChangeType = ctDeleted))
              or
              ((ObjectState.ChangeType in [ctInserted, ctModified, ctModifiedReference]) and
              ((CurrentChangeType in [ctDeleted, ctModifiedReference])
                or(ObjectsTable.Timestamp.AsDateTime < ObjectState.Timestamp)));
          end;

          if AllowReadObject then
          begin
            ReadObject(Reader);
            if ActiveTransaction <> nil then
              ActiveTransaction.ChangedObjects.Add(ObjectState);
          end else
            SkipObject(Reader);
        end;
        Reader.ReadListEnd;
        Commit;
      except
        Rollback;
        raise;
      end;
    finally
      FreeObjects(FStoredFieldmaps);
      FreeAndNil(FStoredFieldmaps);
      Reader.Free;
      EndReplicating;
      DoProgress(SImportingData, 100, Counter.Abort);
    end;
  end;
end;

procedure TDBManager.ImportChanges(const FileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    ImportChanges(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TDBManager.SaveObjectToStream(Stream: TStream);
begin
  SaveObjectToStream(DatabaseExt, Stream);
end;

procedure TDBManager.SaveObjectToStream(Database: IDatabaseExt; Stream: TStream);
var
  Writer: TWriter;
begin
  with Database do
  begin
    Writer := TWriter.Create(Stream, STREAM_BUFSIZE);
    try
      WriteObjectState(Writer);
      WriteObject(Writer, ObjectsTable.ObjectType.AsString, DecodeVariant(ObjectsTable.ObjectKey.AsString));
    finally
      Writer.Free;
    end;
  end;
end;

procedure TDBManager.LoadObjectFromStream(Stream: TStream);
var
  Reader: TReader;
  ObjectState: TObjectState;
begin
  with DatabaseExt do
  begin
    StartTransactionCount;
    try
      Reader := TReader.Create(Stream, STREAM_BUFSIZE);
      try
        ObjectState := ReadObjectState(Reader);
        ReadObject(Reader);
        ActiveTransaction.ChangedObjects.Add(ObjectState);
      finally
        Reader.Free;
      end;
      Commit;
    except
      Rollback;
      raise;
    end;
  end;
end;

procedure TDBManager.WriteObjectState(Writer: TWriter);
begin
  with DatabaseExt.ObjectsTable do
  begin
    Writer.WriteString(strOBJECT_SIGNATURE);
    Writer.WriteString(ObjectType.AsString);
    Writer.WriteString(ObjectKey.AsString);
    Writer.WriteInteger(ChangeType.AsInteger);
    Writer.WriteInteger(ChangeStatus.AsInteger);
    Writer.WriteInteger(ReplicationID.AsInteger);
    Writer.WriteInteger(SnapshotID.AsInteger);
    Writer.WriteDate(Timestamp.AsDateTime);
    Writer.WriteString(UserName.AsString);
  end;
end;

function TDBManager.ReadObjectState(Reader: TReader): TObjectState;
var
  TempSig: String;
begin
  TempSig := Reader.ReadString; // Read object signature
  if TempSig <> strOBJECT_SIGNATURE then
    DatabaseError(SStreamReadError);

  Result := TObjectState.Create;
  with Result do
  begin
    ObjectType := Reader.ReadString;
    ObjectStrKey := Reader.ReadString;
    ObjectKey := DecodeVariant(ObjectStrKey);
    ChangeType := TChangeType(Reader.ReadInteger);
    ChangeStatus := TChangeStatus(Reader.ReadInteger);
    ReplicationID := Reader.ReadInteger;
    SnapshotID := Reader.ReadInteger;
    Timestamp := Reader.ReadDate;
    UserName := Reader.ReadString;
  end;
end;

procedure TDBManager.WriteObject(Writer: TWriter; const ObjectType: String; ObjectKey: Variant);
var
  TableDef: TTableDefinition;
begin
  Writer.WriteListBegin;
  TableDef := DatabaseExt.Schema.GetObjectTable(ObjectType);
  if Assigned(TableDef) then
    WriteRecords(Writer, TableDef, TableDef.ObjectKeyFields, TableDef.ObjectKeyCaseInsensitive, ObjectKey);
  Writer.WriteListEnd;
end;

procedure TDBManager.ReadObject(Reader: TReader);
begin
  Reader.ReadListBegin;
  while not Reader.EndOfList do
    ReadRecords(Reader);
  Reader.ReadListEnd;
end;

procedure TDBManager.SkipObject(Reader: TReader);
begin
  Reader.ReadListBegin;
  while not Reader.EndOfList do
    SkipRecords(Reader);
  Reader.ReadListEnd;
end;

procedure TDBManager.ReadFieldMap(Reader: TReader; const TableName: String; FieldMap: TList = nil; Table: TDataSet = nil);
var
  FieldNames: TStringList;
  FreeFieldNames: Boolean;
  I, Idx: Integer;
begin
  FieldNames := nil;
  FreeFieldNames := False;
  try
    // If there's a field map in the stream 
    if Reader.ReadBoolean then
    begin
      FieldNames := TStringList.Create;
      FreeFieldNames := True;
      Reader.ReadListBegin;
      while not Reader.EndOfList do
        FieldNames.Add(Reader.ReadString);
      Reader.ReadListEnd;
      if FStoredFieldmaps <> nil then
      begin
        FStoredFieldmaps.AddObject(TableName, FieldNames);
        FreeFieldNames := False;
      end;
    end else if FStoredFieldmaps <> nil then
    begin
      Idx := FStoredFieldmaps.IndexOf(TableName);
      if Idx >= 0 then
        FieldNames := TStringList(FStoredFieldmaps.Objects[Idx]);
    end;
    // Finally, assign field map
    if (Table <> nil) and (FieldMap <> nil) then
    begin
      if FieldNames <> nil then
      begin
        FieldMap.Clear;
        for I := 0 to FieldNames.Count - 1 do
          FieldMap.Add(Table.FieldByName(FieldNames[I]));
      end else
        DatabaseErrorFmt(SNoFieldmapFoundForTable, [TableName]);
    end;
  finally
    if FreeFieldNames then
      FreeAndNil(FieldNames);
  end;
end;

procedure TDBManager.SkipRecords(Reader: TReader);
var
  TableName: String;
begin
  // Skip record's header
  TableName := Reader.ReadString;
  {ObjectKeyFields := }Reader.ReadString;
  {CaseInsensitive := }Reader.ReadBoolean;
  {StrKeyValues := }Reader.ReadString;

  // Read fieldmap
  ReadFieldMap(Reader, TableName);

  // Skip records
  Reader.ReadListBegin;
  while not Reader.EndOfList do
  begin
    // Skip record data
    Reader.ReadListBegin;
    while not Reader.EndOfList do
      SkipFieldValue(Reader);
    Reader.ReadListEnd;

    // Skip child records
    Reader.ReadListBegin;
    while not Reader.EndOfList do
      SkipRecords(Reader);
    Reader.ReadListEnd;
  end;
  Reader.ReadListEnd;
end;

procedure TDBManager.ReadRecords(Reader: TReader);
var
  I: Integer;
  TableName: String;
  ObjectKeyFields: String;
  CaseInsensitive: Boolean;
  StrKeyValues: String;
  KeyValues: Variant;
  FieldMap: TList;
  Cursor: TDBRangeCursor;
begin
  with DatabaseExt do
  begin
    // Read records header
    TableName := Reader.ReadString;
    ObjectKeyFields := Reader.ReadString;
    CaseInsensitive := Reader.ReadBoolean;
    StrKeyValues := Reader.ReadString;
    KeyValues := DecodeVariant(StrKeyValues);
    // Read records
    Cursor := GetRangeCursor(TableName, ObjectKeyFields, '', CaseInsensitive, KeyValues);
    FieldMap := TList.Create;
    try
      // Read fieldmap if it is stored
      ReadFieldMap(Reader, TableName, FieldMap, Cursor.DataSet);

      Cursor.SetSuppressAutoIncValues(True);

      if not GetInTransaction then
        StartTransaction;

      // Erase all data for the current range
      while not Cursor.DataSet.EOF do Cursor.DataSet.Delete;

      // Read records
      Reader.ReadListBegin;
      while not Reader.EndOfList do
      begin
        // Read record data
        Cursor.DataSet.Insert;
        try
          Reader.ReadListBegin;

          for I := 0 to FieldMap.Count - 1 do
          begin
            if TField(FieldMap[I]).DataSet <> Cursor.DataSet then
              DatabaseErrorFmt(SInvalidFieldMap, [TableName]);
            ReadFieldValue(Reader, TField(FieldMap[I]));
          end;

          // Make sure, that it's the end
          if not Reader.EndOfList then
            DatabaseError(SStreamReadError);
          Reader.ReadListEnd;

          Cursor.DataSet.Post;
        except
          Cursor.DataSet.Cancel;
          raise
        end;

        // Read child records
        Reader.ReadListBegin;
        while not Reader.EndOfList do
          ReadRecords(Reader);
        Reader.ReadListEnd;
      end;
      Reader.ReadListEnd;
    finally
      FieldMap.Free;
      Cursor.SetSuppressAutoIncValues(False);
      Cursor.Free;
    end;
  end;
end;

procedure TDBManager.WriteRecords(Writer: TWriter; TableDef: TTableDefinition;
  const ObjectKeyFields: String; CaseInsensitive: Boolean; KeyValues: Variant);
var
  I: Integer;
  ForeignKeyValues: Variant;
  ForeignTableDef: TTableDefinition;
  Cursor: TDBRangeCursor;
begin
  // Write records header
  Writer.WriteString(TableDef.TableName);
  Writer.WriteString(ObjectKeyFields);
  Writer.WriteBoolean(CaseInsensitive);
  Writer.WriteString(EncodeVariant(KeyValues));
  // Write records
  Cursor := // TBDERangeCursor.CreateExt(FDatabaseExt, TableDef.TableName, ObjectKeyFields, '', CaseInsensitive, KeyValues);
    DatabaseExt.GetRangeCursor(TableDef.TableName, ObjectKeyFields, '', CaseInsensitive, KeyValues);
  try
    // Store fieldmap if it's not stored yet
    I := -1;
    if FStoredFieldmaps <> nil then
      I := FStoredFieldmaps.IndexOf(TableDef.TableName);

    Writer.WriteBoolean(I < 0);

    if I < 0 then
    begin
      Writer.WriteListBegin;
      for I := 0 to Cursor.DataSet.FieldCount - 1 do
        Writer.WriteString(Cursor.DataSet.Fields[I].FieldName);
      Writer.WriteListEnd;

      if FStoredFieldmaps <> nil then
        FStoredFieldmaps.Add(TableDef.TableName);
    end;

    Writer.WriteListBegin;
    Cursor.DataSet.First;
    while not Cursor.DataSet.EOF do
    begin
      // Write record data
      Writer.WriteListBegin;
      for I := 0 to Cursor.DataSet.FieldCount - 1 do
        WriteFieldValue(Writer, Cursor.DataSet.Fields[I]);
      Writer.WriteListEnd;

      Writer.WriteListBegin;
      // Write child records
      for I := 0 to TableDef.Relations.Count - 1 do
        with TableDef.Relations[I] do
          if RelationKind = rkChildren then
          begin
            // Write records from table 'ForeignTable'
            ForeignTableDef := FDatabaseExt.Schema.GetTableDef(nil, ForeignTable);
            if ForeignTableDef = nil then
              DatabaseErrorFmt(STableDefintionNotFound, [ForeignTable]);

            ForeignKeyValues := Cursor.DataSet.FieldValues[KeyFields];
            WriteRecords(Writer, ForeignTableDef, ForeignKeyFields, CaseInsensitive, ForeignKeyValues);
          end;
      Writer.WriteListEnd;

      Cursor.DataSet.Next;
    end;
    Writer.WriteListEnd;
  finally
    Cursor.Free;
  end;
end;

procedure TDBManager.WriteRecords(Writer: TWriter; const TableName, ObjectKeyFields: String;
  CaseInsensitive: Boolean; KeyValues: Variant);
var
  I: Integer;
  Cursor: TDBRangeCursor;
begin
  // Write records header
  Writer.WriteString(TableName);
  Writer.WriteString(ObjectKeyFields);
  Writer.WriteBoolean(CaseInsensitive);
  Writer.WriteString(EncodeVariant(KeyValues));
  // Write records
  Cursor := FDatabaseExt.GetRangeCursor(TableName, ObjectKeyFields, '', CaseInsensitive, KeyValues);
  try
    // Store fieldmap
    Writer.WriteBoolean(True);
    Writer.WriteListBegin;
    for I := 0 to Cursor.DataSet.FieldCount - 1 do
      Writer.WriteString(Cursor.DataSet.Fields[I].FieldName);
    Writer.WriteListEnd;
    // Store records
    Writer.WriteListBegin;
    Cursor.DataSet.First;
    while not Cursor.DataSet.EOF do
    begin
      // Write record data
      Writer.WriteListBegin;
      for I := 0 to Cursor.DataSet.FieldCount - 1 do
        WriteFieldValue(Writer, Cursor.DataSet.Fields[I]);
      Writer.WriteListEnd;

      // Write child records (none in this case)
      Writer.WriteListBegin;
      Writer.WriteListEnd;

      Cursor.DataSet.Next;
    end;
    Writer.WriteListEnd;
  finally
    Cursor.Free;
  end;
end;

procedure TDBManager.SaveTableToStream(Stream: TStream; const TableName: String);
var
  Writer: TWriter;
begin
  Writer := TWriter.Create(Stream, 4096);
  try
    WriteRecords(Writer, TableName, '', False, NULL);
  finally
    Writer.Free;
  end;
end;

procedure TDBManager.LoadTableFromStream(Stream: TStream);
var
  Reader: TReader;
begin
  with DatabaseExt do
  begin
    Reader := TReader.Create(Stream, 4096);
    try
      StartTransactionCount;
      try
        ReadRecords(Reader);
        Commit;
      except
        Rollback;
        raise;
      end;
    finally
      Reader.Free;
    end;
  end;
end;

procedure TDBManager.SaveTableToFile(const FileName, TableName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveTableToStream(FileStream, TableName);
  finally
    FileStream.Free;
  end;
end;

procedure TDBManager.LoadTableFromFile(const FileName: String);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadTableFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TDBManager.SaveDatabaseToFile(const FileName: String);
var
  FileStream: TFileStream;
  Tables: TStringList;
  I: Integer;
begin
  with DatabaseExt do
  begin
    CheckActive;
    Tables := TStringList.Create;
    try
      FileStream := TFileStream.Create(FileName, fmCreate);
      try
        GetTableNames(Tables);
        for I := 0 to Tables.Count - 1 do
          SaveTableToStream(FileStream, Tables[I]);
      finally
        FileStream.Free;
      end;
    finally
      Tables.Free;
    end;
  end;
end;

procedure TDBManager.LoadDatabaseFromFile(const FileName: String);
var
  FileStream: TFileStream;
begin
  with DatabaseExt do
  begin
    BeginReplicating;
    FileStream := TFileStream.Create(FileName, fmOpenRead);
    try
      StartTransactionCount;
      try
        while FileStream.Position < FileStream.Size do
          LoadTableFromStream(FileStream);
        Commit;
      except
        Rollback;
        raise;
      end;
    finally
      EndReplicating;
      FileStream.Free;
    end;
  end;
end;

procedure TDBManager.SynchronizeWith(DestDatabase: IDatabaseExt; UseObjectsTableOnly: Boolean = False);
const
  sqlSelectObjects = 'select * from %s where ReplicationID > %d order by ObjectType, ObjectKey';
  Resolve: array [TChangeType, TChangeType] of TReplicationAction = (
  { src\dest |  Deleted,  Reference,  Content,    Modified,   Inserted }
  { Deleted }  (raSkip,   raDownload, raDownload, raDownload, raDownload),
  { Reference }(raUpload, raSkip,     raDownload, raDownload, raDownload),
  { Content }  (raUpload, raUpload,   raConfirm,  raConfirm,  raConfirm),
  { Modified } (raUpload, raUpload,   raConfirm,  raConfirm,  raConfirm),
  { Inserted } (raUpload, raUpload,   raConfirm,  raConfirm,  raConfirm)
  );
var
  ReplicationID, DestReplicationID, CurReplicationID: Integer;
  SnapshotID: Integer;
  Action: TReplicationAction;
  SaveDestSchema: TDatabaseSchema;
  CompareResult: Integer;
  AdvanceSrc, AdvanceDest: Boolean;
  UseDestInfo: Boolean;
  Counter: TProgressCounter;
  DestCommitted: Boolean;
  DataStream: TStream;
  DestObjectsTable: TObjectsTable;
begin
  with DatabaseExt do
  begin
    DestCommitted := False;
    // Check Schema, System table & SnapshotID
    CheckSchema;
    SnapshotID := GetSnapshotID; // This will also check system table
    if SnapshotID = 0 then
      DatabaseError(SSynchronizationNotApplicable);
    if not CheckObjectsTable(True) then
      DatabaseError(SSynchronizationNotApplicable);

    // Save update options and disable cascade operations and error constraints
    BeginReplicating;
    DestDatabase.BeginReplicating;
    // Setup own schema to destination database - we can only process databases
    // with identical schemas
    SaveDestSchema := DestDatabase.Schema;
    if DestDatabase.Schema = nil then
      DestDatabase.Schema := Schema;
    try
      DestDatabase.Connected := True;

      // Check snapshot & system tables
      if not DestDatabase.CheckObjectsTable(True) then
        DatabaseError(SDestDatabaseDoesnotSupportSynchronization);
      if (not DestDatabase.CheckSystemTable) or (DestDatabase.GetSnapshotID <> 0) then
        DatabaseError(SCanNotSynchronizeWithAnotherSnapshot);

      // Start Transaction for both databases
      StartTransaction;
      DestDatabase.StartTransaction;
      try
        // Get current replication ID's
        ReplicationID := GetReplicationID;
        DestReplicationID := DestDatabase.GetReplicationID;
        CurReplicationID := DestReplicationID + 1;

        // Assign next replication ID for both databases
        // It will be used for all changes enterd within this transaction
        DestDatabase.ActiveTransaction.ReplicationID := CurReplicationID;
        DatabaseExt.ActiveTransaction.ReplicationID := CurReplicationID;

        // Setup ranges on Objects table by Replication ID
        DestObjectsTable := nil;
        try
          // +++ TTable(ObjectsTable.DataSet).IndexName := idxByReplicationID;
          // +++ TTable(ObjectsTable.DataSet).SetRange([ReplicationID], [ReplicationID]);
          ObjectsTable.DataSet.Filter := 'ReplicationID = ' + IntToStr(ReplicationID);
          ObjectsTable.DataSet.Filtered := True;

          DestObjectsTable := TObjectsTable.Create(ExecuteSQL(DestDatabase, Format(sqlSelectObjects, [GetObjectsTableName, ReplicationID])), True);

          InitMinMax(Counter, 0, ObjectsTable.DataSet.RecordCount + DestObjectsTable.DataSet.RecordCount);
          DestObjectsTable.DataSet.First;

          DestObjectsTable.DataSet.BlockReadSize := 100;

          ObjectsTable.DataSet.First;
          AdvanceSrc := False;
          AdvanceDest := False;
          // Iterate both objects tables in the same order and compare records
          while (not ObjectsTable.DataSet.EOF) or (not DestObjectsTable.DataSet.EOF) do
          begin
            // Start with reporting progress
            if Progress(Counter, Integer(AdvanceSrc) + Integer(AdvanceDest)) then
              DoProgress(SSynchronizing, Counter.PercentDone, Counter.Abort);
            if Counter.Abort then
              DatabaseError(SOperationAbortedByUser);
            // Determine which cursor to move.
            Action := raConfirm;
            AdvanceSrc := True;
            AdvanceDest := True;
            UseDestInfo := False;
            // Compare Records
            if ObjectsTable.DataSet.EOF or DestObjectsTable.DataSet.EOF then
            begin
              if ObjectsTable.DataSet.EOF then CompareResult := -1
              else CompareResult := 1;
            end else begin
              CompareResult := CompareStr(DestObjectsTable.ObjectType.AsString, ObjectsTable.ObjectType.AsString);
              if CompareResult = 0 then
                CompareResult := CompareStr(DestObjectsTable.ObjectKey.AsString, ObjectsTable.ObjectKey.AsString);
            end;

            // Decide which record to advance after operation
            if CompareResult < 0 then begin
              // Download information
              Action := raDownload;
              AdvanceSrc := False;
            end else begin
              // Upload or Download information
              case TChangeStatus(ObjectsTable.ChangeStatus.AsInteger) of
                csInactive: begin
                  // Download information for this object
                  Action := raDownload;
                  UseDestInfo := True;
                end;
                csConfirm:
                  // Don't update this object, since it have to be confirmed by user
                  Action := raSkip;
                csConfirmed:
                  // Upload information for this object, since it has already been confirmed
                  Action := raUpload;
              end;
              if CompareResult > 0 then begin
                // If still not decided...
                if Action = raConfirm then
                  Action := raUpload;
                AdvanceDest := False;
              end;
            end;

            // Final stage: performing replication action
            while True do
              case Action of
                raUpload: begin
                  if UseObjectsTableOnly then
                  begin
                    if DestDatabase.FindKey(ObjectsTable.DataSet, [ObjectsTable.ObjectType.AsString, ObjectsTable.ObjectKey.AsString])
                    then
                      DestDatabase.ObjectsTable.DataSet.Edit
                    else DestDatabase.ObjectsTable.DataSet.Insert;
                    try
                      DestDatabase.ObjectsTable.ObjectType.Assign(ObjectsTable.ObjectType);
                      DestDatabase.ObjectsTable.ObjectKey.Assign(ObjectsTable.ObjectKey);
                      DestDatabase.ObjectsTable.SnapshotID.Assign(ObjectsTable.SnapshotID);
                      DestDatabase.ObjectsTable.ReplicationID.AsInteger := CurReplicationID;
                      DestDatabase.ObjectsTable.ChangeType.Assign(ObjectsTable.ChangeType);
                      DestDatabase.ObjectsTable.ChangeStatus.Assign(ObjectsTable.ChangeStatus);
                      DestDatabase.ObjectsTable.UserName.Assign(ObjectsTable.UserName);
                      DataStream := DestDatabase.ObjectsTable.DataSet.CreateBlobStream(DestDatabase.ObjectsTable.ObjectData, bmWrite);
                      try
                        SaveObjectToStream(DataStream);
                      finally
                        DataStream.Free;
                      end;
                      DestDatabase.ObjectsTable.DataSet.Post;
                    except
                      DestDatabase.ObjectsTable.DataSet.Cancel;
                      raise;
                    end;
                  end else begin
                    UpdateObject(DatabaseExt, DestDatabase);
                  end;
                  break;
                end;
                raDownload: begin
                  if UseObjectsTableOnly then begin
                    if UseDestInfo then begin
                      if FindKey(DestDatabase.ObjectsTable.DataSet,
                        [ObjectsTable.ObjectType.AsString, ObjectsTable.ObjectKey.AsString])
                      then
                        DataStream := DestDatabase.ObjectsTable.DataSet.CreateBlobStream(
                          DestDatabase.ObjectsTable.ObjectData, bmRead)
                      else DataStream := nil;
                    end else
                      DataStream := DestObjectsTable.DataSet.CreateBlobStream(
                        DestObjectsTable.ObjectData, bmRead);

                    if DataStream <> nil then
                    try
                      DataStream.Position := 0;
                      LoadObjectFromStream(DataStream);
                    finally
                      DataStream.Free;
                    end;
                  end else begin
                    if UseDestInfo then begin
                      if FindKey(DestDatabase.ObjectsTable.DataSet,
                        [ObjectsTable.ObjectType.AsString, ObjectsTable.ObjectKey.AsString])
                      then
                        UpdateObject(DestDatabase, DatabaseExt);
                    end else begin
                      UpdateObject(DestDatabase, DatabaseExt, DestObjectsTable);
                    end;
                  end;
                  break;
                end;
                raConfirm: begin
                  // Conflict if both are: Inserted, Modified or ModifiedContent
                  // Compare types of change
                  Action := Resolve[TChangeType(ObjectsTable.ChangeType.AsInteger),
                    TChangeType(DestObjectsTable.ChangeType.AsInteger)];

                  // We have a conflict here
                  if Action = raConfirm then
                    case ReplicationConflictOptions of
                      rcoAcceptLaterVersion: begin
                        if ObjectsTable.Timestamp.AsDateTime >= DestObjectsTable.Timestamp.AsDateTime then
                          Action := raUpload
                        else Action := raDownload
                      end;
                      rcoAcceptSnapshotVersion: Action := raUpload;
                      rcoAcceptRemoteVersion: Action := raDownload;
                      else { rcoLetUserResolveConflict } begin
                        // Try resolving it by callback
                        if (Action = raConfirm) and Assigned(FOnReplicationConflict) then
                          FOnReplicationConflict(Self, DatabaseExt, DestDatabase, Action);
                        // Postpone resolution and mark record for later confirmation
                        if Action = raConfirm then
                        begin
                          // Mark update for later cofirmation
                          ObjectsTable.DataSet.Edit;
                          try
                            ObjectsTable.ChangeStatus.AsInteger := Integer(csConfirm);
                            ObjectsTable.ReplicationID.AsInteger := CurReplicationID + 1;
                            DataStream := ObjectsTable.DataSet.CreateBlobStream(
                              ObjectsTable.ObjectData, bmWrite);
                            try
                              DataStream.Position := 0;
                              // Store remote object in blob for confirmation purposes
                              if FindKey(DestDatabase.ObjectsTable.DataSet,
                                [DestObjectsTable.ObjectType.AsString, DestObjectsTable.ObjectKey.AsString])
                              then begin
                                if UseObjectsTableOnly then
                                  DestDatabase.ObjectsTable.ObjectData.SaveToStream(DataStream)
                                else SaveObjectToStream(DestDatabase, DataStream);
                              end;
                            finally
                              DataStream.Free;
                            end;
                            ObjectsTable.DataSet.Post;
                            break;
                          except
                            ObjectsTable.DataSet.Cancel;
                            raise;
                          end;
                        end;
                      end; { else }
                    end; { case ReplicationConflictOptions }
                end; { raConfirm }
                raSkip: break;
              end; { case / while }

            // Advance records and progress
            if AdvanceSrc then
              ObjectsTable.DataSet.Next;
            if AdvanceDest then
              DestObjectsTable.DataSet.Next;
          end;
        finally
          ObjectsTable.DataSet.Filtered := False;
          ObjectsTable.DataSet.Filter := '';
          DestObjectsTable.Free;
        end;

        // Update Replication IDs for both databases
        SetReplicationID(CurReplicationID + 1);
        DestDatabase.SetReplicationID(CurReplicationID + 2);

        // Finally commit transactions,
        // write changes to both 'objects' tables
        // and execute object-level triggers
        DestDatabase.Commit;
        DestCommitted := True;
        Commit;
      except
        if not DestCommitted then
          DestDatabase.Rollback;
        Rollback;
        raise;
      end;

    finally
      // Restore Update Options & Schema
      EndReplicating;
      DestDatabase.EndReplicating;
      DestDatabase.Schema := SaveDestSchema;
      DoProgress(SSynchronizing, 100, Counter.Abort);
    end;
  end;
end;

procedure TDBManager.CreateNewDatabase(OverwriteTables,
  CreateSystemTable, CreateObjectsTable: Boolean);
begin
  DatabaseExt.CreateNewDatabase(OverwriteTables, CreateSystemTable, CreateObjectsTable);
end;

procedure TDBManager.ReverseEngineer;
begin
  DatabaseExt.ReverseEngineer;
end;

procedure TDBManager.DoProgress(const Operation: String;
  PercentDone: Byte; var Abort: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, Operation, PercentDone, Abort);
end;

end.
