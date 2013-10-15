(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Classes that implement document-journal paradigm
(*  Contains:
(*                TDBDocumentType
(*                TDBDocument
(*                TDBJournal
(*
(*  Copyright (c) 2005-2011, Context Software LLC
(*
(*  ------------------------------------------------------------
(*  FILE        : dbDocument.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.32
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009, 2010, XE
(*
(******************************************************************************)
unit dbDocument;

interface

uses SysUtils, Classes, Contnrs, DB, dbSchema;

type
  TDBDocumentType = class;
  TDBDocument = class;

  (*
      Groups of functionality used from ISchemaDatabase interface:
      1. Transactions
      2. RangeCursor
  *)


  {:$ TDocumentEvent type is used by the TDBDocumentType's events. }
  TDocumentEvent = procedure (Sender: TDBDocumentType; Document: TDBDocument) of object;
  {:$ TDocumentEvent type is used by the TDBDocumentType's events. }
  TGenerateIDEvent = function (Sender: TDBDocumentType; Document: TDBDocument): String of object;
  {:$ TDocumentEvent type is used by the TDBDocumentType's events. }
  TDatabaseEvent = procedure (Sender: TObject; DocumentType: TDBDocumentType; const DocumentID: Variant; Database: ISchemaDatabase) of object;

  {:$ TDBDocumentType represents a document type. }
  TDBDocumentType = class (TComponent)
  protected
    FSchema: TDatabaseSchema;
    FDescription: String;
    FMasterTable: String;
    FDocTypeID: String;
    FDocumentType: String;
    FCaseInsensitive: Boolean;
    FDocumentIDField: String;

    FEditFormName: String;
    FLookupFormName: String;

    FOnSave: TDocumentEvent;
    FOnNew: TDocumentEvent;
    FOnLoad: TDocumentEvent;
    FOnDelete: TDatabaseEvent;
    FOnMakeEntries: TDocumentEvent;
    FOnDeleteEntries: TDatabaseEvent;
    FOnGenerateID: TGenerateIDEvent;
    procedure SetDocumentType(const Value: String);
    procedure SetDocTypeID(const Value: String);
    procedure RegisterDocumentType;

    function GenerateID(ADocument: TDBDocument): String; virtual;
    procedure New(ADocument: TDBDocument); virtual;
    procedure Load(ADocument: TDBDocument); virtual;
    procedure Save(ADocument: TDBDocument); virtual;
    procedure MakeEntries(ADocument: TDBDocument); virtual;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    {:$ Creates an instance of TDBDocumentType object. }
    constructor Create(AOwner: TComponent); override;
    {:$ Destroys the instance of TDBDocumentType object. }
    destructor Destroy; override;
    {:$ Raises exception if there's no database schema associated with this object. }
    procedure CheckSchema;
    {:$ Deletes document referred by the DocumentID parameter from the document  }
    {:$ database passed in Database parameter. }
    procedure Delete(const DocumentID: Variant; Database: ISchemaDatabase); virtual;
    {:$ Deletes document entries from journals. }
    procedure DeleteEntries(const DocumentID: Variant; Database: ISchemaDatabase); virtual;
  published
    {:$ Name of document type. }
    property DocumentType: String read FDocumentType write SetDocumentType;
    {:$ Description of document type. }
    property Description: String read FDescription write FDescription;
    {:$ The name of the master (header) table containing documents of this type. }
    property MasterTable: String read FMasterTable write FMasterTable;
    {:$ The name of a field in the master (header) table (see MasterTable property) }
    {:$ that uniquely identifies the document. }
    property DocumentIDField: String read FDocumentIDField write FDocumentIDField;
    {:$ Specifies whether the DocumentID should be regarded as case sensetive. }
    property CaseInsensitive: Boolean read FCaseInsensitive write FCaseInsensitive default False;
    {:$ Abbreviation for this document type. }
    property DocTypeID: String read FDocTypeID write SetDocTypeID;
    {:$ Reference to a TDatabaseSchema component. }
    {:: Schema may contain information about the database structure as weel as some }
    {:: additional information like referential integrity constraints, triggers and more. }
    property Schema: TDatabaseSchema read FSchema write FSchema;
    {:$ The name of the form, used for editing this type of document. }
    property EditFormName: String read FEditFormName write FEditFormName;
    {:$ The name of the form, used for looking up this type of document. }
    property LookupFormName: String read FLookupFormName write FLookupFormName;
    { Events }
    {:$ Triggered when new document is initialized. }
    property OnNew: TDocumentEvent read FOnNew write FOnNew;
    {:$ Triggered when document is being loaded. }
    property OnLoad: TDocumentEvent read FOnLoad write FOnLoad;
    {:$ Triggered when document is being stored in the document Database. }
    property OnSave: TDocumentEvent read FOnSave write FOnSave;
    {:$ Triggered when document is being deleted from the document Database. }
    property OnDelete: TDatabaseEvent read FOnDelete write FOnDelete;
    {:$ Triggered when document is being entered into journals. }
    property OnMakeEntries: TDocumentEvent read FOnMakeEntries write FOnMakeEntries;
    {:$ Triggered when document's entries are being removed from journals. }
    property OnDeleteEntries: TDatabaseEvent read FOnDeleteEntries write FOnDeleteEntries;
    {:$ Triggered when document's ID is being generated. }
    property OnGenerateID: TGenerateIDEvent read FOnGenerateID write FOnGenerateID;
  end;

  TDBJournalRule = class;
  TDBJournal = class;
  TDocDataSets = class;

  {:$ TDataSetItem }
  TDataSetItem = class (TCollectionItem)
  protected
    FName: String;
    FTableName: String;
    FDocumentIDField: String;
    FItemIDField: String;
    FCaseInsensitive: Boolean;
    FDataSet: TDataSet;
    FDataSets: TDocDataSets;
    procedure SetDataSets(const Value: TDocDataSets);
  public
    {:$ Creates an instance of TDataSetItem object. }
    {:: This method should never be used directly. }
    {:: Use Add method of TDocDataSets collection instead. }
    constructor Create(Collection: TCollection); override;
    {:$ Destroys the instance of TDatabaseUpdate object. }
    destructor Destroy; override;
    {:$ Specifies the name of the TDatabaseUpdate as it appears in Object Inspector. }
    function GetDisplayName: string; override;
    {:$ Returns associated database schema. }
    function GetSchema: TDatabaseSchema;
  published
    {:$ Specifies name of this dataset item. }
    property Name: String read FName write FName;
    {:$ Specifies the name of the corresponding database table. }
    property TableName: String read FTableName write FTableName;
    {:$ Specifies the name of the DocumentID field in the corresponding database table. }
    property DocumentIDField: String read FDocumentIDField write FDocumentIDField;
    {:$ Specifies the name of the ItemID field in the corresponding database table. }
    property ItemIDField: String read FItemIDField write FItemIDField;
    {:$ Specifies whether DocumentID field is case sensetive. }
    property CaseInsensitive: Boolean read FCaseInsensitive write FCaseInsensitive default False;
    {:$ Contains reference to a dataset working as a cursor for the database table. }
    property DataSet: TDataSet read FDataSet write FDataSet;
    {:$ Contains a collection (possibly empty) of subordinated datasets comprising document. }
    property DataSets: TDocDataSets read FDataSets write SetDataSets;
  end;

  {:$ Contains a collection of possibly subordinated datasets comprising document. }
  TDocDataSets = class (TOwnedCollection)
  private
    function GetDataSetItem(Index: Integer): TDataSetItem;
    procedure SetDataSetItem(Index: Integer; const Value: TDataSetItem);
  public
    {:$ Creates and adds to the collection new data set item of type TDataSetItem. }
    function Add: TDataSetItem;
    {:$ Finds dataset by the name of dataset item. }
    function Find(const Name: String): TDataSet;
    {:$ Provides array-like access to the collection of dataset items. }
    property Items[Index: Integer]: TDataSetItem read GetDataSetItem write SetDataSetItem; default;
    {:$ Provides array-like access to the datasets by the names of correposnding items. }
    property DataSet[const Name: String]: TDataSet read Find;
  end;

  {:$ Contains rules (TDBJournalRule) specifying how different types of documents should be }
  {:$ entered into a journal. }
  TDBJournalRules = class (TOwnedCollection)
    function GetItem(Index: Integer): TDBJournalRule;
    procedure SetItem(Index: Integer; const Value: TDBJournalRule);
  public
    {:$ Creates and adds to the collection new journal rule of type TDBJournalRule. }
    function Add: TDBJournalRule;
    {:$ Provides array-like access to contained journal rules (TDBJournalRule). }
    property Items[Index: Integer]: TDBJournalRule read GetItem write SetItem; default;
  end;

  {:$ Used by OnMakeEntries event of TDBJournalRule object. }
  TOnJournalRule = procedure (Sender: TDBJournal; Document: TDBDocument) of object;

  {:$ Represents a rule describing how specific type of document (referred by }
  {:$ DocumentType property) should be entered into journal. }
  TDBJournalRule = class (TCollectionItem)
  protected
    FDocumentType: String;
    FOnMakeEntries: TOnJournalRule;
  public
    {:$ Creates an instance of TDBJournalRule object. }
    constructor Create(Collection: TCollection); override;
    {:$ Returns display name for this rule (used by IDE's collection editor). }
    function GetDisplayName: string; override;
  published
    {:$ Specifies the document type this rule applies to. }
    property DocumentType: String read FDocumentType write FDocumentType;
    {:$ Triggered with the document of the type specified by DocumentType property }
    {:$ is being ientered into journal. }
    property OnMakeEntries: TOnJournalRule read FOnMakeEntries write FOnMakeEntries;

    // property Command: TCtxDBCommand
  end;

  {:$ Represents journal-type entity. Journal is a table where each document }
  {:$ occupies certain range of rows. It may be regarded as index. This type of }
  {:$ entity often used by bookkeeping applications. E.g.: General Journal, }
  {:$ Accounts Payable Journal, etc. }
  TDBJournal = class (TComponent)
  protected
    FJournalName: String;
    FJournalTable: String;
    FDocumentIDField: String;
    FEntryNoField: String;
    FSchema: TDatabaseSchema;
    FRules: TDBJournalRules;
    FOnDeleteEntries: TDatabaseEvent;
    procedure SetRules(const Value: TDBJournalRules);
    procedure RegisterJournal;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    {:$ Creates an instance of TDBJournal object. }
    constructor Create(AOwner: TComponent); override;
    {:$ Destroys the instance of TDBJournal object. }
    destructor Destroy; override;
    {:$ Raises exception if there's no database schema associated with this object. }
    procedure CheckSchema;
    {:$ Makes journal entries for a specific document. }
    procedure MakeEntries(Document: TDBDocument); virtual;
    {:$ Deletes journal entries for a specific document. }
    procedure DeleteEntries(Document: TDBDocument); overload; virtual;
    {:$ Deletes journal entries for a specific document. }
    procedure DeleteEntries(DocumentType: TDBDocumentType; const DocumentID: Variant; Database: ISchemaDatabase); overload; virtual;
    {:$ Creates range cursor for the underlying database table to facilitate }
    {:$ making or deleting journal entries. }
    function GetRangeCursor(Document: TDBDocument): TDBRangeCursor; overload; virtual;
    {:$ Creates range cursor for the underlying database table to facilitate }
    {:$ making or deleting journal entries. }
    function GetRangeCursor(const DocumentID: Variant; Database: ISchemaDatabase): TDBRangeCursor; overload; virtual;
  published
    {:$ The name of this journal. }
    property JournalName: String read FJournalName write FJournalName;
    {:$ Reference to DatabaseSchema component, describing the database this journal belongs to. }
    property Schema: TDatabaseSchema read FSchema write FSchema;
    {:$ Name of the database table representing this journal. }
    property JournalTable: String read FJournalTable write FJournalTable;
    {:$ Name of the EntryNo field in the database table representing this journal. }
    property EntryNoField: String read FEntryNoField write FEntryNoField;
    {:$ Name of the DocumentID field in the database table representing this journal. }
    property DocumentIDField: String read FDocumentIDField write FDocumentIDField;
    {:$ The collection of rules specifying how to enter particular types of document }
    {:$ into this journal. }
    property Rules: TDBJournalRules read FRules write SetRules;

    {:$ This event is triggered when document entries are being deleted. }
    property OnDeleteEntries: TDatabaseEvent read FOnDeleteEntries write FOnDeleteEntries;
  end;

  {:$ State of documents }
  TDBDocumentState = (dcsInactive, dcsActive, dcsClosing, dcsLoading, dcsSaving);

  {:$ TDBDocument represents a document of a certain type. }
  TDBDocument = class (TComponent)
  protected
    FState: TDBDocumentState;
    FDocumentID: Variant;
    FMasterTable: TDataSet;
    FDataSets: TDocDataSets;
    FDocumentType: String;
    FDBDocumentType: TDBDocumentType;
    FDatabaseName: String;
    FDatabase: TComponent;
    FModified: Boolean;

    function GetActive: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure SetDataSets(const Value: TDocDataSets);
    procedure SetDocumentType(const Value: String);
    procedure SetDatabaseName(const Value: String);
    function GetDBDocumentType: TDBDocumentType;
    function GetDatabase: ISchemaDatabase;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    {:$ Creates an instance of TDBDocument object. }
    constructor Create(AOwner: TComponent); override;
    {:$ Destroys the instance of TDBDocument object. }
    destructor Destroy; override;

    {:$ Checks whether the document (and its underlying datasets) is active and }
    {:$ raises exception if it's not. }
    procedure CheckActive;
    {:$ Raises exception if there's no document type (TDBDocumentType) associated with this document. }
    procedure CheckDocumentType;
    {:$ Raises exception if there's no document Database (ISchemaDatabase) associated with this document. }
    procedure CheckDatabase;

    {:$ Initializes new document. }
    procedure New; virtual;
    {:$ Opens an existing document identified by DocumentID. }
    procedure Open(const DocumentID: Variant); virtual;
    {:$ Closes loaded document and cleans up memory. }
    procedure Close; virtual;
    {:$ Saves opened document. }
    procedure Save(Enter: Boolean = True); virtual;
    {:$ Deletes opened document. }
    procedure Delete; virtual;

    {:$ Specifies whether opened document has been modified and have to be saved. }
    property Modified: Boolean read FModified write FModified;
    {:$ Specifies DocumentID value of opened document. }
    property DocumentID: Variant read FDocumentID;
    {:$ Specifies the type of the document associated with this component. }
    property DBDocumentType: TDBDocumentType read GetDBDocumentType;
    {:$ Specifies the database database associated with this component. }
    property Database: ISchemaDatabase read GetDatabase;
    {:$ Provides access to the document's state. }
    property State: TDBDocumentState read FState;
    {:$ Property Active is read-only and is set to true if the document is loaded and can be accessed. }
    {:$ Use Load or New methods to set Active to true. }
    property Active: Boolean read GetActive write SetActive;
  published
    {:$ Type of the document. }
    property DocumentType: String read FDocumentType write SetDocumentType;
    {:$ Name of the database database associated with this component. }
    property DatabaseName: String read FDatabaseName write SetDatabaseName;
    {:$ Name of the master table }
    property MasterTable: TDataSet read FMasterTable write FMasterTable;
    {:$ Collection of possibly subordinated datasets comprising the document. }
    property DataSets: TDocDataSets read FDataSets write SetDataSets;
  end;

  TDBSourceFieldDef = class (TCollectionItem)
  protected
    FDataSize: Integer;
    FForeignDocumentType: String;
    FLookupRelation: String;
    FResultField: String;
    FFieldName: String;
    FFieldKind: TFieldKind;
    FDataType: TFieldType;
    function GetDisplayName: String; override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property FieldName: String read FFieldName write FFieldName;
    property DataType: TFieldType read FDataType write FDataType;
    property FieldKind: TFieldKind read FFieldKind write FFieldKind;
    property DataSize: Integer read FDataSize write FDataSize;
    { Relation, this lookup is based upon (if any) }
    property LookupRelation: String read FLookupRelation write FLookupRelation;
    property ResultField: String read FResultField write FResultField;
    { Only make sence if there's more then one type of document in table }
    property ForeignDocumentType: String read FForeignDocumentType write FForeignDocumentType;
  end;

  TDBSourceFieldDefs = class (TOwnedCollection)
  protected
    function GetItem(Index: Integer): TDBSourceFieldDef;
    procedure SetItem(Index: Integer; const Value: TDBSourceFieldDef);
  public
    {:$ Finds macro by name. Returns nil if no macro with this name found. }
    function Find(const Name: String): TDBSourceFieldDef;
    {:$ Provides array-like access to contained macros. }
    property Items[Index: Integer]: TDBSourceFieldDef read GetItem write SetItem; default;
  end;

  {:$ Remove all records from the collection of the datasets. }
  procedure ClearDataSets(DataSets: TDocDataSets);
  {:$ Opens or closes all datasets from the collection of the datasets. }
  procedure ActivateDataSets(DataSets: TDocDataSets; Active: Boolean = True);
  {:$ Load records from database database (Database) into master dataset (Master) }
  {:$ and the collection of datasets (DataSets) }
  procedure LoadRecords(Schema: TDatabaseSchema; Database: ISchemaDatabase;
    Master: TDataSet; DataSets: TDocDataSets);
  {:$ Stores records from master dataset (Master) and the collection of datasets }
  {:$ (DataSets) into the database. }
  procedure SaveRecords(Schema: TDatabaseSchema; Database: ISchemaDatabase;
    Master: TDataSet; DataSets: TDocDataSets);

  {:$ Returnes TDBDocumentType associated with the type passed as DocumentType parameter. }
  function FindDocumentType(const DocumentType: String): TDBDocumentType;
  {:$ Returnes TDBDocumentType associated with the abbreviation passed as DocTypeID parameter. }
  function FindDocTypeID(const DocTypeID: String): TDBDocumentType;

var
  DBDocumentTypes: TList;
  DBJournals: TList;

implementation

uses TypInfo;

procedure ClearDataSets(DataSets: TDocDataSets);
var
  I: Integer;
begin
  for I := 0 to DataSets.Count - 1 do
  begin
    if DataSets[I].DataSet <> nil then
      ClearDataSet(DataSets[I].DataSet);
    ClearDataSets(DataSets[I].DataSets);
  end;
end;

procedure ActivateDataSets(DataSets: TDocDataSets; Active: Boolean = True);
var
  I: Integer;
begin
  for I := 0 to DataSets.Count - 1 do
  begin
    if DataSets[I].DataSet <> nil then
      DataSets[I].DataSet.Active := Active;
    ActivateDataSets(DataSets[I].DataSets, Active);
  end;
end;

procedure LoadRecords(Schema: TDatabaseSchema; Database: ISchemaDatabase;
  Master: TDataSet; DataSets: TDocDataSets);
var
  I: Integer;
  KeyValue: Variant;
  Dest: TDataSet;
begin
  for I := 0 to DataSets.Count - 1 do
  if DataSets[I].DataSet <> nil then
  begin
    Dest := DataSets[I].DataSet;
    KeyValue := Master.FieldValues[DataSets[I].DocumentIDField];
    with Database.GetRangeCursor(DataSets[I].TableName, DataSets[I].DocumentIDField, '',
      DataSets[I].CaseInsensitive, KeyValue, DataSets[I].ItemIDField) do
    try
      ClearDataSet(Dest);
      Dest.DisableControls;
      try
        DataSet.First;
        while not DataSet.EOF do
        begin
          CopyRecord(DataSet, Dest);
          // Recursive call
          LoadRecords(Schema, Database, DataSet, DataSets[I].DataSets);
          DataSet.Next;
        end;
      finally
        Dest.EnableControls;
      end;
    finally
      Free;
    end;
  end;
end;

procedure SaveRecords(Schema: TDatabaseSchema;
  Database: ISchemaDatabase; Master: TDataSet; DataSets: TDocDataSets);
var
  I: Integer;
  KeyValue: Variant;
  Items: TDataSet;
  IndexName, IndexFieldNames: String;
begin
  for I := 0 to DataSets.Count - 1 do
  if DataSets[I].DataSet <> nil then
  begin
    Items := DataSets[I].DataSet;
    // Save items order
    IndexName := '';
    IndexFieldNames := GetStrProp(Items, propIndexFieldNames);
    if IndexFieldNames = '' then
      if GetPropInfo(Items, propIndexName) <> nil then
        IndexName := GetStrProp(Items, propIndexName)
      else
        IndexName := '';
    try
      SetStrProp(Items, propIndexFieldNames, DataSets[I].ItemIDField);
      KeyValue := Master.FieldValues[DataSets[I].DocumentIDField];
      with Database.GetRangeCursor(DataSets[I].TableName, DataSets[I].DocumentIDField, '',
        DataSets[I].CaseInsensitive, KeyValue, DataSets[I].ItemIDField) do
      try
        // Make sure all items have DocumentID field properly assigned
        AssignMasterField(Items, DataSets[I].DocumentIDField, KeyValue);
        // Copy records
        CopyDataSetWithKey(Items, DataSet, DataSets[I].ItemIDField);
        // Recursive call
        SaveRecords(Schema, Database, DataSet, DataSets[I].DataSets);
      finally
        Free;
      end;
    finally
      // Restore Items order
      if IndexFieldNames <> '' then
        SetStrProp(Items, propIndexFieldNames, IndexFieldNames)
      else if GetPropInfo(Items, propIndexName) <> nil then
        SetStrProp(Items, propIndexName, IndexName);
    end;
  end;
end;

function FindDocumentType(const DocumentType: String): TDBDocumentType;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to DBDocumentTypes.Count - 1 do
    if AnsiCompareText(TDBDocumentType(DBDocumentTypes[I]).DocumentType, DocumentType) = 0
    then begin
      Result := TDBDocumentType(DBDocumentTypes[I]);
      exit;
    end;
end;

function FindDocTypeID(const DocTypeID: String): TDBDocumentType;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to DBDocumentTypes.Count - 1 do
    if AnsiCompareText(TDBDocumentType(DBDocumentTypes[I]).DocTypeID, DocTypeID) = 0
    then begin
      Result := TDBDocumentType(DBDocumentTypes[I]);
      exit;
    end;
end;

{ TDBDocumentType }

constructor TDBDocumentType.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocumentType := '';
  FDocTypeID := '';
  FDescription := '';
  FMasterTable := '';
  FCaseInsensitive := False;
  FDocumentIDField := fldDocumentID;
  FSchema := nil;
  FEditFormName := '';
  FLookupFormName := '';
end;

destructor TDBDocumentType.Destroy;
begin
  if DBDocumentTypes <> nil then
    DBDocumentTypes.Remove(Self);
  inherited Destroy;
end;

procedure TDBDocumentType.CheckSchema;
begin
  if FSchema = nil then
    DatabaseErrorFmt(SSchemaIsNotAssigned, [FDocumentType]);
end;

procedure TDBDocumentType.Delete(const DocumentID: Variant; Database: ISchemaDatabase);
begin
  if Assigned(FOnDelete) then
    OnDelete(Self, Self, DocumentID, Database)
  else begin
    Database.StartTransaction;
    try
      // Delete Journal Entries
      DeleteEntries(DocumentID, Database);
      // Delete Document Records
      with Database.GetRangeCursor(MasterTable, DocumentIDField, '',
        CaseInsensitive, DocumentID) do
      try
        // Underlying child records if exists will be deleted by schema cascade relations
        ClearDataSet(DataSet);
      finally
        Free;
      end;
      Database.Commit;
    except
      Database.Rollback;
      raise;
    end;
  end;
end;

procedure TDBDocumentType.DeleteEntries(const DocumentID: Variant; Database: ISchemaDatabase);
var
  I: Integer;
begin
  if Assigned(FOnDeleteEntries) then
    FOnDeleteEntries(Self, Self, DocumentID, Database)
  else begin
    Database.StartTransaction;
    try
      for I := 0 to DBJournals.Count - 1 do
        TDBJournal(DBJournals[I]).DeleteEntries(Self, DocumentID, Database);

      Database.Commit;
    except
      Database.Rollback;
      raise;
    end;
  end;
end;

procedure TDBDocumentType.MakeEntries(ADocument: TDBDocument);
var
  I: Integer;
begin
  if Assigned(FOnMakeEntries) then
    FOnMakeEntries(Self, ADocument)
  else begin
    for I := 0 to DBJournals.Count - 1 do
      TDBJournal(DBJournals[I]).MakeEntries(ADocument);
  end;
end;

procedure TDBDocumentType.Load(ADocument: TDBDocument);
var
  LocateOptions: TLocateOptions;
begin
  if Assigned(FOnLoad) then
    FOnLoad(Self, ADocument)
  else with ADocument do begin
    CheckDatabase;
    CheckSchema;
    LocateOptions := [];
    if CaseInsensitive then
      Include(LocateOptions, loCaseInsensitive);
    if not MasterTable.Locate(DocumentIDField, DocumentID, LocateOptions) then
      DatabaseErrorFmt('Document (%s) not found: #%s', [DocumentType, DocumentID]);
    // Load detail records. Master table is already positioned on the
    // master (header) record.
    LoadRecords(Schema, Database, MasterTable, DataSets);
  end;
end;

procedure TDBDocumentType.New(ADocument: TDBDocument);
begin
  if Assigned(FOnNew) then FOnNew(Self, ADocument);
end;

procedure TDBDocumentType.Save(ADocument: TDBDocument);
begin
  if Assigned(FOnSave) then
    FOnSave(Self, ADocument)
  else with ADocument do begin
    CheckDatabase;
    CheckSchema;
    if MasterTable.State in dsEditModes then
      MasterTable.Post;
    SaveRecords(Schema, Database, MasterTable, DataSets);
  end;
end;

function TDBDocumentType.GenerateID(ADocument: TDBDocument): String;
begin
  Result := '';
  if Assigned(FOnGenerateID) then
    Result := FOnGenerateID(Self, ADocument);
end;

procedure TDBDocumentType.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FSchema) then
    Schema := nil;
  inherited;
end;

procedure TDBDocumentType.SetDocumentType(const Value: String);
begin
  if FDocumentType <> Value then
  begin
    if FindDocumentType(Value) <> nil then
      DatabaseErrorFmt(SDocumentTypeMustBeUnique, [Value]);

    FDocumentType := Value;
    RegisterDocumentType;
  end;
end;

procedure TDBDocumentType.SetDocTypeID(const Value: String);
begin
  if FDocTypeID <> Value then
  begin
    if FindDocTypeID(Value) <> nil then
      DatabaseErrorFmt(SDocumentTypeMustBeUnique, [Value]);
    FDocTypeID := Value;
    RegisterDocumentType;
  end;
end;

procedure TDBDocumentType.RegisterDocumentType;
begin
  if FDocumentType <> '' then
  begin
    if DBDocumentTypes.IndexOf(Self) < 0 then
      DBDocumentTypes.Add(Self);
  end else begin
    if DBDocumentTypes <> nil then
      DBDocumentTypes.Remove(Self);
  end;
end;

{ TDataSetItem }

constructor TDataSetItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDataSet := nil;
  FName := '';
  FTableName := '';
  FDocumentIDField := fldDocumentID;
  FItemIDField := fldItemID;
  FCaseInsensitive := False;
  FDataSets := TDocDataSets.Create(Self, TDataSetItem);
end;

destructor TDataSetItem.Destroy;
begin
  FDataSets.Free;
  inherited Destroy;
end;

function TDataSetItem.GetDisplayName: string;
begin
  Result := FName;
end;

function TDataSetItem.GetSchema: TDatabaseSchema;
var
  DocumentType: TDBDocumentType;
begin
  Result := nil;
  if TDocDataSets(Collection).GetOwner is TDataSetItem then
    Result := TDataSetItem(TDocDataSets(Collection).GetOwner).GetSchema
  else if TDocDataSets(Collection).GetOwner is TDBDocument then
  begin
    DocumentType := TDBDocument(TDocDataSets(Collection).GetOwner).DBDocumentType;
    if DocumentType <> nil then
      Result := DocumentType.Schema;
  end;
end;

procedure TDataSetItem.SetDataSets(const Value: TDocDataSets);
begin
  FDataSets.Assign(Value);
end;

{ TDBDocument }

constructor TDBDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModified := False;
  FState := dcsInactive;
  FDocumentID := '';
  FMasterTable := nil;
  FDatabaseName := '';
  FDatabase := nil;
  FDocumentType := '';
  FDBDocumentType := nil;
  FDataSets := TDocDataSets.Create(Self, TDataSetItem);
end;

destructor TDBDocument.Destroy;
begin
  // Active := False;
  FDataSets.Free;
  FModified := False;
  inherited Destroy;
end;

procedure TDBDocument.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  if (Operation = opRemove) then
  begin
    if AComponent = FDatabase then
      FDatabase := nil
    else if AComponent = FDBDocumentType then
      FDBDocumentType := nil
    else begin
      if AComponent = FMasterTable then
        MasterTable := nil;
      for I := 0 to DataSets.Count - 1 do
        if DataSets[I].DataSet = AComponent then
          DataSets[I].DataSet := nil;
    end;
  end;
  inherited;
end;

procedure TDBDocument.CheckActive;
begin
  if not Active then
    DatabaseError(SDocumentInactive);
end;

procedure TDBDocument.CheckDocumentType;
begin
  if DBDocumentType = nil then
    DatabaseErrorFmt(SDocumentTypeNotFound, [DocumentType]);
end;

procedure TDBDocument.CheckDatabase;
begin
  if Database = nil then
    DatabaseErrorFmt(SDatabaseNotFound, [DatabaseName]);
end;

procedure TDBDocument.Close;
begin
  if not Active then exit;
  FState := dcsClosing;
  try
    if MasterTable.State in dsEditModes then
      MasterTable.Cancel;

    if not (csDestroying in ComponentState) then
      ClearDataSets(DataSets);
    ActivateDataSets(DataSets, False);
    MasterTable.Active := False;
    FState := dcsInactive;
  except
    FState := dcsActive;
    raise;
  end;
end;

procedure TDBDocument.Delete;
begin
  CheckDocumentType;
  Close;
  FDBDocumentType.Delete(DocumentID, Database);
  FModified := False;
end;

procedure TDBDocument.New;
begin
  if Active then exit;
  CheckDocumentType;
  FState := dcsLoading;
  try
    MasterTable.Active := True;
    if MasterTable.State in dsEditModes then
      MasterTable.Cancel;
    MasterTable.Insert;
    ActivateDataSets(DataSets, True);
    ClearDataSets(DataSets);

    FDocumentID := FDBDocumentType.GenerateID(Self);
    FDBDocumentType.New(Self);
    FModified := True;
    FState := dcsActive;
  except
    FState := dcsInactive;
    raise;
  end;
end;

procedure TDBDocument.Open(const DocumentID: Variant);
begin
  if Active then exit;
  CheckDocumentType;
  FState := dcsLoading;
  try
    MasterTable.Active := True;
    ActivateDataSets(DataSets, True);
    ClearDataSets(DataSets);
    FDocumentID := DocumentID;
    DBDocumentType.Load(Self);
    FModified := False;
    FState := dcsActive;
  except
    FState := dcsInactive;
    raise;
  end;
end;

procedure TDBDocument.Save(Enter: Boolean);
begin
  CheckActive;
  CheckDocumentType;
  CheckDatabase;
  FState := dcsSaving;
  try
    Database.StartTransaction;
    try
      DBDocumentType.Save(Self);
      if Enter then
        DBDocumentType.MakeEntries(Self)
      else DBDocumentType.DeleteEntries(DocumentID, Database);
      FModified := False;
      Database.Commit;
    except
      Database.Rollback;
      raise;
    end;
  finally
    FState := dcsActive;
  end;
end;

procedure TDBDocument.SetActive(const Value: Boolean);
begin
  if Active <> Value then begin
    if Value then Open(DocumentID) else Close;
  end;
end;

function TDBDocument.GetActive: Boolean;
begin
  Result := FState = dcsActive;
end;

procedure TDBDocument.SetDataSets(const Value: TDocDataSets);
begin
  FDataSets.Assign(Value);
end;

procedure TDBDocument.SetDocumentType(const Value: String);
begin
  FDocumentType := Value;
  FDBDocumentType := nil;
  GetDBDocumentType;
end;

procedure TDBDocument.SetDatabaseName(const Value: String);
begin
  FDatabaseName := Value;
  FDatabase := nil;
  GetDatabase;
end;

function TDBDocument.GetDBDocumentType: TDBDocumentType;
begin
  if FDBDocumentType = nil then
  begin
    FDBDocumentType := FindDocumentType(FDocumentType);
    if FDBDocumentType <> nil then
      FDBDocumentType.FreeNotification(Self);
  end;
  Result := FDBDocumentType;
end;

function TDBDocument.GetDatabase: ISchemaDatabase;
begin
  if FDatabase = nil then
  begin
    FDatabase := FindDatabase(FDatabaseName);
    if FDatabase <> nil then
      FDatabase.FreeNotification(Self);
  end;
  if (FDatabase = nil) or not FDatabase.GetInterface(ISchemaDatabase, Result) then
    Result := nil
end;

{ TDocDataSets }

function TDocDataSets.Add: TDataSetItem;
begin
  Result := TDataSetItem(inherited Add);
end;

function TDocDataSets.Find(const Name: String): TDataSet;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if AnsiCompareText(Name, TDataSetItem(Items[I]).Name) = 0 then
    begin
      Result := TDataSetItem(Items[I]).DataSet;
      exit;
    end;
end;

function TDocDataSets.GetDataSetItem(Index: Integer): TDataSetItem;
begin
  Result := TDataSetItem(inherited GetItem(Index));
end;

procedure TDocDataSets.SetDataSetItem(Index: Integer;
  const Value: TDataSetItem);
begin
  inherited SetItem(Index, Value);
end;

{ TDBJournal }

constructor TDBJournal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRules := TDBJournalRules.Create(Self, TDBJournalRule);
  FJournalName := '';
  FDocumentIDField := fldDocumentID;
  FEntryNoField := fldEntryNo;
  RegisterJournal;
end;

destructor TDBJournal.Destroy;
begin
  if DBJournals <> nil then
    DBJournals.Remove(Self);
  FRules.Free;
  inherited Destroy;
end;

procedure TDBJournal.CheckSchema;
begin
  if FSchema = nil then
    DatabaseErrorFmt(SSchemaIsNotAssigned, [FJournalName]);
end;

procedure TDBJournal.DeleteEntries(Document: TDBDocument);
begin
  with Document do
    DeleteEntries(DBDocumentType, DocumentID, Database);
end;

procedure TDBJournal.DeleteEntries(DocumentType: TDBDocumentType;
  const DocumentID: Variant; Database: ISchemaDatabase);
begin
  if Assigned(FOnDeleteEntries) then
    FOnDeleteEntries(Self, DocumentType, DocumentID, Database)
  else begin
    Database.StartTransaction;
    try
      with GetRangeCursor(DocumentID, Database) do
      try
        ClearDataSet(DataSet);
      finally
        Free;
      end;
      Database.Commit;
    except
      Database.Rollback;
      raise;
    end;
  end;
end;

function TDBJournal.GetRangeCursor(const DocumentID: Variant; Database: ISchemaDatabase): TDBRangeCursor;
begin
  if Database = nil then
    DatabaseErrorFmt(SDatabaseIsNotAssigned, [DocumentID]);

  // Note: We hardcode case sensitive search here! Why?
  Result := Database.GetRangeCursor(JournalTable, FDocumentIDField, '', False, DocumentID);
end;

function TDBJournal.GetRangeCursor(Document: TDBDocument): TDBRangeCursor;
begin
  Result := GetRangeCursor(Document.DocumentID, Document.Database);
end;

procedure TDBJournal.MakeEntries(Document: TDBDocument);
var
  I: Integer;
begin
  for I := 0 to Rules.Count - 1 do
    if AnsiCompareText(Rules[I].DocumentType,
      Document.DocumentType) = 0 then
    begin
      if Assigned(Rules[I].OnMakeEntries) then
        Rules[I].OnMakeEntries(Self, Document);
    end;
end;

procedure TDBJournal.SetRules(const Value: TDBJournalRules);
begin
  FRules.Assign(Value);
end;

procedure TDBJournal.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) then
  begin
    if AComponent = FSchema then
      Schema := nil;
  end;
  inherited;
end;

procedure TDBJournal.RegisterJournal;
begin
  if DBJournals.IndexOf(Self) < 0 then
    DBJournals.Add(Self);
end;

{ TDBJournalRules }

function TDBJournalRules.Add: TDBJournalRule;
begin
  Result := TDBJournalRule(inherited Add);
end;

function TDBJournalRules.GetItem(Index: Integer): TDBJournalRule;
begin
  Result := TDBJournalRule(inherited GetItem(Index));
end;

procedure TDBJournalRules.SetItem(Index: Integer;
  const Value: TDBJournalRule);
begin
  inherited SetItem(Index, Value);
end;

{ TDBJournalRule }

constructor TDBJournalRule.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDocumentType := '';
end;

function TDBJournalRule.GetDisplayName: String;
begin
  Result := FDocumentType;
  if Result = '' then
    Result := inherited GetDisplayName;
end;


{ TDBSourceFieldDef }

constructor TDBSourceFieldDef.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFieldName := '';
  FDataType := DB.ftUnknown;
  FFieldKind := fkData;
  FDataSize := 0;
  FLookupRelation := '';
  FResultField := '';
  FForeignDocumentType := '';
end;

function TDBSourceFieldDef.GetDisplayName: String;
begin
  Result := FieldName;
end;

{ TDBSourceFieldDefs }

function TDBSourceFieldDefs.Find(const Name: String): TDBSourceFieldDef;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if AnsiSameText(Items[I].FieldName, Name) then
    begin
      Result := Items[I];
      exit;
    end;
  Result := nil;
end;

function TDBSourceFieldDefs.GetItem(Index: Integer): TDBSourceFieldDef;
begin
  Result := TDBSourceFieldDef(inherited Items[Index]);
end;

procedure TDBSourceFieldDefs.SetItem(Index: Integer;
  const Value: TDBSourceFieldDef);
begin
  inherited Items[Index] := Value;
end;

initialization
  DBJournals := TList.Create;
  DBDocumentTypes := TList.Create;
finalization
  DBJournals.Free;
  DBDocumentTypes.Free;
end.
