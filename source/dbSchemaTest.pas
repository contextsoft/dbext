(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Classes that implement testing for the database schema.
(*  Contains:
(*                TDBSchemaMessage
(*                TDBSchemaMessages - collection of TDBSchemaMessage
(*                TDBSchemaTester - class, that implements testing
(*                      of the database schema.
(*
(*  Copyright (c) 2005-2011, Context Software LLC
(*
(*  ------------------------------------------------------------
(*  FILE        : dbSchemaTest.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.36
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009, 2010, XE
(*
(******************************************************************************)
unit dbSchemaTest;

interface

uses Classes, SysUtils, DB, dbSchema, dbEngProfile;

type
  TDBSchemaTester = class;
  TDBSchemaMessage = class;
  TDBSchemaMessages = class;

  TDBSchemaMessageObject = (smoSchema, smoEnum, smoTable, smoField,
    smoIndex, smoRelation, smoTrigger, smoRelationship,
    smoDomain, smoView, smoStoredProc, smoSequence, smoTableConstraint,
    smoCustomObject, smoModule, smoUnknown);

  TDBSchemaMessageSeverity = (smsHint, smsWarning, smsError);
  TDBSchemaMessageCode = (smcNone,
    smcInvalidSchemaName,
    smcEnumInvalidName,
    smcEnumEmpty,
    smcEnumInvalidItem,
    smcEnumDuplicateIdentifier,
    smcEnumInvalidIntValue,
    smcEnumDuplicateValue,
    smcTableInvalidName,
    smcTableDuplicateName,
    smcTableDuplicateIdentifier,
    smcTableNoFieldsDefined,
    smcTableNoPrimaryIndex,

    smcFieldInvalidName,
    smcFieldDuplicateName,
    smcFieldInvalidDomain,
    smcFieldTypeUndefined,
    smcFieldTypeNotSupportedByDatabase,
    smcFieldInvalidSize,
    smcFieldInvalidAggExpr,
    smcFieldInvalidRelation,
    smcFieldInvalidRelationType,

    smcIndexFieldNotFound,
    smcIndexInvalidFieldType,
    smcIndexInvalidName,
    smcIndexDuplicateName,
    smcIndexNoFields,
    smcRelationInvalidName,
    smcRelationDuplicateName,
    smcTriggerInvalidName,
    smcTriggerDuplicateName,
    smcTriggerNoPrimaryKey,
    smcTriggerEmptyDefinition,
    smcViewInvalidName,
    smcViewDuplicateName,
    smcViewEmptyDefinition,
    smcStoredProcInvalidName,
    smcStoredProcDuplicateName,
    smcStoredProcEmptyDefinition,
    smcDomainInvalidName,
    smcDomainDuplicateName,
    smcDomainTypeUndefined,
    smcDomainInvalidSize,
    smcSequenceInvalidName,
    smcSequenceDuplicateName,
    smcSequenceInvalidDefinition,
    smcTableConstraintInvalidName,
    smcTableConstraintDuplicateName,
    smcTableConstraintEmptyDefinition,
    smcRelationshipInvalidName,
    smcRelationshipDuplicateName,
    smcRelationshipFieldNotFound,
    smcRelationshipInvalidMasterTable,
    smcRelationshipInvalidDetailTable,
    smcRelationshipMasterKeyFieldsNotIndexed, // only for engines not supporting real fks
    smcRelationshipDetailKeyFieldsNotIndexed, // only for engines not supporting real fks
    smcRelationshipFieldTypeMismatch,
    smcRelationshipFieldCountMismatch,
    smcRelationshipNoKeyFieldsDefined,
    smcRelationshipMasterRecordNotOptional,
    smcRelationshipMasterRecordOptional,
    smcRelationshipOneToOneCannotBeEnforced,
    smcRelationshipManyToOneCannotBeEnforced,

    smcFieldDomainNameCaseMismatch,
    smcIndexFieldNameCaseMismatch,
    smcRelationshipMasterFieldNameCaseMismatch,
    smcRelationshipDetailFieldNameCaseMismatch,

    smcFieldNameInvalidPattern,
    smcDomainNameInvalidPattern,
    smcIndexNameInvalidPattern,
    smcConstraintNameInvalidPattern,
    smcTriggerNameInvalidPattern,
    smcTableNameInvalidPattern,
    smcViewNameInvalidPattern,
    smcStoredProcNameInvalidPattern,
    smcRelationshipNameInvalidPattern,
    smcSequenceNameInvalidPattern,

    smcCustomObjectDuplicateName,
    smcCustomObjectInvalidName,
    smcCustomObjectNameInvalidPattern,

    smcIdentifierIsTooLong,
    smcFieldInvalidEnum,

    smcInvalidPropValue, //Add DB 21/07/2009
    smcCustomMessage, //Add DB 21/07/2009

    smcRelationshipNullifyDetailFieldsRequired,
    smcRelationshipDefaultDetailFieldsNotDefault,

    smcUnknown);

  TDBSchemaMessageCodes = set of TDBSchemaMessageCode;

  TOnStatusMessage = procedure (Sender: TObject; const StatusMessage: String) of object;
  TOnResultMessage = procedure (Sender: TObject; ResultMessage: TDBSchemaMessage) of object;
  TOnItemValidate = procedure (ASender: TDBSchemaTester; AItem: TSchemaCollectionItem;
    const MessageObject: TDBSchemaMessageObject) of object;

  TDBSchemaMessage = class (TCollectionItem)
  private
    FCode: TDBSchemaMessageCode;
    FObjectType: TDBSchemaMessageObject;
    FSeverity: TDBSchemaMessageSeverity;
    FObjectNames: array [TDBSchemaMessageObject] of String;
    FContextInfo: String;
    FContextObject: TObject;

    function GetDefaultMessage: String;
    function GetContextName: String;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;

    function GetNames(Index: TDBSchemaMessageObject): String;
    procedure SetNames(const Index: TDBSchemaMessageObject; const Value: String);

    {:$ Specifies the text that appears in list view. }
    function GetDisplayName: String; override;

    function GetField(const FieldText: String; Data: Pointer): String;

    property DefaultMessage: String read GetDefaultMessage;
    property ContextObject: TObject read FContextObject write FContextObject;
  published
    property Code: TDBSchemaMessageCode read FCode write FCode default smcNone;
    property Severity:  TDBSchemaMessageSeverity read FSeverity write FSeverity default smsError;
    property ObjectType: TDBSchemaMessageObject read FObjectType write FObjectType default smoSchema;
    property ContextInfo: String read FContextInfo write FContextInfo;

    property ContextName: String read GetContextName;
    property TableName: String index smoTable read GetNames write SetNames;
    property EnumName: String index smoEnum read GetNames write SetNames;
    property FieldName: String index smoField read GetNames write SetNames;
    property IndexName: String index smoIndex read GetNames write SetNames;
    property RelationName: String index smoRelation read GetNames write SetNames;
    property TriggerName: String index smoTrigger read GetNames write SetNames;
    property RelationshipName: String index smoRelationship read GetNames write SetNames;
    property DomainName: String index smoDomain read GetNames write SetNames;
    property ViewName: String index smoView read GetNames write SetNames;
    property StoredProcName: String index smoStoredProc read GetNames write SetNames;
    property SequenceName: String index smoSequence read GetNames write SetNames;
    property TableConstraintName: String index smoTableConstraint read GetNames write SetNames;
    property CustomObjectName: String index smoCustomObject read GetNames write SetNames;
  end;

  { collection of TDBSchemaMessage }
  TDBSchemaMessages = class (TOwnedCollection)
    function GetItem(Index: Integer): TDBSchemaMessage;
    procedure SetItem(Index: Integer; Value: TDBSchemaMessage);
  public
    {:$ Creates and adds a new TDBSchemaMessage item to the TDBSchemaMessages collection. }
    function Add(Code: TDBSchemaMessageCode = smcNone; ObjectType: TDBSchemaMessageObject = smoSchema;
      Severity: TDBSchemaMessageSeverity = smsError): TDBSchemaMessage;
    {:$ Provides access to the TDBSchemaMessage items in the collection by index. }
    property Items[Index: Integer]: TDBSchemaMessage read GetItem write SetItem; default;
  end;

  TDBSchemaTester = class (TComponent)
  protected
    FMessages: TDBSchemaMessages;
    FSchema: TDatabaseSchema;
    FShowHints: Boolean;
    FShowWarnings: Boolean;
    FIdentifiers: TStringList;
    FDisabledMessages: TStrings;
    FWrongNames: TStringList;
    FDBEngineProfile: TDBEngineProfile;
    FOnResultMessage: TOnResultMessage;
    FOnStatusMessage: TOnStatusMessage;
    FOnItemValidate: TOnItemValidate;

    FErrorCount: integer;

    procedure SetDisabledMessages(const Value: String);
    function GetDisabledMessages: String;
    procedure SetMessages(const Value: TDBSchemaMessages);
    procedure SetSchema(const Value: TDatabaseSchema);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function IsValidIdentifier(Obj: TObject; const Str: String; ACode: TDBSchemaMessageCode): Boolean;
    function IsNameUnique(Item: TSchemaCollectionItem): Boolean;
    procedure TestEnumeration(Enumeration: TEnumeration); virtual;
    procedure TestTable(TableDef: TTableDefinition); virtual;
    procedure TestField(FieldDef: TFieldDefinition); virtual;
    procedure TestIndex(IndexDef: TIndexDefinition); virtual;
    procedure TestRelation(Relation: TRelation); virtual;
    procedure TestTrigger(TriggerDef: TTriggerDefinition); virtual;
    procedure TestView(ViewDef: TViewDefinition); virtual;
    procedure TestStoredProc(StoredProc: TStoredProcDefinition); virtual;
    procedure TestDomain(Domain: TDomain); virtual;
    procedure TestSequence(Sequence: TSequence); virtual;
    procedure TestRelationship(Relationship: TRelationship); virtual;
    procedure TestTableConstraint(TableConstraint: TTableConstraint); virtual;
    procedure TestCustomObject(CustomObject: TCustomObject); virtual;
    procedure TestSchema(Schema: TDatabaseSchema = nil); virtual;
    procedure PrepareWrongNames;
    procedure ClearResults;
    function TopLevelUnique(Item: TSchemaCollectionItem): Boolean;
    function IsCodeDisabled(Code: TDBSchemaMessageCode): Boolean;
    function AddResult(Code: TDBSchemaMessageCode = smcNone; ObjectType: TDBSchemaMessageObject = smoSchema;
      Severity: TDBSchemaMessageSeverity = smsError;
      ContextObject: TObject = nil; ContextInfo: String = ''): TDBSchemaMessage;
    procedure DoStatus(const StatusMessage: String);
    procedure DoValidate(AItem: TSchemaCollectionItem; const ObjectType: TDBSchemaMessageObject);

    property DisabledMessages: String read GetDisabledMessages write SetDisabledMessages;
    property DBEngineProfile: TDBEngineProfile read FDBEngineProfile;
    property ErrorCount: integer read FErrorCount;
  published
    property TestResults: TDBSchemaMessages read FMessages write SetMessages;
    property Schema: TDatabaseSchema read FSchema write SetSchema;
    property ShowHints: Boolean read FShowHints write FShowHints default True;
    property ShowWarnings: Boolean read FShowWarnings write FShowWarnings default True;
    { Events }
    property OnStatusMessage: TOnStatusMessage read FOnStatusMessage write FOnStatusMessage;
    property OnResultMessage: TOnResultMessage read FOnResultMessage write FOnResultMessage;
    property OnItemValidate: TOnItemValidate read FOnItemValidate write FOnItemValidate;
  end;

resourcestring
  { Severity types }
  SHint = 'Hint';
  SWarning = 'Warning';
  SError = 'Error';

  { Status messages }
  STestingSchema = 'Processing schema ';
  STestingComplete = 'Done.';
  STestingEnum = 'Processing enumeration ';
  STestingTable = 'Processing table ';
  STestingField = 'Processing field ';
  STestingIndex = 'Processing index ';
  STestingRelation = 'Processing relation ';
  STestingTrigger = 'Processing trigger ';
  STestingView = 'Processing view ';
  STestingDomain = 'Processing domain ';
  STestingSequence = 'Processing sequence ';
  STestingStoredProc = 'Processing stored procedures ';
  STestingTableConstraint = 'Processing table constraint ';
  STestingRelationship = 'Processing relationship ';
  STestingCustomObject = 'Processing object ';

  { Error messages }
  SProfileNotFound = 'Database engine profile not found: "%s"';

  { Result messages }
  SUnknown = 'Unknown';

  SInvalidSchemaName = 'Schema name is missing';

  SEnumInvalidName = 'Invalid enumeration name';
  SEnumEmpty = 'Enumeration "<%EnumName%>" is empty';
  SEnumInvalidItem = 'Invalid item in enumeration "<%EnumName%>"';
  SEnumDuplicateIdentifier = 'Duplicate identifier in enumeration "<%EnumName%>"';
  SEnumInvalidIntValue = 'Invalid Integer value in enumeration "<%EnumName%>"';
  SEnumDuplicateValue = 'Duplicate value (all values must be unique!) in enumeration "<%EnumName%>"';

  STableInvalidName = 'Invalid or empty table name';
  STableDuplicateName = 'Duplicate table name "<%TableName%>"';
  STableDuplicateIdentifier = 'Duplicate identifier in table "<%TableName%>"';
  STableNoFieldsDefined = 'No fields defined for table "<%TableName%>"';
  STableNoPrimaryIndex = 'No primary index defined for table "<%TableName%>"';

  SFieldInvalidName = 'Invalid or empty field name "<%TableName%>"';
  SFieldDuplicateName = 'Duplicate field name "<%TableName%>"."<%FieldName%>"';
  SFieldInvalidDomain = 'Domain "<%DomainName%>" not found in "<%TableName%>"."<%FieldName%>"';
  SFieldTypeUndefined = 'Field type undefined "<%TableName%>"."<%FieldName%>"';
  SFieldTypeNotSupportedByDatabase = 'Data type not supported by database engine "<%ContextName%>"';
  SFieldInvalidEnum = 'Enumeration "<%EnumName%>" not found in "<%TableName%>"."<%FieldName%>"';

  SFieldInvalidSize = 'Invalid field size "<%TableName%>"."<%FieldName%>"';
  SFieldInvalidAggExpr = 'Aggregate expression must not be empty "<%TableName%>"."<%FieldName%>"';
  SFieldInvalidRelation = 'Aggregate relation not found "<%TableName%>"."<%FieldName%>"';
  SFieldInvalidRelationType = 'Aggregate relation type if invalid "<%TableName%>"."<%FieldName%>". Must be One-To-Many or One-To-One.';

  SFieldDomainNameCaseMismatch = 'Domain name "<%DomainName%>" does not match case of the referred domain';

  SIndexInvalidName = 'Invalid or empty index name in table "<%TableName%>"';
  SIndexDuplicateName = 'Duplicate index "<%IndexName%>" in table "<%TableName%>"';
  SIndexFieldNotFound = 'Field not found in index "<%IndexName%>" in table "<%TableName%>"';
  SIndexInvalidFieldType = 'Field in index "<%IndexName%>" in table "<%TableName%>" cannot be indexed';
  SIndexNoFields = 'No fields or expression defined for index "<%IndexName%>" in table "<%TableName%>"';
  SIndexFieldNameCaseMismatch = 'Field in index "<%IndexName%>" in table "<%TableName%>" does not match case of the referred field';

  SRelationInvalidName = 'Invalid or empty relation name in table "<%TableName%>"';
  SRelationDuplicateName = 'Duplicate relation "<%RelationName%>" in table "<%TableName%>"';

  SRelationshipInvalidName = 'Invalid or empty relationship name';
  SRelationshipDuplicateName = 'Duplicate relationship name "<%RelationshipName%>"';
  SRelationshipFieldNotFound = 'One of key fields not found in relationship "<%RelationshipName%>"';
  SRelationshipInvalidMasterTable = 'Master table not found in relationship "<%RelationshipName%>"';
  SRelationshipInvalidDetailTable = 'Detail table not found in relationship "<%RelationshipName%>"';
  SRelationshipMasterKeyFieldsNotIndexed = 'Master key fields not indexed  in relationship "<%RelationshipName%>"';
  SRelationshipDetailKeyFieldsNotIndexed = 'Detail key fields not indexed  in relationship "<%RelationshipName%>"';
  SRelationshipFieldTypeMismatch = 'Master and detail key fields have different types or sizes in relationship "<%RelationshipName%>"';
  SRelationshipFieldCountMismatch = 'Different number of master and detail key fields defined for relationship "<%RelationshipName%>"';
  SRelationshipNoKeyFieldsDefined = 'No key fields defined for relationship "<%RelationshipName%>"';
  SRelationshipMasterRecordNotOptional = 'Cannot enforce "master record optional" condition due to "not null" constraint in relationship "<%RelationshipName%>"';
  SRelationshipMasterRecordOptional = 'Cannot enforce "master record NOT optional" condition due to lack of "not null" constraint in relationship "<%RelationshipName%>"';
  SRelationshipOneToOneCannotBeEnforced = 'Cannot enforce 0..1:1 condition due to lack of "unique" constraint or index in relationship "<%RelationshipName%>"';
  SRelationshipManyToOneCannotBeEnforced = 'Cannot enforce 0..N:1 condition due to "unique" constraint in relationship "<%RelationshipName%>"';

  SRelationshipMasterFieldNameCaseMismatch = 'Master key field name does not match referred field in relationship "<%RelationshipName%>"';
  SRelationshipDetailFieldNameCaseMismatch = 'Detail key field name does not match referred field in relationship "<%RelationshipName%>"';

  STriggerInvalidName = 'Invalid or empty trigger name in table "<%TableName%>"';
  STriggerDuplicateName = 'Duplicate trigger name "<%TriggerName%>" in table "<%TableName%>"';
  STriggerNoPrimaryKey = 'Primary index for table "<%TableName%>" must exist if upon-commit trigger "<%TriggerName%>" is defined';
  STriggerEmptyDefinition = 'Definition is empty for trigger "<%TriggerName%>"';

  SViewInvalidName = 'Invalid or empty view name "<%ViewName%>"';
  SViewDuplicateName = 'Duplicate view name "<%ViewName%>"';
  SViewEmptyDefinition = 'Definition is empty for view "<%ViewName%>"';

  SStoredProcInvalidName = 'Invalid or empty stored procedure name "<%StoredProcName%>"';
  SStoredProcDuplicateName = 'Duplicate stored procedure name "<%StoredProcName%>"';
  SStoredProcEmptyDefinition = 'Definition is empty for stored procedure "<%StoredProcName%>"';

  SDomainInvalidName = 'Invalid or empty domain name "<%DomainName%>"';
  SDomainDuplicateName = 'Duplicate domain name "<%DomainName%>"';
  SDomainTypeUndefined = 'Domain type undefined "<%DomainName%>"';
  SDomainInvalidSize = 'Invalid data size in domain "<%DomainName%>"';

  SSequenceInvalidName = 'Invalid or empty sequence name "<%SequenceName%>"';
  SSequenceDuplicateName  = 'Duplicate sequence name "<%SequenceName%>"';
  SSequenceInvalidDefinition = 'Invalid parameters in sequence "<%SequenceName%>"';

  STableConstraintInvalidName = 'Invalid or empty table check constraint name "<%TableConstraintName%>" in table "<%TableName%>"';
  STableConstraintDuplicateName = 'Duplicate table check constraint name "<%TableConstraintName%>" in table "<%TableName%>"';
  STableConstraintEmptyDefinition = 'Definition is empty for table check constraint "<%TableConstraintName%>" in table "<%TableName%>"';

  SFieldNameInvalidPattern = 'Field name "<%TableName%>"."<%FieldName%>" contains characters or patterns not allowed by database engine';
  SDomainNameInvalidPattern = 'Field name "<%TableName%>"."<%FieldName%>" contains characters or patterns not allowed by database engine';
  SIndexNameInvalidPattern = 'Index name "<%IndexName%>" in table "<%TableName%>" contains characters or patterns not allowed by database engine';
  SConstraintNameInvalidPattern = 'Constraint name "<%TableConstraintName%>" in table "<%TableName%>" contains characters or patterns not allowed by database engine';
  STriggerNameInvalidPattern = 'Trigger name "<%IndexName%>" for table "<%TableName%>" contains characters or patterns not allowed by database engine';
  STableNameInvalidPattern = 'Table name "<%TableName%>" contains characters or patterns not allowed by database engine';
  SViewNameInvalidPattern = 'View name "<%ViewName%>" contains characters or patterns not allowed by database engine';
  SStoredProcNameInvalidPattern = 'Procedure name "<%StoredProcName%>" contains characters or patterns not allowed by database engine';
  SRelationshipNameInvalidPattern = 'Foreign key name "<%RelationshipName%>" contains characters or patterns not allowed by database engine';
  SSequenceNameInvalidPattern = 'Sequence name "<%SequenceName%>" contains characters or patterns not allowed by database engine';

  SCustomObjectDuplicateName = 'Invalid or empty object name "<%CustomObjectName%>"';
  SCustomObjectInvalidName = 'Duplicate object name "<%CustomObjectName%>"';
  SCustomObjectNameInvalidPattern = 'Object name "<%CustomObjectName%>" contains characters or patterns not allowed by database engine';

  SRelationshipNullifyDetailFieldsRequired = 'Creating a constraint with Nullify action but some of the fields are having "NOT NULL" constraint.';
  SRelationshipDefaultDetailFieldsNotDefault = 'Creating a constraints with Set Default action but some of the fields are not having a default value.';

  SIdentifierIsTooLong = 'Identifier exceeds maximum allowed length for this database';

const
  DBSchemaMessageCodeMessages: array [TDBSchemaMessageCode] of String = ('',
    SInvalidSchemaName,
    SEnumInvalidName,
    SEnumEmpty,
    SEnumInvalidItem,
    SEnumDuplicateIdentifier,
    SEnumInvalidIntValue,
    SEnumDuplicateValue,
    STableInvalidName,
    STableDuplicateName,
    STableDuplicateIdentifier,
    STableNoFieldsDefined,
    STableNoPrimaryIndex,
    SFieldInvalidName,
    SFieldDuplicateName,
    SFieldInvalidDomain,
    SFieldTypeUndefined,
    SFieldTypeNotSupportedByDatabase,
    SFieldInvalidSize,
    SFieldInvalidAggExpr,
    SFieldInvalidRelation,
    SFieldInvalidRelationType,
    SIndexFieldNotFound,
    SIndexInvalidFieldType,
    SIndexInvalidName,
    SIndexDuplicateName,
    SIndexNoFields,
    SRelationInvalidName,
    SRelationDuplicateName,
    STriggerInvalidName,
    STriggerDuplicateName,
    STriggerNoPrimaryKey,
    STriggerEmptyDefinition,
    SViewInvalidName,
    SViewDuplicateName,
    SViewEmptyDefinition,
    SStoredProcInvalidName,
    SStoredProcDuplicateName,
    SStoredProcEmptyDefinition,
    SDomainInvalidName,
    SDomainDuplicateName,
    SDomainTypeUndefined,
    SDomainInvalidSize,
    SSequenceInvalidName,
    SSequenceDuplicateName,
    SSequenceInvalidDefinition,
    STableConstraintInvalidName,
    STableConstraintDuplicateName,
    STableConstraintEmptyDefinition,
    SRelationshipInvalidName,
    SRelationshipDuplicateName,
    SRelationshipFieldNotFound,
    SRelationshipInvalidMasterTable,
    SRelationshipInvalidDetailTable,
    SRelationshipMasterKeyFieldsNotIndexed,
    SRelationshipDetailKeyFieldsNotIndexed,
    SRelationshipFieldTypeMismatch,
    SRelationshipFieldCountMismatch,
    SRelationshipNoKeyFieldsDefined,
    SRelationshipMasterRecordNotOptional,
    SRelationshipMasterRecordOptional,
    SRelationshipOneToOneCannotBeEnforced,
    SRelationshipManyToOneCannotBeEnforced,
    SFieldDomainNameCaseMismatch,
    SIndexFieldNameCaseMismatch,
    SRelationshipMasterFieldNameCaseMismatch,
    SRelationshipDetailFieldNameCaseMismatch,
    SFieldNameInvalidPattern,
    SDomainNameInvalidPattern,
    SIndexNameInvalidPattern,
    SConstraintNameInvalidPattern,
    STriggerNameInvalidPattern,
    STableNameInvalidPattern,
    SViewNameInvalidPattern,
    SStoredProcNameInvalidPattern,
    SRelationshipNameInvalidPattern,
    SSequenceNameInvalidPattern,
    SCustomObjectDuplicateName,
    SCustomObjectInvalidName,
    SCustomObjectNameInvalidPattern,

    SIdentifierIsTooLong,
    SFieldInvalidEnum,
    '', //smcInvalidPropValue
    '', //smcCustomMessage

    SRelationshipNullifyDetailFieldsRequired,
    SRelationshipDefaultDetailFieldsNotDefault,

    SUnknown);

  DBSchemaMessageSeverityDisplayText: array [TDBSchemaMessageSeverity] of String =
    (SHint, SWarning, SError);

  function GetSchemaMessageObject(Obj: TObject): TDBSchemaMessageObject;

implementation

uses TypInfo, Math;

resourcestring
  SSchemaIsNotAssigned = 'Schema is not assigned';

function GetSchemaMessageObject(Obj: TObject): TDBSchemaMessageObject;
begin
  Result := smoUnknown;
  if Obj = nil then exit;
  if Obj.InheritsFrom(TDatabaseSchema) then
    Result := smoSchema
  else if Obj is TEnumeration then
    Result := smoEnum
  else if Obj is TDomain then
    Result := smoDomain
  else if Obj is TSequence then
    Result := smoSequence
  else if Obj is TStoredProcDefinition then
    Result := smoStoredProc
  else if Obj is TModuleDefinition then
    Result := smoModule
  else if Obj is TViewDefinition then
    Result := smoView
  else if Obj is TTableDefinition then
    Result := smoTable
  else if Obj is TCustomObject then
    Result := smoCustomObject
  else if Obj is TFieldDefinition then
    Result := smoField
  else if Obj is TIndexDefinition then
    Result := smoIndex
  else if Obj is TRelation then
    Result := smoRelation
  else if Obj is TTriggerDefinition then
    Result := smoTrigger
  else if Obj is TTableConstraint then
    Result := smoTableConstraint
  else if Obj is TRelationship then
    Result := smoRelationship;
end;

{ TDBSchemaMessage }

constructor TDBSchemaMessage.Create(Collection: TCollection);
var
  I: TDBSchemaMessageObject;
begin
  inherited Create(Collection);
  FCode := smcNone;
  FSeverity := smsError;
  FObjectType := smoSchema;
  FContextInfo := '';
  FContextObject := nil;
  for I := Low(TDBSchemaMessageObject) to High(TDBSchemaMessageObject) do
    FObjectNames[I] := '';
end;

procedure TDBSchemaMessage.Assign(Source: TPersistent);
var
  I: TDBSchemaMessageObject;
begin
  if Source is TDBSchemaMessage then
  begin
    FCode := TDBSchemaMessage(Source).Code;
    FSeverity := TDBSchemaMessage(Source).Severity;
    FObjectType := TDBSchemaMessage(Source).ObjectType;
    for I := Low(TDBSchemaMessageObject) to High(TDBSchemaMessageObject) do
      FObjectNames[I] := TDBSchemaMessage(Source).GetNames(I);
  end else inherited;
end;

function TDBSchemaMessage.GetDefaultMessage: String;
begin
  Result := ParseFields(DBSchemaMessageCodeMessages[Code], '<%', '%>', GetField, nil);
  if ContextInfo <> '' then
    if Code <> smcInvalidPropValue then
      Result := Result + ' (''' + ContextInfo + ''')' else
      Result := Result + ' ' + ContextInfo;
end;

function TDBSchemaMessage.GetNames(Index: TDBSchemaMessageObject): String;
begin
  Result := FObjectNames[Index];
end;

procedure TDBSchemaMessage.SetNames(const Index: TDBSchemaMessageObject;
  const Value: String);
begin
  FObjectNames[Index] := Value;
end;

function TDBSchemaMessage.GetDisplayName: String;
begin
  Result := Format('[%s] %3.3d %s', [
    DBSchemaMessageSeverityDisplayText[Severity], Integer(Code),
    DefaultMessage]);
end;

function TDBSchemaMessage.GetField(const FieldText: String; Data: Pointer): String;
begin
  try
    if IsPublishedProp(Self, FieldText) then
      Result := GetPropValue(Self, FieldText) else
      Result := 'ERROR';
  except
    Result := 'ERROR';  
  end;
end;

function TDBSchemaMessage.GetContextName: String;
begin
  Result := '';
  if (ContextObject <> nil) and ContextObject.InheritsFrom(TSchemaCollectionItem) then
    Result := TSchemaCollectionItem(ContextObject).FullName;
end;

{ TDBSchemaMessages }

function TDBSchemaMessages.Add(Code: TDBSchemaMessageCode = smcNone; ObjectType: TDBSchemaMessageObject = smoSchema;
  Severity: TDBSchemaMessageSeverity = smsError): TDBSchemaMessage;
begin
  Result := TDBSchemaMessage(inherited Add);
  Result.Code := Code;
  Result.ObjectType := ObjectType;
  Result.Severity := Severity;
end;

function TDBSchemaMessages.GetItem(Index: Integer): TDBSchemaMessage;
begin
  Result := TDBSchemaMessage(inherited GetItem(Index));
end;

procedure TDBSchemaMessages.SetItem(Index: Integer;
  Value: TDBSchemaMessage);
begin
  inherited SetItem(Index, Value);
end;

{ TDBSchemaTester }

constructor TDBSchemaTester.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMessages := TDBSchemaMessages.Create(Self, TDBSchemaMessage);
  FSchema := nil;
  FShowHints := True;
  FShowWarnings := True;
  FIdentifiers := nil;
  FDisabledMessages := TStringList.Create;
end;

destructor TDBSchemaTester.Destroy;
begin
  inherited Destroy;
  FMessages.Free;
  FDisabledMessages.Free;
end;

procedure TDBSchemaTester.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FSchema) and (Operation = opRemove) then
    FSchema := nil;
  inherited;
end;

procedure TDBSchemaTester.ClearResults;
begin
  FMessages.Clear;
  FErrorCount := 0;
end;

function TDBSchemaTester.IsCodeDisabled(Code: TDBSchemaMessageCode): Boolean;
begin
  Result := False;
  if FDisabledMessages.Count = 0 then exit;
  Result := FDisabledMessages.IndexOf(IntToStr(Ord(Code))) >= 0;
end;

function TDBSchemaTester.AddResult(Code: TDBSchemaMessageCode;
  ObjectType: TDBSchemaMessageObject; Severity: TDBSchemaMessageSeverity;
  ContextObject: TObject; ContextInfo: String): TDBSchemaMessage;
begin
  if (Severity = smsError) and not IsCodeDisabled(Code) then
    inc(FErrorCount);
  Result := nil;
  if ((Severity = smsError)
    or ((Severity = smsWarning) and FShowWarnings)
    or ((Severity = smsHint) and FShowHints))
    and not IsCodeDisabled(Code)
  then begin
    Result := FMessages.Add(Code, ObjectType, Severity);
    Result.ContextObject := ContextObject;
    Result.ContextInfo := ContextInfo;
    if ContextObject <> nil then
      Result.FObjectNames[ObjectType] := GetStrProp(ContextObject, 'name');

    // Assign table name
    if Assigned(ContextObject) and ContextObject.InheritsFrom(TTableCollectionItem) then
      Result.TableName := TTableCollectionItem(ContextObject).TableName;
    if Assigned(ContextObject) and ContextObject.InheritsFrom(TFieldDefinition) then
    begin
      Result.DomainName := TFieldDefinition(ContextObject).Domain;
      Result.EnumName := TFieldDefinition(ContextObject).Enumeration;
    end;

    if Assigned(FOnResultMessage) then
      FOnResultMessage(Self, Result);
  end;
end;

procedure TDBSchemaTester.SetMessages(const Value: TDBSchemaMessages);
begin
  FMessages.Assign(Value);
end;

procedure TDBSchemaTester.SetSchema(const Value: TDatabaseSchema);
begin
  if FSchema <> Value then
  begin
    FSchema := Value;
    if Assigned(FSchema) then
      FSchema.FreeNotification(Self);
  end;
end;

procedure TDBSchemaTester.DoStatus(const StatusMessage: String);
begin
  if Assigned(FOnStatusMessage) then
    FOnStatusMessage(Self, StatusMessage);
end;

procedure TDBSchemaTester.DoValidate(AItem: TSchemaCollectionItem;
  const ObjectType: TDBSchemaMessageObject);
begin
  if Assigned(FOnItemValidate) then
    FOnItemValidate(Self, AItem, ObjectType);
end;

procedure TDBSchemaTester.SetDisabledMessages(const Value: String);
begin
  FDisabledMessages.CommaText := Value;
end;

function TDBSchemaTester.GetDisabledMessages: String;
begin
  Result := FDisabledMessages.CommaText;
end;

function TDBSchemaTester.IsValidIdentifier(Obj: TObject; const Str: String; ACode: TDBSchemaMessageCode): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Str = '' then exit;

  if Length(Str) > FDBEngineProfile.MaxIdLength then
    AddResult(smcIdentifierIsTooLong, GetSchemaMessageObject(Obj), smsError, Obj, Str);

  if not FSchema.EncloseIdentifiersInQuotes then
    if FWrongNames.Indexof(AnsiUpperCase(Str)) >= 0 then
      AddResult(ACode, GetSchemaMessageObject(Obj), smsWarning, Obj, Str);

  for I := 1 to Length(Str) do
    if AnsiPos(Str[I], FDBEngineProfile.EncloseNames) > 0 then exit;

  Result := True;
end;

{ Testing routines by class }

procedure TDBSchemaTester.TestSchema(Schema: TDatabaseSchema);
var
  I: Integer;
begin
  if Schema = nil then
    Schema := FSchema;
  if not Assigned(Schema) then
    DatabaseError(SSchemaIsNotAssigned);

  FDBEngineProfile := FindDBEngineProfile(Schema.TargetDB);
  if FDBEngineProfile = nil then
    raise Exception.CreateFmt(SProfileNotFound, [Schema.TargetDB]);
  PrepareWrongNames;

  DoStatus(STestingSchema + Schema.SchemaName);
  ClearResults;
  FIdentifiers := TStringList.Create;
  try
    FIdentifiers.Duplicates := dupError;
    FIdentifiers.Sorted := True;

    DisabledMessages := FDBEngineProfile.EngineProps.Values['DisabledMessages'];

    // Test schema properties
    if not IsValidIdentifier(Schema, Schema.SchemaName, smcInvalidSchemaName) then
      AddResult(smcInvalidSchemaName);
    // Test enumerations
    for I := 0 to Schema.Enumerations.Count - 1 do
      TestEnumeration(Schema.Enumerations[I]);
    // Test domains
    for I := 0 to Schema.Domains.Count - 1 do
      TestDomain(Schema.Domains[I]);
    // Test sequences
    for I := 0 to Schema.Sequences.Count - 1 do
      TestSequence(Schema.Sequences[I]);
    // Test tables
    for I := 0 to Schema.TableDefs.Count - 1 do
      TestTable(Schema.TableDefs[I]);
    // Test relationships
    for I := 0 to Schema.Relationships.Count - 1 do
      TestRelationship(Schema.Relationships[I]);
    // Test views
    for I := 0 to Schema.ViewDefs.Count - 1 do
      TestView(Schema.ViewDefs[I]);
    // Test stored procs
    for I := 0 to Schema.StoredProcs.Count - 1 do
      TestStoredProc(Schema.StoredProcs[I]);
    // Test custom objects
    for I := 0 to Schema.CustomObjects.Count - 1 do
      TestCustomObject(Schema.CustomObjects[I]);
  finally
    FIdentifiers.Free;
    DoStatus(STestingComplete);
    FreeAndNil(FWrongNames);
  end;
end;

procedure TDBSchemaTester.TestEnumeration(Enumeration: TEnumeration);
var
  I: Integer;
  ConstValue: String;
  ConstName: String;
  EqPos: Integer;
  Values: TStringList;
  IntValue: Integer;
begin
  DoStatus(STestingEnum + Enumeration.Name);
  // Callback external validation
  DoValidate(Enumeration, smoEnum);
  // Test enumeration
  if not IsValidIdentifier(Enumeration, Enumeration.Name, smcEnumInvalidName) then
    AddResult(smcEnumInvalidName, smoEnum, smsError, Enumeration);
  if Enumeration.Items.Count = 0 then
    AddResult(smcEnumEmpty, smoEnum, smsWarning, Enumeration)
  else begin
    Values := TStringList.Create;
    try
      Values.Sorted := True;
      Values.Duplicates := dupError;
      for I := 0 to Enumeration.Items.Count - 1 do
      begin
        EqPos := AnsiPos('=', Enumeration.Items[I]);
        if EqPos < 1 then
          AddResult(smcEnumInvalidItem, smoEnum, smsError, Enumeration, Enumeration.Items[I])
        else begin
          ConstValue := Trim(Copy(Enumeration.Items[I], 1, EqPos - 1));
          ConstName := Trim(Copy(Enumeration.Items[I], EqPos + 1, Length(Enumeration.Items[I])));
          if not IsValidIdentifier(Enumeration, ConstName, smcEnumInvalidItem) then
            AddResult(smcEnumInvalidItem, smoEnum, smsError, Enumeration, Enumeration.Items[I]);
          if FIdentifiers.IndexOf(Enumeration.TypePrefix + ConstName) >= 0 then
            AddResult(smcEnumDuplicateIdentifier, smoEnum, smsError, Enumeration, Enumeration.TypePrefix + ConstName);
          FIdentifiers.Add(Enumeration.TypePrefix + ConstName);
          if Enumeration.IntConsts then
          begin
            try
              IntValue := StrToInt(ConstValue);
              if Values.IndexOf(IntToStr(IntValue)) >= 0 then
                AddResult(smcEnumDuplicateValue, smoEnum, smsError, Enumeration, ConstValue);
            except
              AddResult(smcEnumInvalidIntValue, smoEnum, smsError, Enumeration, ConstValue);
            end;
          end else begin
            if Values.IndexOf(ConstValue) >= 0 then
              AddResult(smcEnumDuplicateValue, smoEnum, smsError, Enumeration, ConstValue);
          end;
        end;
      end;
    finally
      Values.Free;
    end;
  end;
end;

procedure TDBSchemaTester.TestTable(TableDef: TTableDefinition);
var
  I: Integer;
begin
  // Callback external validation
  DoValidate(TableDef, smoTable);

  with TableDef do
  begin
    DoStatus(STestingTable + TableName);
    // Test table properties
    if not IsValidIdentifier(TableDef, TableName, smcTableInvalidName) then
      AddResult(smcTableInvalidName, smoTable, smsError, TableDef);
    if not IsNameUnique(TableDef) then
      AddResult(smcTableDuplicateName, smoTable, smsError, TableDef);
    if FIdentifiers.IndexOf(TypePrefix + TableName) >= 0 then
      AddResult(smcTableDuplicateIdentifier, smoTable, smsError, TableDef, TypePrefix + TableName);

    if not FDBEngineProfile.IsValidNamePattern(Name) then
      AddResult(smcTableNameInvalidPattern, smoTable, smsWarning, TableDef);

    if FIdentifiers.IndexOf(TypePrefix + TableName) < 0 then // +++ DB 22102009
      FIdentifiers.Add(TypePrefix + TableName);

    if (not IsView{ or FDBEngineProfile.ViewSupport(FieldDefs.ClassType)}) and
      (FieldDefs.Count = 0) then
      AddResult(smcTableNoFieldsDefined, smoTable, smsError, TableDef);


    if (not IsView {or (FDBEngineProfile.ViewSupport(Relations.ClassType) and FDBEngineProfile.ViewSupport(IndexDefs.ClassType))}) and
      (Replicate or (Relations.Count > 0)) and (GetPrimaryIndex = nil) then
      AddResult(smcTableNoPrimaryIndex, smoTable, smsWarning, TableDef);


    // Test fields
    for I := 0 to FieldDefs.Count - 1 do
      TestField(FieldDefs[I]);
    // Test indexes
    for I := 0 to IndexDefs.Count - 1 do
      TestIndex(IndexDefs[I]);
    // Test relations
    for I := 0 to Relations.Count - 1 do
      TestRelation(Relations[I]);
    // Test table check constraints
    for I := 0 to Constraints.Count - 1 do
      TestTableConstraint(Constraints[I]);
    // Test triggers
    for I := 0 to Triggers.Count - 1 do
      TestTrigger(Triggers[I]);
  end;
end;

procedure TDBSchemaTester.TestField(FieldDef: TFieldDefinition);
var
  Relation: TRelation;
  D: TDomain;
  En: TEnumeration;
  EngFldType: String;
begin
  // Callback external validation
  DoValidate(FieldDef, smoField);

  // Testing field definition
  with FieldDef do
  begin
    DoStatus(STestingField + TableDef.TableName + '.' + Name);

    // Test field name to be unqie and non-empty
    if not IsValidIdentifier(FieldDef, Name, smcFieldInvalidName) then
      AddResult(smcFieldInvalidName, smoField, smsError, FieldDef);
    if not IsNameUnique(FieldDef) then
      AddResult(smcFieldDuplicateName, smoField, smsError, FieldDef);

    if not FDBEngineProfile.IsValidNamePattern(Name) then
      AddResult(smcFieldNameInvalidPattern, smoField, smsWarning, FieldDef);

    if Domain <> '' then
    begin
      D := Schema.Domains.Find(Domain);
      if D = nil then
        AddResult(smcFieldInvalidDomain, smoField, smsError, FieldDef)
      else if FSchema.EncloseIdentifiersInQuotes and FDBEngineProfile.NamesCaseSensitive and (D.Name <> Domain) then
        AddResult(smcFieldDomainNameCaseMismatch, smoField, smsError, FieldDef)
    end;

    if Enumeration <> '' then
    begin
      En := Schema.Enumerations.Find(Enumeration);
      if En = nil then
        AddResult(smcFieldInvalidEnum, smoField, smsWarning, FieldDef);
    end;


    if DataType = ftUnknown then
      AddResult(smcFieldTypeUndefined, smoField, smsError, FieldDef);

    EngFldType := SQLFieldType; // FDBEngineProfile.GetEngineFieldType();
    if not FDBEngineProfile.IsValidFieldType(EngFldType) then
      AddResult(smcFieldTypeNotSupportedByDatabase, smoField, smsError, FieldDef, EngFldType);

    // Test valid Size for strings
    if (DataType in setStringFieldTypes) and (Size < 1) then
      AddResult(smcFieldInvalidSize, smoField, smsError, FieldDef, IntToStr(Size));

    // Test aggregate type, relation and expression
    if AggregateType <> aNone then
    begin
      if (AggregateType = aSum) and (Trim(AggregateExpression) = '') then
        AddResult(smcFieldInvalidAggExpr, smoField, smsError, FieldDef);

      Relation := TableDef.Relations.Find(RelationName);
      if Relation = nil then
        AddResult(smcFieldInvalidRelation, smoField, smsError, FieldDef, RelationName)
      else begin
        // Relation type must NOT be many-to-X
        if Relation.RelationType in [rtManyToOne, rtManyToMany] then
          AddResult(smcFieldInvalidRelationType, smoField, smsError, FieldDef, RelationName)
      end;
    end;
  end;
end;

procedure TDBSchemaTester.TestIndex(IndexDef: TIndexDefinition);
var
  I: Integer;
  FieldDef: TFieldDefinition;
begin
  // Callback external validation
  DoValidate(IndexDef, smoIndex);

  // Testing index definition
  with IndexDef do
  begin
    DoStatus(STestingIndex + TableDef.TableName + '.' + Name);

    // Test index name to be unqie and non-empty
    if FDBEngineProfile.NamedPrimaryKeys or not (ixPrimary in IndexDef.Options) then
      if not IsValidIdentifier(IndexDef, Name, smcIndexInvalidName) then
        AddResult(smcIndexInvalidName, smoIndex, smsError, IndexDef);

    // +++ Only check to be unique among indexes (MS SQL)
    if not IsNameUnique(IndexDef) then
      AddResult(smcIndexDuplicateName, smoIndex, smsError, IndexDef);

    if not FDBEngineProfile.IsValidNamePattern(IndexDef.Name) then
      AddResult(smcIndexNameInvalidPattern, smoIndex, smsWarning, IndexDef);

    // Make sure that all fields exist or IndexExpression is not empty
    if (IndexFields.Count = 0) and (IndexDef.IndexExpression = '') then
      AddResult(smcIndexNoFields, smoIndex, smsError, IndexDef) else
    if not IndexDef.IsExpression then
      for I := 0 to IndexFields.Count - 1 do
      begin
        FieldDef := TableDef.FieldDefs.Find(IndexFields[I].Name);
        if FieldDef = nil then
          AddResult(smcIndexFieldNotFound, smoIndex, smsError, IndexDef, IndexFields[I].Name)
        else begin
          if (FieldDef.DataType in setCannotBeIndexed)
            and not FDBEngineProfile.AllowIndexType(FieldDef.SQLFieldType)
          then
            AddResult(smcIndexInvalidFieldType, smoIndex, smsError, IndexDef,
              FieldDef.Name + ': ' + FieldDef.SQLFieldType);
          if FSchema.EncloseIdentifiersInQuotes and FDBEngineProfile.NamesCaseSensitive and (FieldDef.Name <> IndexFields[I].Name) then
            AddResult(smcIndexFieldNameCaseMismatch, smoIndex, smsError, IndexDef, IndexFields[I].Name);
        end;
      end;
  end;
end;

procedure TDBSchemaTester.TestRelation(Relation: TRelation);
begin
  // Callback external validation
  DoValidate(Relation, smoRelation);
  // Testing relation definition
  with Relation do
  begin
    DoStatus(STestingRelation + TableDef.TableName + '.' + Name);
    // Test relation name to be unqie and non-empty
    if not IsValidIdentifier(Relation, Name, smcRelationInvalidName) then
      AddResult(smcRelationInvalidName, smoRelation, smsError, Relation);
    if not IsNameUnique(Relation) then
      AddResult(smcRelationDuplicateName, smoRelation, smsError, Relation);
  end;
end;

procedure TDBSchemaTester.TestTrigger(TriggerDef: TTriggerDefinition);
begin
  // Callback external validation
  DoValidate(TriggerDef, smoTrigger);
  // Testing trigger definition
  with TriggerDef do
  begin
    DoStatus(STestingTrigger + TableDef.TableName + '.' + Name);
    // Test trigger name to be unqie and non-empty
    if not IsValidIdentifier(TriggerDef, Name, smcTriggerInvalidName) then
      AddResult(smcTriggerInvalidName, smoTrigger, smsError, TriggerDef);
    if not IsNameUnique(TriggerDef) then
      AddResult(smcTriggerDuplicateName, smoTrigger, smsError, TriggerDef);

    if not FDBEngineProfile.IsValidNamePattern(TriggerDef.Name) then
      AddResult(smcTriggerNameInvalidPattern, smoTrigger, smsWarning, TriggerDef);

    // Make sure, that if it is object trigger, primary key
    // exists for table
    if (TriggerType = ttUponCommit) and (TableDef.GetPrimaryIndex = nil) then
      AddResult(smcTriggerNoPrimaryKey, smoTrigger, smsError, TriggerDef);

    if Trim(Definition) = '' then
      AddResult(smcTriggerEmptyDefinition, smoTrigger, smsError, TriggerDef);
  end;
end;

procedure TDBSchemaTester.TestView(ViewDef: TViewDefinition);
begin
  // Callback external validation
  DoValidate(ViewDef, smoView);
  // Testing view definition
  with ViewDef do
  begin
    DoStatus(STestingView + Name);
    // Test view name to be unqie and non-empty
    if not IsValidIdentifier(ViewDef, Name, smcViewInvalidName) then
      AddResult(smcViewInvalidName, smoView, smsError, ViewDef);
    if not IsNameUnique(ViewDef) then
      AddResult(smcViewDuplicateName, smoView, smsError, ViewDef);
    if Trim(Definition.Text) = '' then
      AddResult(smcViewEmptyDefinition, smoView, smsError, ViewDef);

    if not FDBEngineProfile.IsValidNamePattern(ViewDef.Name) then
      AddResult(smcViewNameInvalidPattern, smoView, smsWarning, ViewDef);
  end;
end;

procedure TDBSchemaTester.TestDomain(Domain: TDomain);
var
  EngFldType: String;
begin
  // Callback external validation
  DoValidate(Domain, smoDomain);
  // Testing Domain definition
  with Domain do
  begin
    DoStatus(STestingDomain + Name);
    // Test Domain name to be unqie and non-empty
    if not IsValidIdentifier(Domain, Name, smcDomainInvalidName) then
      AddResult(smcDomainInvalidName, smoDomain, smsError, Domain);
    if not IsNameUnique(Domain) then
      AddResult(smcDomainDuplicateName, smoDomain, smsError, Domain);

    if not FDBEngineProfile.IsValidNamePattern(Domain.Name) then
      AddResult(smcDomainNameInvalidPattern, smoDomain, smsWarning, Domain);

    if (DataType = ftUnknown) and (SQLFieldType = '') then
      AddResult(smcDomainTypeUndefined, smoDomain, smsError, Domain);

    EngFldType := SQLFieldType; // FDBEngineProfile.GetEngineFieldType();
    if not FDBEngineProfile.IsValidFieldType(EngFldType) then
      AddResult(smcFieldTypeNotSupportedByDatabase, smoDomain, smsError, Domain, EngFldType);

    // Test valid Size for strings
    if (DataType in setStringFieldTypes) and (Size < 1) then
      AddResult(smcDomainInvalidSize, smoDomain, smsError, Domain);
  end;
end;

function SameFieldType(DataType1, DataType2: TFieldDataType): Boolean;
begin
  if DataType1 = ftAutoInc then
    DataType1 := ftInteger;
  if DataType2 = ftAutoInc then
    DataType2 := ftInteger;
  Result := DataType1 = DataType2;
end;

procedure TDBSchemaTester.TestRelationship(Relationship: TRelationship);
var
  MasterList, DetailList: TStringList;
  I: Integer;
  AllFieldsFound: Boolean;
  DetailFieldsRequired: Boolean;
  NameDetailFieldRequired: string;
  DetailFieldsUnique: Boolean;
  DetailFieldsHasDefault: Boolean;
  NameDetailFieldHasNoDefault: string;
begin
  // Callback external validation
  DoValidate(Relationship, smoRelationship);

  MasterList := TStringList.Create;
  DetailList := TStringList.Create;
  try
    // Testing Relationship definition
    with Relationship do
    begin
      DoStatus(STestingRelationship + Relationship.Name);
      // Test Relationship name to be unqie and non-empty
      if not IsValidIdentifier(Relationship, Relationship.Name, smcRelationshipInvalidName) then
        AddResult(smcRelationshipInvalidName, smoRelationship, smsError, Relationship);
      if not IsNameUnique(Relationship) then
        AddResult(smcRelationshipDuplicateName, smoRelationship, smsError, Relationship);

      if not FDBEngineProfile.IsValidNamePattern(Relationship.Name) then
        AddResult(smcRelationshipNameInvalidPattern, smoRelationship, smsWarning, Relationship);

      if MasterTableDef = nil then
        AddResult(smcRelationshipInvalidMasterTable, smoRelationship, smsError, Relationship)
      else MasterTableDef.GetFieldList(MasterList, MasterKeyFields);

      if DetailTableDef = nil then
        AddResult(smcRelationshipInvalidDetailTable, smoRelationship, smsError, Relationship)
      else DetailTableDef.GetFieldList(DetailList, DetailKeyFields);

      AllFieldsFound := False;
      if MasterList.Count <> DetailList.Count then
        AddResult(smcRelationshipFieldCountMismatch, smoRelationship, smsError, Relationship)
      else if MasterList.Count = 0 then
        AddResult(smcRelationshipNoKeyFieldsDefined, smoRelationship, smsError, Relationship)
      else begin
        AllFieldsFound := True;
        for I := 0 to MasterList.Count - 1 do
        begin
          if MasterList.Objects[I] = nil then
            AddResult(smcRelationshipFieldNotFound, smoRelationship, smsError, Relationship, MasterList[I]);
          if DetailList.Objects[I] = nil then
            AddResult(smcRelationshipFieldNotFound, smoRelationship, smsError, Relationship, DetailList[I]);

          if (MasterList.Objects[I] <> nil) and (DetailList.Objects[I] <> nil) then
          begin
            if FSchema.EncloseIdentifiersInQuotes and FDBEngineProfile.NamesCaseSensitive then
            begin
              if MasterList[I] <> TFieldDefinition(MasterList.Objects[I]).Name then
                AddResult(smcRelationshipMasterFieldNameCaseMismatch, smoRelationship, smsWarning, Relationship, MasterList[I]);
              if DetailList[I] <> TFieldDefinition(DetailList.Objects[I]).Name then
                AddResult(smcRelationshipDetailFieldNameCaseMismatch, smoRelationship, smsWarning, Relationship, DetailList[I]);
            end;

            if not SameFieldType(TFieldDefinition(MasterList.Objects[I]).DataType, TFieldDefinition(DetailList.Objects[I]).DataType) then
              AddResult(smcRelationshipFieldTypeMismatch, smoRelationship, smsWarning, Relationship, MasterList[I])
            else if (TFieldDefinition(MasterList.Objects[I]).DataType in setStringFieldTypes) and
              (TFieldDefinition(MasterList.Objects[I]).Size <> TFieldDefinition(DetailList.Objects[I]).Size)
            then
              AddResult(smcRelationshipFieldTypeMismatch, smoRelationship, smsWarning, Relationship, MasterList[I]);
          end else
            AllFieldsFound := False;
        end;
      end;

      if AllFieldsFound and (DetailCardinality <> dcLogical) then
      begin
        DetailFieldsRequired := True;
        DetailFieldsHasDefault := True;
        for I := 0 to DetailList.Count - 1 do
        begin
          DetailFieldsRequired := DetailFieldsRequired and TFieldDefinition(DetailList.Objects[I]).Required;
          if TFieldDefinition(DetailList.Objects[I]).Required and (NameDetailFieldRequired = '') then
            NameDetailFieldRequired := Relationship.DetailTableName+'.'+TFieldDefinition(DetailList.Objects[I]).Name;
          DetailFieldsHasDefault := DetailFieldsHasDefault and (TFieldDefinition(DetailList.Objects[I]).DefaultExpression <> '');
          if not DetailFieldsHasDefault and (NameDetailFieldHasNoDefault = '') then
            NameDetailFieldHasNoDefault := Relationship.DetailTableName+'.'+TFieldDefinition(DetailList.Objects[I]).Name;
        end;

        if ((Relationship.DeleteAction = raNullify) or (Relationship.UpdateAction = raNullify))
          and DetailFieldsRequired then
          AddResult(smcRelationshipNullifyDetailFieldsRequired, smoRelationship, smsWarning, Relationship, NameDetailFieldRequired);

        if ((Relationship.DeleteAction = raSetDefault) or (Relationship.UpdateAction = raSetDefault))
          and not DetailFieldsHasDefault then
          AddResult(smcRelationshipDefaultDetailFieldsNotDefault, smoRelationship, smsWarning, Relationship, NameDetailFieldHasNoDefault);

        if MasterRecordOptional and DetailFieldsRequired then
          AddResult(smcRelationshipMasterRecordNotOptional, smoRelationship, smsWarning, Relationship)
        else if not MasterRecordOptional and not DetailFieldsRequired then
          AddResult(smcRelationshipMasterRecordOptional, smoRelationship, smsWarning, Relationship);

        DetailFieldsUnique := DetailTableDef.IsUniqueKey(DetailKeyFields, CaseInsensitive);

        if (DetailCardinality = dcOne) and not DetailFieldsUnique then
          AddResult(smcRelationshipOneToOneCannotBeEnforced, smoRelationship, smsWarning, Relationship)
        else if (DetailCardinality = dcMany) and DetailFieldsUnique then
          AddResult(smcRelationshipManyToOneCannotBeEnforced, smoRelationship, smsWarning, Relationship);
      end;

      if (MasterTableDef <> nil) and not MasterTableDef.PartOfIndex(MasterKeyFields, CaseInsensitive) then
        AddResult(smcRelationshipMasterKeyFieldsNotIndexed, smoRelationship, smsWarning, Relationship, MasterKeyFields);
      if (DetailTableDef <> nil) and not DetailTableDef.PartOfIndex(DetailKeyFields, CaseInsensitive) then
        AddResult(smcRelationshipDetailKeyFieldsNotIndexed, smoRelationship, smsWarning, Relationship, DetailKeyFields);
    end;
  finally
    MasterList.Free;
    DetailList.Free;
  end;
end;


(*
var
  ForeignTableDef: TTableDefinition;

    // Make sure that key fields exist
    TestFields(TableDef, Relation.KeyFields);
    // Make sure index exists for key fields
    if not TableDef.PartOfIndex(KeyFields, CaseInsensitive) then
      AddResult(smcRelationKeyFieldsNotIndexed, smoRelation, smsWarning, Relation, KeyFields)
    else begin
      // Make sure TRelationType is supported by existing index
      if TableDef.IsUniqueKey(KeyFields, CaseInsensitive) then
      begin
        if not (RelationType in [rtOneToOne, rtOneToMany]) then
          AddResult(smcRelationWrongType, smoRelation, smsError, Relation, RelationTypeText[RelationType]);
      end else if RelationType in [rtOneToOne, rtOneToMany] then
        AddResult(smcRelationWrongType, smoRelation, smsError, Relation, RelationTypeText[RelationType]);
    end;

    // Make sure that foreign table exist
    ForeignTableDef := Schema.TableDefs.Find(ForeignTable);
    if ForeignTableDef = nil then
      AddResult(smcRelationInvalidForeignTable, smoRelation, smsError, Relation, ForeignTable)
    else begin
      // .. and foreign key fields too
      TestFields(ForeignTableDef, Relation.ForeignKeyFields);

      // Test peer relation
      if GetPeerRelation = nil then
      begin
        if RelationKind <> rkReference then
          AddResult(smcRelationPeerRequired, smoRelation, smsError, Relation)
        else AddResult(smcRelationNoPeerDefined, smoRelation, smsHint, Relation);
      end;

      // Make sure that right index exists for foreign key fields
      if not ForeignTableDef.PartOfIndex(ForeignKeyFields, CaseInsensitive) then
        AddResult(smcRelationFKeyFieldsNotIndexed, smoRelation, smsWarning, Relation, ForeignKeyFields)
      else begin
        // Make sure TRelationType is supported by existing index
        if ForeignTableDef.IsUniqueKey(ForeignKeyFields, CaseInsensitive) then
        begin
          if not (RelationType in [rtOneToOne, rtManyToOne]) then
            AddResult(smcRelationWrongType, smoRelation, smsError, Relation, RelationTypeText[RelationType]);
        end else if RelationType in [rtOneToOne, rtManyToOne] then
          AddResult(smcRelationWrongType, smoRelation, smsError, Relation, RelationTypeText[RelationType]);
      end;
    end;

    // Parent or children relations should correspond to RelationType
    if RelationKind <> rkReference then
    begin
      if (RelationType in [rtOneToMany, rtManyToMany]) and (RelationKind = rkParent) then
        AddResult(smcRelationWrongType, smoRelation, smsError, Relation, RelationTypeText[RelationType]);
      if (RelationType in [rtManyToOne, rtManyToMany]) and (RelationKind = rkChildren) then
        AddResult(smcRelationWrongType, smoRelation, smsError, Relation, RelationTypeText[RelationType]);
    end;

    // Parent should ne required
    if (RelationKind = rkParent) and not RequireOneRecord then
      AddResult(smcRelationParentMustBeRequired, smoRelation, smsHint, Relation);
*)

procedure TDBSchemaTester.TestSequence(Sequence: TSequence);
begin
  // Callback external validation
  DoValidate(Sequence, smoSequence);
  // Testing Sequence definition
  with Sequence do
  begin
    DoStatus(STestingSequence + Name);
    // Test Sequence name to be unqie and non-empty
    if not IsValidIdentifier(Sequence, Name, smcSequenceInvalidName) then
      AddResult(smcSequenceInvalidName, smoSequence, smsError, Sequence);
    if not IsNameUnique(Sequence) then
      AddResult(smcSequenceDuplicateName, smoSequence, smsError, Sequence);

    if not FDBEngineProfile.IsValidNamePattern(Sequence.Name) then
      AddResult(smcSequenceNameInvalidPattern, smoSequence, smsWarning, Sequence);

    {if Unknown ??? then
      AddResult(smcSequenceInvalidDefinition, smoSequence, smsError, Sequence);}
  end;
end;

procedure TDBSchemaTester.TestStoredProc(StoredProc: TStoredProcDefinition);
begin
  // Callback external validation
  DoValidate(StoredProc, smoStoredProc);
  // Testing StoredProc definition
  with StoredProc do
  begin
    DoStatus(STestingStoredProc + Name);
    // Test StoredProc name to be unqie and non-empty
    if not IsValidIdentifier(StoredProc, Name, smcStoredProcInvalidName) then
      AddResult(smcStoredProcInvalidName, smoStoredProc, smsError, StoredProc);
    if not IsNameUnique(StoredProc) then
      AddResult(smcStoredProcDuplicateName, smoStoredProc, smsError, StoredProc);
    if Trim(Definition.Text) = '' then
      AddResult(smcStoredProcEmptyDefinition, smoStoredProc, smsError, StoredProc);
    if not FDBEngineProfile.IsValidNamePattern(StoredProc.Name) then
      AddResult(smcStoredProcNameInvalidPattern, smoStoredProc, smsWarning, StoredProc);
  end;
end;

procedure TDBSchemaTester.TestTableConstraint(TableConstraint: TTableConstraint);
begin
  // Callback external validation
  DoValidate(TableConstraint, smoTableConstraint);
  // Testing TableConstraint definition
  with TableConstraint do
  begin
    DoStatus(STestingTableConstraint + Name);
    // Test StoredProc name to be unqie and non-empty
    if not IsValidIdentifier(TableConstraint, Name, smcTableConstraintInvalidName) then
      AddResult(smcTableConstraintInvalidName, smoTableConstraint, smsError, TableConstraint);
    if not IsNameUnique(TableConstraint) then
      AddResult(smcTableConstraintDuplicateName, smoTableConstraint, smsError, TableConstraint);

    if not FDBEngineProfile.IsValidNamePattern(TableConstraint.Name) then
      AddResult(smcConstraintNameInvalidPattern, smoTableConstraint, smsWarning, TableConstraint);

    if Trim(Check) = '' then
      AddResult(smcTableConstraintEmptyDefinition, smoTableConstraint, smsError, TableConstraint);
  end;
end;

procedure TDBSchemaTester.TestCustomObject(CustomObject: TCustomObject);
begin
  // Callback external validation
  DoValidate(CustomObject, smoCustomObject);
  with CustomObject do
  begin
    DoStatus(STestingCustomObject + Name);
    if not IsValidIdentifier(CustomObject, Name, smcCustomObjectInvalidName) then
      AddResult(smcCustomObjectInvalidName, smoCustomObject, smsError, CustomObject);
    if not IsNameUnique(CustomObject) then
      AddResult(smcCustomObjectDuplicateName, smoCustomObject, smsError, CustomObject);
    if not FDBEngineProfile.IsValidNamePattern(CustomObject.Name) then
      AddResult(smcCustomObjectNameInvalidPattern, smoCustomObject, smsWarning, CustomObject);
  end;
end;

function TDBSchemaTester.TopLevelUnique(Item: TSchemaCollectionItem): Boolean;

  function UniqueInCollection(Collection: TSchemaItemsCollection): Boolean;
  var
    I: Integer;
    Temp: TSchemaCollectionItem;
  begin
    Result := True;
    for I := 0 to Collection.Count - 1 do
    begin
      Temp := TSchemaCollectionItem(Collection.Items[I]);
      if not FDBEngineProfile.IsTopLevelObject(Temp.GetSchemaClassName) then continue;
      if (Temp <> Item) and AnsiSameText(Item.Name, Temp.Name) then
      begin
        Result := False;
        exit;
      end;
    end;
  end;

  function UniqueInTables(Collection: TSchemaItemsCollection): Boolean;
  var
    I: Integer;
    Temp: TTableDefinition;
    CheckItem: Boolean;
  begin
    Result := True;
    if Collection.Count = 0 then exit;
    CheckItem := FDBEngineProfile.IsTopLevelObject(TTableDefinition(Collection.Items[0]).GetSchemaClassName);
    for I := 0 to Collection.Count - 1 do
    begin
      Temp := TTableDefinition(Collection.Items[I]);
      if CheckItem and (Temp <> Item) and AnsiSameText(Item.Name, Temp.Name) then
      begin
        Result := False;
        exit;
      end;
      // Check subitems
      Result := UniqueInCollection(Temp.FieldDefs)
        and UniqueInCollection(Temp.IndexDefs)
        and UniqueInCollection(Temp.Triggers)
        and UniqueInCollection(Temp.Constraints);
      if not Result then exit;
    end;
  end;

begin
  Result := UniqueInTables(FSchema.TableDefs)
    and UniqueInCollection(FSchema.Relationships)
    and UniqueInCollection(FSchema.Domains)
    and UniqueInCollection(FSchema.Sequences)
    and UniqueInCollection(FSchema.StoredProcs)
    and UniqueInCollection(FSchema.ViewDefs)
    and UniqueInCollection(FSchema.Enumerations)
    and UniqueInCollection(FSchema.CustomObjects);
end;

function TDBSchemaTester.IsNameUnique(Item: TSchemaCollectionItem): Boolean;
begin
  if FDBEngineProfile.IsTopLevelObject(Item.GetSchemaClassName) then
    Result := TopLevelUnique(Item)
  else Result := dbSchema.IsUnique(Item, Item.Name);
end;

procedure TDBSchemaTester.PrepareWrongNames;
var
  I: Integer;
begin
  FWrongNames := TStringList.Create;
  for I := 0 to FDBEngineProfile.KeyWords.Count-1 do
    FWrongNames.Add(AnsiUpperCase(FDBEngineProfile.KeyWords[I]));
  FWrongNames.Duplicates := dupIgnore;
  FWrongNames.Sorted := True;
end;



end.
