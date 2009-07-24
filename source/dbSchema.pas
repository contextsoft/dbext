(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Classes that implement generic database schema.
(*  Contains:
(*                TDatabaseSchema
(*                TDatabaseUpdates - collection of TDatabaseUpdate
(*                TDatabaseUpdate - description of an update
(*                TTableDefinitions - collection of TTableDefinition
(*                TTableDefinition - description of a physical table:
(*                  TFieldDefinitions - a collection of TFieldDefinition
(*                  TIndexDefinitions - a collection of TIndexDefinition
(*                  TRelations - a collection of TRelation
(*                  TTriggerDefinitions - a collection of TTriggerDefinition
(*                  TTableConstraints - a collection of TTableConstraint
(*                TRelationships - collection of TRelationship
(*                TViewDefinitions - collection of TViewDefinition
(*                TStoredProcDefinitions - collection of TStoredProcDefinition
(*                TSequences - collection of TSequence
(*                TDomains - collection of TDomain
(*
(*  Copyright (c) 2005-2009, Context Software LLC
(*
(*  ------------------------------------------------------------
(*  FILE        : dbSchema.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.02
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009
(*
(******************************************************************************)
unit dbSchema;

{$DEFINE CTXDBEXT30}

{$I CtxVer.inc}

interface

uses
{$IFnDEF VER130}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Contnrs, TypInfo, CtxDataTypes,
  DB, CtxDBIntf;

const
  dbSchemaLibVersion = 303;

{$IFDEF D2009_ORLATER}
type
  TDataSetBookmark = TBytes;
const
  NilBookmark = nil;
{$ELSE}
type
  TDataSetBookmark = String;
const
  NilBookmark = '';
{$ENDIF}

type
  TDatabaseSchema = class;
  TDatabaseUpdates = class;
  TDatabaseUpdate = class;
  TTableDefinition = class;
  TTriggerDefinition = class;
  TRelation = class;
  TRelationship = class;
  TRelationships = class;
  TSchemaCollectionItem = class;
  TSchemaCollectionItemClass = class of TSchemaCollectionItem;

  TFieldDataType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, ftBytes,
    ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle,
    ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString, ftLargeint,
    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, ftVariant,
    ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, ftNChar,
    ftNClob, ftRecRev);

  TItemOperation = (ioCreate, ioAlter, ioDrop, ioNone);

  TOnSchemaItem = procedure (Schema: TDatabaseSchema; Item: TSchemaCollectionItem; var Abort: Boolean; Data: Pointer = nil) of object;

  {:$ TDatabaseProgress type is used by the database's OnSQLFieldTypeChanged event. }
  {:$ Item may contain reference to an instance of TDomain or TFieldDefinition object whose }
  {:$ SQLFieldType propery has changed. The even is provided so the application has }
  {:$ chance to update other related properties, such as DataType or Size to correspond to }
  {:$ the newly assigned SQLFieldType. }
  TOnSQLFieldTypeChanged = procedure (Sender: TDatabaseSchema; Item: TSchemaCollectionItem; const Value: String);

  {:$ TDatabaseProgress type is used by the database's OnProgress event. }
  TDatabaseProgress = procedure (Sender: TObject; const Status: String; PercentDone: Byte; var Abort: Boolean) of object;

  {:$ The TOnFieldProc type is used as callback procedure passed to IterateFields method of TDataSetReference object. }
  TOnFieldProc = procedure (PropInfo: PPropInfo; Data: Pointer) of object;

  {:$ The TOnParseField type is used as callback function passed to ParseFields helper routine. }
  TOnParseField = function (const FieldText: String; Data: Pointer): String of object;

  {:$ The TChangeType represents the type of update perfromed on a database object. }
  TChangeType = (ctDeleted, ctModifiedReference, ctModifiedContent, ctModified, ctInserted);
  {:$ The TChangeTypes represents the set of updates perfromed on a database object.}
  TChangeTypes = set of TChangeType;

  TAggregateType = (aNone, aSum, aCount);

  {:$ The TRelationType represents the type of relation between two tables. }
  TRelationType = (rtOneToOne, rtOneToMany, rtManyToOne, rtManyToMany);

  {:$ The TRelationAction represents the type of action to be performed when data is changed on one side of the relation. }
  TRelationAction = (raIgnore, raCascade, raNullify, raError, raSetDefault);
  {:$ The TRelationKind represents the kind of relation between two tables. }
  TRelationKind = (rkReference, rkParent, rkChildren);

  {:$ TSchemaVersion record represents the version of the database schema. }
  TSchemaVersion = record
    case Integer of
      0: (Major: Smallint; Minor: Smallint);
      1: (IntVer: Integer);
  end;

  TCompareItem = class
  protected
    function GetItemOperation: TItemOperation;
  public
    ParentItem: TCompareItem;
    SrcObj: TObject;
    DestObj: TObject;
    SubItems: TObjectList;
    PropsEqual: Boolean;

    constructor Create(ASrcObj, ADestObj: TObject; AParentItem: TCompareItem = nil);
    destructor Destroy; override;

    procedure SwapSrcDest;
    procedure CompareObjects; virtual;

    property Operation: TItemOperation read GetItemOperation;
    function GetObj: TObject;
    function GetSubItems: TObjectList;
    function GetPropValue(AObj: TObject; const PropName: String): String; virtual;
    function GetDefaultPropValue(AObj: TObject; const PropName: String): String; virtual;
    class procedure SwapSrcDestList(List: TList);
  end;

  TCompareSchema = class (TCompareItem)
  public
    procedure CompareObjects; override;
    function SrcSchema: TDatabaseSchema;
    function DestSchema: TDatabaseSchema;
  end;

  TCompareSchemaItem = class (TCompareItem)
  public
    procedure CompareObjects; override;
    function GetItem: TSchemaCollectionItem;
    function SrcItem: TSchemaCollectionItem;
    function DestItem: TSchemaCollectionItem;
    function GetPropValue(AObj: TObject; const PropName: String): String; override;
  end;

  {:$ TSchemaItemsCollection is a generic ancestor for all collection items owned directly by TDatabaseSchema component. }
  TSchemaItemsCollection = class (TOwnedCollection)
  private
    function GetItem(Index: Integer): TSchemaCollectionItem;
    procedure SetItem(Index: Integer; const Value: TSchemaCollectionItem);
  public
    function GetSchema: TDatabaseSchema; virtual;
    function Add: TSchemaCollectionItem;
    function GetAutoName(AItem: TSchemaCollectionItem; const Template: String = '<N>'): String;
    {:$ Returns an index of a collection item by name within this collection. }
    {:: If the item is not found the returned value is -1. }
    function IndexOf(const AName: String): Integer; virtual;
    {:$ Locates an item by name within the collection. }
    function Find(const Name: String): TSchemaCollectionItem;
    {:$ Locates an item by name within the collection. }
    function FindByItemID(ItemID: Integer): TSchemaCollectionItem;
    {:$ Locates a copy of an item in this collection, using ItemID, Name and GetSchemaClassName properties. }
    function LocateItem(AnItem: TSchemaCollectionItem; ByName: Boolean = True): TSchemaCollectionItem;
    {:$ Compares two collections and returns items that don't match. }
    procedure Compare(Dest: TSchemaItemsCollection; List: TList; ByName: Boolean = False); virtual;
    {:$ This method is used in import instead of Assign because it does not overwrite }
    {:$ collectin items, however it only replaces the ones changed and doesn't call Clear. }
    {:: After the call to CopyFrom the collection content is identical to the }
    {:: ASource collection. }
    procedure CopyFrom(ASource: TSchemaItemsCollection);
    {:: Sort collection items alphabetically by Name property. }
    procedure Sort(CompareFunc: TListSortCompare = nil);

    property Items[Index: Integer]: TSchemaCollectionItem read GetItem write SetItem; default;
  end;

  {:$ TTableDefItemsCollection is a generic ancestor for all collection items owned directly by TTableDefinition object. }
  TTableDefItemsCollection = class (TSchemaItemsCollection)
  public
    function GetTableDef: TTableDefinition; virtual;
    function GetSchema: TDatabaseSchema; override;
    procedure AssignTo(Dest: TPersistent); override;
  end;

  TSchemaCollectionItem = class (TCollectionItem)
  protected
    FSign: Integer;
    FItemID: Integer;
    FProps: TStrings;
    FName: String;
    FDescription: String;
    FOldIndex: Integer;
    FUpdateCounter: Integer;
    FCategory: String;
    FProduceSQL: Boolean;

    function GetFullName: String; virtual;
    function GetSchema: TDatabaseSchema; virtual;
    procedure SetProps(const Value: TStrings);
    procedure PropsChanged(Sender: TObject); virtual;
    procedure Rename(const Value: String); virtual;
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    procedure ReadItemID(Reader: TReader);
    procedure WriteItemID(Writer: TWriter);
    procedure ReadOldIndex(Reader: TReader);
    procedure WriteOldIndex(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure InternalUpdate; virtual;
    procedure ValidateRename(const NewName: String); virtual;
    procedure ObjectRenamed(const OldName: String); virtual;

    function GetItemID: Integer; virtual;
    procedure SetItemID(const Value: Integer); virtual;

    function  GetDisplayLabel: String; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: Boolean;

    procedure SetPropValue(const PropName, Value: String); virtual;
    function GetPropValue(const PropName: String): String; virtual;

    procedure GetPropNames(List: TStrings); virtual;
    function GetSchemaClassName: String; virtual;
    function GetAutoName(const Template: String = ''): String; virtual;
    procedure UpdateDefinition(const Value: String = ''); virtual;

    {:$ Specifies the name of the item as it appears in Object Inspector. }
    function GetDisplayName: String; override;
    {:$ Copies the contents from another TDomain object. }
    procedure Assign(Source: TPersistent); override;
    {:$ Compares two item's physical properties. Returns true if they're identical. }
    function Compare(Dest: TCompareSchemaItem): Boolean; virtual;
    function PropsEqual(Dest: TCompareSchemaItem; const PropName: String): Boolean;

    procedure CopyFrom(ASource: TPersistent; AWithSubItems: boolean = True); virtual;

    property Schema: TDatabaseSchema read GetSchema;
    property ItemID: Integer read GetItemID write SetItemID;
    property FullName: String read GetFullName;
    property DisplayLabel: String read GetDisplayLabel;
  published
    {:$ Specifies the category this object belongs to. }
    property Category: String read FCategory write FCategory;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex stored False;
    property Name: String read FName write Rename;
    property Description: String read FDescription write FDescription;
    property Props: TStrings read FProps write SetProps;
    property ProduceSQL: Boolean read FProduceSQL write FProduceSQL default True;
  end;

  TTableCollectionItem = class (TSchemaCollectionItem)
  protected
    function GetTableName: String;
    procedure SetTableName(const Value: String);
    function GetFullName: String; override;
    function GetSchema: TDatabaseSchema; override;
    function GetTableDef: TTableDefinition; virtual;
  public
    //function GetAutoName(const Template: String = ''): String; override;
    property TableDef: TTableDefinition read GetTableDef;
  published
    property TableName: String read GetTableName write SetTableName stored False;
  end;

  {:$ TDatabaseUpdate is a collection item representing certain update operation
  {:$ that have to be performed on the database in order to update it to a Version }
  {:$ specified in the Version or VersionLabel property. }
  TDatabaseUpdate = class (TSchemaCollectionItem)
  protected
    FTableName: String;
    FSQLScript: String;
    FIterate: Boolean;
    FVersion: TSchemaVersion;
    FIgnoreSQLError: Boolean;

    function GetVersionLabel: String;
    procedure SetVersion(const Value: TSchemaVersion);
    procedure SetVersionLabel(const Value: String);
    function  GetDisplayLabel: String; override;
  public
    {:$ Creates an instance of TDatabaseUpdate object. }
    {:: This method should never be used directly. }
    {:: Use Add method of TDatabaseUpdates collection instead. }
    constructor Create(Collection: TCollection); override;
    {:$ Destroys the instance of TDatabaseUpdate object. }
    destructor Destroy; override;
    {:$ Copies the contents of another TDatabaseUpdate object. }
    procedure Assign(Source: TPersistent); override;
    {:$ Specifies the name of the item as it appears in Object Inspector. }
    function GetDisplayName: String; override;

    {:$ Conatins information about which version of the Schema this update is related to. }
    property Version: TSchemaVersion read FVersion write SetVersion;
  published
    {:$ Conatins SQL script to be performed by database engine in order to update to the specified version. }
    property SQLScript: String read FSQLScript write FSQLScript;
    {:: Set Iterate to True to force database to invoke OnDataSetRecord event for every record }
    {:: in a dataset returned by SQLScript. SQLScript must be assigned and return live result set. }
    property Iterate: Boolean read FIterate write FIterate;
    {:: Set IgnoreSQLError to True to ignore any exception raised by SQLScript.<br> }
    {:: The default value is False.}
    property IgnoreSQLError: Boolean read FIgnoreSQLError write FIgnoreSQLError default False;
    {:$ TableName contains the name of the table this update is related to. }
    {:: This property is optional.}
    property TableName: String read FTableName write FTableName;
    {:$ Contains a String representation of the version. }
    property VersionLabel: String read GetVersionLabel write SetVersionLabel;
  end;

  {:$ TDatabaseUpdates collection contains a list of TDatabaseUpdate items.}
  TDatabaseUpdates = class (TSchemaItemsCollection)
  protected
    function GetItem(Index: Integer): TDatabaseUpdate;
    procedure SetItem(Index: Integer; Value: TDatabaseUpdate);
  public
    {:$ Creates and adds a new TDatabaseUpdate item to the TDatabaseUpdates collection. }
    function Add: TDatabaseUpdate;
    {:$ Updates collection after an item has changed. }
    procedure Update(Item: TCollectionItem); override;
    {:$ Provides access to the TDatabaseUpdate items in the collection by index. }
    property Items[Index: Integer]: TDatabaseUpdate read GetItem write SetItem; default;
  end;

  {:$ TDomain is a collection item representing a table field definition. }
  TDomain = class (TSchemaCollectionItem)
  protected
    FDataType: TFieldDataType;
    FSQLFieldType: String;
    FSize: Integer;
    procedure SetDataType(Value: TFieldDataType);
    procedure SetSQLFieldType(const Value: String);
    procedure ObjectRenamed(const OldName: String); override;
  public
    {:$ Creates and instance of TDomain object. }
    constructor Create(Collection: TCollection); override;
    {:$ Destroys the instance of TDomain object. }
    destructor Destroy; override;

    {:$ When domain properties has changed, this method must be called explicitly }
    {:$ in order to reassign field properties from domain properties. }
    procedure UpdateFields;
    function IsReferenced: Boolean;

    {:$ Copies the contents from another TDomain object. }
    procedure Assign(Source: TPersistent); override;
    function GetSchemaClassName: String; override;
  published
    {:$ Determines the logical type of a domain. }
    {:: Use DataType to find out what type of data the domain contains. When adding domain definitions to schema, }
    {:: set DataType to specify what type of field is being defined. }
    property DataType: TFieldDataType read FDataType write SetDataType;
    property Size: Integer read FSize write FSize;
    property SQLFieldType: String read FSQLFieldType write SetSQLFieldType;
  end;

  {:$ TDomains collection contains a list of TDomain items.}
  TDomains = class(TSchemaItemsCollection)
  protected
    function GetDomain(Index: Integer): TDomain;
    procedure SetDomain(Index: Integer; Value: TDomain);
  public
    {:$ Creates and adds a new TDomain item to the TDomains collection. }
    function Add: TDomain;
    {:$ Locates a field definition item by name within the TDomains collection. }
    function Find(const Name: String): TDomain;
    {:$ Provides access to the TDomain items in the collection by index. }
    property Items[Index: Integer]: TDomain read GetDomain write SetDomain; default;
  end;

  {:$ TSequence is a collection item representing a view definition. }
  TSequence = class (TSchemaCollectionItem)
  protected
    FIncrement: Integer;
    FInitialValue: Integer;
  public
    {:$ Copies the contents from another TSequence object. }
    procedure Assign(Source: TPersistent); override;
    function GetSchemaClassName: String; override;
  published
    property Increment: Integer read FIncrement write FIncrement;
    property InitialValue: Integer read FInitialValue write FInitialValue;
  end;

  {:$ TSequences is a collection contains a list of TSequence items.}
  TSequences = class(TSchemaItemsCollection)
  protected
    function GetSequence(Index: Integer): TSequence;
    procedure SetSequence(Index: Integer; Value: TSequence);
  public
    {:$ Creates and adds a new TSequence item to the TSequences collection. }
    function Add: TSequence;
    {:$ Locates a field definition item by name within the TDomains collection. }
    function Find(const Name: String): TSequence;
    {:$ Provides access to the TSequence items in the collection by index. }
    property Items[Index: Integer]: TSequence read GetSequence write SetSequence; default;
  end;

  {:$ TColumnDef is a collection item representing a view or stored proc result column. }
  TColumnDef = class (TSchemaCollectionItem)
  protected
    FSourceTable: String;
    FSourceField: String;
    FDataType: TFieldDataType;
    procedure SetDataType(const Value: TFieldDataType);
    procedure SetSourceField(const Value: String);
    procedure SetSourceTable(const Value: String);
  public
    {:: Resynch data type from refered table and field if any found. }
    procedure UpdateDataType;

    function GetSchemaClassName: String; override;
    {:$ Compares two item's physical properties. Returns true if they're identical. }
    function Compare(Dest: TCompareSchemaItem): Boolean; override;
    {:$ Copies the contents from another TColumnDef object. }
    procedure Assign(Source: TPersistent); override;
    {:$ Specifies the name of the TColumnDef as it appears in Object Inspector. }
    function GetDisplayName: String; override;
  published
    property SourceTable: String read FSourceTable write SetSourceTable;
    property SourceField: String read FSourceField write SetSourceField;
    property DataType: TFieldDataType read FDataType write SetDataType default ftUnknown;
  end;

  {:$ TColumnDefs collection contains a list of TColumnDef items.}
  TColumnDefs = class (TSchemaItemsCollection)
  protected
    function GetItem(Index: Integer): TColumnDef;
    procedure SetItem(Index: Integer; Value: TColumnDef);
  public
    {:$ Creates and adds a new TColumnDef item to the TColumnDefs collection. }
    function Add: TColumnDef;
    {:$ Provides access to the TColumnDef items in the collection by index. }
    property Items[Index: Integer]: TColumnDef read GetItem write SetItem; default;
  end;

  {:$ TViewDefinition is a collection item representing a view definition. }
  TViewDefinition = class (TSchemaCollectionItem)
  protected
    FDefinition: TStrings;
    FColumnDefs: TColumnDefs;
    procedure SetDefinition(const Value: TStrings);
    procedure SetColumnDefs(const Value: TColumnDefs);
  public
    {:$ Creates and instance of TViewDefinition object. }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure SetPropValue(const PropName, Value: String); override;
    {:$ Compares two item's physical properties. Returns true if they're identical. }
    function Compare(Dest: TCompareSchemaItem): Boolean; override;
    {:$ Copies the contents from another TViewDefinition object. }
    procedure Assign(Source: TPersistent); override;
    function GetSchemaClassName: String; override;
    procedure UpdateDefinition(const Value: String = ''); override;
  published
    property Definition: TStrings read FDefinition write SetDefinition;
    property ColumnDefs: TColumnDefs read FColumnDefs write SetColumnDefs;
  end;

  {:$ TViewDefinitions is a collection contains a list of TViewDefinition items.}
  TViewDefinitions = class(TSchemaItemsCollection)
  protected
    function GetViewDefinition(Index: Integer): TViewDefinition;
    procedure SetViewDefinition(Index: Integer; Value: TViewDefinition);
  public
    {:$ Creates and adds a new TDomain item to the TDomains collection. }
    function Add: TViewDefinition;
    {:$ Locates a field definition item by name within the TDomains collection. }
    function Find(const Name: String): TViewDefinition;
    {:$ Provides access to the TDomain items in the collection by index. }
    property Items[Index: Integer]: TViewDefinition read GetViewDefinition write SetViewDefinition; default;
  end;

  {:$ TStoredProcDefinition is a collection item representing a stored procs. }
  TStoredProcDefinition = class (TSchemaCollectionItem)
  protected
    FDefinition: TStrings;
    FIsFunction: Boolean;

    procedure SetDefinition(const Value: TStrings);
  public
    {:$ Creates and instance of TStoredProcDefinition object. }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    function GetSchemaClassName: String; override;
    procedure SetPropValue(const PropName, Value: String); override;
    procedure Assign(Source: TPersistent); override;
    function Compare(Dest: TCompareSchemaItem): Boolean; override;
    procedure UpdateDefinition(const Value: String = ''); override;
  published
    property Definition: TStrings read FDefinition write SetDefinition;
    property IsFunction: Boolean read FIsFunction write FIsFunction default False;
  end;

  {:$ TStoredProcDefinitions is a collection contains a list of TStoredProcDefinition items.}
  TStoredProcDefinitions = class (TSchemaItemsCollection)
  protected
    function GetStoredProcDefinition(Index: Integer): TStoredProcDefinition;
    procedure SetStoredProcDefinition(Index: Integer; Value: TStoredProcDefinition);
  public
    {:$ Creates and adds a new TDomain item to the TDomains collection. }
    function Add: TStoredProcDefinition;
    {:$ Locates a field definition item by name within the TDomains collection. }
    function Find(const Name: String): TStoredProcDefinition;
    {:$ Provides access to the TDomain items in the collection by index. }
    property Items[Index: Integer]: TStoredProcDefinition read GetStoredProcDefinition write SetStoredProcDefinition; default;
  end;

  {:$ TModuleDefinition is a collection item representing a module. }
  TModuleDefinition = class (TViewDefinition)
  public
    function GetSchemaClassName: String; override;
  end;

  {:$ TModuleDefinitions is a collection contains a list of TModuleDefinition items.}
  TModuleDefinitions = class (TSchemaItemsCollection)
  protected
    function GetModuleDefinition(Index: Integer): TModuleDefinition;
    procedure SetModuleDefinition(Index: Integer; Value: TModuleDefinition);
  public
    {:$ Creates and adds a new TModuleDefinition item to the TModuleDefinitions collection. }
    function Add: TModuleDefinition;
    {:$ Locates a field definition item by name within the TModuleDefinitions collection. }
    function Find(const Name: String): TModuleDefinition;
    {:$ Provides access to the TDomain items in the collection by index. }
    property Items[Index: Integer]: TModuleDefinition read GetModuleDefinition write SetModuleDefinition; default;
  end;

  {:$ TCustomObject is a collection item representing a generic database object, such as exception, }
  {:$ file or external function defintion. }
  TCustomObject = class (TSchemaCollectionItem)
  private
    FSchemaClassName: String;
    FDefinition: String;
    procedure SetSchemaClassName(const Value: String);
  public
    constructor Create(Collection: TCollection); override;
    function GetSchemaClassName: String; override;
    {:$ Compares two item's physical properties. Returns true if they're identical. }
    function Compare(Dest: TCompareSchemaItem): Boolean; override;
    {:$ Copies the contents from another TViewDefinition object. }
    procedure Assign(Source: TPersistent); override;
  published
    property SchemaClassName: String read FSchemaClassName write SetSchemaClassName;
    property Definition: String read FDefinition write FDefinition;
  end;

  {:$ TCustomObjects is a collection contains a list of TCustomObject items.}
  TCustomObjects = class (TSchemaItemsCollection)
  protected
    function GetCustomObject(Index: Integer): TCustomObject;
    procedure SetCustomObject(Index: Integer; Value: TCustomObject);
  public
    {:$ Creates and adds a new TCustomObject item to the TCustomObjects collection. }
    function Add(const ASchemaClassName: String = ''): TCustomObject;
    {:$ Locates a field definition item by name within the TCustomObjects collection. }
    function Find(const Name: String): TCustomObject;
    {:$ Provides access to the TDomain items in the collection by index. }
    property Items[Index: Integer]: TCustomObject read GetCustomObject write SetCustomObject; default;
  end;

  TFieldDependancy = (fdPrimaryKey, fdForeighnKey, fdIndex);
  TFieldDependances = set of TFieldDependancy;

  {:$ TFieldDefinition is a collection item representing a table field definition. }
  TFieldDefinition = class (TTableCollectionItem)
  protected
    // FOldName: String;
    // FFieldNo: Integer;
    // FInternalCalcField: Boolean;
    FPrecision: Integer;
    FAttributes: TFieldAttributes;
    FDataType: TFieldDataType;
    FDomain: String;
    FSQLFieldType: String;
    FSize: Integer;
    FEnumeration: String;
    FDisplayFormat: String;
    FDefaultExpression: String;
    FDisplayWidth: Integer;
    FEditMask: String;
    FRelationName: String;
    FAggregateExpression: String;
    FAggregateType: TAggregateType;
    FIdentity: Boolean;
    FComputeAs: String;

    procedure SetComputeAs(const Value: String);
    function GetIsComputed: Boolean;
    function GetIdentity: Boolean;
    procedure SetIdentity(const Value: Boolean);
    function GetIsUnique: Boolean;
    procedure SetIsUnique(const Value: Boolean);
    function GetIsPrimaryKey: Boolean;
    procedure SetIsPrimaryKey(const Value: Boolean);
    procedure SetSQLFieldType(const Value: String);
    procedure SetAggregateType(const Value: TAggregateType);
    procedure SetAggregateExpression(const Value: String);
    procedure SetRelationName(const Value: String);
    // function GetFieldNo: Integer;
    procedure SetDataType(const Value: TFieldDataType);
    function GetRequired: Boolean;
    procedure SetRequired(const Value: Boolean);
    function GetDisplayLabel: String; override;
    procedure ObjectRenamed(const OldName: String); override;
    procedure SetDomain(const Value: String);
  public
    {:$ Creates and instance of TFieldDefinition item. }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure AssignFromDomain;

    function IsInheritedProp(const PropName: String): Boolean;

//    function GetAutoName(const Template: String = ''): String; override;
    procedure SetPropValue(const PropName, Value: String); override;
    function GetPropValue(const PropName: String): String; override;

    procedure UpdateDomainReference;

    function GetSchemaClassName: String; override;
    {:$ Copies the contents from another TFieldDefinition or TFieldDef object. }
    procedure Assign(Source: TPersistent); override;
    {:$ Copies the contents to another TFieldDefinition or TFieldDef object. }
    procedure AssignTo(Dest: TPersistent); override;
    {:$ Returns field's type and size in readable form, like String[20] }
    function GetDisplayFieldTypeSize: String;
    {:$ Return whether this field is a part of primary index, foreighn key (relation) }
    {:$ and/or some of the other imdexes. }
    function GetDependances: TFieldDependances;
    {:$ Determines if the field is a field whose value is calculated inetrnally by the database. }
    {:: If this field is set to True, the expression is stored in ComputeExpression property. }
    property IsComputed: Boolean read GetIsComputed;
  published
    property IsPrimaryKey: Boolean read GetIsPrimaryKey write SetIsPrimaryKey stored False default False;
    property IsUnique: Boolean read GetIsUnique write SetIsUnique stored False default False;
    {:$ Specifies whether a nonblank value for a field is required. }
    {:: Use Required to find out if a field requires a value or if the field can be blank. }
    property Required: Boolean read GetRequired write SetRequired default False;
    {:$ Provides access to domain name for this field. If this field is empty the domain is not assigned. }
    property Domain: String read FDomain write SetDomain;
    {:$ Returns formatted SQL type & size for the selected target database engine. }
    property DisplayFieldTypeSize: String read GetDisplayFieldTypeSize;
    {:$ Provides access to exact SQL type for the selected target database engine. }
    property SQLFieldType: String read FSQLFieldType write SetSQLFieldType;
    {:$ Identifies attributes of the field associated with the field def. }
    property Attributes: TFieldAttributes read FAttributes write FAttributes default [];
    {:$ Determines the type of a physical field. }
    {:: Use DataType to find out what type of data the field contains. When adding field definitions to schema, set DataType to specify what type of field is being defined. }
    property DataType: TFieldDataType read FDataType write SetDataType default ftUnknown;
    {:$ Specifies the number of digits of accuracy used to store the field value.}
    {:: When defining a BCD field, set Precision to the total number of digits stored }
    {:: for each field value. The value of Precision is ignored unless DataType is ftBCD. }
    property Precision: Integer read FPrecision write FPrecision default 0;
    {:$ Contains the size associated with the field in the physical database table. }
    {:: Size is meaningful only for a field definition object with one of the following }
    {:: TFieldType values: ftString, ftBCD, ftBytes, ftVarBytes, ftBlob, ftMemo or ftGraphic. }
    {:: For String and byte fields, Size is the number of bytes reserved in the table }
    {:: for the field. For a BCD field, Size is the number of digits following }
    {:: the decimal point. For a BLOB, memo, or graphic field, Size is the number }
    {:: of bytes from the field’s value that are stored in the actual database table.}
    property Size: Integer read FSize write FSize;
    {:$ Specifies the previous name of this physical field. }
    // property RenamedFrom: String read FOldName write FOldName stored False;
    { Extended field descriptions }
    {:$ Represents a diplasy label that will appear for this field in the captions of grids and other controls. }
    property DisplayLabel: String read GetDisplayLabel write FDescription stored False;
    {:$ The enumeration type for this field. }
    property Enumeration: String read FEnumeration write FEnumeration;
    {:$ The default display format that will be assigned to persistent fields corresponding to this field definition. }
    property DisplayFormat: String read FDisplayFormat write FDisplayFormat;
    {:$ The default display width that will be assigned to persistent fields corresponding to this field definition. }
    property DisplayWidth: Integer read FDisplayWidth write FDisplayWidth default 0;
    {:$ Specifies an SQL expression that is assigned to the field if the user does not provide a value. }
    property DefaultExpression: String read FDefaultExpression write FDefaultExpression;
    {:$ The default edit mask that will be assigned to persistent fields corresponding to this field definition. }
    property EditMask: String read FEditMask write FEditMask;
    {:: Aggregate function. Used in conjunction with relation field. }
    property AggregateType: TAggregateType read FAggregateType write SetAggregateType default aNone;
    {:: Relation that used in conjunction with aggregate function to produce }
    {:: automatically updated calculated field. }
    property RelationName: String read FRelationName write SetRelationName;
    {:: Foreign field name. }
    property AggregateExpression: String read FAggregateExpression write SetAggregateExpression;
    {:: If true this field is considered to be an Identity. This property is always True for AutoInc fields. }
    property Identity: Boolean read GetIdentity write SetIdentity default False;
    {:: Expression used to compute this field on server. When this property assign assigned }
    {:: IsComputed will return true. }
    property ComputeAs: String read FComputeAs write SetComputeAs;
  end;

  {:$ TFieldDefinitions collection contains a list of TFieldDefinition items.}
  TFieldDefinitions = class(TTableDefItemsCollection)
  protected
    function GetFieldDef(Index: Integer): TFieldDefinition;
    procedure SetFieldDef(Index: Integer; Value: TFieldDefinition);
  public
    {:$ Creates and adds a new TFieldDefinition item to the TFieldDefinitions collection. }
    function Add: TFieldDefinition; overload;
    {:$ Creates and adds a new TFieldDefinition item to the TFieldDefinitions collection. }
    procedure Add(const Name: String; DataType: TFieldDataType; Size: Integer = 0;
      Required: Boolean = False); overload;
    {:$ Locates a field definition item by name within the TFieldDefinitions collection. }
    function Find(const Name: String): TFieldDefinition;
    {:$ Returns an index of a field definition item by its old name within the TFieldDefinitions collection. }
    {:: If field definition is not found the returned value is -1. }
    // function IndexOfRenamed(const AOldName: String): Integer;
    {:$ Provides access to the TFieldDefinition items in the collection by index. }
    property Items[Index: Integer]: TFieldDefinition read GetFieldDef write SetFieldDef; default;
  end;

  TIndexDefinition = class;

  TIndexField = class (TTableCollectionItem)
  protected
    FDescending: Boolean;
    FCaseInsensitive: Boolean;
    function GetIndexDef: TIndexDefinition; virtual;
    procedure ValidateRename(const NewName: String); override;
    function GetItemID: Integer; override;
    procedure SetItemID(const Value: Integer); override;
  public
    {:$ Copies the contents from another TIndexField object. }
    procedure Assign(Source: TPersistent); override;
    function GetSchemaClassName: String; override;
    function Compare(Dest: TCompareSchemaItem): Boolean; override;
    function GetFieldID: Integer;
    property IndexDef: TIndexDefinition read GetIndexDef;
  published
    property Descending: Boolean read FDescending write FDescending;
    property CaseInsensitive: Boolean read FCaseInsensitive write FCaseInsensitive;
  end;

  TIndexFields = class (TTableDefItemsCollection)
  protected
    procedure SetIndexField(const Index: Integer; Value: TIndexField);
    function GetIndexField(const Index: Integer): TIndexField;
  public
    function GetTableDef: TTableDefinition; override;
    function GetIndexDef: TIndexDefinition; virtual;
    // procedure Compare(Dest: TSchemaItemsCollection; List: TList; ByName: Boolean = False); override;
    function Add: TIndexField; overload;
    property Items[const Index: Integer]: TIndexField read GetIndexField write SetIndexField; default;
  end;

  {:$ TIndexDefinition is a collection item representing a definition for a physical index. }
  TIndexDefinition = class (TTableCollectionItem)
  protected
    FOptions: TIndexOptions;
    FIndexFields: TIndexFields;

    function GetDummyBool: Boolean;
    function GetDummyStr: String;
    function GetCaseInsFields: String;
    function GetDescFields: String;
    procedure SetIndexFields(const Value: TIndexFields);
    procedure DoAddDescField(Value: Boolean);
    procedure DoAddIndexField(const Value: String);
    procedure SetNoCase(const Value: Boolean);
    procedure SetUnique(const Value: Boolean);
    procedure SetPrimaryKey(const Value: Boolean);
    function GetNoCase: Boolean;
    function GetUnique: Boolean;
    function GetDisplayProps: String;
    function GetPrimaryKey: Boolean;

    function GetFieldProps: String;
    procedure SetFieldProps(const Value: String);
    function GetDescending: Boolean;
    procedure SetDescending(const Value: Boolean);
    function GetFields: String;
    procedure SetCaseInsFields(const Value: String);
    procedure SetDescFields(const Value: String);
    procedure SetFields(const Value: String);
    procedure SetOptions(const Value: TIndexOptions);
    procedure SetDisplayFields(const Value: String);

    procedure ValidateRename(const NewName: String); override;
  public
    function GetSchemaClassName: String; override;
    {:$ Specifies the name of the TIndexDefinition as it appears in Object Inspector. }
    function GetDisplayName: String; override;
    {:$ Creates and instance of TIndexDefinition object. }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    {:$ Copies the contents from another TIndexDefinition or TIndexDef object. }
    procedure Assign(Source: TPersistent); override;
    {:$ Copies the contents to another TIndexDefinition or TIndexDef object. }
    procedure AssignTo(Dest: TPersistent); override;

    procedure UpdateFieldReferences;

//    function GetAutoName(const Template: String = ''): String; override;
    {:$ Returns True is the specified field is a part of this index. }
    function HasField(const FieldName: String): Boolean;
    {:$ Compares two item's physical properties. Returns true if they're identical. }
    function Compare(Dest: TCompareSchemaItem): Boolean; override;
  published
    property DisplayProps: String read GetDisplayProps;
    property Unique: Boolean read GetUnique write SetUnique stored False default False;
    property PrimaryKey: Boolean read GetPrimaryKey write SetPrimaryKey stored False default False;
    property Descending: Boolean read GetDescending write SetDescending stored False default False;
    property NoCase: Boolean read GetNoCase write SetNoCase stored False default False;

    property AddIndexField: String read GetDummyStr write DoAddIndexField stored False;
    property AddDescField: Boolean read GetDummyBool write DoAddDescField stored False;

    property IndexFields: TIndexFields read FIndexFields write SetIndexFields;

    {:$ Specifies the fields of the index that are case-insensitive. }
    property CaseInsFields: String read GetCaseInsFields write SetCaseInsFields stored False;
    {:$ Specifies the fields of the index that are to be in descending order. }
    property DescFields: String read GetDescFields write SetDescFields stored False;
    {:$ Identifies the fields that comprise the index. }
    {:: When creating an index, set Fields to a list of fields separated by semi-colons }
    {:: (no semi-colon is required if there is only one field in the index).}
    property Fields: String read GetFields write SetFields stored False;
    {:: This property retuns concatenated set of all custom properties of index fields. }
    property FieldProps: String read GetFieldProps write SetFieldProps stored False;
    property DisplayFields: String read GetFields write SetDisplayFields stored False;

    { ----------- DEPRECATED FIELDS ------------------ }
    {:$ Describes the characteristics of the index. }
    {:: This field is provided for compatibility with VCL's TIndexDef object. }
    property Options: TIndexOptions read FOptions write SetOptions default [];
  end;

  {:$ TIndexDefinitions collection contains a list of TIndexDefinition items.}
  TIndexDefinitions = class(TTableDefItemsCollection)
  protected
    function GetIndexDef(Index: Integer): TIndexDefinition;
    procedure SetIndexDef(Index: Integer; Value: TIndexDefinition);
  public
    {:$ Creates and adds a new TIndexDefinition item to the TIndexDefinitions collection. }
    function Add: TIndexDefinition; overload;
    {:$ Creates and adds a new TIndexDefinition item to the TIndexDefinitions collection. }
    procedure Add(const Name, Fields: String; Options: TIndexOptions); overload;
    {:$ Locates an index definition item by name within the TIndexDefinitions collection. }
    function Find(const Name: String): TIndexDefinition;
    {:$ Locates an index definition in the Items array. }
    {:: Use FindIndexForFields to search for the index that starts with the fields }
    {:: specified in the Fields parameter. If the index is constructed using more than }
    {:: one field, separate the fields with semi-colons.<br> }
    {:: FindIndexForFields returns the index definition that exactly matches }
    {:: the fields specified (in the order specified), if one exists. If no exact }
    {:: match is found, FindIndexForFields returns the first index that begins with }
    {:: the indicated fields. If no match is found, FindIndexForFields raises an }
    {:: EDatabaseError exception.}
    function FindIndexForFields(const Fields: String): TIndexDefinition;
    {:$ Locates an index definition in the Items array. }
    {:: Use GetIndexForFields to search for the index that starts with the fields }
    {:: specified in the Fields parameter. If more than one field comprises the index, }
    {:: separate the fields with semicolons. Set the CaseInsensitive parameter to True }
    {:: to restrict the search to only the case insensitive indexes in the Items array. <br>}
    {:: GetIndexForFields returns the index definition that exactly matches the specified
    {:: fields in the order specified, if it exists. If no exact match is found, GetIndexForFields
    {:: returns the first index that begins with the indicated fields. If no match
    {:: can be found, GetIndexForFields returns nil. }
    {:! When CaseInsensitive is True, descending indexes are considered even if they are case-sensitive. }
    function GetIndexForFields(const Fields: String; CaseInsensitive: Boolean): TIndexDefinition;
    {:$ Provides access to the TIndexDefinition items in the collection by index. }
    property Items[Index: Integer]: TIndexDefinition read GetIndexDef write SetIndexDef; default;
  end;

  TRelationships = class (TSchemaItemsCollection)
  private
    function GetItem(Index: Integer): TRelationship;
    procedure SetItem(Index: Integer; const Value: TRelationship);
  public
    {:$ Creates and adds a new TTableDefinition item to the TTableDefinitions collection. }
    function Add: TRelationship;
    function AddRelationship(const DetailTable, MasterTable: String): TRelationship;
    {:$ Locates TTableDefinition item by its table name within the TTableDefinitions collection. }
    function Find(const Name: String): TRelationship;
    {:$ Provides access to the TTableDefinition items in the collection by index by index. }
    property Items[Index: Integer]: TRelationship read GetItem write SetItem; default;
  end;

  TRelationSide = (sideDetail, sideMaster);
  {$NODEFINE TRelationSide}

  (*$HPPEMIT 'namespace Dbschema' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef int TRelationSide;' *)
  (*$HPPEMIT '  #pragma option push -b-' *)
  (*$HPPEMIT '  enum TRelationSideValues {sideDetail=0, sideMaster=1};' *)
  (*$HPPEMIT '  #pragma option pop' *)
  (*$HPPEMIT '}'*)

  TDetailCardinality = (dcOne, dcMany, dcLogical);
  TDualStringProp = array [TRelationSide] of String;

  TRelationship = class (TSchemaCollectionItem)
  protected
    FTableName: TDualStringProp;
    FKeyFields: TDualStringProp;
    FRelationName: TDualStringProp;
    FCondition: TDualStringProp;
    FRelDescription: TDualStringProp;
    FRelation: array [TRelationSide] of TRelation;
    FRelationIndex: array [TRelationSide] of Integer;

    FDetailCardinality: TDetailCardinality;
    FMasterRecordOptional: Boolean;
    FMasterOwnsDetails: Boolean;
    FDeleteAction: TRelationAction;
    FUpdateAction: TRelationAction;
    FEnforceForeignKey: Boolean;
    FCaseInsensitive: Boolean;
    FDeleteErrorMessage: String;
    FUpdateErrorMessage: String;
    FRequireRecordErrorMessage: String;
    procedure SetMasterRecordOptional(const Value: Boolean);
    function GetRelation(const Index: TRelationSide): TRelation;
    function GetTableDef(const Index: TRelationSide): TTableDefinition;
    procedure InternalUpdate; override;
    procedure WriteRelationIndex(Writer: TWriter);
    procedure ReadRelationIndex(Reader: TReader);
    procedure DefineProperties(Filer: TFiler); override;
  public
    {:$ Creates an instance of TRelation object. }
    constructor Create(Collection: TCollection); override;
    {:$ Destroys the instance of TRelation object. }
    destructor Destroy; override;
    {:$ Copies the contents from another TRelation object. }
    procedure Assign(Source: TPersistent); override;
    {:$ Specifies the name of the TRelation as it appears in Object Inspector. }
    function GetDisplayName: String; override;

    procedure SetPropValue(const PropName, Value: String); override;

    function GetSchemaClassName: String; override;
    {:$ Compares two item's physical properties. Returns true if they're identical. }
    function Compare(Dest: TCompareSchemaItem): Boolean; override;

    procedure UpdateFieldReferences(const Side: TRelationSide);

    function Side(Relation: TRelation): TRelationSide;
    procedure UpdateRelations;
    procedure InvalidateRelation(Relation: TRelation);
    procedure SwapSides;
    procedure UpdateKeyFields;

    function GetCondition(const Index: TRelationSide): String;
    function GetKeyFields(const Index: TRelationSide): String;
    function GetRelationName(const Index: TRelationSide): String;
    function GetTableName(const Index: TRelationSide): String;
    function GetRelDescription(const Index: TRelationSide): String;
    procedure SetCondition(const Index: TRelationSide; const Value: String);
    procedure SetKeyFields(const Index: TRelationSide; const Value: String);
    procedure SetRelationName(const Index: TRelationSide; const Value: String);
    procedure SetTableName(const Index: TRelationSide; const Value: String);
    procedure SetRelDescription(const Index: TRelationSide; const Value: String);

    property DetailRelation: TRelation index sideDetail read GetRelation;
    property MasterRelation: TRelation index sideMaster read GetRelation;
    property DetailTableDef: TTableDefinition index sideDetail read GetTableDef;
    property MasterTableDef: TTableDefinition index sideMaster read GetTableDef;
  published
    property DetailTableName: String index sideDetail read GetTableName write SetTableName;
    property DetailKeyFields: String index sideDetail read GetKeyFields write SetKeyFields;
    property DetailRelationName: String index sideDetail read GetRelationName write SetRelationName;
    property DetailCondition: String index sideDetail read GetCondition write SetCondition;
    property DetailDescription: String index sideDetail read GetRelDescription write SetRelDescription;

    property MasterTableName: String index sideMaster read GetTableName write SetTableName;
    property MasterKeyFields: String index sideMaster read GetKeyFields write SetKeyFields;
    property MasterRelationName: String index sideMaster read GetRelationName write SetRelationName;
    property MasterCondition: String index sideMaster read GetCondition write SetCondition;
    property MasterDescription: String index sideMaster read GetRelDescription write SetRelDescription;

    property DetailCardinality: TDetailCardinality read FDetailCardinality write FDetailCardinality default dcMany;
    property MasterRecordOptional: Boolean read FMasterRecordOptional write SetMasterRecordOptional default True;

    property MasterOwnsDetails: Boolean read FMasterOwnsDetails write FMasterOwnsDetails default False;
    property DeleteAction: TRelationAction read FDeleteAction write FDeleteAction default raIgnore;
    property UpdateAction: TRelationAction read FUpdateAction write FUpdateAction default raIgnore;
    property EnforceForeignKey: Boolean read FEnforceForeignKey write FEnforceForeignKey default True;

    // The following properties are client-side only
    property CaseInsensitive: Boolean read FCaseInsensitive write FCaseInsensitive default False;
    property DeleteErrorMessage: String read FDeleteErrorMessage write FDeleteErrorMessage;
    property UpdateErrorMessage: String read FUpdateErrorMessage write FUpdateErrorMessage;
    property RequireRecordErrorMessage: String read FRequireRecordErrorMessage write FRequireRecordErrorMessage;
  end;

  TMultiplicity = (mUndefined, mOne, mZeroOrOne, mZeroOrMany);

  {:$ TRelation is a collection item representing a relation between two physical tables. }
  TRelation = class (TTableCollectionItem)
  protected
    FRelationship: TRelationship;

    function GetIsForeignKey: Boolean;
    function GetSide: TRelationSide;
    function GetMultiplicity: TMultiplicity;
    function GetEnforceForeignKey: Boolean;
    function GetRelationshipName: String;
    procedure SetRelationshipName(const Value: String);
    function GetDescription: String;
    function GetCaseInsensitive: Boolean;
    function GetCondition: String;
    function GetDeleteAction: TRelationAction;
    function GetForeignCondition: String;
    function GetForeignKeyFields: String;
    function GetForeignTable: String;
    function GetKeyFields: String;
    function GetPeerRelationName: String;
    function GetRelationKind: TRelationKind;
    function GetRelationType: TRelationType;
    function GetRequireOneRecord: Boolean;
    function GetUpdateAction: TRelationAction;
    procedure SetDescription(const Value: String);
    procedure SetDeleteErrorMessage(const Value: String);
    procedure SetPeerRelationName(const Value: String);
    procedure SetRequireRecordErrorMessage(const Value: String);
    procedure SetUpdateErrorMessage(const Value: String);
    function GetRequireRecordErrorMessage: String;
    function GetDeleteErrorMessage: String;
    function GetUpdateErrorMessage: String;

    procedure SetCaseInsensitive(const Value: Boolean);
    procedure SetCondition(const Value: String);
    procedure SetForeignCondition(const Value: String);
    procedure SetForeignKeyFields(const Value: String);
    procedure SetForeignTable(const Value: String);
    procedure SetKeyFields(const Value: String);
    procedure SetRelationKind(const Value: TRelationKind);
    procedure SetRelationType(const Value: TRelationType);
    procedure SetDeleteAction(const Value: TRelationAction);
    procedure SetRequireOneRecord(const Value: Boolean);
    procedure SetUpdateAction(const Value: TRelationAction);
    procedure DoAddForeignKeyField(const Value: String);
    procedure DoAddKeyField(const Value: String);
    function GetDummyStr: String;
    procedure ObjectRenamed(const OldName: String); override;
    function GetRelationship: TRelationship; virtual;
    function FindPeerRelation: TRelation;
    function GetItemID: Integer; override;
    function GetRelationIndex: Integer;
  public
    {:$ Specifies the name of the TRelation as it appears in Object Inspector. }
    function GetDisplayName: String; override;
    {:$ Creates an instance of TRelation object. }
    constructor Create(Collection: TCollection); override;
    {:$ Destroys the instance of TRelation object. }
    destructor Destroy; override;
    {:$ Copies the contents from another TRelation object. }
    procedure Assign(Source: TPersistent); override;
    {:$ Specifies the locate options to be used in }
    function GetLocateOptions: TLocateOptions;
    {:$ Provides access to a peer relation object defined for the foreign table. }
    {:: Peer relation object is an object representing the same relation from the point of view of the foreign table.<br> }
    {:: If this relation has RelationKind = rkChildren, then the peer relation must have RelationKind set to rkParent. }
    function GetPeerRelation: TRelation;

    function Compare(Dest: TCompareSchemaItem): Boolean; override;
    function GetSchemaClassName: String; override;
//    function GetAutoName(const Template: String = ''): String; override;

    function IsDataTemporary: Boolean;
    property Relationship: TRelationship read GetRelationship;

    function EffectiveDeleteAction: TRelationAction;
    function EffectiveUpdateAction: TRelationAction;
    function EffectiveDeleteErrorMessage: String;
    function EffectiveUpdateErrorMessage: String;
    function EffectiveRequireRecordErrorMessage: String;
    property EnforceForeignKey: Boolean read GetEnforceForeignKey;
    property Multiplicity: TMultiplicity read GetMultiplicity;
    property Side: TRelationSide read GetSide;
    property RelationIndex: Integer read GetRelationIndex;
    property IsForeignKey: Boolean read GetIsForeignKey;
  published
    property AddKeyField: String read GetDummyStr write DoAddKeyField stored False;
    property AddForeignKeyField: String read GetDummyStr write DoAddForeignKeyField stored False;

    property RelationshipName: String read GetRelationshipName write SetRelationshipName stored False;
    {:$ Specifies a set of key fields for this relation. }
    property KeyFields: String read GetKeyFields write SetKeyFields stored IsDataTemporary;
    {:$ Specifies a foreign table for this relation. }
    property ForeignTable: String read GetForeignTable write SetForeignTable stored IsDataTemporary;
    {:$ Specifies a set of fields in the foreign table for this relation. }
    property ForeignKeyFields: String read GetForeignKeyFields write SetForeignKeyFields stored IsDataTemporary;
    {:$ Specifies whether the key is case insensitive. }
    property CaseInsensitive: Boolean read GetCaseInsensitive write SetCaseInsensitive stored IsDataTemporary;
    {:$ Specifies the additional filter for a foreign table. }
    {:: ForeignFilter property can be used to narrow down the range of related records }
    {:: by providing additional restrictions. For instance, it can be used to }
    {:: relate a customer to the unpaid receipts only. In this case, the relation }
    {:: can be used to disable delete or update operations for customers, who have }
    {:: upaid balances. The properties for such relation should be set as follows: <br> }
    {:: with Relation do begin }
    {::   KeyFields := 'CustomerID'; }
    {::   ForeignTable := 'Receipts'; }
    {::   ForeignKeyFields := 'CustomerID'; }
    {::   ForeignFilter := 'Balance > 0'; }
    {:: end; }
    property ForeignCondition: String read GetForeignCondition write SetForeignCondition stored IsDataTemporary;
    {:$ Specifies a field, which determine if this relation is applicable to the current record. }
    {:: If ConditionField is not empty, then the relation only applies if this field is not null or contains a boolean 'True' value. }
    property Condition: String read GetCondition write SetCondition stored IsDataTemporary;
    {:$ Specifies the type of the relation between to tables. }
    property RelationType: TRelationType read GetRelationType write SetRelationType stored IsDataTemporary;
    {:$ Specifies the kind of the relation between to tables. }
    property RelationKind: TRelationKind read GetRelationKind write SetRelationKind stored IsDataTemporary default rkReference;
    {:$ Specifies whether there have to be at least one record on the other side of the relation. }
    {:: Use this property to control the bahviour of update operations. }
    {:: If one or more of the KeyFields for this relation is modified or if }
    {:: non empty values are entered as a result of insert operation, the database }
    {:: (if the appropriate UpdateOptions are set) will check if KeyFields }
    {:: refer to existing records in a foreign table. If there are no records }
    {:: in the foreign table the exception will be raised and the update }
    {:: operation will be aborted. }
    {:! It is up to the database object how to implement this functionality. }
    {:! Generally, TDatabaseSchema is only used to contain information, that is }
    {:! necessary to perform certain integrity checks and operations, but does not }
    {:! implement them directly. The database object (like TxxxDatabaseExt) }
    {:! that refer to this schama must implement the corresponding functionality. }
    property RequireOneRecord: Boolean read GetRequireOneRecord write SetRequireOneRecord stored IsDataTemporary default False;
    {:$ Specifies what action should be taken if a record is being deleted in the table. }
    property DeleteAction: TRelationAction read GetDeleteAction write SetDeleteAction stored IsDataTemporary default raIgnore;
    {:$ Specifies what action should be taken if key fields are being updated in the table. }
    property UpdateAction: TRelationAction read GetUpdateAction write SetUpdateAction stored IsDataTemporary default raIgnore;
    {:$ Name of reverse relation in foreign table. }
    property PeerRelationName: String read GetPeerRelationName write SetPeerRelationName stored IsDataTemporary;
    { --- Deprecated properties --- }
    {:$ Specifies the error message, that should appear if delete operation has failed due to specified integrity constaint. }
    property DeleteErrorMessage: String read GetDeleteErrorMessage write SetDeleteErrorMessage stored IsDataTemporary;
    {:$ Specifies the error message, that should appear if update operation has failed due to specified integrity constaint. }
    property UpdateErrorMessage: String read GetUpdateErrorMessage write SetUpdateErrorMessage stored IsDataTemporary;
    {:$ Specifies the error message, that should appear if update operation has failed because the key fields in the table }
    {:: does not refer to one or more records in the foreign table. }
    property RequireRecordErrorMessage: String read GetRequireRecordErrorMessage write SetRequireRecordErrorMessage stored IsDataTemporary;
    property Description: String read GetDescription write SetDescription stored IsDataTemporary;
  end;

  {:$ TRelations collection contains a list of TRelation items.}
  TRelations = class(TTableDefItemsCollection)
  protected
    function GetRelation(Index: Integer): TRelation;
    procedure SetRelation(Index: Integer; Value: TRelation);
  public
    {:$ Creates and adds a new TRelation item to the TRelations collection. }
    function Add: TRelation;
    {:$ Locates TRelation item by its name within the TRelations collection. }
    function Find(const Name: String): TRelation;
    {:$ Provides access to the TRelation items in the collection by index. }
    property Items[Index: Integer]: TRelation read GetRelation write SetRelation; default;
  end;

  TAggregateLink = class (TTableCollectionItem)
  protected
    FAggregateType: TAggregateType;
    FForeignFieldName: String;
    FAggregateExpression: String;
    FKeyFields: String;
    FForeignKeyFields: String;
    FForeignTable: String;
    FRelationType: TRelationType;
    FRelationKind: TRelationKind;
    FCondition: String;
    FForeignCondition: String;
    FCaseInsensitive: Boolean;
  public
    constructor CreateLink(Collection: TCollection; Relation: TRelation; ForeignFieldDef: TFieldDefinition);

    property KeyFields: String read FKeyFields;
    property ForeignKeyFields: String read FForeignKeyFields;
    property ForeignTable: String read FForeignTable;
    property RelationType: TRelationType read FRelationType;
    property RelationKind: TRelationKind read FRelationKind;
    property Condition: String read FCondition;
    property ForeignCondition: String read FForeignCondition;
    property CaseInsensitive: Boolean read FCaseInsensitive;
    property AggregateExpression: String read FAggregateExpression;
    property ForeignFieldName: String read FForeignFieldName;
    property AggregateType: TAggregateType read FAggregateType;
  end;

  TAggregateLinkData = class (TObject)
  protected
    FUpdateKeys: TStringList;
    FAggregateLink: TAggregateLink;
    function GetUpdateKeys: TStrings;
  public
    constructor Create(AggregateLink: TAggregateLink);
    destructor Destroy; override;

    procedure AddKey(const KeyValue: String);
    procedure ClearKeys;
    function HasKeys: Boolean;

    property UpdateKeys: TStrings read GetUpdateKeys;
    property AggregateLink: TAggregateLink read FAggregateLink;
  end;

  {:$ TAggregateLinks collection contains a list of TAggregateLink items.}
  TAggregateLinks = class (TTableDefItemsCollection)
  protected
    function GetAggregateLink(Index: Integer): TAggregateLink;
    procedure SetAggregateLink(Index: Integer;
      const Value: TAggregateLink);
  public
    {:$ Provides access to the TAggregateLink items in the collection by index. }
    property Items[Index: Integer]: TAggregateLink read GetAggregateLink write SetAggregateLink; default;
  end;

  TTriggerType = (ttBefore, ttAfter, ttUponCommit, ttServerSide);
  TTriggerActive = (taAlways, taExceptWhenReplicating, taOnlyWhenReplicating, taNever);
  TSelectTriggers = set of TTriggerActive;

  {:$ TTriggerDefinition is a collection item representing a trigger that will be }
  {:$ executed when a record in a table is updated or deleted. }
  TTriggerDefinition = class (TTableCollectionItem)
  protected
    FDefinition: String;
    FTriggerWhen: TChangeTypes;
    FTriggerType: TTriggerType;
    FTriggerActive: TTriggerActive;
    function GetTableName: String;
    function GetTriggerWhen(Idx: TChangeType): boolean;
    procedure SetTriggerWhen(Idx: TChangeType; Value: boolean);
  public
    {:$ Returns display name for object trigger, that will appear in object inspector. }
    function GetDisplayName: String; override;
    {:$ Creates an instance of TTriggerDefinition item. }
    constructor Create(Collection: TCollection); override;

    function GetSchemaClassName: String; override;

    procedure SetPropValue(const PropName, Value: String); override;

    procedure UpdateDefinition(const Value: String = ''); override;
    procedure Assign(ASource: TPersistent); override;
    {:$ Name of the table this trigger relates to. }
    property TableName: String read GetTableName;
  published
    {:$ TriggerWhen property controls when to execute this trigger. }
    property TriggerWhen: TChangeTypes read FTriggerWhen write FTriggerWhen;
    property TriggerType: TTriggerType read FTriggerType write FTriggerType;
    property TriggerActive: TTriggerActive read FTriggerActive write FTriggerActive default taAlways;
    {:$ SQLScript property contains an optional SQL script that will be executed. }
    property SQLScript: String read FDefinition write FDefinition stored False;
    property Definition: String read FDefinition write FDefinition;
    property OnDeleted: boolean index ctDeleted read GetTriggerWhen write SetTriggerWhen stored False;
    property OnUpdated: boolean index ctModified read GetTriggerWhen write SetTriggerWhen stored False;
    property OnInserted: boolean index ctInserted read GetTriggerWhen write SetTriggerWhen stored False;
  end;

  {:$ TTriggerDefinitions collection contains a list of TTriggerDefinition items.}
  TTriggerDefinitions = class(TTableDefItemsCollection)
  private
    function GetTableTriggers(Index: Integer): TTriggerDefinition;
    procedure SetTableTriggers(Index: Integer; const Value: TTriggerDefinition);
  public
    {:$ Creates and adds a new TTriggerDefinition item to the TTriggerDefinitions collection. }
    function Add: TTriggerDefinition;
    {:$ Provides access to the TTriggerDefinition items in the collection by index. }
    property Items[Index: Integer]: TTriggerDefinition read GetTableTriggers write SetTableTriggers; default;
  end;

  {:$ TableConstraint is a collection item representing table constraints. }
  TTableConstraint = class (TTableCollectionItem)
  protected
    FCheck: String;
  public
    {:$ Compares two item's physical properties. Returns true if they're identical. }
    function Compare(Dest: TCompareSchemaItem): Boolean; override;
    {:$ Copies the contents from another TTableConstraint object. }
    procedure Assign(Source: TPersistent); override;
    function GetSchemaClassName: String; override;
  published
    property Check: String read FCheck write FCheck;
  end;

  {:$ TTableConstraints is a collection contains a list of TTableConstraint items.}
  TTableConstraints = class(TTableDefItemsCollection)
  protected
    function GetConstraint(Index: Integer): TTableConstraint;
    procedure SetConstraint(Index: Integer; Value: TTableConstraint);
  public
    {:$ Creates and adds a new TTableConstraint item to the TTableConstraints collection. }
    function Add: TTableConstraint;
    function Find(const Name: String): TTableConstraint;
    {:$ Provides access to the TDomain items in the collection by index. }
    property Items[Index: Integer]: TTableConstraint read GetConstraint write SetConstraint; default;
  end;

  {:$ TTableDefinition is a collection item representing a physical table. }
  TTableDefinition = class (TSchemaCollectionItem)
  protected
    FTypePrefix: String;
    FIndexDefs: TIndexDefinitions;
    FFieldDefs: TFieldDefinitions;
    FRelations: TRelations;
    FAggregateLinks: TAggregateLinks;
    FTriggers: TTriggerDefinitions;
    FConstraints: TTableConstraints;

    // Fields related to replications
    FReplicate: Boolean;
    FMasterRecord: Boolean;
    FObjectType: String; // Effectively a table name alias
    FObjectKeyFields: String;
    FObjectKeyCaseInsensitive: Boolean;

    // View
    FIsView: boolean;
    FDefinition: TStrings;

    function StoreFieldDefs: Boolean;
    function StoreIndexDefs: Boolean;
    function StoreRelations: Boolean;
    function StoreTriggers: Boolean;
    function StoreConstraints: Boolean;
    procedure SetConstraints(const Value: TTableConstraints);
    procedure SetFieldDefs(const Value: TFieldDefinitions);
    procedure SetIndexDefs(const Value: TIndexDefinitions);
    procedure SetRelations(const Value: TRelations);
    procedure SetTriggers(const Value: TTriggerDefinitions);
    procedure SetObjectType(const Value: String);
    function ObjectTypeAssigned: Boolean;
    procedure CheckObjectTypeUnique(const NewValue: String);
    procedure ObjectRenamed(const OldName: String); override;
    procedure ReadRelations(Reader: TReader);
    procedure WriteRelations(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    function GetHasIdentityFields: Boolean;
    procedure SetIsView(Value: boolean);
    procedure SetDefinition(const Value: TStrings);
  public
    constructor Create(Collection: TCollection); override;
    // constructor CreateView(Collection: TCollection);
    destructor Destroy; override;

    procedure SetPropValue(const PropName, Value: String); override;
    procedure Assign(ASource: TPersistent); override;
    procedure UpdateDefinition(const Value: String = ''); override;
    {:$ This method is used in import instead of Assign because it does not overwrite }
    {:$ field definitions, relations, triggers, etc., but replaces only the ones changed. }
    {:: After the call to CopyFrom the structure of table defintion is identical to the }
    {:: ASource table definition. }
    procedure CopyFrom(ASource: TPersistent; AWithSubItems: boolean = True); override;

    procedure Prepare;
    procedure UnPrepare;

    function GetSchemaClassName: String; override;
    {:$ Returns definition of the primary index for this table. }
    {:: Returns nil if not primary index is specified. }
    function GetPrimaryIndex: TIndexDefinition;
    function GetPrimaryKeyFields: String;

    {:$ Locates constraint by name among primary key, foreign keys & checks. }
    function FindConstraint(const Name: String): TTableCollectionItem;
    {:$ Returns true if index exists for Fields (including case sensitivity). }
    function PartOfIndex(const Fields: String; CaseInsensitive: Boolean = False): Boolean;
    {:$ Returns true if any unique or primary index is a part of Fields (including case sensitivity). }
    function IsUniqueKey(const Fields: String; CaseInsensitive: Boolean = False): Boolean;
    {:$ Returns list of TFieldDefinition objects named in FieldNames (delimited by ';') }
    procedure GetFieldList(List: TList; const FieldNames: String); overload;
    {:$ Returns list of TFieldDefinition objects named in FieldNames (delimited by ';') }
    procedure GetFieldList(List: TStrings; const FieldNames: String); overload;
    {:$ Compares two item's physical properties. Returns true if they're identical. }
    function Compare(Dest: TCompareSchemaItem): Boolean; override;

    {:$ Returns effective object type. If FObjectType is not specified, then table's name is returned. }
    function GetObjectType: String;

    {:: Internal collection of calculated fields }
    property AggregateLinks: TAggregateLinks read FAggregateLinks;
    {:$ Specifies the fields (delimited by semi-colon) that comprise object's key.<br> }
    {:! Fields specified by the ObjectKeyFields property must be a unique key for the table.  }
    property ObjectKeyFields: String read FObjectKeyFields;
    {:$ Tells whether the object key fields should be treated as case insensitive. }
    property ObjectKeyCaseInsensitive: Boolean read FObjectKeyCaseInsensitive;
    {:$ If true, then a record in this table represents an object. Otheriwse, it's
    {:$ part of another object connected by child-parent relationship. }
    property MasterRecord: Boolean read FMasterRecord;

    {:$ Specifies the collection of relations for this table. }
    property Relations: TRelations read FRelations write SetRelations;
    {:$ Determines if there's any field definition with Identity = True. }
    property HasIdentityFields: Boolean read GetHasIdentityFields;
  published
    {:$ Specifies the name of the physical table. }
    property TableName: String read FName write Rename stored False;
    {:$ Specifies the collection of field definitions for the physical table. }
    property FieldDefs: TFieldDefinitions read FFieldDefs write SetFieldDefs stored StoreFieldDefs;
    {:$ Specifies the collection of index definitions for the physical table. }
    property IndexDefs: TIndexDefinitions read FIndexDefs write SetIndexDefs stored StoreIndexDefs;
    { Extended table properties }
    {:$ Specifies the collection of triggers for this table. }
    property Triggers: TTriggerDefinitions read FTriggers write SetTriggers stored StoreTriggers;
    {:$ Specifies the collection of table constraints for this table. }
    property Constraints: TTableConstraints read FConstraints write SetConstraints stored StoreConstraints;

    {:$ Specifies the name of the object type represented by a record in the table. }
    property ObjectType: String read FObjectType write SetObjectType stored ObjectTypeAssigned;
    {:$ Specifies whether the changes made to this table should be stored to be used for }
    {:$ replicating it's content with the main database. }
    {:: Generally all the tables should have Replicate set to True, except for those }
    {:: containing the information that does not directly result from user input, but }
    {:: updated from on the content of other objects. For instance, a table }
    {:: that contains totals for some other table should not be replicated, but updated }
    {:: whenever the other table changed. Another example of a table, that should }
    {:: not be relpicated is a table, containing settings sepecific for this }
    {:: particular database snapshot. }
    property Replicate: Boolean read FReplicate write FReplicate default True;
    {:$ Specifies the prefix for auto generated DataSet reference class. }
    property TypePrefix: String read FTypePrefix write FTypePrefix;
    property IsView: boolean read FIsView write SetIsView;
    property Definition: TStrings read FDefinition write SetDefinition;

    { !!! IMPORTANT: When adding published properties to the TTableDefinition, add them }
    { (copying) to Assign, CopyFrom, ApplyDifferent and CreateDifference methods. }
  end;

  {:$ TTableDefinitions collection contains a list of TTableDefinition items.}
  TTableDefinitions = class (TSchemaItemsCollection)
  private
    function GetItem(Index: Integer): TTableDefinition;
    procedure SetItem(Index: Integer; const Value: TTableDefinition);
  public
    {:$ Creates and adds a new TTableDefinition item to the TTableDefinitions collection. }
    function Add(AIsView: boolean = False): TTableDefinition;
    {:$ Locates TTableDefinition item by its table name within the TTableDefinitions collection. }
    function Find(const Name: String): TTableDefinition;
    {:$ Provides access to the TTableDefinition items in the collection by index by index. }
    property Items[Index: Integer]: TTableDefinition read GetItem write SetItem; default;
  end;

  {:$ TEnumeration is a collection item representing a type of enumeration. }
  TEnumeration = class (TSchemaCollectionItem)
  private
    function GetDelphiDecl: String;
  protected
    FItems: TStringList;
    FDescriptions: TStringList;
    FShortDescriptions: TStringList;
    FTypePrefix: String;
    FIntConsts: Boolean;
    function GetDescriptions: TStrings;
    function GetShortDescriptions: TStrings;
    function GetItems: TStrings;
    procedure SetDescriptions(const Value: TStrings);
    procedure SetShortDescriptions(const Value: TStrings);
    procedure SetItems(const Value: TStrings);
    procedure ObjectRenamed(const OldName: String); override;
    function  GetDisplayLabel: String; override;
  public
    function GetDisplayName: String; override;
    procedure Assign(ASource: TPersistent); override;

    {:$ Compares two item's physical properties. Returns true if they're identical. }
    function Compare(Dest: TCompareSchemaItem): Boolean; override;

    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property DelphiDecl: String read GetDelphiDecl;
    {:$ Stores pairs Value=Name, that defines this enumeration. }
    {:: E.g. Order states:<br>}
    {::   '0=Open'<br>}
    {::   '1=Processing'<br>}
    {::   '2=Complete'<br>}
    {:: See also Descriptions property.}
    property Items: TStrings read GetItems write SetItems;
    {:$ Stores pairs Value=Descripts, that describes items of this enumeration. }
    {:: E.g. Order states:<br>}
    {::   '0=Order in open'<br>}
    {::   '1=Order is in processing'<br>}
    {::   '2=Order is complete'<br>}
    {:: See also Items property.}
    property Descriptions: TStrings read GetDescriptions write SetDescriptions;
    {:$ Stores pairs Value=ShortDescripts, that describes items of this enumeration. }
    {:: See also Items property.}
    property ShortDescriptions: TStrings read GetShortDescriptions write SetShortDescriptions;
    {:$ Specifies the DisplayLabel for this enumeration. }
    property DisplayLabel: String read GetDisplayLabel write FDescription;
    {:$ Specifies the prefix for auto generated constants. }
    property TypePrefix: String read FTypePrefix write FTypePrefix;
    {:$ Specifies the type of auto generated constants (Integer or String). }
    property IntConsts: Boolean read FIntConsts write FIntConsts default False;
  end;

  {:$ TEnumerations collection contains a list of TEnumeration items.}
  TEnumerations = class(TSchemaItemsCollection)
  protected
    function GetEnumeration(Index: Integer): TEnumeration;
    procedure SetEnumeration(Index: Integer; const Value: TEnumeration);
  public
    {:$ Creates and adds a new TEnumeration item to the TEnumerations collection. }
    function Add: TEnumeration;
    {:$ Locates TEnumeration item by its name within the TEnumerations collection. }
    function Find(const Name: String): TEnumeration;
    {:$ Provides access to the TEnumeration items in the collection by index. }
    property Items[Index: Integer]: TEnumeration read GetEnumeration write SetEnumeration; default;
  end;

  TDatabaseSchemaDesigner = class
  protected
    FSchema: TDatabaseSchema;
    FModulePath: String;
    FDesignerForm: TObject; // TForm;
    FOnChanged: TNotifyEvent;
  public
    constructor Create(Owner: TDatabaseSchema);
    destructor Destroy; override;

    procedure SchemaChanged; virtual;
    procedure Modified; virtual;

    property Schema: TDatabaseSchema read FSchema;
    property DesignerForm: TObject read FDesignerForm write FDesignerForm;
    property ModulePath: String read FModulePath write FModulePath;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  {:$ Represents a database schema. }
  TDatabaseSchema = class(TComponent)
  private
    FTempRelationships: TRelationships;
  protected
    { Protected declarations }
    FUpdates: TDatabaseUpdates;
    FDomains: TDomains;
    FSequences: TSequences;
    FViewDefs: TViewDefinitions;
    FStoredProcs: TStoredProcDefinitions;
    FModules: TModuleDefinitions;
    FTableDefs: TTableDefinitions;
    FRelationships: TRelationships;
    FEnumerations: TEnumerations;
    FCustomObjects: TCustomObjects;
    FVersion: TSchemaVersion;
    FSchemaName: String;
    FDescription: String;
    FPrepared: Boolean;
    FDesigner: TDatabaseSchemaDesigner;
    FOnReadError: TReaderError;
    FCustomProps: TStringList;
    FDefaultValues: TStringList;
    FNextItemID: Integer;
    FSchemaID: String;
    FTargetDB: String;
    FSysTable: String;

    FCompanyName: String;
    FLegalCopyright: String;
    FAuthor: String;
    FUpdateCounter: Integer;
    FDSDFileName: String;

    FReadLibVersion: Integer;
    FStreamContainsVersion: Boolean;
    FEncloseIdentifiersInQuotes: Boolean;

    procedure SetDSDFileName(const Value: String);
    function GetLibVersion: Integer;
    procedure SetLibVersion(const Value: Integer);
    function GetSchemaID: String;
    function StoreDomains: Boolean;
    function StoreEnumerations: Boolean;
    function StoreSequences: Boolean;
    function StoreStoredProcs: Boolean;
    function StoreTableDefs: Boolean;
    function StoreUpdates: Boolean;
    function StoreViewDefs: Boolean;
    function StoreRelationships: Boolean;
    function StoreModules: Boolean;
    function StoreCustomObjects: Boolean;

    procedure SetRelationships(const Value: TRelationships);
    procedure SetStoredProcs(const Value: TStoredProcDefinitions);
    procedure SetViewDefs(const Value: TViewDefinitions);
    procedure SetUpdates(AValue: TDatabaseUpdates);
    procedure SetTableDefs(const Value: TTableDefinitions);
    procedure SetEnumerations(const Value: TEnumerations);
    procedure SetDomains(const Value: TDomains);
    procedure SetSequences(const Value: TSequences);
    procedure SetModules(const Value: TModuleDefinitions);
    procedure SetCustomObjects(const Value: TCustomObjects);

    function  GetCustomProps: TStrings;
    procedure SetCustomProps(const Value: TStrings);
    function  GetDefaultValues: TStrings;
    procedure SetDefaultValues(const Value: TStrings);
    function  GetEmpty: boolean;

    function GetVersionLabel: String;
    procedure SetVersionLabel(const Value: String);
    procedure SetPrepared(const Value: Boolean);
    procedure DoOnReadError(Reader: TReader; const Message: String; var Handled: Boolean);

    procedure ReadSchemaID(Reader: TReader);
    procedure WriteSchemaID(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;

    procedure Loaded; override;
    procedure InternalUpdate; virtual;
    procedure UpdateSQLFieldTypes;
    procedure UpdateLibVersion; virtual;
    procedure UpdateViews; virtual;

    procedure DoSQLFieldTypeChanged(Item: TSchemaCollectionItem; const Value: String);
  public
    { Public declarations }
    {:$ Creates an instance of the TDatabaseSchema component. }
    constructor Create(AOwner: TComponent); override;
    {:$ Destroys the instance of the TDatabaseSchema component. }
    destructor Destroy; override;

    {:$ Prepare database schema for use by fixing up all references. }
    procedure Prepare;
    {:$ Unprepare database schema and relaease all references. }
    procedure UnPrepare;

    {:$ Saves schema information to stream. }
    procedure SaveToStream(Stream: TStream); virtual;
    {:$ Loads schema information from the stream. }
    procedure LoadFromStream(Stream: TStream); virtual;

    function FormatName(const Name, Fmt: String): String;

    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: Boolean;

    procedure ForEachItem(OnSchemaItem: TOnSchemaItem;
      ItemClass: TSchemaCollectionItemClass = nil; ExactMatchClass: Boolean = False; Data: Pointer = nil);
    procedure UpdateItemName(Schema: TDatabaseSchema; Item: TSchemaCollectionItem; var Abort: Boolean; Data: Pointer = nil);
    procedure DoFindItemByID(Schema: TDatabaseSchema; Item: TSchemaCollectionItem; var Abort: Boolean; Data: Pointer = nil);

    function FindItemByID(ItemID: Integer): TSchemaCollectionItem;

    {:$ Saves schema information to String. }
    function SaveToStr: String;
    {:$ Loads schema information from the String. }
    procedure LoadFromStr(Value: String);

    procedure UpdateRelationships;

    {:$ Compares two database schemes and returns items that don't match. }
    procedure Compare(Dest: TDatabaseSchema; List: TList);

    {:$ Locates table defintion by either by TableName or by the properties of }
    {:$ TDataSet component passed as the Table parameter. }
    function GetTableDef(Table: TDataSet; const TableName: String = ''): TTableDefinition; overload;
    {:$ Locates table defintion by TableName }
    function GetTableDef(const TableName: String): TTableDefinition; overload;
    {:$ Locates table definition by object type. }
    function GetObjectTable(const ObjectType: String): TTableDefinition;

    function FindIndexGlobal(const IndexName: String): TIndexDefinition;

    procedure GetTableCategories(List: TStrings);

    procedure UpdateFieldDefinitions;
    function TopLevelUnique(Item: TSchemaCollectionItem; const NewName: String): Boolean;

    {:$ Saves schema information to file. }
    procedure SaveToFile(const FileName: String);
    {:$ Loads schema information from file. }
    procedure LoadFromFile(const FileName: String);
    {:$ Loads schema information from DBS file. }
    procedure LoadFromDBSFile(const FileName: String);
    {:$ Loads schema information from DSD file. }
    procedure LoadFromDSDFile(const FileName: String);
    {:$ Updates version of the database schema from the list of updates. }
    procedure UpdateVersion; virtual;

    {:: Assigns Database Schema from a different schema component. }
    procedure Assign(Source: TPersistent); override;
    {:: Erases all information contained in the database schema. }
    procedure Clear;

    {:: Returns next unique item's ID. }
    function GetNextItemID: Integer;

    {:: Reset Schema ID by setting it to empty String. New ID will be automatically }
    {:: generated when necessary. }
    procedure ResetSchemaID;

    {:$ Returns the version of the database schema. }
    property Version: TSchemaVersion read FVersion;
    {:$ Returns the minor part of the version of the database schema. }
    property MinorVersion: SmallInt read FVersion.Minor write FVersion.Minor;
    {:$ Returns the major part of the version of the database schema. }
    property MajorVersion: SmallInt read FVersion.Major write FVersion.Major;
    {:: Whether the schema is prepared, so all references are fixed up. }
    property Prepared: Boolean read FPrepared write SetPrepared;
    {:: Do not use this property. Intended for deign-time use within IDE. }
    property Designer: TDatabaseSchemaDesigner read FDesigner;

    property OnReadError: TReaderError read FOnReadError write FOnReadError;
    property SchemaID: String read GetSchemaID write FSchemaID;
    property ReadLibVersion: Integer read FReadLibVersion;
    {:$ Returns True if database schema is empty. }
    property Empty: boolean read GetEmpty;
    {:$ Returns maximal ItemID value. }
    property MaxItemID: integer read FNextItemID;
  published
    { Published declarations }
    property TargetDB: String read FTargetDB write FTargetDB;
    {:$ Contains the list of updates for the database schema. }
    property Updates: TDatabaseUpdates read FUpdates write SetUpdates stored StoreUpdates;
    {:$ Contains the list of domain definitions for the database schema. }
    property Domains: TDomains read FDomains write SetDomains stored StoreDomains;
    {:$ Contains the list of sequence definitions for the database schema. }
    property Sequences: TSequences read FSequences write SetSequences stored StoreSequences;
    {:$ Contains the list of view definitions for the database schema. }
    property ViewDefs: TViewDefinitions read FViewDefs write SetViewDefs stored StoreViewDefs;
    {:$ Contains the list of stored procs definitions for the database schema. }
    property StoredProcs: TStoredProcDefinitions read FStoredProcs write SetStoredProcs stored StoreStoredProcs;
    {:$ Contains the list of module definitions for the database schema. }
    property Modules: TModuleDefinitions read FModules write SetModules stored StoreModules;
    {:$ Contains the list of custom objects for the database schema. }
    property CustomObjects: TCustomObjects read FCustomObjects write SetCustomObjects stored StoreCustomObjects;
    {:$ Contains the list of all relationships in the database schema. }
    property Relationships: TRelationships read FRelationships write SetRelationships stored StoreRelationships;
    {:$ Contains the list of table definitions for the database schema. }
    property TableDefs: TTableDefinitions read FTableDefs write SetTableDefs stored StoreTableDefs;
    {:$ Contains the list of enumerations for the database schema. }
    property Enumerations: TEnumerations read FEnumerations write SetEnumerations stored StoreEnumerations;
    {:$ Returns the name of the schema. }
    property SchemaName: String read FSchemaName write FSchemaName;
    {:$ Returns the version as text. }
    property VersionLabel: String read GetVersionLabel write SetVersionLabel stored False;
    {:$ Schema Description. }
    property Description: String read FDescription write FDescription;
    {:$ Author of this schema. This field is only used for documenting purposes. }
    property Author: String read FAuthor write FAuthor;
    {:$ Company, this schema belongs to. This field is only used for documenting purposes. }
    property CompanyName: String read FCompanyName write FCompanyName;
    {:$ Legal Copyright. This field is only used for documenting purposes. }
    property LegalCopyright: String read FLegalCopyright write FLegalCopyright;
    {:$ List of possible custom properties, that field definitions may have. }
    property CustomProps: TStrings read GetCustomProps write SetCustomProps;
    {:$ Version of Database Extensions Library. }
    property LibVersion: Integer read GetLibVersion write SetLibVersion;
    {:$ Name and (optionally) path of the .dsd file name containing complete schema, diagram and version history. }
    {:$ This property is design-time only. }
    property DSDFileName: String read FDSDFileName write SetDSDFileName;
    {:$ Specifies whether the identifiers must be enclosed in quotes or square brackets,  }
    {:$ depending on database engine properties. }
    property EncloseIdentifiersInQuotes: Boolean read FEncloseIdentifiersInQuotes write FEncloseIdentifiersInQuotes default True;
    {:$ List of possible custom properties, that field definitions may have. }
    property DefaultValues: TStrings read GetDefaultValues write SetDefaultValues;
    property SystemTableName: String read FSysTable write FSysTable;
  end;

  TDBSchemaVersion = class(TDatabaseSchema)
  protected
    FDate: TDateTime;
    FUpdateAdded: Boolean;
    function GetIndex: Integer;
    procedure InternalUpdate; override;
  public
    constructor CreateCheckpoint(Schema: TDatabaseSchema);
    constructor CreateDifference(Src, Dest: TDatabaseSchema);
    procedure ApplyDifference(Dest: TDatabaseSchema);
    procedure UpdateVersion; override;
    property Version: TSchemaVersion read FVersion write FVersion;
    property Index: Integer read GetIndex;
  published
    property TargetDB stored False;
    property Updates stored False;
    property CustomProps stored False;
    property SchemaName stored False;
    property MinorVersion: SmallInt read FVersion.Minor write FVersion.Minor;
    property MajorVersion: SmallInt read FVersion.Major write FVersion.Major;
    property Date: TDateTime read FDate write FDate;
    property UpdateAdded: Boolean read FUpdateAdded write FUpdateAdded;
  end;

  TSchemaVersionHistory = class (TComponent)
  protected
    FHistoryVersion: Integer;
    function GetVersionCount: Integer;
    function GetVersions(Idx: Integer): TDBSchemaVersion;
    function GetCurHistoryVersion: Integer; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetChildOwner: TComponent; override;

    function AddVersionCheckpoint(Schema: TDatabaseSchema): TDBSchemaVersion;
    procedure AddVersion(NewVersion, Difference: TDBSchemaVersion);
    function GetCheckpoint: TDBSchemaVersion;
    function CreateSnapshot(Version: TDBSchemaVersion; Snapshot: TDatabaseSchema = nil): TDatabaseSchema;
    function GetLastAddedCheckpoint(ASchema: TDatabaseSchema): TDatabaseSchema;

    property Versions[Idx: Integer]: TDBSchemaVersion read GetVersions; default;
    property VersionCount: Integer read GetVersionCount;
    property ReadHistoryVersion: Integer read FHistoryVersion;
  published
    property HistoryVersion: Integer read GetCurHistoryVersion write FHistoryVersion;
  end;

  {:$ Represents a reference to a dataset of a certain structure. }
  {:: TDataSetReference can be inherited by declaring properties of }
  {:: type TField. This will allow to construct references for the }
  {:: business classes, that can be used in business logic procedures. }
  {:: See code Examples for more details on how to use this type of objects. }
  TDataSetReference = class (TPersistent)
  protected
    FDataSet: TDataSet;
    FOwnDataSet: Boolean;
    procedure UpdateField(PropInfo: PPropInfo; Data: Pointer);
    procedure CreatePersistentField(PropInfo: PPropInfo; Data: Pointer);
  public
    {:$ Creates an instance of TDataSetReference object. }
    constructor Create(DataSet: TDataSet; OwnDataSet: Boolean = False);
    {:$ Destroys the instance of TDataSetReference object. }
    destructor Destroy; override;
    {:$ Assigns a data set to the instance of TDataSetReference object. }
    procedure AssignDataSet(DataSet: TDataSet);
    {:$ Invokes OnField procedure for each field-type property, declared in TDataSetReference object. }
    procedure IterateFields(OnField: TOnFieldProc; Data: Pointer);
    {:$ Maps properties of the TDataSetReference object to the fields in the underlying data set. }
    procedure UpdateFields;
    {:$ Creates persistent fields in the underlying data set, corresponding to the field-type }
    {:$ properties declared in the TDataSetReference object. }
    procedure CreatePersistentFields;
    {:$ Refreshes the underlying data set. Equivalent of TDataSetReference.DataSet.Refresh. }
    procedure Refresh;
    {:$ Provides direct access to the unrelying data set. }
    property DataSet: TDataSet read FDataSet write AssignDataSet;
    {:$ Specifies whether the reference object owns the underlying data set. }
    {:: If OwnDataSet is set to True, then the DataSet will be destroyed together }
    {:: with the reference object or when nil is assigned to the DataSet property. }
    property OwnDataSet: Boolean read FOwnDataSet write FOwnDataSet;
  end;

  {:$ Represents a database cursor for a range of rows. }
  TDBRangeCursor = class
  public
    DataSet: TDataSet;
    KeyFieldsList: TList;
    KeyValues: Variant;

    procedure UpdateFrom(SrcCursor: TDBRangeCursor); virtual;
    procedure SetSuppressAutoIncValues(Value: Boolean); virtual;
  end;

  {:$ Represents a status of a changed object. }
  TChangeStatus = (csActive, csInactive, csConfirm, csConfirmed);

  {:$ Represents a list of operations that could be performed during update procedure. }
  TUpdateOption = (uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking);
  {:$ Represents a set of operations that could be performed during update procedure. }
  TUpdateOptions = set of TUpdateOption;

  {:$ Reference to the Objects table. }
  TObjectsTable = class (TDataSetReference)
  private
    FObjectData: TBlobField;
    FTimestamp: TDateTimeField;
    FReplicationID: TIntegerField;
    FChangeStatus: TIntegerField;
    FChangeType: TIntegerField;
    FSnapshotID: TIntegerField;
    FObjectKey: TStringField;
    FUserName: TStringField;
    FObjectType: TStringField;
  published
    property ObjectType: TStringField read FObjectType write FObjectType;
    property ObjectKey: TStringField read FObjectKey write FObjectKey;
    property ReplicationID: TIntegerField read FReplicationID write FReplicationID;
    property SnapshotID: TIntegerField read FSnapshotID write FSnapshotID;
    property Timestamp: TDateTimeField read FTimestamp write FTimestamp;
    property ChangeType: TIntegerField read FChangeType write FChangeType;
    property ChangeStatus: TIntegerField read FChangeStatus write FChangeStatus;
    property UserName: TStringField read FUserName write FUserName;
    property ObjectData: TBlobField read FObjectData write FObjectData;
  end;

  {:$ Reference to the System table. }
  TSystemTable = class (TDataSetReference)
  private
    FSchema: TBlobField;
    FMajorVersion: TIntegerField;
    FReplicationID: TIntegerField;
    FMinorVersion: TIntegerField;
    FSnapshotID: TIntegerField;
    FSchemaName: TStringField;
  published
    property SchemaName: TStringField read FSchemaName write FSchemaName;
    property ReplicationID: TIntegerField read FReplicationID write FReplicationID;
    property SnapshotID: TIntegerField read FSnapshotID write FSnapshotID;
    property MinorVersion: TIntegerField read FMinorVersion write FMinorVersion;
    property MajorVersion: TIntegerField read FMajorVersion write FMajorVersion;
    property Schema: TBlobField read FSchema write FSchema;
  end;

  {:$ TObjectState objects contain information about objects being }
  {:$ modified within a transaction. }
  TObjectState = class
  public
    ObjectType: String;
    ObjectStrKey: String;
    ObjectKey: Variant;
    ChangeType: TChangeType;
    ChangeStatus: TChangeStatus;
    ReplicationID: Integer;
    SnapshotID: Integer;
    UserName: String;
    Timestamp: TDateTime;
  end;

  {:$ TActiveTransaction object contains information about current transaction. }
  {:: TActiveTransaction is used internall by TxxxDatabaseExt and generally you }
  {:: never need to instantiate objects of this class. }
  TActiveTransaction = class
  public
    ChangedObjects: TList;
    SnapshotID: Integer;
    ReplicationID: Integer;
    Timestamp: TDateTime;
    Aggregates: TObjectList;

    constructor Create;
    destructor Destroy; override;

    function WriteChange(TableDef: TTableDefinition; ObjectKey: Variant;
      AChangeType: TChangeType; SnapshotID: Integer = -1; const UserName: String = '';
      ChangeStatus: TChangeStatus = csActive): Boolean;

    function GetAggregateLinkData(AggregateLink: TAggregateLink): TAggregateLinkData;
  end;

  ISchemaDatabase = interface (ICtxDatabase)
    ['{AF1A1DD6-0EC9-4CDB-A598-51C82F83A92E}']
    procedure ReverseEngineer;
    function GetSchema: TDatabaseSchema;
    procedure SetSchema(Value: TDatabaseSchema);

    function GetRangeCursor(const TableName, KeyFields, TableFilter: String;
      CaseInsensitive: Boolean; KeyValues: Variant; const ExtraKeyFields: String = ''): TDBRangeCursor; overload;
    function GetRangeCursor(Relation: TRelation; KeyValues: Variant): TDBRangeCursor; overload;

    function GetVersion: TSchemaVersion;
    procedure SetVersion(const Value: TSchemaVersion);
    function GetSystemTableName: String;

    property Schema: TDatabaseSchema read GetSchema write SetSchema;
  end;

  {:$ Creates schema version record from major and minor parts. }
  function SchemaVersion(MajorVersion, MinorVersion: Integer): TSchemaVersion;
  {:$ Compares two version records. }
  function CompareVersions(Version1, Version2: TSchemaVersion): Integer;
  {:$ Returns true if CurrentVersion is less or equal to Version. }
  function VersionIsOkey(CurrentVersion, Version: TSchemaVersion; AllowNewer: Boolean = False): Boolean;
  {:$ Converts version record to its String representation (e.g. 1.43) }
  function VersionToStr(const Version: TSchemaVersion): String;
  {:$ Converts String representation to version record. }
  function StrToVersion(const Value: String): TSchemaVersion;

  function GetDatabaseVersion(Database: ISchemaDatabase): TSchemaVersion;
  procedure SetDatabaseVersion(Database: ISchemaDatabase; Value: TSchemaVersion);

  {:$ Determines whether SubStr is listed in Str. Assumes, that Str is }
  {:$ delimited by Delim. Ex.:<br> }
  {:$   ListedIn('Name', 'Name;LastName', ';') = True <br>}
  {:$   ListedIn('Last', 'Name;LastName', ';') = False }
  function ListedIn(SubStr, Str, Delim: String): Boolean;

  function AnsiDateToStr(Value: TDateTime): String;
  function AnsiTimeToStr(Value: TDateTime; MilitaryTime: Boolean): String;
  function AnsiDateTimeToStr(Value: TDateTime; MilitaryTime: Boolean): String;

  function FormatSQLConst(FieldType: TFieldType; Value: Variant): String;
  function CreateRangeFilter(KeyFieldsList: TList; KeyValues: Variant): String;

  procedure DecodeDatabaseURL(const DatabaseURL: String; var ConnectionType, RemoteHost, DatabaseName: String);
  function EncodeDatabaseURL(ConnectionType, RemoteHost, DatabaseName: String): String;
  function ContainsParam(const Name: String; ParamList: TStrings): Boolean;

  {:$ Returns next token within Str starting from StartPos (1-based), using Divider to separate tokens. }
  function NextToken(const Str, Delimiter: String; var StartPos: Integer): String;
  {:$ Concatenates an array of strings separating them with the Delimiter. }
  function Concatenate(Strings: array of String; const Delimiter: String): String; overload;
  {:$ Concatenates an array of strings separating them with the Delimiter. }
  function Concatenate(Strings: TStrings; const Delimiter: String): String; overload;
  {:$ Get list of token contained in Str and separated by Delimiter. }
  procedure GetTokens(const Str, Delimiter: String; List: TStrings);
  {:$ Replaces field name passed in OldName parameter withing Fields String by }
  {:$ NewName and returns resulted String. }
  function ReplaceFieldName(const Fields, OldName, NewName: String): String;
  {:$ Extracts object name from pairs of quote. Object name may follow "schema" name. }
  {:$ E.g. given [schema]."table name" the output will be 'table name'. }
  function ExtractObjectName(const Value: String): String; overload;
  function ExtractObjectName(const Value: String; var LeftPart: String): String; overload;

  {:$ Converts any variant value to String representation. See also DecodeVariant. }
  function EncodeVariant(Value: Variant): String;
  {:$ Decodes variant value from its String representation. See also EncodeVariant.}
  function DecodeVariant(Value: String): Variant;
  {:$ Returns true is V is NULL or empty or is array with all items null or empty.  }
  function VarIsNullExt(V: Variant): Boolean;

  {:$ Destroys objects contained in TList array. }
  procedure FreeObjects(List: TList); overload;
  {:$ Destroys objects contained in Objects array of TStrings. }
  procedure FreeObjects(List: TStrings); overload;

  {:$ Internal helper function. }
  function GetVirtualKey(const TableName, KeyFields: String; KeyValues: Variant; CaseInsensitive: Boolean = False): String;
  {:$ Internal helper function. }
  function FindVirtualKey(VirtualKeys: TStrings; const Key: String): Integer;
  {:$ Internal helper function. }
  function VirtualKeyExists(VirtualKeys: TStrings; const Key: String): Boolean;

  {:$ Return key values corresponding to the list of KeyFields. }
  function GetKeyValues(DataSet: TDataSet; const KeyFields: String; var OldKeyValues, NewKeyValues: Variant; KeyFieldsList: TList = nil): Boolean;
  {:$ Clear fields contained in KeyFieldsList array. }
  procedure ClearFields(DataSet: TDataSet; KeyFieldsList: TList);
  {:$ Assign KeyValues to fields contained in KeyFieldsList array. }
  {:! This method calls Edit and Post methods of DataSet to put it into editable mode. }
  procedure AssignFields(DataSet: TDataSet; KeyFieldsList: TList; KeyValues: Variant);
  {:$ Assign KeyValues to fields contained in KeyFieldsList array. }
  {:! This method assumes the dataset containing key felds is already in editable mode. }
  procedure AssignKeyFields(KeyFieldsList: TList; KeyValues: Variant);
  {:$ Copy record from source to destination dataset. }
  procedure CopyRecord(SrcTable, DestTable: TDataSet);
  {:$ Assign current record from source to destination dataset. }
  procedure AssignRecord(SrcTable, DestTable: TDataSet; ClearBeforeAssign: Boolean = False);
  {:$ Assign Master field value passed as Value parameter to all records of the DataSet. }
  procedure AssignMasterField(const DataSet: TDataSet; const MasterField: String; Value: Variant);
  {:$ Remove all records from the dataset. }
  procedure ClearDataSet(DataSet: TDataSet);
  {:$ Copy source (Src) dataset to destination (Dest) dataset. }
  procedure CopyDataSet(Src, Dest: TDataSet);
  {:$ Copy source (Src) dataset to destination (Dest) dataset. }
  procedure CopyDataSetWithKey(Src, Dest: TDataSet; const KeyField: String);

  {:$ Saves dataset into variant array. Stores field defs as a first 3 rows (Name, Type, Size). }
  function DataSetToVariant(DataSet: TDataSet; MaxRecCount: Integer = -1): OleVariant;

  {:$ Converts field class to physical data type. }
  function FieldClassToDataType(FieldClass: TFieldClass): TFieldType;
  {:$ Creates field for the Owner dataset. }
  function CreateField(Owner: TDataSet; FieldClass: TFieldClass; const FieldName: String; DataType: TFieldType = DB.ftUnknown; Size: Integer = 0; Required: Boolean = False): TField;

  {:$ Returnes component, implementing ISchemaDatabase associated with the database name passed as DatabaseName parameter. }
  function FindDatabase(const DatabaseName: String): TComponent;

  {:$ Parses input String (InpStr) and invokes OnParseField callback procedure }
  {:$ for each "field" starting from Open substring and ending by Close substring. }
  {:$ Example:  ParseFields('This <%text%> contains two <%fields%>.', '<%', '%>', OnParseField); }
  function ParseFields(const InpStr, Open, Close: String; OnParseField: TOnParseField; Data: Pointer): String;
  function ReverseString(const Str: String): String;

  {:$ Retrives the list of values of a String property which name is passed }
  {:$ as PropName parameter from all collection items and returns it in the Values parameter. }
  procedure GetValues(Collection: TCollection; Values: TStrings; PropName: String = 'Name'; Append: boolean = False); overload;

  {:$ Retrives the list of values of a String property which name is passed }
  {:$ as PropName parameter from all list items and returns it in the Values parameter. }
  procedure GetValues(List: TList; Values: TStrings; PropName: String = 'Name'; Append: boolean = False); overload;

  {:$ Retrives the list of *distinct* values of a String property which name is passed }
  {:$ as PropName parameter from all collection or list items and returns it in the Values parameter. }
  procedure GetDistinctValues(List: TObject; Values: TStrings; PropName: String = 'Name'); overload;

  {:$ Returns true if the NewValue is unique value for the published property named PropName }
  {:$ amongst the items of the collection owning CollectionItem }
  function IsUnique(CollectionItem: TCollectionItem; const NewValue: String; const PropName: String = 'Name'): Boolean;

  procedure AddPublishedProp(Instance: TObject; PropName: String; Values: TStrings);
  function ValueFromIndex(Strings: TStrings; Idx: Integer): String;
  procedure SaveStrToFile(const FileName, Value: String);
  function LoadStrFromFile(const FileName: String): String;
  function VarArrayToStr(const V: OleVariant): String;
  function StrToVarArray(const S: String): OleVariant;

  procedure GetFieldTypes(List: TStrings);
  function IsValidFieldType(const FieldType: String): Boolean;
  function GetFieldTypeByName(const Name: String): TFieldDataType;
  procedure GetPropNames(Instance: TObject; List: TStrings);

  function GetItemCollection(Parent: TObject; ItemClass: TSchemaCollectionItemClass): TSchemaItemsCollection;

  procedure GenerateSchemaDeclarations(Schema: TDatabaseSchema; Strings: TStrings);
  function NameToIdent(const TypePrefix, Name: String): String;
  function KeyFieldsRequired(TableDef: TTableDefinition; KeyFields: String): Boolean;

  function CompareRelProps(SrcRel, DestRel: TRelationship): Boolean;
  function RelationshipsEqual(Src, Dest: TRelationship): Boolean;
  function CheckVersion(Database: ISchemaDatabase): Boolean;

  function UpdateDatabase(Database: ISchemaDatabase; OnProgress: TDatabaseProgress = nil): Boolean;
  function FormatName(const Name, Fmt: String): String;

  function ValidObj(Obj: TSchemaCollectionItem): Boolean;

const
  ANSI_DATE_SEPARATOR = '-';
  ANSI_TIME_SEPARATOR = ':';
  ANSI_12HOUR_AM = 'AM';
  ANSI_12HOUR_PM = 'PM';
  ANSI_DATE_FORMAT = 'yyyy-mm-dd';
  ANSI_TIME_FORMAT = 'hh:mm:ss.zzz ampm';
  ANSI_DECIMAL_SEPARATOR = '.';

  sqlCreateSystemTable =
    'CREATE TABLE %s (VERINFO CHAR(20))';
  sqlInitSystemTable =
    'INSERT INTO %s(VERINFO) VALUES (''%s'')';
  sqlSetVersion =
    'UPDATE %s SET VERINFO = ''%s''';
  sqlGetVersion =
    'SELECT VERINFO FROM %s';

  defSysTableName = 'SysTable';

resourcestring
  SUnsupportedDataType = 'Unsupported data type';
  SCapabilityNotSupported = 'Capability not supported';
  SDatabaseSchemaIsNotAssigned = 'Database Schema is not assigned';
  SUpdatingDatabaseVersion = 'Updating Database Structure...';
  SDone = 'Done.';

  SDefaultDeleteErrorMessage = 'Record in the %s table cannot be deleted, because it already has related records in the %s table. Please delete those records prior to deleting the master record.';
  SDefaultUpdateErrorMessage = 'Field %s in the %s table cannot be updated, because it already references records in the %s table.';
  SDefaultRequireRecordErrorMessage = 'Field %s in the %s table does not contain a valid or null reference to the %s table.';

  SInvalidVariantString = 'Invalid variant String';
  SFieldTypeNotSupported = 'Field type not supported. Field: %s';
  SDataSetRefFieldNotFound = 'Field %s not found in DataSet %s';

  SEmptyObjectName = 'Name of a %s must not be empty';
  SDuplicateObjectName = 'Duplicate name for %s object: %s';

  SObjectTypeMustBeUnique = 'Object type must be unique: %s';

  SFieldNotFound = 'Field "%s" not found in table "%s"';

  SEmptyIndexName = 'Index name cannot be empty except for primary indexes';
  SNoFieldsDefined = 'Index must have at least one field defined';

  SDocumentInactive = 'Document must be active to perform this operation.';
  SDocumentTypeNotFound = 'Document type not found: %s';
  SDocumentTypeMustBeUnique = 'Document type %s has already been registered.';
  SDatabaseNotFound = 'Database not found: %s';
  SSchemaIsNotAssigned = 'Schema is not assigned to %s';
  SDatabaseIsNotAssigned = 'Database is not assigned. DocumentID: %s';

  SInvalidFieldType = 'Invalid field type: %s';

  SInvalidRelation = 'Internal error: attempt to access invalid relation. Table: %s; Relation: %s';

  SUnableToAssignDSDFileName = 'Unable to assign DSDFileName property while the designer is active';

  SOneToOne = 'one to one';
  SManyToOne = 'many to one';
  SOneToMany = 'one to many';
  SManyToMany = 'many to many';


const
  VERSION_NOT_AVAILABLE: String = 'N/A';

  propTableName         = 'TableName';
  propIndexFieldNames   = 'IndexFieldNames';
  propIndexName         = 'IndexName';
  propName              = 'Name';

  fldDocumentID         = 'DocumentID'; // Default field name for document's ID
  fldEntryNo            = 'EntryNo';    // Default field name for journal's Entry No field
  fldItemNo             = 'ItemNo';     // Default field name for document item's Item No field
  fldItemID             = 'ItemID';     // Default field name for document item's ItemID field

const
  OppositeRelationType: array [TRelationType] of TRelationType = (
    rtOneToOne, rtManyToOne, rtOneToMany, rtManyToMany);
  OppositeRelationKind: array [TRelationKind] of TRelationKind = (
    rkReference, rkChildren, rkParent);

  RelationTypeText: array [TRelationType] of String = (
    SOneToOne, SManyToOne, SOneToMany, SManyToMany);

  uoNone: TUpdateOptions = [];
  uoAll: TUpdateOptions = [uoEnableErrorConstraints, uoEnableCascadeConstraints, uoEnableAggregates, uoEnableTriggers, uoEnableChangeTracking];

  Undefined: TSchemaVersion = (Major: 0; Minor: 0);

  setStringFieldTypes = [ftString, ftBytes, ftVarBytes, ftFixedChar, ftWideString, ftArray, ftNChar];

  setCannotBeIndexed = [
    ftUnknown, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle,
    ftDBaseOle, ftTypedBinary, ftCursor, ftADT, ftArray, ftReference,
    ftDataSet, ftOraBlob, ftOraClob, ftInterface, ftIDispatch, ftNClob,
    ftRecRev];

const
  DefaultSQLFieldTypes: array [TFieldDataType] of String = (
    'UNKNOWN', 'VARCHAR', 'SMALLINT', 'INTEGER', 'WORD', 'BOOLEAN',
    'FLOAT', 'CURRENCY', 'BCD', 'DATE', 'TIME', 'DATETIME', 'BYTES',
    'VARBYTES', 'AUTOINC', 'BLOB', 'MEMO', 'GRAPHIC', 'FMTMEMO', 'PARADOXOLE',
    'DBASEOLE', 'TYPEDBINARY', 'CURSOR', 'CHAR', 'NVARCHAR', 'LARGEINT',
    'ADT', 'ARRAY', 'REFERENCE', 'DATASET', 'ORABLOB', 'ORACLOB', 'VARIANT',
    'INTERFACE', 'IDISPATCH', 'GUID', 'TIMESTAMP', 'FMTBCD', 'NCHAR', 'NCLOB',
    'RECREV'
  );
  FieldDataTypes: array [TFieldDataType] of String = (
    'ftUnknown', 'ftString', 'ftSmallint', 'ftInteger', 'ftWord', 'ftBoolean',
    'ftFloat', 'ftCurrency', 'ftBCD', 'ftDate', 'ftTime', 'ftDateTime', 'ftBytes',
    'ftVarBytes', 'ftAutoInc', 'ftBlob', 'ftMemo', 'ftGraphic', 'ftFmtMemo', 'ftParadoxOle',
    'ftDBaseOle', 'ftTypedBinary', 'ftCursor', 'ftFixedChar', 'ftWideString', 'ftLargeint',
    'ftADT', 'ftArray', 'ftReference', 'ftDataSet', 'ftOraBlob', 'ftOraClob', 'ftVariant',
    'ftInterface', 'ftIDispatch', 'ftGuid', 'ftTimeStamp', 'ftFMTBcd', 'ftNChar', 'ftNClob',
    'ftRecRev');

  VCLToFieldDataType: array [TFieldType] of TFieldDataType = (
    ftUnknown, ftString, ftSmallint, ftInteger, ftWord, ftBoolean,
    ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, ftBytes,
    ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle,
    ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString, ftLargeint,
    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, ftVariant,
    ftInterface, ftIDispatch, ftGuid
    {$IFDEF D6_ORLATER},ftTimeStamp, ftFMTBcd{$ENDIF}
    {$IFDEF D2006_ORLATER}, ftNChar, ftNClob, ftTimeStamp, ftUnknown{$ENDIF}
    {$IFDEF D2009_ORLATER}, ftLargeInt, ftInteger, ftSmallInt, ftFloat, ftUnknown, ftUnknown, ftUnknown{$ENDIF}
  );

  FieldDataTypeToVCL: array [TFieldDataType] of TFieldType = (
    DB.ftUnknown, DB.ftString, DB.ftSmallint, DB.ftInteger, DB.ftWord,
    DB.ftBoolean, DB.ftFloat, DB.ftCurrency, DB.ftBCD, DB.ftDate, DB.ftTime, DB.ftDateTime, DB.ftBytes,
    DB.ftVarBytes, DB.ftAutoInc, DB.ftBlob, DB.ftMemo, DB.ftGraphic, DB.ftFmtMemo, DB.ftParadoxOle,
    DB.ftDBaseOle, DB.ftTypedBinary, DB.ftCursor, DB.ftFixedChar, DB.ftWideString, DB.ftLargeint,
    DB.ftADT, DB.ftArray, DB.ftReference, DB.ftDataSet, DB.ftOraBlob, DB.ftOraClob, DB.ftVariant,
    DB.ftInterface, DB.ftIDispatch, DB.ftGuid,
    {$IFnDEF VER130}
    DB.ftTimeStamp, DB.ftFMTBcd,
    {$ELSE}
    DB.ftDateTime, DB.ftBcd,
    {$ENDIF}
    {$IFDEF VER180}
    DB.ftFixedWideChar, DB.ftWideMemo,
    {$ELSE}
    DB.ftWideString, DB.ftMemo,
    {$ENDIF}
    DB.ftInteger
  );

const
  OppositeSide: array [TRelationSide] of TRelationSide = (sideMaster, sideDetail);
  SMultiplicity: array [TMultiplicity] of String = ('*', '1', '0..1', '0..N');

var
  GlobalSQLFieldTypeChanged: TOnSQLFieldTypeChanged;

implementation

uses
{$IFDEF VER130}
  ActiveX,
{$ENDIF}
  DbConsts, Math, dbSQLLexer, StrUtils;

{$I CtxD2009.inc}

{ Routines }

const
  cSignValidObj = $0A0B0C0D;  


function ValidObj(Obj: TSchemaCollectionItem): boolean;
begin
  Result := Obj <> nil;
  if Result then
    Result := Obj.FSign = cSignValidObj;
end;

{$IFDEF VER130}
function CreateGUID(out Guid: TGUID): HResult;
begin
  Result := CoCreateGuid(Guid);
end;

function GUIDToString(const GUID: TGUID): String;
var
  P: PWideChar;
begin
  if not Succeeded(StringFromCLSID(GUID, P)) then
    raise Exception.Create('Invalid GUID');
  Result := P;
  CoTaskMemFree(P);
end;
{$ENDIF}

function SchemaVersion(MajorVersion, MinorVersion: Integer): TSchemaVersion;
begin
  Result.Major := MajorVersion;
  Result.Minor := MinorVersion;
end;

function CompareVersions(Version1, Version2: TSchemaVersion): Integer;
begin
  if Version1.Major > Version2.Major then
    Result := 1
  else if Version1.Major < Version2.Major then
    Result := -1
  else
    if Version1.Minor > Version2.Minor then
      Result := 1
    else if Version1.Minor < Version2.Minor then
      Result := -1
    else Result := 0;
end;

function VersionIsOkey(CurrentVersion, Version: TSchemaVersion; AllowNewer: Boolean = False): Boolean;
begin
  if CurrentVersion.Major > Version.Major then
    Result := False
  else if CurrentVersion.Major < Version.Major then
    Result := AllowNewer
  else
    if CurrentVersion.Minor > Version.Minor then
      Result := False
    else if CurrentVersion.Minor < Version.Minor then
      Result := AllowNewer
    else Result := True;
end;

function VersionToStr(const Version: TSchemaVersion): String;
begin
  if (Version.Major > 0) and (Version.Minor > 0) then
    Result := Format('%d.%2.2d', [Version.Major, Version.Minor])
  else Result := VERSION_NOT_AVAILABLE;
end;

function StrToVersion(const Value: String): TSchemaVersion;
var
  P: Integer;
  TempValue: String;
begin
  TempValue := Trim(Value);
  if (TempValue = '') or (TempValue = VERSION_NOT_AVAILABLE) then begin
    Result := SchemaVersion(-1, -1);
    exit;
  end;
  P := Pos('.', TempValue);
  if P <= 0 then
    P := Length(TempValue) + 1;

  Result.Major := StrToIntDef(copy(TempValue, 1, P-1), -1);
  Result.Minor := StrToIntDef(copy(TempValue, P+1, Length(TempValue)), 0);
end;

function GetDatabaseVersion(Database: ISchemaDatabase): TSchemaVersion;
var
  Query: TDataSet;
  STable: String;
begin
  Result := SchemaVersion(0, 0);
  STable := '';
  if Database.GetSchema <> nil then
    STable := Database.GetSchema.SystemTableName;
  if Trim(STable) = '' then
    STable := Database.GetSystemTableName;
  if Trim(STable) = '' then
    STable := defSysTableName;

  Query := Database.CreateQuery('');
  try
    Database.SetQuerySQL(Query, Format(sqlGetVersion, [STable]));
    Query.Active := True;
    Result := StrToVersion(Query.Fields[0].AsString);
  except
  end;
  Query.Free;
end;

procedure SetDatabaseVersion(Database: ISchemaDatabase; Value: TSchemaVersion);
var
  Query: TDataSet;
  STable: String;
begin
  STable := '';
  if Database.GetSchema <> nil then
    STable := Database.GetSchema.SystemTableName;
  if Trim(STable) = '' then
    STable := Database.GetSystemTableName;
  Query := Database.CreateQuery('');
  try
    try
      Database.SetQuerySQL(Query, Format(sqlSetVersion, [STable, VersionToStr(Value)]));
      Database.ExecSQL(Query);
    except
      // Attempt to create system table
      Database.SetQuerySQL(Query, Format(sqlCreateSystemTable, [STable]));
      Database.ExecSQL(Query);
      Database.SetQuerySQL(Query, Format(sqlInitSystemTable, [STable, VersionToStr(Value)]));
      Database.ExecSQL(Query);
    end;
  finally
    Query.Free;
  end;
end;

function ListedIn(SubStr, Str, Delim: String): Boolean;
begin
  Result := (SubStr <> '') and (Str <> '')
    and (AnsiPos(Delim+SubStr+Delim, Delim+Str+Delim) > 0);
end;

function PadLeft(const Value: String; Padding: Integer; PadChar: Char): String;
var
  NumToPad: Integer;
  I: Integer;
begin
  Result := Value;
  if Length(Result) >= Padding then
    exit;
  NumToPad := Padding - Length(Result);
  for I := 1 to NumToPad do
    Result := PadChar + Result;
end;

function AnsiDateToStr(Value: TDateTime): String;
var
  TempYear: Word;
  TempMonth: Word;
  TempDay: Word;
begin
  DecodeDate(Value,TempYear,TempMonth,TempDay);
  Result :=
    PadLeft(IntToStr(TempYear),4,'0')+ANSI_DATE_SEPARATOR+
    PadLeft(IntToStr(TempMonth),2,'0')+ANSI_DATE_SEPARATOR+
    PadLeft(IntToStr(TempDay),2,'0');
end;

function AnsiTimeToStr(Value: TDateTime; MilitaryTime: Boolean): String;
var
   TempHour: Word;
   TempMin: Word;
   TempSec: Word;
   TempMSec: Word;
   IsPM: Boolean;
begin
   DecodeTime(Value,TempHour,TempMin,TempSec,TempMSec);
   IsPM:=False;
   if (not MilitaryTime) then
      begin
      IsPM:=(TempHour >= 12);
      if IsPM then
         TempHour:=(TempHour-12);
      if (TempHour=0) then
         TempHour:=12;
      end;
   Result:=PadLeft(IntToStr(TempHour),2,'0')+ANSI_TIME_SEPARATOR+
           PadLeft(IntToStr(TempMin),2,'0');
   if (TempSec <> 0) then
      begin
      Result:=Result+ANSI_TIME_SEPARATOR+PadLeft(IntToStr(TempSec),2,'0');
      if (TempMSec <> 0) then
         Result:=Result+ANSI_DECIMAL_SEPARATOR+IntToStr(TempMSec);
      end;
   if (not MilitaryTime) then
      begin
      if IsPM then
         Result:=Result+' '+ANSI_12HOUR_PM
      else
         Result:=Result+' '+ANSI_12HOUR_AM;
      end;
end;

function AnsiDateTimeToStr(Value: TDateTime; MilitaryTime: Boolean): String;
begin
   Result:=AnsiDateToStr(Value)+' '+
           AnsiTimeToStr(Value,MilitaryTime);
end;

function FormatSQLConst(FieldType: TFieldType; Value: Variant): String;
begin
  case FieldType of
    DB.ftString, DB.ftFixedChar, DB.ftWideString, DB.ftMemo:
                Result := AnsiQuotedStr(VarToStr(Value), '''');
    DB.ftAutoInc, DB.ftLargeint, DB.ftSmallint, DB.ftInteger, DB.ftWord,
    DB.ftFloat, DB.ftCurrency, DB.ftBCD:
                Result := VarToStr(Value);
    DB.ftBoolean:  Result := BooleanIdents[Boolean(Value)];
    DB.ftDate:     Result := AnsiQuotedStr(AnsiDateToStr(VarToDateTime(Value)), '''');
    DB.ftTime:     Result := AnsiQuotedStr(AnsiTimeToStr(VarToDateTime(Value), True), '''');
    DB.ftDateTime: Result := AnsiQuotedStr(AnsiDateTimeToStr(VarToDateTime(Value), True), '''');
    DB.ftGuid:     Result := AnsiQuotedStr(VarToStr(Value), '''');
    else           DatabaseError(SUnsupportedDataType);
  end;
end;

function CreateRangeFilter(KeyFieldsList: TList; KeyValues: Variant): String;
var
  I: Integer;
begin
  if KeyFieldsList.Count = 1 then
    with TField(KeyFieldsList.First) do
      Result := FieldName + ' = ' + FormatSQLConst(DataType, KeyValues)
  else begin
    Result := '';
    for I := 0 to KeyFieldsList.Count - 1 do
    with TField(KeyFieldsList[I]) do
    begin
      if Result <> '' then
        Result := Result + ' and ';
      Result := Result + Format('(%s = %s)', [FieldName, FormatSQLConst(DataType, KeyValues[I])]);
    end;
  end;
end;

procedure DecodeDatabaseURL(const DatabaseURL: String; var ConnectionType, RemoteHost, DatabaseName: String);
var
  P: Integer;
begin
  P := Pos(':\\', DatabaseURL);
  if P < 1 then begin
    // local
    ConnectionType := '';
    RemoteHost := '';
    DatabaseName := DatabaseURL;
  end else begin
    // remote lan or wan
    ConnectionType := copy(DatabaseURL, 1, P-1);
    RemoteHost := copy(DatabaseURL, P+3, Length(DatabaseURL));
    P := Pos('\', RemoteHost);
    DatabaseName := copy(RemoteHost, P+1, Length(RemoteHost));
    RemoteHost := copy(RemoteHost, 1, P-1);
  end;
end;

function EncodeDatabaseURL(ConnectionType, RemoteHost, DatabaseName: String): String;
begin
  Result := '';
  if ConnectionType <> '' then
    Result := Result + ConnectionType + ':\\' + RemoteHost + '\';
  Result := Result + DatabaseName;
end;

function ContainsParam(const Name: String; ParamList: TStrings): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to ParamList.Count - 1 do
    if AnsiSameText(ParamList.Names[I], Name) then exit;
  Result := False;
end;

function NextToken(const Str, Delimiter: String; var StartPos: Integer): String;
var
  _pos: PChar;
begin
  Result := '';
  if StartPos > Length(Str) then exit;
  _pos := StrPos(@Str[StartPos], PChar(Delimiter));
  if _pos <> nil then
  begin
    Result := copy(Str, StartPos, Integer(_pos - @Str[StartPos]));
    StartPos := StartPos + Integer(_pos - @Str[StartPos]) + Length(Delimiter);
  end else begin
    Result := copy(Str, StartPos, Length(Str));
    StartPos := Length(Str) + 1;
  end;
end;

function Concatenate(Strings: array of String; const Delimiter: String): String;
var
  I: Integer;
begin
  I := Low(Strings);
  Result := '';
  { Look for first non empty word }
  while (I <= High(Strings)) and (Strings[I] = '') do
    Inc(I);
  if I <= High(Strings) then
    Result := Strings[I]
  else exit;
  Inc(I);

  while True do
  begin
    { Look for next non empty word }
    while (I <= High(Strings)) and (Strings[I] = '') do
      Inc(I);
    if I <= High(Strings) then
      Result := Result + Delimiter + Strings[I]
    else exit;
    Inc(I);
  end;
end;

function Concatenate(Strings: TStrings; const Delimiter: String): String;
var
  I: Integer;
begin
  Result := '';
  if Strings = nil then exit;
  I := 0;
  { Look for first non empty word }
  while (I < Strings.Count) and (Strings[I] = '') do
    Inc(I);
  if I < Strings.Count then
    Result := Strings[I]
  else exit;
  Inc(I);

  while True do
  begin
    { Look for next non empty word }
    while (I < Strings.Count) and (Strings[I] = '') do
      Inc(I);
    if I < Strings.Count then
      Result := Result + Delimiter + Strings[I]
    else exit;
    Inc(I);
  end;
end;

procedure GetTokens(const Str, Delimiter: String; List: TStrings);
var
  P: Integer;
  S: String;
begin
  List.Clear;
  P := 1;
  while P <= Length(Str) do
  begin
    S := NextToken(Str, Delimiter, P);
    if S <> '' then List.Add(S);
  end;
end;

function ReplaceFieldName(const Fields, OldName, NewName: String): String;
var
  I: Integer;
  S: String;
begin
  Result := Fields;
  if (OldName = '') or (Length(Fields) < Length(OldName)) then exit;

  // Speed things up - if Fields = OldName which is quite often
  if (Length(Fields) = Length(OldName)) and AnsiSameText(Fields, OldName) then
  begin
    Result := NewName;
    exit;
  end;

  // We suspect there could be many fields
  Result := '';
  I := 1;
  while I <= Length(Fields) do
  begin
    S := NextToken(Fields, ';', I);
    if AnsiSameText(S, OldName) then
      S := NewName;
    if Result <> '' then
      Result := Result + ';';
    Result := Result + S;
  end;
end;

function ExtractObjectName(const Value: String): String;
var
  Temp: String;
begin
  Result := ExtractObjectName(Value, Temp);
end;

function ExtractObjectName(const Value: String; var LeftPart: String): String;
var
  P: Integer;
  Quote: Char;
begin
  // Remove all quotes and brackets and schema name if there
  LeftPart := Trim(Value);
  Result := '';
  if LeftPart = '' then exit;
  // Extract quoted or unquoted item from the end till period
  P := Length(LeftPart);
  if CharInSet(LeftPart[P], ['"', '`', ']']) then
  begin
    if LeftPart[P] = ']' then
      Quote := '['
    else Quote := LeftPart[P];
    Dec(P);
    while (P > 0) and (LeftPart[P] <> Quote) do
      Dec(P);
    Result := copy(LeftPart, P + 1, Length(LeftPart) - P - 1);
    Dec(P);
    LeftPart := copy(LeftPart, 1, P);
    if (P > 0) and (LeftPart[P] = '.') then
      Delete(LeftPart, P, 1);
  end else
  begin
    while (P > 0) and CharInSet(LeftPart[P], [#32..#255]-['.']) do
      Dec(P);
    if P > 0 then
    begin
      Result := copy(LeftPart, P + 1, MaxInt);
      LeftPart := copy(LeftPart, 1, P - 1);
    end else begin
      Result := LeftPart;
      LeftPart := '';
    end;
  end;
end;

function EncodeVariant(Value: Variant): String;
var
  I: Integer;
begin
  if VarIsArray(Value) then
  begin
    Result := 'A';
    for I := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
      Result := Result + '"' + EncodeVariant(Value[I]) + '",';
    System.Delete(Result, Length(Result), 1);
  end else case VarType(Value) of
    varEmpty, varNull: Result := '';
    varByte, varSmallint, varInteger: Result := 'I'+VarToStr(Value);
    varSingle, varDouble, varCurrency: Result := 'F'+VarToStr(Value);
    varDate: Result := 'D'+VarToStr(Double(VarToDateTime(Value)));
    varOleStr, varString: Result := 'S'+VarToStr(Value);
    varBoolean: Result := 'B'+VarToStr(Integer(Value));
    else DatabaseError(SInvalidVariantString);
  end;
end;

function CommaTextToVar(const Value: PChar): Variant;
var
  P, P1: PChar;
  S: String;
  I: Integer;
begin
  Result := VarArrayCreate([0, 15], varVariant); // Maximum allow 16 keys
  I := 0;
  P := Value;
  while CharInSet(P^, [#1..' ']) do P := CharNext(P);
  while P^ <> #0 do
  begin
    if P^ = '"' then
      S := AnsiExtractQuotedStr(P, '"')
    else
    begin
      P1 := P;
      while (P^ >= ' ') and (P^ <> ',') do P := CharNext(P);
      SetString(S, P1, P - P1);
    end;
    Result[I] := DecodeVariant(S);
    Inc(I);
    while CharInSet(P^, [#1..' ']) do P := CharNext(P);
    if P^ = ',' then
      repeat
        P := CharNext(P);
      until not CharInSet(P^, [#1..' ']);
  end;
  if I > 0 then
    VarArrayRedim(Result, I-1)
  else Result := NULL;
end;

function DecodeVariant(Value: String): Variant;
var
  P: PChar;
begin
  Result := NULL;
  if Value = '' then exit;
  P := PChar(Value);
  case P^ of
    'A': Result := CommaTextToVar(CharNext(P));
    'B': Result := Boolean(StrToInt(CharNext(P)));
    'D': Result := VarFromDateTime(StrToFloat(CharNext(P)));
    'F': Result := StrToFloat(CharNext(P));
    'I': Result := StrToInt(CharNext(P));
    'S': Result := copy(Value, 2, Length(Value));
    else DatabaseError(SInvalidVariantString);
  end;
end;

function VarIsNullExt(V: Variant): Boolean;
var
  I: Integer;
begin
  if VarIsArray(V) then
  begin
    Result := False;
    for I := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
      if not (VarIsNull(V[I]) or VarIsEmpty(V[I])) then exit;
    Result := True;
  end else Result := VarIsNull(V) or VarIsEmpty(V);
end;

procedure AddPublishedProp(Instance: TObject; PropName: String; Values: TStrings);
begin
  Values.Values[PropName] := GetPropValue(Instance, PropName);
end;

function ValueFromIndex(Strings: TStrings; Idx: Integer): String;
begin
  if Idx >= 0 then
    Result := Copy(Strings[Idx], Length(Strings.Names[Idx]) + 2, MaxInt) else
    Result := '';
end;

procedure SaveStrToFile(const FileName, Value: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Stream.Write(Value[1], Length(Value));
  finally
    Stream.Free;
  end;
end;

function LoadStrFromFile(const FileName: String): String;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Result, Stream.Size);
    Stream.Read(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

function VarArrayToStr(const V: OleVariant): String;
var
  P: Pointer;
  Size: Integer;
begin
  Result := '';
  if VarIsArray(V) and (VarType(V) and varTypeMask = varByte) then
  begin
    Size := VarArrayHighBound(V, 1) - VarArrayLowBound(V, 1) + 1;
    if Size > 0 then
    try
      SetLength(Result, Size);
      P := VarArrayLock(V);
      try
        Move(P^, Result[1], Size);
      finally
        VarArrayUnlock(V);
      end;
    except
      raise Exception.Create('Error in VarArrayToStr');
    end;
  end;
end;

function StrToVarArray(const S: String): OleVariant;
var
  P: Pointer;
begin
  Result := NULL;
  if Length(S) > 0 then
  begin
    Result := VarArrayCreate([0, Length(S) - 1], varByte);
    P := VarArrayLock(Result);
    try
      Move(S[1], P^, Length(S));
    finally
      VarArrayUnlock(Result);
    end;
  end;
end;

procedure FreeObjects(List: TList);
var
  I: Integer;
  Obj: TObject;
begin
  for I := 0 to List.Count - 1 do
  begin
    Obj := TObject(List[I]);
    if Obj <> nil then
    begin
      List[I] := nil;
      Obj.Free;
    end;
  end;
end;

procedure FreeObjects(List: TStrings);
var
  I: Integer;
  Obj: TObject;
begin
  for I := 0 to List.Count - 1 do
  begin
    Obj := List.Objects[I];
    List.Objects[I] := nil;
    Obj.Free;
  end;
end;

function GetVirtualKey(const TableName, KeyFields: String; KeyValues: Variant; CaseInsensitive: Boolean = False): String;
begin
  if CaseInsensitive then
    Result := AnsiUpperCase(Format('%s.%s=%s', [TableName, KeyFields, EncodeVariant(KeyValues)]))
  else Result := Format('%s.%s=%s', [AnsiUpperCase(TableName), AnsiUpperCase(KeyFields), EncodeVariant(KeyValues)]);
end;

function FindVirtualKey(VirtualKeys: TStrings; const Key: String): Integer;
var
  I: Integer;
begin
  for I := 0 to VirtualKeys.Count - 1 do
    if VirtualKeys[I] = Key {Case sensetive!} then begin
      Result := I;
      exit;
    end;
  Result := -1;
end;

function VirtualKeyExists(VirtualKeys: TStrings; const Key: String): Boolean;
begin
  Result := FindVirtualKey(VirtualKeys, Key) >= 0;
end;

function GetKeyValues(DataSet: TDataSet; const KeyFields: String; var OldKeyValues, NewKeyValues: Variant; KeyFieldsList: TList = nil): Boolean;
var
  J: Integer;
  KeyListWasNil: Boolean;
  Fld: TField;
begin
  KeyListWasNil := KeyFieldsList = nil;
  if not KeyListWasNil then
    KeyFieldsList.Count := 0; // don't reset capacity
  if Pos(';', KeyFields) > 0 then
  begin
    Result := False;
    if KeyListWasNil then KeyFieldsList := TList.Create;
    try
      DataSet.GetFieldList(KeyFieldsList, KeyFields);
      OldKeyValues := VarArrayCreate([0, KeyFieldsList.Count - 1], varVariant);
      NewKeyValues := VarArrayCreate([0, KeyFieldsList.Count - 1], varVariant);
      // Compare old & new values for the key fields
      for J := 0 to KeyFieldsList.Count - 1 do
      with TField(KeyFieldsList[J]) do
      begin
        OldKeyValues[J] := OldValue;
        NewKeyValues[J] := Value;
        if OldValue <> Value then
          Result := True;
      end;
    finally
      if KeyListWasNil and (KeyFieldsList <> nil) then
        KeyFieldsList.Free;
    end;
  end else
  begin
    Fld := DataSet.FieldByName(KeyFields);
    if KeyFieldsList <> nil then
      KeyFieldsList.Add(Fld);
    with Fld do
    begin
      OldKeyValues := OldValue;
      NewKeyValues := Value;
      Result := OldValue <> Value;
    end;
  end;
end;

procedure ClearFields(DataSet: TDataSet; KeyFieldsList: TList);
var
  I: Integer;
begin
  DataSet.Edit;
  try
    if KeyFieldsList.Count = 1 then
      TField(KeyFieldsList.First).Clear
    else
      for I := 0 to KeyFieldsList.Count - 1 do
        TField(KeyFieldsList[I]).Clear;
    DataSet.Post;
  except
    DataSet.Cancel;
    raise;
  end;
end;

procedure AssignFields(DataSet: TDataSet; KeyFieldsList: TList; KeyValues: Variant);
var I: Integer;
begin
  if KeyFieldsList.Count = 0 then exit;
  DataSet.Edit;
  try
    if KeyFieldsList.Count = 1 then
      TField(KeyFieldsList.First).Value := KeyValues
    else
      for I := 0 to KeyFieldsList.Count - 1 do
        TField(KeyFieldsList[I]).Value := KeyValues[I];
    DataSet.Post;
  except
    DataSet.Cancel;
    raise;
  end;
end;

procedure AssignKeyFields(KeyFieldsList: TList; KeyValues: Variant);
var
  I: Integer;
begin
  if KeyFieldsList.Count = 1 then
    TField(KeyFieldsList.First).Value := KeyValues
  else
    for I := 0 to KeyFieldsList.Count - 1 do
      TField(KeyFieldsList[I]).Value := KeyValues[I];
end;

procedure CopyRecord(SrcTable, DestTable: TDataSet);
begin
  if not (DestTable.State in dsEditModes) then DestTable.Insert;
  try
    AssignRecord(SrcTable, DestTable);
    DestTable.Post;
  except
    DestTable.Cancel;
    raise;
  end;
end;

procedure AssignRecord(SrcTable, DestTable: TDataSet; ClearBeforeAssign: Boolean = False);
var
  I: Integer;
  SrcFld, DestFld: TField;
begin
  for I := 0 to DestTable.FieldCount - 1 do
  begin
    // Assuming, that most likely the tables are identical...
    DestFld := DestTable.Fields[I];
    if I < SrcTable.FieldCount then
      SrcFld := SrcTable.Fields[I]
    else SrcFld := nil;
    // .. and if not, just find the corresponding field
    if (SrcFld = nil) or (not AnsiSameText(SrcFld.FieldName, DestFld.FieldName)) then
      SrcFld := SrcTable.FindField(DestFld.FieldName);
    // Finally assign value (it will use variant representation internally)
    if ((ClearBeforeAssign) or (SrcFld <> nil)) and not DestFld.ReadOnly then
    begin
      if (SrcFld <> nil) and (DestFld.DataType <> SrcFld.DataType) then
      begin
        if SrcFld.IsNull then
          DestFld.Clear
        else if SrcFld.DataType = db.ftBoolean then
          DestFld.AsString := IntToStr(Integer(SrcFld.AsBoolean))
        else
          DestFld.AsString := SrcFld.AsString;
      end else
        DestFld.Assign(SrcFld);
    end;
  end;
end;

procedure AssignMasterField(const DataSet: TDataSet; const MasterField: String; Value: Variant);
var
  Fld: TField;
begin
  if DataSet = nil then exit;
  with DataSet do
  begin
    Fld := FieldByName(MasterField);
    DisableControls;
    try
      First;
      while not EOF do
      begin
        if Fld.AsVariant <> Value then
        begin
          Edit;
          try
            Fld.AsVariant := Value;
            Post;
          except
            Cancel;
            raise;
          end;
        end;
        Next;
      end;
    finally
      EnableControls;
    end;
  end;
end;

procedure ClearDataSet(DataSet: TDataSet);
begin
  if DataSet = nil then exit;
  with DataSet do
  begin
    DisableControls;
    try
      First;
      while not EOF do
        Delete;
    finally
      EnableControls;
    end;
  end
end;

procedure CopyDataSet(Src, Dest: TDataSet);
var
  B: TDataSetBookmark;
begin
  Src.DisableControls;
  Dest.DisableControls;
  B := Src.Bookmark;
  try
    Dest.First;
    Src.First;
    if Dest.EOF then begin
      while not Src.EOF do
      begin
        CopyRecord(Src, Dest);
        Src.Next;
      end;
    end else if Src.EOF then begin
      while not Dest.EOF do
        Dest.Delete;
    end else if (Dest.RecordCount = 1) and (Src.RecordCount = 1) then begin
      Dest.Edit;
      CopyRecord(Src, Dest);
    end else begin
      while not Dest.EOF do
        Dest.Delete;
      while not Src.EOF do
      begin
        CopyRecord(Src, Dest);
        Src.Next;
      end;
    end;
  finally
    Src.Bookmark := B;
    Src.EnableControls;
    Dest.EnableControls;
  end;
end;

procedure CopyDataSetWithKey(Src, Dest: TDataSet; const KeyField: String);
var
  MaxSrcKeyValue: Variant;
  SrcKeyField: TField;
  DestKeyField: TField;
  B: TDataSetBookmark;
begin
  B := Src.Bookmark;
  Src.DisableControls;
  Dest.DisableControls;
  try
    Dest.First;
    Src.First;
    if Dest.EOF then begin
      while not Src.EOF do
      begin
        CopyRecord(Src, Dest);
        Src.Next;
      end;
    end else if Src.EOF then begin
      while not Dest.EOF do
        Dest.Delete;
    end else if (Dest.RecordCount = 1) and (Src.RecordCount = 1) then begin
      Dest.Edit;
      CopyRecord(Src, Dest);
    end else begin
      SrcKeyField := Src.FieldByName(KeyField);
      DestKeyField := Dest.FieldByName(KeyField);
      MaxSrcKeyValue := SrcKeyField.Value; // not EOF because of the ifs above
      while not (Src.EOF and Dest.EOF) do
      begin
        // Op: Insert into Dest, Edit Dest, Delete Dest
        if not Src.EOF then
          MaxSrcKeyValue := SrcKeyField.Value;
        if Dest.EOF or (DestKeyField.Value > MaxSrcKeyValue) then
        begin
          if Src.EOF then begin
            Dest.Delete;
          end else begin
            Dest.Insert;
            CopyRecord(Src, Dest);
            Dest.Next;
            Src.Next;
          end;
        end else if DestKeyField.Value = MaxSrcKeyValue then
        begin
          if not Src.EOF then begin
            Dest.Edit;
            CopyRecord(Src, Dest);
            Dest.Next;
            Src.Next;
          end else break;
        end else { if (not Dest.EOF) and (DestKeyField.Value < MaxSrcKeyValue) then }
          Dest.Delete;
      end;
    end;
  finally
    Src.Bookmark := B;
    Src.EnableControls;
    Dest.EnableControls;
  end;
end;


function FieldClassToDataType(FieldClass: TFieldClass): TFieldType;
begin
  if FieldClass = TIntegerField then
    Result := DB.ftInteger
  else if FieldClass = TSmallIntField then
    Result := DB.ftSmallInt
  else if FieldClass = TStringField then
    Result := DB.ftString
  else if FieldClass = TWordField then
    Result := DB.ftWord
  else if FieldClass = TAutoIncField then
    Result := DB.ftAutoInc
  else if FieldClass = TGUIDField then
    Result := DB.ftGuid
  else if FieldClass = TBooleanField then
    Result := DB.ftBoolean
  else if FieldClass = TCurrencyField then
    Result := DB.ftCurrency
  else if FieldClass = TFloatField then
    Result := DB.ftFloat
  else if FieldClass = TDateTimeField then
    Result := DB.ftDateTime
  else if FieldClass = TDateField then
    Result := DB.ftDate
  else if FieldClass = TTimeField then
    Result := DB.ftTime
  else if FieldClass = TBlobField then
    Result := DB.ftBlob
  else if FieldClass = TMemoField then
    Result := DB.ftMemo
  else if FieldClass = TGraphicField then
    Result := DB.ftGraphic
  else if FieldClass = TBCDField then
    Result := DB.ftBCD
  else Result := DB.ftUnknown;
end;

function DataSetToVariant(DataSet: TDataSet; MaxRecCount: Integer = -1): OleVariant; {array}
var
  I: Integer;
  WasActive: Boolean;
  RecCount, CurSize: Integer;

  procedure Grow(NewSize: Integer);
  begin
    if CurSize <> NewSize then
    begin
      CurSize := NewSize;
      VarArrayRedim(Result, CurSize - 1);
    end;
  end;

begin
  Result := NULL;
  if DataSet = nil then exit;
  WasActive := DataSet.Active;
  DataSet.Active := True;
  try
    if DataSet.FieldCount = 0 then exit;
    RecCount := 0;
    CurSize := 10;
    Result := VarArrayCreate([0, DataSet.FieldCount, 0, CurSize - 1], varVariant);
    with DataSet do
    try
      // Store Name, DataType and Size for all fields in the first 3 rows
      for I := 0 to FieldCount - 1 do
      with Fields[I] do
      begin
        Result[I, 0] := FieldName;
        Result[I, 1] := Integer(DataType);
        Result[I, 2] := Size;
      end;
      RecCount := 3;
      if RecordCount + 3 > CurSize then
        Grow(RecordCount + 3);
      DisableControls;
      try
        First;
        while ((MaxRecCount < 0) or (RecCount - 3 < MaxRecCount)) and not EOF do
        begin
          if RecCount >= CurSize then
            Grow(CurSize + 64);
          for I := 0 to DataSet.FieldCount - 1 do
            Result[I, RecCount] := Fields[I].AsVariant;
          DataSet.Next;
          Inc(RecCount);
        end;
      finally
        EnableControls;
      end;
    finally
      Grow(RecCount);
    end;
  finally
    DataSet.Active := WasActive;
  end;
end;

function CreateField(Owner: TDataSet; FieldClass: TFieldClass; const FieldName: String; DataType: TFieldType = DB.ftUnknown; Size: Integer = 0; Required: Boolean = False): TField;
begin
  Result := FieldClass.Create(Owner);
  try
    Result.FieldName := FieldName;
    if DataType = DB.ftUnknown then
      DataType := FieldClassToDataType(FieldClass);
    if DataType = DB.ftUnknown then
      DatabaseErrorFmt(SFieldTypeNotSupported, [FieldName]);
    Result.SetFieldType(DataType);
    Result.Size := Size;
    Result.Required := Required;
    Result.DataSet := Owner;
  except
    Result.Free;
    raise;
  end;
end;

function FindDatabase(const DatabaseName: String): TComponent;
var
  I: Integer;
  DB: ISchemaDatabase;
begin
  Result := nil;
  for I := 0 to DBDatabases.Count - 1 do
  if TObject(DBDatabases[I]).GetInterface(ISchemaDatabase, DB) then
  begin
    if AnsiCompareText(DB.DatabaseName, DatabaseName) = 0
    then begin
      Result := TComponent(DBDatabases[I]);
      exit;
    end;
  end;
end;

function ReverseString(const Str: String): String;
var
  I, Cnt: Integer;
  C: Char;
begin
  Result := Str;
  Cnt := Length(Str);
  for I := 1 to Length(Str) div 2 do
  begin
    C := Result[I];
    Result[I] := Result[Cnt - I + 1];
    Result[Cnt - I + 1] := C;
  end;
end;

function ParseFields(const InpStr, Open, Close: String; OnParseField: TOnParseField; Data: Pointer): String;
var
  OpenPos, ClosePos: Integer;
  Res: String;

  function PosFrom(const SubStr, Str: String; FromPos: Integer): Integer;
  var P: PChar;
  begin
    P := StrPos(@PChar(Str)[FromPos - 1], PChar(SubStr));
    if P <> nil then
      Result := Longint(P) - Longint(PChar(Str)) + 1
    else Result := 0;
  end;

begin
  Result := '';
  Res := InpStr;
  ClosePos := 1;
  repeat
    // Look for next open bracket from last Close pos
    OpenPos := PosFrom(Open, Res, ClosePos);
    if OpenPos = 0 then break;
    Result := Result + copy(Res, ClosePos, OpenPos - ClosePos);
    Inc(OpenPos, Length(Open)); // Skip open bracket
    // Open bracket found. Look for Close
    ClosePos := PosFrom(Close, Res, OpenPos);
    if ClosePos = 0 then break;
    // Close Bracket Found. Extract/Replace field
    Result := Result + OnParseField(copy(Res, OpenPos, ClosePos - OpenPos), Data);
    Inc(ClosePos, Length(Close));
  until false;
  Result := Result + copy(Res, ClosePos, Length(Res));
end;

procedure GetValues(Collection: TCollection; Values: TStrings; PropName: String = 'Name'; Append: boolean = False);
var I: Integer;
begin
  if not Append then
    Values.Clear;
  for I := 0 to Collection.Count - 1 do
    Values.AddObject(GetStrProp(Collection.Items[I], PropName), Collection.Items[I]);
end;

procedure GetValues(List: TList; Values: TStrings; PropName: String = 'Name'; Append: boolean = False);
var I: Integer;
begin
  if not Append then
    Values.Clear;
  for I := 0 to List.Count - 1 do
    Values.AddObject(GetStrProp(TObject(List[I]), PropName), List[I]);
end;

procedure GetDistinctValues(List: TObject; Values: TStrings; PropName: String = 'Name'); overload;
var
  TempList: TStringList;
begin
  TempList := TStringList.Create;
  try
    TempList.Duplicates := dupIgnore;
    TempList.Sorted := True;
    if List.InheritsFrom(TCollection) then
      GetValues(TCollection(List), TempList, PropName)
    else if List.InheritsFrom(TList) then
      GetValues(TList(List), TempList, PropName);
    Values.Assign(TempList);
  finally
    TempList.Free;
  end;
end;

function IsUnique(CollectionItem: TCollectionItem; const NewValue: String; const PropName: String = 'Name'): Boolean;
var I: Integer;
begin
  Result := True;
  // DB: if collection is not assigned, ignore it.
  if CollectionItem.Collection = nil then
    exit;
  for I := 0 to CollectionItem.Collection.Count - 1 do
    if (CollectionItem.Collection.Items[I] <> CollectionItem)
      and (AnsiSameText(NewValue, GetStrProp(CollectionItem.Collection.Items[I], PropName))) then
    begin
      Result := False;
      exit;
    end;
end;

procedure GetFieldTypes(List: TStrings);
var
  I: TFieldDataType;
begin
  List.Clear;
  for I := Low(FieldDataTypes) to High(FieldDataTypes) do
    List.Add(FieldDataTypes[I]);
end;

function IsValidFieldType(const FieldType: String): Boolean;
begin
  Result := (FieldType <> '') and not CharInSet(FieldType[1], ['{', '?', '*']);
end;

function GetFieldTypeByName(const Name: String): TFieldDataType;
begin
  for Result := Low(FieldDataTypes) to High(FieldDataTypes) do
    if AnsiSameText(FieldDataTypes[Result], Name) then exit;
  Result := ftUnknown;
end;

procedure GetPropNames(Instance: TObject; List: TStrings);
const
  tkValueProps = [tkInteger, tkChar, tkEnumeration, tkFloat,
    tkString, tkSet, tkWChar, tkLString, tkWString,
    tkVariant, tkInt64, tkClass];
var
  I: Integer;
  PropList: PPropList;
  Count: Integer;
  PropValue: TObject;
begin
  Count := GetTypeData(Instance.ClassInfo)^.PropCount;
  if Count <= 0 then exit;
  GetMem(PropList, Count * SizeOf(Pointer));
  try
    Count := GetPropList(Instance.ClassInfo, tkProperties, PropList);
    for I := 0 to Count - 1 do
    with PropList^[I]^ do
      if (PropType^.Kind in tkValueProps) and IsStoredProp(Instance, PropList^[I]) then
      begin
        if (PropType^.Kind = tkClass) then
        begin
          PropValue := TObject(GetOrdProp(Instance, PropList^[I]));
          if not PropValue.InheritsFrom(TStrings) then continue;
        end;
        List.Add(String(Name));
      end;
  finally
    FreeMem(PropList);
  end;
end;

function GetItemCollection(Parent: TObject;
  ItemClass: TSchemaCollectionItemClass): TSchemaItemsCollection;
begin
  if ItemClass = TFieldDefinition then
    Result := (Parent as TTableDefinition).FieldDefs
  else if ItemClass = TIndexDefinition then
    Result := (Parent as TTableDefinition).IndexDefs
  else if ItemClass = TRelation then
    Result := (Parent as TTableDefinition).Relations
  else if ItemClass = TTriggerDefinition then
    Result := (Parent as TTableDefinition).Triggers
  else if ItemClass = TTableConstraint then
    Result := (Parent as TTableDefinition).Constraints
  else if ItemClass = TRelationship then
    Result := (Parent as TDatabaseSchema).Relationships
  else if ItemClass = TTableDefinition then
    Result := (Parent as TDatabaseSchema).TableDefs
  else if ItemClass = TDomain then
    Result := (Parent as TDatabaseSchema).Domains
  else if ItemClass = TEnumeration then
    Result := (Parent as TDatabaseSchema).Enumerations
  else if ItemClass = TViewDefinition then
    Result := (Parent as TDatabaseSchema).ViewDefs
  else if ItemClass = TStoredProcDefinition then
    Result := (Parent as TDatabaseSchema).StoredProcs
  else if ItemClass = TModuleDefinition then
    Result := (Parent as TDatabaseSchema).Modules
  else if ItemClass = TSequence then
    Result := (Parent as TDatabaseSchema).Sequences
  else if ItemClass = TIndexField then
    Result := (Parent as TIndexDefinition).IndexFields
  else if ItemClass = TCustomObject then
    Result := (Parent as TDatabaseSchema).CustomObjects
  else Result := nil;
end;

const
 DelphiKeyWords =
   ' and array as asm begin case class const constructor destructor dispinterface div '+
   ' do downto else end except exports file finalization finally for function goto '+
   ' if implementation in inherited initialization inline interface is label library mod nil '+
   ' not object of or out packed procedure program property raise record repeat '+
   ' resourcestring set shl shr String then threadvar to try type unit until '+
   ' uses var while with xor private protected public published automated at on ';

{ Helper Routines }

function NameToIdent(const TypePrefix, Name: String): String;
var
  I: Integer;
const
  set_digits = ['0'..'9'];
  set_alnum  = ['0'..'9', 'a'..'z', 'A'..'Z', '_'];
begin
  Result := '';
  for I := 1 to Length(Name) do
    if CharInSet(Name[I], set_alnum) then
      Result := Result + Name[I];
  Result := TypePrefix + Result;
  if Length(Result) = 0 then
    raise Exception.CreateFmt('Invalid identifier: "%s"', [Name]);
  if CharInSet(Result[1], set_digits) then
    Result := '_' + Result;
end;

function AdjustName(const Name: String): String;
begin
  if AnsiPos(' ' + AnsiLowerCase(Name) + ' ', DelphiKeyWords) > 0 then
    Result := '_' + Name
  else Result := Name;
end;

procedure GenerateSchemaDeclarations(Schema: TDatabaseSchema; Strings: TStrings);
var
  I, J: Integer;
  EnumConstName, EnumConstValue,
  EnumConstDescription, EnumConstShortDescription: String;
  Idents: TStringList;
  FieldClass: TFieldClass;
  UnitName: String;

  function EnumPrefix(const Prefix: String): String;
  begin
    Result := Prefix;
    if Result = '' then Result := 'en';
  end;

  function TablePrefix(const Prefix: String): String;
  begin
    Result := Prefix;
    if Result = '' then Result := 'T';
  end;

begin
  UnitName := NameToIdent('', Schema.SchemaName);
  Strings.Add('(******************************************************************************)');
  Strings.Add('(*  This file contains pascal declarations imported from database schema');
  Strings.Add('(*  This file will be written by the ''Update Schema Declaration'' procedure');
  Strings.Add('(*  of TDatabaseSchema component editor.');
  Strings.Add('(*  Changes to this file will be discarded during the update process.');
  Strings.Add('(*');
  Strings.Add('(*  Database Schema: ' + Schema.SchemaName);
  Strings.Add('(*  Version: ' + Schema.VersionLabel);
  Strings.Add('(******************************************************************************)');
  Strings.Add('unit ' + UnitName + ';');
  Strings.Add('');
  Strings.Add('interface');
  Strings.Add('');
  Strings.Add('uses Classes, DB, dbSchema;');
  Strings.Add('');
  with Schema do
  if Enumerations.Count > 0 then
  begin
    Strings.Add('const');
    Strings.Add('  { Enumerations }');
    Idents := TStringList.Create;
    try
      Idents.Sorted := True;
      Idents.Duplicates := dupError;
      for I := 0 to Enumerations.Count - 1 do
      with Enumerations[I] do
      begin
        Strings.Add('  { ' + DisplayLabel +' }');
        for J := 0 to Items.Count - 1 do
        begin
          EnumConstName := ValueFromIndex(Items, J);
          EnumConstValue := Items.Names[J];
          EnumConstDescription := Descriptions.Values[EnumConstValue];
          if EnumConstDescription = '' then
            EnumConstDescription := EnumConstName;
          EnumConstShortDescription := ShortDescriptions.Values[EnumConstValue];
          EnumConstName := NameToIdent(EnumPrefix(TypePrefix), EnumConstName);
          if Idents.IndexOf(EnumConstName) < 0 then
            Idents.Add(EnumConstName)
          else raise Exception.CreateFmt('Duplicate identifier: %s. Enumeration: %s', [EnumConstName, DisplayLabel]);
          if IntConsts then
            Strings.Add(Format('  %s = %s; { %s }', [AdjustName(EnumConstName), EnumConstValue, EnumConstDescription]))
          else Strings.Add(Format('  %s = ''%s''; { %s }', [AdjustName(EnumConstName), EnumConstValue, EnumConstDescription]));
        end;
      end;
      Strings.Add('');
    finally
      Idents.Free;
    end;
  end;

  with Schema do
  if TableDefs.Count > 0 then
  begin
    Strings.Add('type');
    Strings.Add('  { Tables Declarations }');
    for I := 0 to TableDefs.Count - 1 do
    begin
      Strings.Add('');
      Strings.Add(Format('  %s = class (TDataSetReference)', [
        AdjustName(TablePrefix(TableDefs[I].TypePrefix) + TableDefs[I].TableName)]));

      Strings.Add('  protected');
      for J := 0 to TableDefs[I].FieldDefs.Count - 1 do
      with TableDefs[I].FieldDefs[J] do
      begin
        FieldClass := DefaultFieldClasses[FieldDataTypeToVCL[DataType]];
        if FieldClass <> nil then
          Strings.Add(Format('    F%s: %s;', [AdjustName(Name), FieldClass.ClassName]));
      end;

      Strings.Add('  published');
      for J := 0 to TableDefs[I].FieldDefs.Count - 1 do
      with TableDefs[I].FieldDefs[J] do
      begin
        FieldClass := DefaultFieldClasses[FieldDataTypeToVCL[DataType]];
        if FieldClass <> nil then
          Strings.Add(Format('    property %s: %s read F%0:s write F%0:s;', [AdjustName(Name), FieldClass.ClassName]));
      end;
      Strings.Add('  end;');
    end;
  end;
  Strings.Add('');
  Strings.Add('implementation');
  Strings.Add('');
  Strings.Add('end.');
end;

function KeyFieldsRequired(TableDef: TTableDefinition; KeyFields: String): Boolean;
var
  P: Integer;
  FldName: String;
  FldDef: TFieldDefinition;
begin
  // Returns True if all key fields are not null
  Result := False;
  if TableDef = nil then exit;
  P := 1;
  while P <= Length(KeyFields) do
  begin
    FldName := NextToken(KeyFields, ';', P);
    if FldName = '' then continue;
    FldDef := TableDef.FieldDefs.Find(FldName);
    if (FldDef = nil) or not FldDef.Required then
    begin
      Result := False;
      exit;
    end;
    Result := True;
  end;
end;

function CompareRelProps(SrcRel, DestRel: TRelationship): Boolean;
begin
  Result := (SrcRel.Name = DestRel.Name)
    and (SrcRel.FTableName[sideDetail] = DestRel.FTableName[sideDetail])
    and (SrcRel.FTableName[sideMaster] = DestRel.FTableName[sideMaster])
    and (SrcRel.FKeyFields[sideDetail] = DestRel.FKeyFields[sideDetail])
    and (SrcRel.FKeyFields[sideMaster] = DestRel.FKeyFields[sideMaster])
    and (SrcRel.FRelationName[sideDetail] = DestRel.FRelationName[sideDetail])
    and (SrcRel.FRelationName[sideMaster] = DestRel.FRelationName[sideMaster]);
end;

function RelationshipsEqual(Src, Dest: TRelationship): Boolean;
begin
  if (Src = nil) or (Dest = nil) then
  begin
    Result := False;
    exit;
  end;
  Result := (Src.FName = Dest.FName)
    and Src.Props.Equals(Dest.Props)
    and (Src.FTableName[sideDetail] = Dest.FTableName[sideDetail])
    and (Src.FTableName[sideMaster] = Dest.FTableName[sideMaster])
    and (Src.FKeyFields[sideDetail] = Dest.FKeyFields[sideDetail])
    and (Src.FKeyFields[sideMaster] = Dest.FKeyFields[sideMaster])
    and (Src.FRelationName[sideDetail] = Dest.FRelationName[sideDetail])
    and (Src.FRelationName[sideMaster] = Dest.FRelationName[sideMaster])
    and (Src.FCondition[sideDetail] = Dest.FCondition[sideDetail])
    and (Src.FCondition[sideMaster] = Dest.FCondition[sideMaster])
    and (Src.FDetailCardinality = Dest.FDetailCardinality)
    and (Src.FMasterRecordOptional = Dest.FMasterRecordOptional)
    and (Src.FMasterOwnsDetails = Dest.FMasterOwnsDetails)
    and (Src.FDeleteAction = Dest.FDeleteAction)
    and (Src.FUpdateAction = Dest.FUpdateAction)
    and (Src.FEnforceForeignKey = Dest.FEnforceForeignKey)
    and (Src.FCaseInsensitive = Dest.FCaseInsensitive)
    and (Src.FDeleteErrorMessage = Dest.FDeleteErrorMessage)
    and (Src.FUpdateErrorMessage = Dest.FUpdateErrorMessage)
    and (Src.FRequireRecordErrorMessage = Dest.FRequireRecordErrorMessage);
end;

function CheckVersion(Database: ISchemaDatabase): Boolean;
begin
  Database.Connected := True;
  if Database.Schema = nil then
    DatabaseError(SDatabaseSchemaIsNotAssigned);
  Result := CompareVersions(Database.GetVersion, Database.Schema.Version) >= 0;
end;

procedure ExecuteUpdate(Database: ISchemaDatabase; const SQLScript: String; Version: TSchemaVersion);
var
  Stmt: String;
  Query: TDataSet;
begin
  Query := Database.CreateQuery('');
  try
    with TSQLLexer.Create(SQLScript) do
    try
      while NextStatement(Stmt) do
        if Stmt <> '' then
        begin
          Database.SetQuerySQL(Query, Stmt);
          Database.ExecSQL(Query);
          Query.Active := False;
        end;
    finally
      Free;
    end;
  finally
    Query.Free;
  end;
  Database.SetVersion(Version);
end;

function UpdateDatabase(Database: ISchemaDatabase; OnProgress: TDatabaseProgress = nil): Boolean;
var
  Abort: Boolean;
  MinUpdate, I: Integer;
  Ver: TSchemaVersion;

  procedure DoProgress(const Status: String; PercentDone: Byte);
  begin
    if Assigned(OnProgress) then
      OnProgress(Database.Schema, Status, PercentDone, Abort);
  end;

begin
  Result := False;
  Database.Connected := True;
  if Database.Schema = nil then
    DatabaseError(SDatabaseSchemaIsNotAssigned);

  Ver := Database.GetVersion;
  // Exit if already have correct version
  if CompareVersions(Ver, Database.Schema.Version) > 0 then exit;

  MinUpdate := 0;
  while (MinUpdate < Database.Schema.Updates.Count)
    and (CompareVersions(Database.Schema.Updates[MinUpdate].Version, Ver) <= 0) do
      Inc(MinUpdate);

  DoProgress(SUpdatingDatabaseVersion, 0);
  try
    for I := MinUpdate to Database.Schema.Updates.Count - 1 do
    with Database.Schema.Updates[I] do
      if CompareVersions(Version, Ver) > 0 then
      begin
        DoProgress(Description, MulDiv(I - MinUpdate, 100, Database.Schema.Updates.Count));
        ExecuteUpdate(Database, SQLScript, Version);
        Ver := Version;
        Result := True;
      end;
  finally
    DoProgress(SDone, 100);
  end;
end;

procedure InternalRelationshipError(Relationship: TRelationship);
{$IFDEF CTXDEBUG}
  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP + 4]
  end;
{$ENDIF}
begin
{$IFDEF CTXDEBUG}
  // Do not localize, this is purely for debug reasons.
  raise Exception.CreateFmt('Internal Error. Invalid relationship %s ::= %s -> %s',
    [Relationship.Name, Relationship.DetailRelationName, Relationship.MasterRelationName]) at ReturnAddr;
{$ENDIF}
end;

function FormatName(const Name, Fmt: String): String;
begin
  if AnsiSameText(Fmt, 'd') or (Fmt = '') then
    Result := AnsiQuotedStr(Name, '"')
  else if AnsiSameText(Fmt, 'b') then
    Result := '[' + Name + ']'
  else if AnsiSameText(Fmt, 'a') then
    Result := AnsiQuotedStr(Name, '`')
  else if AnsiSameText(Fmt, 'q') then
    Result := AnsiQuotedStr(Name, '''')
  else Result := Name;
end;

{ TCompareItem }

constructor TCompareItem.Create(ASrcObj, ADestObj: TObject; AParentItem: TCompareItem = nil);
begin
  SrcObj := ASrcObj;
  DestObj := ADestObj;
  ParentItem := AParentItem;
  if SrcObj <> nil then
    CompareObjects
  else if DestObj <> nil then
  begin
    SwapSrcDest;
    CompareObjects;
    SwapSrcDest;
  end;
  if (SubItems <> nil) and (SubItems.Count = 0) then
    FreeAndNil(SubItems);
end;

destructor TCompareItem.Destroy;
begin
  SubItems.Free;
  inherited;
end;

procedure TCompareItem.SwapSrcDest;
var
  P: Pointer;
begin
  P := DestObj;
  DestObj := SrcObj;
  SrcObj := P;
  SwapSrcDestList(SubItems);
end;

function TCompareItem.GetItemOperation: TItemOperation;
var
  SrcPresent, DestPresent: Boolean;
begin
  SrcPresent := (SrcObj <> nil) and not AnsiSameText(GetPropValue(SrcObj, 'ProduceSQL'), 'False');
  DestPresent := (DestObj <> nil) and not AnsiSameText(GetPropValue(DestObj, 'ProduceSQL'), 'False');

  Result := ioNone;
  if DestPresent and not SrcPresent then
    Result := ioCreate
  else if SrcPresent and not DestPresent then
    Result := ioDrop
  else if SrcPresent and DestPresent then
    Result := ioAlter;
end;

function TCompareItem.GetSubItems: TObjectList;
begin
  if SubItems = nil then
    SubItems := TObjectList.Create(True);
  Result := SubItems;
end;

function TCompareItem.GetDefaultPropValue(AObj: TObject;
  const PropName: String): String;
var
  PropInfo: PPropInfo;
begin
  Result := '';
  PropInfo := GetPropInfo(AObj, PropName);
  if (PropInfo <> nil) then
    case PropInfo^.PropType^^.Kind of
      tkEnumeration: Result := GetEnumName(PropInfo^.PropType^, PropInfo^.Default);
      tkInteger, tkChar: Result := IntToStr(PropInfo^.Default);
    end;
end;

function TCompareItem.GetPropValue(AObj: TObject;
  const PropName: String): String;
begin
  if IsPublishedProp(AObj, PropName) then
    Result := VarToStr(TypInfo.GetPropValue(AObj, PropName, True))
  else Result := '';
end;

class procedure TCompareItem.SwapSrcDestList(List: TList);
var
  I: Integer;
begin
  if List <> nil then
  for I := 0 to List.Count - 1 do
    TCompareItem(List[I]).SwapSrcDest;
end;

procedure TCompareItem.CompareObjects;
begin
  // Nothing to compare here, because objects has no child properties
end;

function TCompareItem.GetObj: TObject;
begin
  Result := DestObj;
  if Result = nil then Result := SrcObj;
end;

function CompareProperties(SrcItem, DestItem: TSchemaCollectionItem): Boolean;
var
  List: TStringList;
  I: Integer;
begin
  Result := False;
  if (SrcItem = nil) or (DestItem = nil) then exit;
  List := TStringList.Create;
  try
    GetPropNames(SrcItem, List);
    for I := 0 to List.Count - 1 do
      if SrcItem.GetPropValue(List[I]) <> DestItem.GetPropValue(List[I]) then exit;
  finally
    List.Free;
  end;
  Result := True;
end;

{ TCompareSchema }

procedure TCompareSchema.CompareObjects;
begin
  SrcSchema.Compare(DestSchema, GetSubItems);
  if (SubItems <> nil) and (SubItems.Count = 0) then
    FreeAndNil(SubItems);
end;

function TCompareSchema.DestSchema: TDatabaseSchema;
begin
  Result := TDatabaseSchema(DestObj);
end;

function TCompareSchema.SrcSchema: TDatabaseSchema;
begin
  Result := TDatabaseSchema(SrcObj);
end;

{ TCompareSchemaItem }

procedure TCompareSchemaItem.CompareObjects;
begin
  SrcItem.Compare(Self);
end;

function TCompareSchemaItem.DestItem: TSchemaCollectionItem;
begin
  Result := TSchemaCollectionItem(DestObj);
end;

function TCompareSchemaItem.SrcItem: TSchemaCollectionItem;
begin
  Result := TSchemaCollectionItem(SrcObj);
end;

function TCompareSchemaItem.GetItem: TSchemaCollectionItem;
begin
  Result := DestItem;
  if Result = nil then Result := SrcItem;
end;

function TCompareSchemaItem.GetPropValue(AObj: TObject;
  const PropName: String): String;
begin
  Result := TSchemaCollectionItem(AObj).GetPropValue(PropName);
end;

{ TSchemaCollectionItem }

constructor TSchemaCollectionItem.Create(Collection: TCollection);
begin
  FSign := cSignValidObj;
  inherited Create(Collection);
  FProps := TStringList.Create;
  TStringList(FProps).OnChange := PropsChanged;
  FItemID := 0;
  FOldIndex := -1;
  FUpdateCounter := 0;
  FProduceSQL := True;
end;

destructor TSchemaCollectionItem.Destroy;
begin
  FProps.Free;
  inherited;
  FSign := 0;
end;

procedure TSchemaCollectionItem.Assign(Source: TPersistent);
begin
  if Source.InheritsFrom(TSchemaCollectionItem) then
  begin
    if Schema.SchemaID = TSchemaCollectionItem(Source).Schema.SchemaID then
      ItemID := TSchemaCollectionItem(Source).ItemID;
    FName := TSchemaCollectionItem(Source).FName;
    FDescription := TSchemaCollectionItem(Source).FDescription;
    FProps.Assign(TSchemaCollectionItem(Source).FProps);
    FProduceSQL := TSchemaCollectionItem(Source).FProduceSQL;
  end else
    inherited;
end;

procedure TSchemaCollectionItem.CopyFrom(ASource: TPersistent; AWithSubItems: boolean = True);
begin
  Assign(ASource);
end;

function TSchemaCollectionItem.GetDisplayName: String;
begin
  Result := Name;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TSchemaCollectionItem.GetPropValue(const PropName: String): String;
var
  PropInfo: PPropInfo;
  PropValue: TObject;
begin
  PropInfo := GetPropInfo(Self, PropName);
  if PropInfo = nil then
  begin
    if AnsiSameText(PropName, 'FullName') then
      Result := FullName
    else if AnsiSameText(PropName, 'ItemID') then
      Result := IntToStr(ItemID)
    else Result := Props.Values[PropName]
  end else begin
    if PropInfo^.PropType^^.Kind = tkClass then
    begin
      PropValue := TObject(GetOrdProp(Self, PropInfo));
      if PropValue.InheritsFrom(TStrings) then
        Result := TStrings(PropValue).Text
      else Result := Props.Values[PropName];
    end else
      Result := VarToStr(TypInfo.GetPropValue(Self, PropName, True))
  end;
end;

{$IFDEF VER130}
function AnsiDequotedStr(const S: String; AQuote: Char): String;
var
  LText: PChar;
begin
  LText := PChar(S);
  Result := AnsiExtractQuotedStr(LText, AQuote);
  if Result = '' then
    Result := S;
end;
{$ENDIF}

procedure TSchemaCollectionItem.SetPropValue(const PropName, Value: String);
const
  cNoEmptyProp = [tkInteger, tkEnumeration, tkFloat, tkSet, tkVariant, tkArray, tkRecord, tkDynArray];
var
  PropInfo: PPropInfo;
  PropValue: TObject;
begin
  if AnsiSameText(PropName, 'UpdateDefinition') then
    UpdateDefinition(Value)
  else begin
    PropInfo := GetPropInfo(Self, PropName);
    if PropInfo = nil then
      Props.Values[PropName] := Value
    else begin
      if PropInfo^.PropType^^.Kind = tkClass then
      begin
        PropValue := TObject(GetOrdProp(Self, PropInfo));
        if PropValue.InheritsFrom(TStrings) then
          TStrings(PropValue).Text := Value
        else Props.Values[PropName] := Value;
      end else begin
        if AnsiSameText(PropName, 'Name') then
          Self.Name := ExtractObjectName(Value)
        else if AnsiSameText(PropName, 'Description') then
          Self.Description := AnsiDequotedStr(Value, '''') else
        begin
          if (PropInfo^.PropType^.Kind in cNoEmptyProp) and (Value = '') then else
            TypInfo.SetPropValue(Self, PropName, Value);
        end;  
      end;
    end;
  end;
end;

function TSchemaCollectionItem.GetSchema: TDatabaseSchema;
begin
  Result := TSchemaItemsCollection(Collection).GetSchema;
end;

procedure TSchemaCollectionItem.PropsChanged(Sender: TObject);
begin
  // Implement in descendants
end;

procedure TSchemaCollectionItem.Rename(const Value: String);
var
  NewName, OldName: String;
begin
  if FName <> Value then
  begin
    OldName := FName;
    NewName := Trim(Value);
    ValidateRename(NewName);  // Make sure the name is valid (unique, not empty)
    FName := NewName;
    ObjectRenamed(OldName);   // Update references to this object's by name
  end;
end;

procedure TSchemaCollectionItem.SetProps(const Value: TStrings);
begin
  FProps.Assign(Value);
end;

function TSchemaCollectionItem.GetItemIndex: Integer;
begin
  Result := Index + 1;
end;

procedure TSchemaCollectionItem.SetItemIndex(const Value: Integer);
begin
  Index := Max(0, Min(Collection.Count - 1, Value - 1));
end;

function TSchemaCollectionItem.GetItemID: Integer;
begin
  // NB: FItemID < 0 is a valid items id for items removed in checkpoints
  // therefore must only compare with zero. The only invalid ItemID is 0
  if (FItemID = 0) and not (csLoading in GetSchema.ComponentState) then
    FItemID := GetSchema.GetNextItemID;
  Result := FItemID;
end;

procedure TSchemaCollectionItem.SetItemID(const Value: Integer);
begin
  FItemID := Value;
  if FItemID > GetSchema.FNextItemID then
    GetSchema.FNextItemID := FItemID;
end;

procedure TSchemaCollectionItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ItemID', ReadItemID, WriteItemID, True);
  Filer.DefineProperty('OldIndex', ReadOldIndex, WriteOldIndex, FOldIndex >= 0);
end;

procedure TSchemaCollectionItem.ReadItemID(Reader: TReader);
begin
  SetItemID(Reader.ReadInteger);
end;

procedure TSchemaCollectionItem.WriteItemID(Writer: TWriter);
begin
  Writer.WriteInteger(ItemID);
end;

function TSchemaCollectionItem.Compare(Dest: TCompareSchemaItem): Boolean;
begin
  Result := CompareProperties(Dest.SrcItem, Dest.DestItem) and
    FProps.Equals(Dest.DestItem.Props);
  if Result and (Dest.DestItem <> nil) then
    Result := AnsiSameText(Category, Dest.DestItem.Category);
  Dest.PropsEqual := Result;
end;

function TSchemaCollectionItem.PropsEqual(Dest: TCompareSchemaItem;
  const PropName: String): Boolean;
begin
  Result := AnsiSameText(Props.Values[PropName], Dest.DestItem.Props.Values[PropName]);
end;

function TSchemaCollectionItem.GetSchemaClassName: String;
begin
  Result := copy(ClassName, 2, MaxInt);
end;

function TSchemaCollectionItem.GetAutoName(const Template: String = ''): String;
var
  S: String;
begin
{  if Template = '' then
    S := GetSchemaClassName+'_<N>' else
    if AnsiPos('<N>', Template) < 1 then
      S := Template+'_<N>' else
      S := Template;}
  if Template = '' then
    S := GetSchemaClassName else
    S := Template;
  Result := TSchemaItemsCollection(Collection).GetAutoName(Self, S);
end;

procedure TSchemaCollectionItem.ReadOldIndex(Reader: TReader);
begin
  FOldIndex := Reader.ReadInteger;
end;

procedure TSchemaCollectionItem.WriteOldIndex(Writer: TWriter);
begin
  Writer.WriteInteger(FOldIndex);
end;

procedure TSchemaCollectionItem.GetPropNames(List: TStrings);
var
  I: Integer;
begin
  dbSchema.GetPropNames(Self, List);
  with Props do
    for I := 0 to Count - 1 do
      if Names[I] <> '' then
        List.Add(Names[I]);
end;

procedure TSchemaCollectionItem.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TSchemaCollectionItem.EndUpdate;
begin
  if FUpdateCounter > 0 then
    Dec(FUpdateCounter);
  if FUpdateCounter = 0 then
    InternalUpdate;
end;

function TSchemaCollectionItem.IsUpdating: Boolean;
begin
  Result := FUpdateCounter <> 0;
end;

procedure TSchemaCollectionItem.InternalUpdate;
begin
  // Implement in descendants
end;

procedure TSchemaCollectionItem.ObjectRenamed(const OldName: String);
begin
  // Implement in descendants. Update referencing objects.
end;

procedure TSchemaCollectionItem.ValidateRename(const NewName: String);
begin
  // Do not validate renames in case of schema version 
  if Schema is TDBSchemaVersion then exit;

  // Item's name must not be empty
  if NewName = '' then
    DatabaseErrorFmt(SEmptyObjectName, [GetSchemaClassName]);
  // And must be unique on its level
  if InheritsFrom(TTableDefinition) or InheritsFrom(TViewDefinition)
    or InheritsFrom(TStoredProcDefinition)
    or InheritsFrom(TModuleDefinition) then
  begin
    if not Schema.TopLevelUnique(Self, NewName) then
      DatabaseErrorFmt(SDuplicateObjectName, [GetSchemaClassName, NewName]);
  end else begin
    if not IsUnique(Self, NewName) then
      DatabaseErrorFmt(SDuplicateObjectName, [GetSchemaClassName, NewName]);
  end;
end;

function TSchemaCollectionItem.GetFullName: String;
begin
  Result := Name;
end;

function TSchemaCollectionItem.GetDisplayLabel: String;
begin
  Result := Name;
end;

procedure TSchemaCollectionItem.UpdateDefinition(const Value: String);
begin
  // Implement in descendands. This method may be called to update
  // object definitions after all properties has been assigned.
  // The parameter is optional and can be ignored.
end;

{ TTableCollectionItem }
{
function TTableCollectionItem.GetAutoName(const Prefix: String): String;
begin
  Result := inherited GetAutoName(TableName + '_' + Prefix);
end;
}
function TTableCollectionItem.GetFullName: String;
begin
  Result := TableName + '.' + Name;
end;

function TTableCollectionItem.GetSchema: TDatabaseSchema;
begin
  Result := GetTableDef.GetSchema;
end;

function TTableCollectionItem.GetTableDef: TTableDefinition;
begin
  Result := TTableDefItemsCollection(Collection).GetTableDef;
end;

function TTableCollectionItem.GetTableName: String;
begin
  Result := GetTableDef.Name;
end;

procedure TTableCollectionItem.SetTableName(const Value: String);
begin
  // Dummy setter
end;

{ TSchemaItemsCollection }

procedure TSchemaItemsCollection.Compare(Dest: TSchemaItemsCollection;
  List: TList; ByName: Boolean = False);
var
  Item: TCompareSchemaItem;
  I: Integer;
begin
  { Search in source collection to find mismatching items }
  if Dest = nil then
  begin
    for I := 0 to Count - 1 do
      List.Add(TCompareSchemaItem.Create(Items[I], nil));
    exit;
  end;
  Item := nil;
  try
    for I := 0 to Count - 1 do
    begin
      if Item = nil then
        Item := TCompareSchemaItem.Create(nil, nil);
      with Item do
      begin
        SrcObj := Items[I];
        DestObj := Dest.LocateItem(SrcItem, ByName);
        if not SrcItem.Compare(Item) then
        begin
          List.Add(Item);
          Item := nil;
        end;
      end;
    end;

    { Search in dest collection to find ones missing in source }
    for I := 0 to Dest.Count - 1 do
    begin
      if Item = nil then
        Item := TCompareSchemaItem.Create(nil, nil);
      with Item do
      begin
        DestObj := Dest.Items[I];
        SrcObj := LocateItem(DestItem, ByName);
        if SrcObj = nil then
        begin
          // Must swap items before comparing
          SrcObj := DestItem;
          DestObj := nil;
          SrcItem.Compare(Item);
          Item.SwapSrcDest;
          List.Add(Item);
          Item := nil;
        end;
      end;
    end;
  finally
    Item.Free;
  end;
end;

function TSchemaItemsCollection.Find(
  const Name: String): TSchemaCollectionItem;
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I >= 0 then
    Result := TSchemaCollectionItem(Items[I])
  else Result := nil;
end;

function TSchemaItemsCollection.FindByItemID(
  ItemID: Integer): TSchemaCollectionItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.ItemID = ItemID then exit;
  end;
  Result := nil;
end;

function TSchemaItemsCollection.GetSchema: TDatabaseSchema;
begin
  Result := TDatabaseSchema(GetOwner);
end;

function TSchemaItemsCollection.IndexOf(const AName: String): Integer;
begin
  for Result := 0 to Count - 1 do
    if AnsiSameText(TSchemaCollectionItem(Items[Result]).Name, AName) then Exit;
  Result := -1;
end;

function TSchemaItemsCollection.GetItem(Index: Integer): TSchemaCollectionItem;
begin
  Result := TSchemaCollectionItem(inherited GetItem(Index));
end;

procedure TSchemaItemsCollection.SetItem(Index: Integer;
  const Value: TSchemaCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TSchemaItemsCollection.Add: TSchemaCollectionItem;
begin
  Result := TSchemaCollectionItem(inherited Add);
end;

function TSchemaItemsCollection.GetAutoName(AItem: TSchemaCollectionItem; const Template: String = '<N>'): String;
var
  Cnt: Integer;
  S: String;
  Temp: TObject;
  CntExist: boolean;
begin
  Result := Template;
  CntExist := AnsiPos('<N>', Template) > 0;
  if CntExist then
    S := Template else
    S := Template+'<N>';
  Cnt := 1;
  repeat
    if not CntExist and (Cnt = 1) then
      Result := AnsiReplaceText(S, '<N>', '') else
      Result := AnsiReplaceText(S, '<N>', IntToStr(Cnt));
    Temp := Find(Result);
    Inc(Cnt);
  until (Temp = nil) or (Temp = AItem);
end;

{  if Template = '' then
    S := GetSchemaClassName+'_<N>' else
    if AnsiPos('<N>', Template) < 1 then
      S := Template+'_<N>' else
      S := Template;}


      {
  if AnsiPos('<N>', Template) = 0 then
    Exit;
  Cnt := 1;
  repeat
    Result := AnsiReplaceText(Template, '<N>', IntToStr(Cnt));
    Inc(Cnt);
  until Find(Result) = nil;

      }

function TSchemaItemsCollection.LocateItem(
  AnItem: TSchemaCollectionItem; ByName: Boolean = True): TSchemaCollectionItem;
var
  I: Integer;
  TempClassName: String;
begin
  TempClassName := AnItem.GetSchemaClassName;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if ((TempClassName = '') or AnsiSameText(Result.GetSchemaClassName, TempClassName)) and
      ((ByName and AnsiSameText(Result.Name, AnItem.Name))
      or (not ByName and (Result.ItemID = AnItem.ItemID)))
    then exit;
  end;
  Result := nil;
end;

procedure TSchemaItemsCollection.CopyFrom(ASource: TSchemaItemsCollection);
var
  I: Integer;
  Item: TSchemaCollectionItem;
begin
  I := 0;
  while I < Count do
  begin
    Item := ASource.Find(Items[I].Name);
    if Item = nil then
      Items[I].Free
    else begin
      Items[I].Assign(Item);
      Inc(I);
    end;
  end;
  for I := 0 to ASource.Count - 1 do
    if Find(ASource[I].Name) = nil then
      Add.Assign(ASource[I]);
end;

{$HINTS OFF}
type
  TPrivateCollection = class(TPersistent)
  private
    FItemClass: TCollectionItemClass;
    FItems: TList;
  end;
{$HINTS ON}

function CompareSchemaItems(Item1, Item2: Pointer): Integer;
begin
  Result := AnsiCompareText(TSchemaCollectionItem(Item1).Name, TSchemaCollectionItem(Item2).Name);
end;

procedure TSchemaItemsCollection.Sort(CompareFunc: TListSortCompare = nil);
begin
  BeginUpdate;
  try
    if not Assigned(CompareFunc) then
      CompareFunc := @CompareSchemaItems;
    TPrivateCollection(Self).FItems.Sort(CompareFunc);
  finally
    EndUpdate;
  end;
end;

{ TTableDefItemsCollection }

procedure TTableDefItemsCollection.AssignTo(Dest: TPersistent);
begin
  if Dest.InheritsFrom(TStrings) then
    GetValues(Self, TStrings(Dest))
  else inherited;
end;

function TTableDefItemsCollection.GetSchema: TDatabaseSchema;
begin
  Result := TTableDefinitions(GetTableDef.Collection).GetSchema;
end;

function TTableDefItemsCollection.GetTableDef: TTableDefinition;
begin
  Result := TTableDefinition(GetOwner);
end;

{ TDatabaseUpdateItem = class (TCollectionItem) }

constructor TDatabaseUpdate.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSQLScript := '';
  FDescription := '';
  FIgnoreSQLError := False;
  FIterate := False;
end;

destructor TDatabaseUpdate.Destroy;
begin
  inherited Destroy;
end;

function TDatabaseUpdate.GetDisplayName: String;
begin
  Result := VersionToStr(Version);
  if FDescription <> '' then
    Result := Result + ' ' + FDescription
  else Result := Result;
end;

procedure TDatabaseUpdate.Assign(Source: TPersistent);
begin
  if Source.InheritsFrom(TDatabaseUpdate) then
  begin
    SQLScript := TDatabaseUpdate(Source).SQLScript;
    IgnoreSQLError := TDatabaseUpdate(Source).IgnoreSQLError;
    Version := TDatabaseUpdate(Source).Version;
    Iterate := TDatabaseUpdate(Source).Iterate;
  end;
  inherited Assign(Source);
end;

function TDatabaseUpdate.GetVersionLabel: String;
begin
  Result := VersionToStr(FVersion);
end;

procedure TDatabaseUpdate.SetVersionLabel(const Value: String);
begin
  Version := StrToVersion(Value);
end;

procedure TDatabaseUpdate.SetVersion(const Value: TSchemaVersion);
begin
  FVersion := Value;
  Changed(False);
end;

function TDatabaseUpdate.GetDisplayLabel: String;
begin
  Result := 'Version update #'+ VersionLabel;
end;

{ TDatabaseUpdates = class (TCollection) }

function TDatabaseUpdates.Add: TDatabaseUpdate;
begin
  Result := TDatabaseUpdate(inherited Add);
end;

function TDatabaseUpdates.GetItem(Index: Integer): TDatabaseUpdate;
begin
  Result := TDatabaseUpdate(inherited GetItem(Index));
end;

procedure TDatabaseUpdates.SetItem(Index: Integer; Value: TDatabaseUpdate);
begin
  inherited SetItem(Index, Value);
end;

procedure TDatabaseUpdates.Update(Item: TCollectionItem);
var
  Schema: TDatabaseSchema;
begin
  Schema := GetSchema;
  if Schema <> nil then
    Schema.UpdateVersion;
  inherited;
end;

{ TDomain }

constructor TDomain.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := '';
  FSize := 0;
  FDataType := ftUnknown;
  FSQLFieldType := '';
  FDescription := '';
end;

destructor TDomain.Destroy;
begin
  inherited Destroy;
end;

procedure TDomain.Assign(Source: TPersistent);
begin
  if Source.InheritsFrom(TDomain) then
  begin
    FDataType := TDomain(Source).FDataType;
    FSQLFieldType := TDomain(Source).FSQLFieldType;
    FSize := TDomain(Source).FSize;
  end;
  inherited Assign(Source);
end;

procedure TDomain.SetDataType(Value: TFieldDataType);
begin
  FDataType := Value;
end;

procedure TDomain.SetSQLFieldType(const Value: String);
var
  Temp: String;
begin
  Temp := ExtractObjectName(Value);
  if FSQLFieldType <> Temp then
  begin
    FSQLFieldType := Temp;
    Schema.DoSQLFieldTypeChanged(Self, FSQLFieldType);
  end;
end;

function TDomain.GetSchemaClassName: String;
begin
  Result := 'Domain';
end;

procedure TDomain.ObjectRenamed(const OldName: String);
var
  I, J: Integer;
begin
  if OldName = '' then exit;
  // Rename it for all fields
  with Schema do
  for I := 0 to TableDefs.Count - 1 do
    for J := 0 to TableDefs[I].FieldDefs.Count - 1 do
    with TableDefs[I].FieldDefs[J] do
      if AnsiSameText(Domain, OldName) then
        Domain := Self.Name;
end;

procedure TDomain.UpdateFields;
var
  I, J: Integer;
begin
  with Schema do
  for I := 0 to TableDefs.Count - 1 do
    for J := 0 to TableDefs[I].FieldDefs.Count - 1 do
    with TableDefs[I].FieldDefs[J] do
      if AnsiSameText(Domain, Self.Name) then
        AssignFromDomain;
end;

function TDomain.IsReferenced: Boolean;
var
  I, J: Integer;
begin
  Result := True;
  with Schema do
  for I := 0 to TableDefs.Count - 1 do
    for J := 0 to TableDefs[I].FieldDefs.Count - 1 do
    with TableDefs[I].FieldDefs[J] do
      if AnsiSameText(Domain, Self.Name) then
        exit;
  Result := False;
end;

{ TDomains }

function TDomains.Add: TDomain;
begin
  Result := TDomain(inherited Add);
end;

function TDomains.Find(const Name: String): TDomain;
begin
  Result := TDomain(inherited Find(Name));
end;

function TDomains.GetDomain(Index: Integer): TDomain;
begin
  Result := TDomain(inherited GetItem(Index));
end;

procedure TDomains.SetDomain(Index: Integer; Value: TDomain);
begin
  inherited SetItem(Index, Value);
end;

{ TSequence }

procedure TSequence.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source.InheritsFrom(TSequence) then
  begin
    FIncrement := TSequence(Source).FIncrement;
    FInitialValue := TSequence(Source).FInitialValue;
  end;
end;

function TSequence.GetSchemaClassName: String;
begin
  Result := 'Sequence';
end;

{ TSequences }

function TSequences.Add: TSequence;
begin
  Result := TSequence(inherited Add);
end;

function TSequences.Find(const Name: String): TSequence;
begin
  Result := TSequence(inherited Find(Name));
end;

function TSequences.GetSequence(Index: Integer): TSequence;
begin
  Result := TSequence(inherited GetItem(Index));
end;

procedure TSequences.SetSequence(Index: Integer; Value: TSequence);
begin
  inherited SetItem(Index, Value);
end;

{ TColumnDef }

function TColumnDef.GetDisplayName: String;
begin
  // TODO: Specify Display Name
  Result := Name;
  if Result = '' then
    Result := FSourceTable + '.' + FSourceField;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TColumnDef.Assign(Source: TPersistent);
begin
  if Source is TColumnDef then
  begin
    // TODO: Assign Properties
    inherited Assign(Source);
    FSourceTable := TColumnDef(Source).FSourceTable;
    FSourceField := TColumnDef(Source).FSourceField;
    FDataType := TColumnDef(Source).FDataType;
  end else
    inherited Assign(Source);
end;

procedure TColumnDef.SetDataType(const Value: TFieldDataType);
begin
  FDataType := Value;
end;

procedure TColumnDef.SetSourceField(const Value: String);
begin
  if FSourceField <> Value then
  begin
    FSourceField := Value;
    UpdateDataType;
  end;
end;

procedure TColumnDef.SetSourceTable(const Value: String);
begin
  if FSourceTable <> Value then
  begin
    FSourceTable := Value;
    UpdateDataType;
  end;
end;

procedure TColumnDef.UpdateDataType;
var
  SrcTable: TTableDefinition;
  SrcField: TFieldDefinition;
begin
  SrcTable := Schema.TableDefs.Find(FSourceTable);
  if SrcTable = nil then exit;
  SrcField := SrcTable.FieldDefs.Find(SourceField);
  if SrcField = nil then exit;
  FDataType := SrcField.DataType;
end;

function TColumnDef.Compare(Dest: TCompareSchemaItem): Boolean;
begin
  Result := inherited Compare(Dest)
    and (FSourceTable = TColumnDef(Dest.DestItem).FSourceTable)
    and (FSourceField = TColumnDef(Dest.DestItem).FSourceField)
    and (FDataType = TColumnDef(Dest.DestItem).FDataType);
  Dest.PropsEqual := Result;
end;

function TColumnDef.GetSchemaClassName: String;
begin
  Result := 'Column';
end;

{ TColumnDefs }

function TColumnDefs.Add: TColumnDef;
begin
  Result := TColumnDef(inherited Add);
end;

function TColumnDefs.GetItem(Index: Integer): TColumnDef;
begin
  Result := TColumnDef(inherited GetItem(Index));
end;

procedure TColumnDefs.SetItem(Index: Integer; Value: TColumnDef);
begin
  inherited SetItem(Index, Value);
end;

{ TViewDefinition }

constructor TViewDefinition.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDefinition := TStringList.Create;
  FColumnDefs := TColumnDefs.Create(Self, TColumnDef);
end;

destructor TViewDefinition.Destroy;
begin
  FColumnDefs.Free;
  FDefinition.Free;
  inherited;
end;

procedure TViewDefinition.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source.InheritsFrom(TViewDefinition) then
  begin
    FDefinition.Assign(TViewDefinition(Source).FDefinition);
    FColumnDefs.Assign(TViewDefinition(Source).FColumnDefs);
  end;
end;

function TViewDefinition.Compare(Dest: TCompareSchemaItem): Boolean;
begin
  Result := inherited Compare(Dest)
    and FDefinition.Equals(TViewDefinition(Dest.DestItem).FDefinition);
  Dest.PropsEqual := Result;
end;

function TViewDefinition.GetSchemaClassName: String;
begin
  Result := 'View';
end;

procedure TViewDefinition.SetDefinition(const Value: TStrings);
begin
  FDefinition.Assign(Value);
end;

procedure TViewDefinition.UpdateDefinition(const Value: String);
begin
  if FDefinition.Count > 0 then
    FDefinition.Insert(0, 'CREATE VIEW ' + Schema.FormatName(Name, Value));
end;

procedure TViewDefinition.SetPropValue(const PropName, Value: String);
begin
  if AnsiSameText(PropName, 'AddDefinition') then
    FDefinition.Text := FDefinition.Text + Value
  else inherited;
end;

procedure TViewDefinition.SetColumnDefs(const Value: TColumnDefs);
begin
  FColumnDefs.Assign(Value);
end;

{ TViewDefinitions }

function TViewDefinitions.Add: TViewDefinition;
begin
  Result := TViewDefinition(inherited Add);
end;

function TViewDefinitions.Find(const Name: String): TViewDefinition;
begin
  Result := TViewDefinition(inherited Find(Name));
end;

function TViewDefinitions.GetViewDefinition(
  Index: Integer): TViewDefinition;
begin
  Result := TViewDefinition(inherited GetItem(Index));
end;

procedure TViewDefinitions.SetViewDefinition(Index: Integer;
  Value: TViewDefinition);
begin
  inherited SetItem(Index, Value);
end;

{ TStoredProcDefinition }

procedure TStoredProcDefinition.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source.InheritsFrom(TStoredProcDefinition) then
  begin
    FDefinition.Assign(TStoredProcDefinition(Source).FDefinition);
    FIsFunction := TStoredProcDefinition(Source).FIsFunction;
  end;
end;

function TStoredProcDefinition.Compare(Dest: TCompareSchemaItem): Boolean;
begin
  Result := inherited Compare(Dest)
    and FDefinition.Equals(TStoredProcDefinition(Dest.DestItem).FDefinition)
    and (FIsFunction = TStoredProcDefinition(Dest.DestItem).FIsFunction);
  Dest.PropsEqual := Result;
end;

constructor TStoredProcDefinition.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDefinition := TStringList.Create;
end;

destructor TStoredProcDefinition.Destroy;
begin
  FDefinition.Free;
  inherited Destroy;
end;

function TStoredProcDefinition.GetSchemaClassName: String;
begin
  Result := 'StoredProc';
end;

procedure TStoredProcDefinition.SetDefinition(const Value: TStrings);
begin
  FDefinition.Assign(Value);
end;

procedure TStoredProcDefinition.SetPropValue(const PropName,
  Value: String);
begin
  if AnsiSameText(PropName, 'AddDefinition') then
    FDefinition.Text := FDefinition.Text + Value
  else inherited;
end;

procedure TStoredProcDefinition.UpdateDefinition(const Value: String);
begin
  if FDefinition.Count = 0 then exit;
  if IsFunction then
    FDefinition.Insert(0, 'CREATE FUNCTION ' + Schema.FormatName(Name, Value))
  else FDefinition.Insert(0, 'CREATE PROCEDURE ' + Schema.FormatName(Name, Value));
end;

{ TStoredProcDefinitions }

function TStoredProcDefinitions.Add: TStoredProcDefinition;
begin
  Result := TStoredProcDefinition(inherited Add);
end;

function TStoredProcDefinitions.Find(
  const Name: String): TStoredProcDefinition;
begin
  Result := TStoredProcDefinition(inherited Find(Name));
end;

function TStoredProcDefinitions.GetStoredProcDefinition(
  Index: Integer): TStoredProcDefinition;
begin
  Result := TStoredProcDefinition(inherited GetItem(Index));
end;

procedure TStoredProcDefinitions.SetStoredProcDefinition(Index: Integer;
  Value: TStoredProcDefinition);
begin
  inherited SetItem(Index, Value);
end;

{ TFieldDefinition }

constructor TFieldDefinition.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := '';
  // FOldName := '';
  // FFieldNo := 0;
  FIdentity := False;
  // FInternalCalcField := False;
  FPrecision := 0;
  FAttributes := [];
  FDataType := ftUnknown;
  FDisplayFormat := '';
  FDefaultExpression := '';
  DisplayWidth := 0;
  FDescription := '';
  FComputeAs := '';
end;

destructor TFieldDefinition.Destroy;
begin
  //DB+++
  if TableDef <> nil then
    TableDef.Schema.UnPrepare;
  //DB++  
  {TableDef.Schema.UnPrepare;}
  inherited Destroy;
end;

procedure TFieldDefinition.Assign(Source: TPersistent);
var
  S: TFieldDefinition;
  S1: TFieldDef;
  N: TNamedItem;
begin
  if Source is TFieldDefinition then
  begin
    if Collection <> nil then
      Collection.BeginUpdate;
    try
      inherited Assign(Source);
      S := TFieldDefinition(Source);

      // FOldName: String;
      // FFieldNo: Integer;
      FRelationName := S.FRelationName;
      FAggregateExpression := S.FAggregateExpression;
      FAggregateType := S.FAggregateType;
      FEditMask := S.FEditMask;
      FEnumeration := S.FEnumeration;
      FDataType := S.DataType;
      FSQLFieldType := S.FSQLFieldType;
      FSize := S.Size;
      FPrecision := S.Precision;
      FAttributes := S.Attributes;
      FDomain := S.Domain;
      // FInternalCalcField := S.InternalCalcField;
      FComputeAs := S.FComputeAs;
      FDisplayFormat := S.DisplayFormat;
      FDefaultExpression := S.DefaultExpression;
      FDisplayWidth := S.DisplayWidth;
      FIdentity := S.Identity;
    finally
      if Collection <> nil then
        Collection.EndUpdate;
    end;
  end else if Source is TFieldDef then
  begin
    if Collection <> nil then
      Collection.BeginUpdate;
    try
      S1 := TFieldDef(Source);
      {FieldNo is defaulted}
      Name := S1.Name;
      DataType := VCLToFieldDataType[S1.DataType];
      Size := S1.Size;
      Precision := S1.Precision;
      Attributes := S1.Attributes;
      // InternalCalcField := S1.InternalCalcField;
      ComputeAs := '';
    finally
      if Collection <> nil then
        Collection.EndUpdate;
    end;
  end else if Source is TNamedItem then
  begin
    if Collection <> nil then
      Collection.BeginUpdate;
    try
      N := TNamedItem(Source);
      {FieldNo is defaulted}
      Name := N.Name;
      DataType := TFieldDataType(GetOrdProp(N, 'DataType'));
      Size := GetOrdProp(N, 'Size');
      Attributes := TFieldAttributes(Byte(GetOrdProp(N, 'Attributes')));
    finally
      if Collection <> nil then
        Collection.EndUpdate;
    end;
  end else inherited;
end;

procedure TFieldDefinition.AssignTo(Dest: TPersistent);
var
  D: TFieldDef;
  N: TNamedItem;
begin
  if Dest is TFieldDef then
  begin
    D := TFieldDef(Dest);
    if D.Collection <> nil then
      D.Collection.BeginUpdate;
    try
      D.Name := Name;
      D.DataType := FieldDataTypeToVCL[DataType];
      D.Size := Size;
      D.Precision := Precision;
      D.Attributes := Attributes;
      D.InternalCalcField := IsComputed;
    finally
      if D.Collection <> nil then
        D.Collection.EndUpdate;
    end;
  end else if Dest is TNamedItem then
  begin
    N := TNamedItem(Dest);
    if N.Collection <> nil then
      N.Collection.BeginUpdate;
    try
      {FieldNo is defaulted}
      N.Name := Name;
      SetOrdProp(N, 'DataType', Integer(DataType));
      SetOrdProp(N, 'Size', Size);
      SetOrdProp(N, 'Attributes', Byte(Attributes));
    finally
      if N.Collection <> nil then
        N.Collection.EndUpdate;
    end;
  end else inherited;
end;

procedure TFieldDefinition.SetDataType(const Value: TFieldDataType);
begin
  if FDataType <> Value then
  begin
    FDataType := Value;
    // FPrecision := 0;
    case Value of
      ftNChar,
      ftFixedChar,
      ftWideString,
      ftString: if FSize = 0 then FSize := 20;
      ftBCD: if FSize = 0 then FSize := 4;
      ftBytes, ftVarBytes: if FSize = 0 then FSize := 16;
      ftArray: if FSize = 0 then FSize := 10;
      else FSize := 0;
    end;

    // MB: 2.14 - this is not necessary. We don't rely on DataType anyway, so
    // there's no reason to reset domain when assigning it.
    // This fixes a problem with resetting domain names to '' after reverse
    // engineering in EndUpdate call, followed by updating DataTypes

    // if not (csLoading in Schema.ComponentState) then
    //  FDomain := '';
  end;
end;

function TFieldDefinition.GetRequired: Boolean;
begin
  Result := faRequired in Attributes;
end;

procedure TFieldDefinition.SetRequired(const Value: Boolean);
begin
  if Value then
    Attributes := Attributes + [faRequired]
  else Attributes := Attributes - [faRequired];
end;

function TFieldDefinition.GetDisplayLabel: String;
begin
  if FDescription <> '' then
    Result := FDescription
  else Result := FName;
end;

procedure TFieldDefinition.ObjectRenamed(const OldName: String);
var
  I, J: Integer;
begin
  if OldName = '' then exit;
  // Update field name in object declaration
  with TableDef do
    FObjectKeyFields := ReplaceFieldName(FObjectKeyFields, OldName, Name);

  // Update field name in indexes
  with TableDef.IndexDefs do
  for I := 0 to Count - 1 do
  begin
    Items[I].Fields := ReplaceFieldName(Items[I].Fields, OldName, Name);
    Items[I].DescFields := ReplaceFieldName(Items[I].DescFields, OldName, Name);
  end;

  // Update field name in own relations
  with TableDef.Relations do
  for I := 0 to Count - 1 do
    Items[I].KeyFields := ReplaceFieldName(Items[I].KeyFields, OldName, Name);

  // Update field name in foreign relations
  with TableDef.Schema.TableDefs do
  for I := 0 to Count - 1 do
    with Items[I].Relations do
    for J := 0 to Count - 1 do
      if AnsiSameText(Self.TableDef.TableName, Items[J].ForeignTable) then
        Items[J].ForeignKeyFields := ReplaceFieldName(Items[J].ForeignKeyFields, OldName, Name);

  TableDef.Schema.Prepared := False;
end;

function TFieldDefinition.GetDependances: TFieldDependances;
var
  I: Integer;
begin
  Result := [];
  with TableDef do
  begin
    for I := 0 to IndexDefs.Count - 1 do
      if IndexDefs[I].HasField(Self.Name) then
      begin
        if ixPrimary in IndexDefs[I].Options then
          Include(Result, fdPrimaryKey);
        Include(Result, fdIndex);
        if Result = [fdPrimaryKey, fdIndex] then break;
      end;
    for I := 0 to Relations.Count - 1 do
      if Relations[I].RequireOneRecord then { RelationType = rtManyToOne }
        if AnsiPos(','+Self.Name+',', ','+Relations[I].KeyFields+',') > 0 then
        begin
          Include(Result, fdForeighnKey);
          break;
        end;
  end;
end;

procedure TFieldDefinition.SetAggregateType(const Value: TAggregateType);
begin
  if FAggregateType <> Value then
  begin
    TableDef.Schema.Prepared := False;
    FAggregateType := Value;
  end;
end;

procedure TFieldDefinition.SetAggregateExpression(const Value: String);
begin
  if FAggregateExpression <> Value then
  begin
    TableDef.Schema.Prepared := False;
    FAggregateExpression := Value;
  end;
end;

procedure TFieldDefinition.SetRelationName(const Value: String);
begin
  if FRelationName <> Value then
  begin
    TableDef.Schema.Prepared := False;
    FRelationName := Value;
  end;
end;

function TFieldDefinition.GetDisplayFieldTypeSize: String;
var
  Scale, FldSize: String;
begin
  if Domain <> '' then
    Result := Domain
  else
  begin
    if FSQLFieldType <> '' then
      Result := FSQLFieldType else
      Result := FieldDataTypes[DataType];

    case DataType of
      ftFixedChar,
      ftNChar,
      ftWideString,
      ftString: Result := Result + Format('(%d)', [Size]);

      ftFMTBcd, ftBCD,
      ftVarBytes, ftBytes,
      ftADT, ftArray: if Size > 0 then Result := Result + Format('(%d)', [Size]);

      ftFloat: begin
        FldSize := '';
        if Precision > 0 then
          FldSize := IntToStr(Precision);
        Scale := GetPropValue('Scale');
        if Scale <> '' then
        begin
          if FldSize <> '' then
            FldSize := FldSize + ',';
          FldSize := FldSize + Scale;
        end;
        if FldSize <> '' then
          Result := Result + Format('(%s)', [FldSize]);
      end;
    end;
  end;
end;

procedure TFieldDefinition.SetSQLFieldType(const Value: String);
var
  TestDomain: TDomain;
  Temp: String;
begin
  Temp := ExtractObjectName(Value);
  if Temp <> FSQLFieldType then
  begin
    TestDomain := Schema.Domains.Find(Temp);
    if TestDomain <> nil then
      Domain := Temp
    else begin
      FSQLFieldType := Temp;
      Schema.DoSQLFieldTypeChanged(Self, FSQLFieldType);
    end;
  end;
end;

procedure TFieldDefinition.SetDomain(const Value: String);
begin
  if FDomain <> Value then
  begin
    if AnsiSameText(FDomain, Value) then
      FDomain := Value
    else begin
      FDomain := Value;
      if not (csLoading in Schema.ComponentState) then
        AssignFromDomain;
    end;
  end;
end;

procedure TFieldDefinition.AssignFromDomain;
var
  Domain: TDomain;
  I: Integer;
  SaveDomain, PropValue: String;
begin
  if FDomain = '' then exit;
  Domain := Schema.Domains.Find(FDomain);
  if Domain = nil then exit;
  SaveDomain := FDomain;
  try
    FSQLFieldType := Domain.SQLFieldType;
    DataType := Domain.DataType;
    Size := Domain.Size;
    for I := 0 to Domain.Props.Count - 1 do
    begin
      PropValue := Trim(ValueFromIndex(Domain.Props, I));
      if PropValue <> '' then
        SetPropValue(Domain.Props.Names[I], PropValue);
    end;
  finally
    FDomain := SaveDomain;
  end;
end;

function TFieldDefinition.GetSchemaClassName: String;
begin
  if IsComputed then
    Result := 'ComputedField' else
    Result := 'Field';
end;

function TFieldDefinition.IsInheritedProp(const PropName: String): Boolean;
var
  DomainDef: TDomain;
begin
  Result := False;
  if Domain <> '' then
  begin
    DomainDef := Schema.Domains.Find(Domain);
    if DomainDef <> nil then
      Result := AnsiSameText(PropName, 'FieldType') or (DomainDef.Props.Values[PropName] <> '');
  end;
end;

procedure TFieldDefinition.SetIsPrimaryKey(const Value: Boolean);
var
  PkIdx: TIndexDefinition;
  IdxFld: TIndexField;
  ConstrName: String;
begin
  if Value then
  begin
    PkIdx := TableDef.GetPrimaryIndex;
    if PkIdx = nil then
    begin
      PkIdx := TableDef.IndexDefs.Add;
      ConstrName := ExtractObjectName(Props.Values['constraintname']);
      if ConstrName = '' then
        ConstrName := 'PK_' + TableName;
      PkIdx.Name := ConstrName;
      PkIdx.Options := [ixPrimary, ixUnique];
    end;
    PkIdx.IndexFields.Add.Name := Name;
    Props.Values['constraintname'] := '';
    Required := True;
  end else begin
    PkIdx := TableDef.GetPrimaryIndex;
    IdxFld := nil;
    if PkIdx <> nil then
      IdxFld := TIndexField(PkIdx.IndexFields.Find(Name));
    if IdxFld <> nil then
      IdxFld.Free;
    if (PkIdx <> nil) and (PkIdx.IndexFields.Count = 0) then
      PkIdx.Free;
  end;
end;

function TFieldDefinition.GetIsPrimaryKey: Boolean;
begin
  Result := fdPrimaryKey in GetDependances;
end;

function TFieldDefinition.GetIsUnique: Boolean;
var
  I: Integer;
begin
  for I := 0 to TableDef.IndexDefs.Count - 1 do
  with TableDef.IndexDefs[I] do
    if Unique and not (ixPrimary in Options) and AnsiSameText(Fields, Self.Name) then
    begin
      Result := True;
      exit;
    end;
  Result := False;
end;

procedure TFieldDefinition.SetIsUnique(const Value: Boolean);
var
  I: Integer;
  IdxDef: TIndexDefinition;
  ConstrName: String;
begin
  if Value <> IsUnique then
  begin
    if Value then
    begin
      IdxDef := TableDef.IndexDefs.Add;
      ConstrName := ExtractObjectName(Props.Values['constraintname']);
      if ConstrName <> '' then
        IdxDef.Name := ConstrName
      else IdxDef.Name := IdxDef.GetAutoName(Self.Name);
      IdxDef.Options := [ixUnique];
      IdxDef.IndexFields.Add.Name := Name;
      Props.Values['constraintname'] := '';
    end else begin
      I := 0;
      while I < TableDef.IndexDefs.Count do
        with TableDef.IndexDefs[I] do
          if Unique and not (ixPrimary in Options) and AnsiSameText(Fields, Self.Name) then
            Free
          else Inc(I);
    end;
  end;
end;

function TFieldDefinition.GetPropValue(const PropName: String): String; 
var
  Idx: Integer;
begin
  if AnsiSameText(PropName, 'prevfieldname') then
  begin
    Idx := Index;
    if Idx = 0 then Result := ''
    else Result := TableDef.FieldDefs[Idx - 1].Name;
  end else
    Result := inherited GetPropValue(PropName);
end;

procedure TFieldDefinition.SetPropValue(const PropName, Value: String);
var
  Relationship: TRelationship;
  FkFields, ConstrName: String;
  PrevField: TFieldDefinition;
begin
  if AnsiSameText(PropName, 'fk_add') then
  begin
    Relationship := Schema.Relationships.AddRelationship(TableDef.Name, '');
    Relationship.BeginUpdate;
    try
      ConstrName := ExtractObjectName(Props.Values['constraintname']);
      if ConstrName <> '' then
        Relationship.DetailRelationName := ConstrName else
        Relationship.DetailRelationName := TableDef.Relations.GetAutoName(nil, 'Relation'+Name+'<N>');
      Relationship.DetailKeyFields := Name;
      Relationship.MasterTableName := ExtractObjectName(Props.Values['fk_table']);
      FkFields := ExtractObjectName(Props.Values['fk_field']);
      if FkFields <> '' then
        Relationship.MasterKeyFields := FkFields;

      Relationship.EnforceForeignKey := True;
      Relationship.DetailCardinality := dcMany;

      Relationship.SetPropValue('deleteaction', Props.Values['deleteaction']); // fk_
      Relationship.SetPropValue('updateaction', Props.Values['updateaction']);
      Relationship.SetPropValue('notforreplication', Props.Values['notforreplication']);
    finally
      Relationship.EndUpdate;
    end;
    Props.Values['constraintname'] := '';
    Props.Values['fk_table'] := '';
    Props.Values['fk_field'] := '';
    Props.Values['deleteaction'] := '';
    Props.Values['updateaction'] := '';
    Props.Values['notforreplication'] := '';
  end else if AnsiSameText(PropName, 'prevfieldname') then
  begin
    PrevField := TableDef.FieldDefs.Find(Value);
    if PrevField <> nil then
      Index := PrevField.Index + 1
    else Index := 0;
  end else
    inherited;
end;

function TFieldDefinition.GetIdentity: Boolean;
begin
  if FDataType = ftAutoInc then
    Result := True
  else Result := FIdentity;
end;

procedure TFieldDefinition.SetIdentity(const Value: Boolean);
begin
  if FDataType <> ftAutoInc then
    FIdentity := Value;
end;

function TFieldDefinition.GetIsComputed: Boolean;
begin
  Result := Trim(FComputeAs) <> '';
end;

procedure TFieldDefinition.SetComputeAs(const Value: String);
begin
  FComputeAs := Value;
  if (FComputeAs <> '') and (FDataType = ftUnknown) then
    DataType := ftFloat; { assign default data type for computed fields }
end;

procedure TFieldDefinition.UpdateDomainReference;
var
  D: TDomain;
begin
  if FDomain = '' then exit;
  D := Schema.Domains.Find(FDomain);
  if D <> nil then
    FDomain := D.Name;
end;
{
function TFieldDefinition.GetAutoName(const Prefix: String): String;
begin
  Result := TSchemaItemsCollection(Collection).GetAutoName(GetSchemaClassName, Prefix);
end;
}
{ TIndexField }

procedure TIndexField.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source.InheritsFrom(TIndexField) then
  begin
    FDescending := TIndexField(Source).FDescending;
    FCaseInsensitive := TIndexField(Source).FCaseInsensitive;
  end;
end;

function TIndexField.Compare(Dest: TCompareSchemaItem): Boolean;
begin
  Result := Assigned(Dest) and (Dest.DestItem <> nil)
    and (ItemID = TIndexField(Dest.DestItem).ItemID)
    and Props.Equals(TIndexField(Dest.DestItem).Props)
    and (Description = TIndexField(Dest.DestItem).Description)
    and (CaseInsensitive = TIndexField(Dest.DestItem).CaseInsensitive)
    and (Descending = TIndexField(Dest.DestItem).Descending)
    // We need to compare ItemID of the associated fields if found!
    and (GetFieldID = TIndexField(Dest.DestItem).GetFieldID);

  Dest.PropsEqual := Result;
end;

function TIndexField.GetFieldID: Integer;
var
  Fld: TFieldDefinition;
begin
  Fld := IndexDef.TableDef.FieldDefs.Find(Name);
  if Fld <> nil then
    Result := Fld.ItemID
  else Result := 0;
end;

function TIndexField.GetIndexDef: TIndexDefinition;
begin
  Result := TIndexFields(Collection).GetIndexDef;
end;

function TIndexField.GetItemID: Integer;
begin
  Result := Index + 1;
end;

function TIndexField.GetSchemaClassName: String;
begin
  Result := 'IndexField';
end;

procedure TIndexField.SetItemID(const Value: Integer);
begin
  // Ignore this value
end;

procedure TIndexField.ValidateRename(const NewName: String);
begin
  // Any index name accepted
end;

{ TIndexFields }

function TIndexFields.GetTableDef: TTableDefinition;
begin
  Result := GetIndexDef.GetTableDef;
end;

function TIndexFields.GetIndexDef: TIndexDefinition;
begin
  Result := TIndexDefinition(GetOwner);
end;

function TIndexFields.Add: TIndexField;
begin
  Result := TIndexField(inherited Add);
end;

function TIndexFields.GetIndexField(const Index: Integer): TIndexField;
begin
  Result := TIndexField(inherited GetItem(Index));
end;

procedure TIndexFields.SetIndexField(const Index: Integer; Value: TIndexField);
begin
  inherited SetItem(Index, Value);
end;

(*
procedure TIndexFields.Compare(Dest: TSchemaItemsCollection; List: TList;
  ByName: Boolean);
var
  I: Integer;
  ListsEqual: Boolean;
  DestCompItem: TCompareSchemaItem;
begin
  // This collection must be compared always by name and sequentially
  if Dest = nil then
    inherited Compare(nil, List, True)
  else begin
    ListsEqual := Count = Dest.Count;
    if ListsEqual then
    begin
      DestCompItem := TCompareSchemaItem.Create(nil, nil);
      try
        for I := 0 to Count - 1 do
        begin
          DestCompItem.SrcObj := Items[I];
          DestCompItem.DestObj := Dest[I];
          if not Items[I].Compare(DestCompItem) then
          begin
            ListsEqual := False;
            break;
          end;
        end;
      finally
        DestCompItem.Free;
      end;
    end;
    if not ListsEqual then
    begin
      inherited Compare(nil, List, True);
      TCompareItem.SwapSrcDestList(List);
      Dest.Compare(nil, List, True);
      TCompareItem.SwapSrcDestList(List);
    end;
  end;
end;
*)

{ TIndexDefinition }

constructor TIndexDefinition.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FName := '';
  FOptions := [];
  FIndexFields := TIndexFields.Create(Self, TIndexField);
end;

destructor TIndexDefinition.Destroy;
begin
  inherited;
  FIndexFields.Free;
end;

procedure TIndexDefinition.Assign(Source: TPersistent);
var
  S: TIndexDefinition;
  S1: TIndexDef;
  N: TNamedItem;
  PropInfo: PPropInfo;
begin
  if Source is TIndexDefinition then
  begin
    if Collection <> nil then
      Collection.BeginUpdate;
    try
      inherited Assign(Source);
      S := TIndexDefinition(Source);
      Options := S.Options;
      IndexFields := S.IndexFields;
    finally
      if Collection <> nil then
        Collection.EndUpdate;
    end;
  end else if Source is TIndexDef then
  begin
    if Collection <> nil then
      Collection.BeginUpdate;
    try
      S1 := TIndexDef(Source);
      Options := S1.Options;
      Name := S1.Name;
      Fields := S1.Fields;
      DescFields := S1.DescFields;
      CaseInsFields := S1.CaseInsFields;
    finally
      if Collection <> nil then
        Collection.EndUpdate;
    end;
  end else if Source is TNamedItem then
  begin
    if Collection <> nil then
      Collection.BeginUpdate;
    try
      N := TNamedItem(Source);
      Name := N.Name;
      PropInfo := GetPropInfo(N, 'Fields');
      if PropInfo <> nil then
        Fields := GetStrProp(N, PropInfo);
      PropInfo := GetPropInfo(N, 'DescFields');
      if PropInfo <> nil then
        DescFields := GetStrProp(N, PropInfo);
      PropInfo := GetPropInfo(N, 'CaseInsFields');
      if PropInfo <> nil then
        CaseInsFields := GetStrProp(N, PropInfo);
      PropInfo := GetPropInfo(N, 'Options');
      if PropInfo <> nil then
        Options := TIndexOptions(Byte(GetOrdProp(N, PropInfo)));
    finally
      if Collection <> nil then
        Collection.EndUpdate;
    end;
  end else inherited;
end;

procedure TIndexDefinition.AssignTo(Dest: TPersistent);
var
  D: TIndexDef;
  N: TNamedItem;
  PropInfo: PPropInfo;
begin
  if Dest is TIndexDef then
  begin
    D := TIndexDef(Dest);
    if D.Collection <> nil then
      D.Collection.BeginUpdate;
    try
      D.Options := Options;
      D.Name := Name;
      D.Fields := Fields;
      D.DescFields := DescFields;
      D.CaseInsFields := CaseInsFields;
    finally
      if D.Collection <> nil then
        D.Collection.EndUpdate;
    end;
  end else if Dest is TNamedItem then
  begin
    N := TNamedItem(Dest);
    if N.Collection <> nil then
      N.Collection.BeginUpdate;
    try
      N.Name := Name;
      PropInfo := GetPropInfo(N, 'Fields');
      if PropInfo <> nil then
        SetStrProp(N, PropInfo, Fields);
      PropInfo := GetPropInfo(N, 'Options');
      if PropInfo <> nil then
        SetOrdProp(N, PropInfo, Byte(Options));
      PropInfo := GetPropInfo(N, 'DescFields');
      if PropInfo <> nil then
        SetStrProp(N, PropInfo, DescFields);
      PropInfo := GetPropInfo(N, 'CaseInsFields');
      if PropInfo <> nil then
        SetStrProp(N, PropInfo, CaseInsFields);
    finally
      if N.Collection <> nil then
        N.Collection.EndUpdate;
    end;
  end else inherited;
end;

function TIndexDefinition.HasField(const FieldName: String): Boolean;
begin
  Result := FIndexFields.Find(FieldName) <> nil;
end;

function TIndexDefinition.GetFields: String;
var
  I: Integer;
begin
  // Iterate IndexFields collection
  Result := '';
  for I := 0 to FIndexFields.Count - 1 do
    Result := Result + ';' + FIndexFields[I].Name;
  if Result <> '' then Delete(Result, 1, 1);
end;

function TIndexDefinition.GetCaseInsFields: String;
var
  I: Integer;
begin
  // Iterate IndexFields collection
  Result := '';
  for I := 0 to FIndexFields.Count - 1 do
  if FIndexFields[I].CaseInsensitive then
    Result := Result + ';' + FIndexFields[I].Name;
  if Result <> '' then Delete(Result, 1, 1);
end;

function TIndexDefinition.GetDescFields: String;
var
  I: Integer;
begin
  // Iterate IndexFields collection
  Result := '';
  for I := 0 to FIndexFields.Count - 1 do
  if FIndexFields[I].Descending then
    Result := Result + ';' + FIndexFields[I].Name;
  if Result <> '' then Delete(Result, 1, 1);
end;

function TIndexDefinition.GetFieldProps: String;
var
  I: Integer;
begin
  // Iterate IndexFields collection
  Result := '';
  for I := 0 to FIndexFields.Count - 1 do
    Result := Result + ', ' + FIndexFields[I].Props.CommaText;
  if Result <> '' then Delete(Result, 1, 2);
end;

procedure TIndexDefinition.SetFieldProps(const Value: String);
begin
  // Ignore any input there
end;

procedure TIndexDefinition.SetFields(const Value: String);
var
  I: Integer;
  FieldName: String;
begin
  FIndexFields.Clear;
  I := 1;
  while I <= Length(Value) do
  begin
    FieldName := NextToken(Value, ';', I);
    if FieldName <> '' then
      FIndexFields.Add.Name := FieldName;
  end;
end;

procedure TIndexDefinition.SetCaseInsFields(const Value: String);
var
  I: Integer;
  Field: TIndexField;
  FieldName: String;
begin
  for I := 0 to FIndexFields.Count - 1 do
    FIndexFields[I].CaseInsensitive := False;
  I := 1;
  while I <= Length(Value) do
  begin
    FieldName := NextToken(Value, ';', I);
    if FieldName <> '' then
    begin
      Field := TIndexField(IndexFields.Find(FieldName));
      if Field <> nil then
        Field.CaseInsensitive := True;
    end;
  end;
end;

procedure TIndexDefinition.SetDescFields(const Value: String);
var
  I: Integer;
  Field: TIndexField;
  FieldName: String;
begin
  for I := 0 to FIndexFields.Count - 1 do
    FIndexFields[I].Descending := False;
  I := 1;
  while I <= Length(Value) do
  begin
    FieldName := NextToken(Value, ';', I);
    if FieldName <> '' then
    begin
      Field := TIndexField(IndexFields.Find(FieldName));
      if Field <> nil then
        Field.Descending := True;
    end;
  end;
end;

procedure TIndexDefinition.SetOptions(const Value: TIndexOptions);
begin
  FOptions := Value;
end;

function TIndexDefinition.GetDisplayName: String;
begin
  Result := FName;
  if (Result = '') and (ixPrimary in FOptions) then
    Result := '<Primary>'; { do not localize }
end;

procedure TIndexDefinition.ValidateRename(const NewName: String);
begin
  if (NewName <> '') and not IsUnique(Self, NewName) then
    DatabaseErrorFmt(SDuplicateObjectName, ['Index', NewName]);
end;

function TIndexDefinition.GetDisplayProps: String;
const
  StrOptions: array [TIndexOption] of String = (
    'Primary', 'Unique', 'Descending', 'Case Insensitive',
    'Expression', 'Non Maintained');
var
  I: TIndexOption;
begin
  Result := '';
  for I := Low(TIndexOption) to High(TIndexOption) do
  if I in Options then
  begin
    if Result <> '' then Result := Result + ', ';
    Result := Result + StrOptions[I];
  end;
end;

function TIndexDefinition.GetSchemaClassName: String;
begin
  if ixPrimary in Options then
    Result := 'PrimaryKey'
  else Result := 'Index';
end;

function TIndexDefinition.GetNoCase: Boolean;
begin
  Result := ixCaseInsensitive in Options;
end;

procedure TIndexDefinition.SetNoCase(const Value: Boolean);
begin
  if Value then
    Include(FOptions, ixCaseInsensitive)
  else Exclude(FOptions, ixCaseInsensitive)
end;

function TIndexDefinition.GetUnique: Boolean;
begin
  Result := (ixPrimary in Options) or (ixUnique in Options);
end;

procedure TIndexDefinition.SetUnique(const Value: Boolean);
begin
  if Value then
    Include(FOptions, ixUnique)
  else Exclude(FOptions, ixUnique)
end;

function TIndexDefinition.GetDescending: Boolean;
begin
  Result := ixDescending in Options;
end;

procedure TIndexDefinition.SetDescending(const Value: Boolean);
begin
  if Value then
    Include(FOptions, ixDescending)
  else Exclude(FOptions, ixDescending);
end;

procedure TIndexDefinition.DoAddIndexField(const Value: String);
begin
  if Value <> '' then
    FIndexFields.Add.Name := Value;
end;

procedure TIndexDefinition.DoAddDescField(Value: Boolean);
begin
  if FIndexFields.Count > 0 then
    FIndexFields[FIndexFields.Count - 1].Descending := Value;
end;

function TIndexDefinition.GetDummyBool: Boolean;
begin
  Result := False;
end;

function TIndexDefinition.GetDummyStr: String;
begin
  Result := '';
end;

procedure TIndexDefinition.SetIndexFields(const Value: TIndexFields);
begin
  FIndexFields.Assign(Value);
end;

function TIndexDefinition.Compare(Dest: TCompareSchemaItem): Boolean;
var
  DestIdx: TIndexDefinition;
  ByName: Boolean;
begin
  DestIdx := Dest.DestItem as TIndexDefinition;
  with Dest do
  begin
    ByName := (DestIdx <> nil) and (Schema.SchemaID <> DestIdx.Schema.SchemaID);

    if DestIdx = nil then
      IndexFields.Compare(nil, GetSubItems, ByName)
    else IndexFields.Compare(DestIdx.IndexFields, GetSubItems, ByName);
    if SubItems.Count = 0 then
      FreeAndNil(SubItems);

    PropsEqual := inherited Compare(Dest) and (Options = DestIdx.Options);

    Result := PropsEqual and (SubItems = nil);
  end;
end;


function TIndexDefinition.GetPrimaryKey: Boolean;
begin
  Result := ixPrimary in Options;
end;

procedure TIndexDefinition.SetPrimaryKey(const Value: Boolean);
begin
  if Value <> PrimaryKey then
  begin
    if Value then
      Options := Options + [ixPrimary]
    else Options := Options - [ixPrimary];
  end;
end;
{
function TIndexDefinition.GetAutoName(const Prefix: String): String;
begin
  if PrimaryKey then
    Result := TSchemaItemsCollection(Collection).GetAutoName('PK_' + TableName, Prefix)
  else Result := inherited GetAutoName(Prefix);
end;
}
procedure TIndexDefinition.UpdateFieldReferences;
var
  I: Integer;
  F: TFieldDefinition;
begin
  for I := 0 to FIndexFields.Count - 1 do
  begin
    F := TableDef.FieldDefs.Find(FIndexFields[I].Name);
    if F <> nil then
      FIndexFields[I].FName := F.Name;
  end;
end;

procedure TIndexDefinition.SetDisplayFields(const Value: String);
begin
  // Nothing
end;


{ TIndexDefinitions }

function TIndexDefinitions.Add: TIndexDefinition;
begin
  Result := TIndexDefinition(inherited Add);
end;

procedure TIndexDefinitions.Add(const Name, Fields: String; Options: TIndexOptions);
var
  IndexDef: TIndexDefinition;
begin
  if IndexOf(Name) >= 0 then
    DatabaseErrorFmt(SDuplicateIndexName, [Name], TComponent(GetOwner));

  IndexDef := Add;
  IndexDef.Name := Name;
  IndexDef.Fields := Fields;
  IndexDef.Options := Options;
end;

function TIndexDefinitions.FindIndexForFields(const Fields: String): TIndexDefinition;
begin
  Result := GetIndexForFields(Fields, False);
  if Result = nil then
    DatabaseErrorFmt(SNoIndexForFields, [Fields], TComponent(GetOwner));
end;

function TIndexDefinitions.GetIndexForFields(const Fields: String;
  CaseInsensitive: Boolean): TIndexDefinition;
var
  Exact: Boolean;
  I, L: Integer;
begin
  L := Length(Fields);
  Exact := True;
  while True do
  begin
    for I := 0 to Count - 1 do
    begin
      Result := Items[I];
      if (Result.Options * [ixDescending, ixExpression] = []) and
        (not CaseInsensitive or (ixCaseInsensitive in Result.Options)) then
        if Exact then
          if AnsiSameText(Fields, Result.Fields) then Exit
          else { not exact match }
        else
          if AnsiSameText(Fields, Copy(Result.Fields, 1, L)) and
            ((Length(Result.Fields) = L) or
            (Result.Fields[L + 1] = ';')) then Exit;
    end;
    if not Exact then Break;
    Exact := False;
  end;
  Result := nil;
end;

function TIndexDefinitions.Find(const Name: String): TIndexDefinition;
begin
  Result := TIndexDefinition(inherited Find(Name));
end;

function TIndexDefinitions.GetIndexDef(Index: Integer): TIndexDefinition;
begin
  Result := TIndexDefinition(inherited Items[Index]);
end;

procedure TIndexDefinitions.SetIndexDef(Index: Integer; Value: TIndexDefinition);
begin
  inherited Items[Index] := Value;
end;

{ TRelationships }

function TRelationships.Add: TRelationship;
begin
  Result := TRelationship(inherited Add);
end;

function TRelationships.AddRelationship(const DetailTable, MasterTable: String): TRelationship;
begin
  Result := Add;
  (*
  if MasterTable = '' then
  begin
    Result.DetailTableName := DetailTable;
  end else
  *)
  begin
    Result.BeginUpdate;
    try
      Result.DetailTableName := DetailTable;
      Result.MasterTableName := MasterTable;
      Result.EnforceForeignKey := True;
    finally
      Result.EndUpdate;
    end;
  end;  
end;

function TRelationships.Find(const Name: String): TRelationship;
begin
  Result := TRelationship(inherited Find(Name));
end;

function TRelationships.GetItem(Index: Integer): TRelationship;
begin
  Result := TRelationship(inherited GetItem(Index));
end;

procedure TRelationships.SetItem(Index: Integer;
  const Value: TRelationship);
begin
  inherited SetItem(Index, Value);
end;

{ TRelationship }

constructor TRelationship.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDetailCardinality := dcMany;
  FMasterRecordOptional := True;
  FMasterOwnsDetails := False;
  FDeleteAction := raIgnore;
  FUpdateAction := raIgnore;
  FEnforceForeignKey := True;
  FCaseInsensitive := False;
end;

destructor TRelationship.Destroy;
begin
  if FRelation[sideDetail] <> nil then
    FreeAndNil(FRelation[sideDetail]);
  if FRelation[sideMaster] <> nil then
    FreeAndNil(FRelation[sideMaster]);
  inherited Destroy;
end;

procedure TRelationship.Assign(Source: TPersistent);
begin
  if Source is TRelationship then
  begin
    inherited Assign(Source);
    FTableName := TRelationship(Source).FTableName;
    FKeyFields := TRelationship(Source).FKeyFields;
    FRelationName := TRelationship(Source).FRelationName;
    FCondition := TRelationship(Source).FCondition;
    // FRelation := TRelationship(Source).FRelation;
    FDetailCardinality := TRelationship(Source).FDetailCardinality;
    FMasterRecordOptional := TRelationship(Source).FMasterRecordOptional;
    FMasterOwnsDetails := TRelationship(Source).FMasterOwnsDetails;
    FDeleteAction := TRelationship(Source).FDeleteAction;
    FUpdateAction := TRelationship(Source).FUpdateAction;
    FEnforceForeignKey := TRelationship(Source).FEnforceForeignKey;
    FCaseInsensitive := TRelationship(Source).FCaseInsensitive;
    FDeleteErrorMessage := TRelationship(Source).FDeleteErrorMessage;
    FUpdateErrorMessage := TRelationship(Source).FUpdateErrorMessage;
    FRequireRecordErrorMessage := TRelationship(Source).FRequireRecordErrorMessage;
    if Assigned(DetailRelation) or Assigned(MasterRelation) then
      UpdateRelations;
  end else
    inherited Assign(Source);
end;

function TRelationship.GetDisplayName: String;
begin
  Result := Name;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TRelationship.GetRelation(const Index: TRelationSide): TRelation;
begin
  Result := FRelation[Index];
end;

procedure TRelationship.SetMasterRecordOptional(const Value: Boolean);
begin
  FMasterRecordOptional := Value;
end;

function TRelationship.GetCondition(const Index: TRelationSide): String;
begin
  Result := FCondition[Index];
end;

function TRelationship.GetKeyFields(const Index: TRelationSide): String;
begin
  Result := FKeyFields[Index];
end;

function TRelationship.GetRelationName(const Index: TRelationSide): String;
begin
  Result := FRelationName[Index];
end;

function TRelationship.GetTableName(const Index: TRelationSide): String;
begin
  Result := FTableName[Index];
end;

function TRelationship.GetRelDescription(
  const Index: TRelationSide): String;
begin
  Result := FRelDescription[Index];
end;

procedure TRelationship.SetCondition(const Index: TRelationSide;
  const Value: String);
begin
  FCondition[Index] := Value;
end;

procedure TRelationship.SetKeyFields(const Index: TRelationSide;
  const Value: String);
begin
  FKeyFields[Index] := Value;
end;

procedure TRelationship.SetRelationName(const Index: TRelationSide; const Value: String);
begin
  FRelationName[Index] := Value;
  if Assigned(FRelation[Index]) then
    FRelation[Index].Name := Value;
end;

procedure TRelationship.UpdateKeyFields;
var
  TempTableDef: TTableDefinition;
begin
  // Assign master key fields to primary index by default if not already assigned
  // Assign detail key fields to match master if not already assigned
  if (MasterKeyFields = '') and (MasterTableName <> '') then
  begin
    TempTableDef := MasterTableDef;
    if TempTableDef <> nil then
    begin
      MasterKeyFields := TempTableDef.GetPrimaryKeyFields;
      // Assume same key fields
      if (DetailKeyFields = '') and (MasterKeyFields <> '') then
        DetailKeyFields := MasterKeyFields;
    end;
  end;
end;

procedure TRelationship.SetTableName(const Index: TRelationSide;
  const Value: String);
var
  NewName: String;
begin
  NewName := Trim(Value);
  if FTableName[Index] <> NewName then
  begin
    if not AnsiSameText(FTableName[Index], NewName) then
    begin
      // Only update relations if table name has actually changed significatnly
      FTableName[Index] := NewName;
      if (FRelation[Index] <> nil) and not AnsiSameText(FRelation[Index].TableName, NewName) then
      begin
        FreeAndNil(FRelation[Index]);
        UpdateRelations;
      end;
      Schema.UnPrepare;
    end else
      FTableName[Index] := NewName;

    if not (csLoading in Schema.ComponentState)
      and not IsUpdating and not Schema.IsUpdating and (Index = sideMaster)
    then
      UpdateKeyFields;
  end;
end;

procedure TRelationship.SetRelDescription(const Index: TRelationSide;
  const Value: String);
begin
  FRelDescription[Index] := Value;
end;

function TRelationship.Side(Relation: TRelation): TRelationSide;
begin
  Result := sideDetail;
  if Relation = FRelation[sideMaster] then
    Result := sideMaster
  else if Relation <> FRelation[sideDetail] then
    InternalRelationshipError(Self);
end;

procedure TRelationship.SwapSides;

  procedure SwapDualString(var A: TDualStringProp);
  var
    S: String;
  begin
    S := A[sideMaster];
    A[sideMaster] := A[sideDetail];
    A[sideDetail] := S;
  end;

var
  Temp: TRelation;
begin
  SwapDualString(FTableName);
  SwapDualString(FKeyFields);
  SwapDualString(FRelationName);
  SwapDualString(FCondition);
  SwapDualString(FRelDescription);
  Temp := FRelation[sideMaster];
  FRelation[sideMaster] := FRelation[sideDetail];
  FRelation[sideDetail] := Temp;
end;

procedure TRelationship.InvalidateRelation(Relation: TRelation);
begin
  if Relation = FRelation[sideDetail] then
    FRelation[sideDetail] := nil;
  if Relation = FRelation[sideMaster] then
    FRelation[sideMaster] := nil;
end;

function ConcatWithDelimiter(const A, B, Delimiter: String): String;
begin
  if A = '' then
    Result := B
  else if B = '' then
    Result := A
  else Result := A + Delimiter + B;
end;

procedure TRelationship.UpdateRelations;

  procedure UpdateRelation(const Side: TRelationSide);
  var
    TableDef: TTableDefinition;
    Rel: TRelation;
  begin
    // Create or update existing relation objects
    if (FRelation[Side] = nil) and (FTableName[Side] <> '') then
    begin
      TableDef := Schema.TableDefs.Find(FTableName[Side]);
      if TableDef <> nil then
      begin
        Rel := TableDef.Relations.Add; {!!!}
        Rel.FRelationship := Self;
        FRelation[Side] := Rel;
        if FRelationName[Side] = '' then
          Rel.Name := Rel.GetAutoName(FTableName[OppositeSide[Side]])
        else begin
          if Rel.TableDef.Relations.Find(FRelationName[Side]) = nil then
            Rel.Name := FRelationName[Side]
          else Rel.Name := Rel.GetAutoName(FTableName[OppositeSide[Side]]);
        end;
        FRelationName[Side] := Rel.Name;
      end;
    end;
    if FRelation[Side] <> nil then
    begin
      if FRelationName[Side] = '' then
        FRelationName[Side] := FRelation[Side].GetAutoName(FTableName[OppositeSide[Side]]);
      if FRelation[Side].FName <> FRelationName[Side] then
        FRelation[Side].FName := FRelationName[Side];
    end;
  end;

begin
  if IsUpdating or Schema.IsUpdating or (csLoading in Schema.ComponentState) then exit;

  UpdateRelation(sideMaster);
  UpdateRelation(sideDetail);
  UpdateKeyFields;

  // Update Master Record is Optinal from Field's required state
  MasterRecordOptional := not KeyFieldsRequired(Schema.TableDefs.Find(DetailTableName), DetailKeyFields);

  // Update own name if necessary
  // if MasterRelationName = '' then InternalRelationshipError(Self);

  if FName = '' then
    Name := GetAutoName(ConcatWithDelimiter(MasterRelationName, DetailRelationName, '_'));
end;

function TRelationship.Compare(Dest: TCompareSchemaItem): Boolean;
begin
  Result := RelationshipsEqual(Self, Dest.DestItem as TRelationship);
  Dest.PropsEqual := Result;
end;

function TRelationship.GetSchemaClassName: String;
begin
  Result := 'Relationship';
end;

function TRelationship.GetTableDef(
  const Index: TRelationSide): TTableDefinition;
begin
  Result := Schema.TableDefs.Find(FTableName[Index]);
end;

procedure TRelationship.InternalUpdate;
begin
  UpdateRelations;
end;

procedure TRelationship.SetPropValue(const PropName, Value: String);
begin
  if AnsiSameText(PropName, 'AddDetailKeyField') then
  begin
    if Value = '' then exit;
    if DetailKeyFields <> '' then DetailKeyFields := DetailKeyFields + ';';
    DetailKeyFields := DetailKeyFields + Value;
  end else if AnsiSameText(PropName, 'AddMasterKeyField') then
  begin
    if Value = '' then exit;
    if MasterKeyFields <> '' then MasterKeyFields := MasterKeyFields + ';';
    MasterKeyFields := MasterKeyFields + Value;
  end else inherited;
end;

procedure TRelationship.UpdateFieldReferences(const Side: TRelationSide);
var
  List: TStringList;
  I: Integer;
begin
  if GetTableDef(Side) = nil then exit;
  List := TStringList.Create;
  try
    GetTableDef(Side).GetFieldList(List, GetKeyFields(Side));
    for I := 0 to List.Count - 1 do
      if List.Objects[I] <> nil then
        List[I] := TFieldDefinition(List.Objects[I]).Name;
    SetKeyFields(Side, Concatenate(List, ';'));
  finally
    List.Free;
  end;
end;

procedure TRelationship.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('RelationIndex', ReadRelationIndex, WriteRelationIndex, True);
end;

procedure TRelationship.ReadRelationIndex(Reader: TReader);
begin
  Reader.ReadListBegin;
  FRelationIndex[sideMaster] := Reader.ReadInteger;
  FRelationIndex[sideDetail] := Reader.ReadInteger;
  Reader.ReadListEnd;
end;

procedure TRelationship.WriteRelationIndex(Writer: TWriter);
begin
  if FRelation[sideMaster] <> nil then
    FRelationIndex[sideMaster] := FRelation[sideMaster].Index;
  if FRelation[sideDetail] <> nil then
    FRelationIndex[sideDetail] := FRelation[sideDetail].Index;

  Writer.WriteListBegin;
  Writer.WriteInteger(FRelationIndex[sideMaster]);
  Writer.WriteInteger(FRelationIndex[sideDetail]);
  Writer.WriteListEnd;
end;

{ TFieldDefinitions }

function TFieldDefinitions.Add: TFieldDefinition;
begin
  Result := TFieldDefinition(inherited Add);
end;

procedure TFieldDefinitions.Add(const Name: String; DataType: TFieldDataType;
  Size: Integer; Required: Boolean);
var
  FieldDef: TFieldDefinition;
begin
  if Name = '' then
    DatabaseError(SFieldNameMissing, TComponent(GetOwner));
  BeginUpdate;
  try
    FieldDef := Add;
    try
      {FieldNo is defaulted}
      FieldDef.Name := Name;
      FieldDef.DataType := DataType;
      FieldDef.Size := Size;
      { Precision is defaulted }
      FieldDef.Required := Required;
    except
      FieldDef.Free;
      raise;
    end;
  finally
    EndUpdate;
  end;
end;

function TFieldDefinitions.Find(const Name: String): TFieldDefinition;
begin
  Result := TFieldDefinition(inherited Find(Name));
end;

function TFieldDefinitions.GetFieldDef(Index: Integer): TFieldDefinition;
begin
  Result := TFieldDefinition(inherited Items[Index]);
end;

procedure TFieldDefinitions.SetFieldDef(Index: Integer; Value: TFieldDefinition);
begin
  inherited Items[Index] := Value;
end;

{ TRelation }

constructor TRelation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FDescription := '';
  FRelationship := nil;
  FName := '';
end;

destructor TRelation.Destroy;
begin
  // Relation can only exist if it refers to Relationship
  if FRelationship <> nil then
  begin
    FRelationship.InvalidateRelation(Self);
    if FRelationship.Collection = Schema.FTempRelationships then
      FRelationship.Free;
  end;
  if FName <> '' then
    TableDef.Schema.UnPrepare;
  // Put nil into Relationship FSrcRelation or FDestRelation
  inherited Destroy;
end;

function TRelation.GetDisplayName: String;
begin
  if Name <> '' then
    Result := Name
  else Result := KeyFields + ' -> ' + ForeignTable + '(' + ForeignKeyFields + ')';
  if Result = '' then
    Result := Description;
end;

function TRelation.GetLocateOptions: TLocateOptions;
begin
  if CaseInsensitive then
    Result := [loCaseInsensitive]
  else Result := [];
end;

function TRelation.GetDeleteErrorMessage: String;
begin
  Result := Relationship.DeleteErrorMessage;
end;

function TRelation.GetUpdateErrorMessage: String;
begin
  Result := Relationship.UpdateErrorMessage;
end;

function TRelation.GetRequireRecordErrorMessage: String;
begin
  Result := Relationship.RequireRecordErrorMessage;
end;

function TRelation.EffectiveDeleteErrorMessage: String;
begin
  Result := Relationship.DeleteErrorMessage;
  if (Result = '') and (EffectiveDeleteAction = raError) then
    Result := Format(SDefaultDeleteErrorMessage, [TableDef.TableName, ForeignTable]);
end;

function TRelation.EffectiveUpdateErrorMessage: String;
begin
  Result := Relationship.UpdateErrorMessage;
  if (Result = '') and (EffectiveUpdateAction = raError) then
    Result := Format(SDefaultUpdateErrorMessage, [KeyFields, TableDef.TableName, ForeignTable]);
end;

function TRelation.EffectiveRequireRecordErrorMessage: String;
begin
  Result := Relationship.RequireRecordErrorMessage;
  if (Result = '') and RequireOneRecord then
    Result := Format(SDefaultRequireRecordErrorMessage, [KeyFields, TableDef.TableName, ForeignTable]);
end;

procedure TRelation.ObjectRenamed(const OldName: String);
begin
  // if Assigned(FRelationship) then
  with Relationship do
    SetRelationName(Side(Self), Self.Name);
end;

procedure TRelation.SetCaseInsensitive(const Value: Boolean);
begin
  with Relationship do
    CaseInsensitive := Value;
end;

procedure TRelation.SetCondition(const Value: String);
begin
  with Relationship do
    SetCondition(Side(Self), Value);
end;

procedure TRelation.SetForeignCondition(const Value: String);
begin
  with Relationship do
    SetCondition(OppositeSide[Side(Self)], Value);
end;

procedure TRelation.SetForeignKeyFields(const Value: String);
begin
  with Relationship do
    SetKeyFields(OppositeSide[Side(Self)], Value);
end;

procedure TRelation.SetForeignTable(const Value: String);
begin
  with Relationship do
    SetTableName(OppositeSide[Side(Self)], Value);
end;

procedure TRelation.SetKeyFields(const Value: String);
begin
  with Relationship do
    SetKeyFields(Side(Self), Value);
end;

procedure TRelation.SetRelationKind(const Value: TRelationKind);
begin
  with Relationship do
    MasterOwnsDetails := Value <> rkReference;
end;

procedure TRelation.SetRelationType(const Value: TRelationType);
begin
  with Relationship do
    DetailCardinality := dcMany;
  with Relationship do
    case Value of
      rtOneToOne: DetailCardinality := dcOne;
      rtOneToMany: begin
        if Side(Self) = sideDetail then
          SwapSides;
        DetailCardinality := dcMany;
      end;
      rtManyToOne: begin
        if Side(Self) = sideMaster then
          SwapSides;
        DetailCardinality := dcMany;
      end;
      rtManyToMany: DetailCardinality := dcLogical;
    end
end;

procedure TRelation.SetDeleteAction(const Value: TRelationAction);
begin
  Relationship.DeleteAction := Value;
end;

procedure TRelation.SetUpdateAction(const Value: TRelationAction);
begin
  Relationship.UpdateAction := Value;
end;

procedure TRelation.SetRequireOneRecord(const Value: Boolean);
begin
  with Relationship do
    if Value and (Schema.IsUpdating or (csLoading in Schema.ComponentState)) then
    begin
      // This is effective only when loading 
      if Side(Self) = sideMaster then
        SwapSides;
      EnforceForeignKey := True;
    end else begin
      if Side(Self) = sideDetail then
        EnforceForeignKey := Value;
    end;
end;

procedure TRelation.Assign(Source: TPersistent);
begin
  if Source.InheritsFrom(TRelation) then
  begin
    inherited Assign(Source);

    Relationship.FRelationName[Relationship.Side(Self)] := FName;
    Relationship.FRelationName[OppositeSide[Relationship.Side(Self)]] := TRelation(Source).PeerRelationName;
    if GetPeerRelation <> nil then
      GetPeerRelation.FName := TRelation(Source).PeerRelationName;

    RequireOneRecord := TRelation(Source).RequireOneRecord;
    KeyFields := TRelation(Source).KeyFields;
    ForeignTable := TRelation(Source).ForeignTable;
    ForeignKeyFields := TRelation(Source).ForeignKeyFields;
    ForeignCondition := TRelation(Source).ForeignCondition;
    Condition := TRelation(Source).Condition; { filter self }

    RelationType := TRelation(Source).RelationType;
    DeleteAction := TRelation(Source).DeleteAction;
    UpdateAction := TRelation(Source).UpdateAction;
    CaseInsensitive := TRelation(Source).CaseInsensitive;
    RelationKind := TRelation(Source).RelationKind;
    RequireOneRecord := TRelation(Source).RequireOneRecord;

    Relationship.DeleteErrorMessage := TRelation(Source).Relationship.DeleteErrorMessage;
    Relationship.UpdateErrorMessage := TRelation(Source).Relationship.UpdateErrorMessage;
    Relationship.RequireRecordErrorMessage := TRelation(Source).Relationship.RequireRecordErrorMessage;

    TableDef.Schema.UnPrepare;
  end else
    inherited Assign(Source);
end;

function TRelation.Compare(Dest: TCompareSchemaItem): Boolean;
begin
  if Dest.DestItem <> nil then
    Result := RelationshipsEqual(Relationship, (Dest.DestItem as TRelation).Relationship)
  else Result := False;
  Dest.PropsEqual := Result;
end;

function TRelation.GetSchemaClassName: String;
begin
  with Relationship do
  if EnforceForeignKey then
  begin
    if Side(Self) = sideMaster then
      Result := 'Reference'
    else Result := 'ForeignKey';
  end else begin
    if Side(Self) = sideMaster then
      Result := 'MasterRelation'
    else Result := 'DetailRelation';
  end;
  (*
  if Side(Self) = sideMaster then
   Result := 'MasterRelation'
  else if EnforceForeignKey then
    Result := 'ForeignKey'
  else Result := 'DetailRelation';
  *)
end;

procedure TRelation.DoAddForeignKeyField(const Value: String);
begin
  if Value = '' then exit;
  if ForeignKeyFields <> '' then ForeignKeyFields := ForeignKeyFields + ';';
  ForeignKeyFields := ForeignKeyFields + Value;
end;

procedure TRelation.DoAddKeyField(const Value: String);
begin
  if Value = '' then exit;
  if KeyFields <> '' then KeyFields := KeyFields + ';';
  KeyFields := KeyFields + Value;
end;

(*
function TRelation.GetRelationship: TRelationship;
begin
  if FRelationship = nil then
  begin
    if not ((Schema is TDBSchemaVersion) or Schema.IsUpdating or (csLoading in Schema.ComponentState)) then
      raise Exception.CreateFmt(SInvalidRelation, [TableDef.Name, Name]);

    FRelationship := Schema.FTempRelationships.Add;
    // Assign itself to detail first! This is very important for import from SQL,
    // bacause then it assumes that this is ForeignKey side.
    FRelationship.FRelation[sideDetail] := Self;
    FRelationship.FRelationName[sideDetail] := Name;
    FRelationship.FTableName[sideDetail] := TableDef.Name;
  end;
  Result := FRelationship;
end;
*)

function TRelation.GetRelationship: TRelationship;
var
  I: Integer;
  Rel: TRelationship;

  function CheckSide(const Side: TRelationSide): Boolean;
  begin
    Result := True;
    if Rel.FRelation[Side] = Self then exit;

    if (Rel.FRelation[Side] = nil)
      and AnsiSameText(Rel.FTableName[Side], TableDef.Name)
      and AnsiSameText(Rel.FRelationName[Side], FName)
    then
      Rel.FRelation[Side] := Self
    else Result := False;
  end;

begin
  if FRelationship = nil then
  begin
    // Locate partially filled Relationship
    if FName <> '' then
    for I := 0 to Schema.Relationships.Count - 1 do
    begin
      Rel := Schema.Relationships[I];
      if CheckSide(sideDetail) or CheckSide(sideMaster) then
      begin
        FRelationship := Rel;
        break;
      end;
    end;
    // Otherwise create a temporary one
    if FRelationship = nil then
    begin
      if (Schema is TDBSchemaVersion) or Schema.IsUpdating or (csLoading in Schema.ComponentState) then
        FRelationship := Schema.FTempRelationships.Add
      else FRelationship := Schema.Relationships.Add;

      // raise Exception.CreateFmt(SInvalidRelation, [TableDef.Name, Name]);

      // Assign itself to detail first! This is very important for import from SQL,
      // bacause then it assumes that this is ForeignKey side.
      FRelationship.FRelation[sideDetail] := Self;
      FRelationship.FRelationName[sideDetail] := FName;
      FRelationship.FTableName[sideDetail] := TableDef.Name;
    end;
  end;
  Result := FRelationship;
  if Result = nil then
    raise Exception.CreateFmt(SInvalidRelation, [TableDef.Name, Name]);
end;

function TRelation.GetDummyStr: String;
begin
  Result := '';
end;

function TRelation.GetPeerRelation: TRelation;
begin
  with Relationship do
    Result := FRelation[OppositeSide[Side(Self)]];
end;

function TRelation.FindPeerRelation: TRelation;
var
  FrgnTableDef: TTableDefinition;
  I: Integer;
begin
  Result := nil;
  if ForeignTable = '' then exit;
  FrgnTableDef := TableDef.GetSchema.GetTableDef(nil, ForeignTable);
  if FrgnTableDef = nil then exit;

  if PeerRelationName <> '' then
    Result := FrgnTableDef.Relations.Find(PeerRelationName);
  if Result <> nil then exit;

  for I := 0 to FrgnTableDef.Relations.Count - 1 do
    with FrgnTableDef.Relations[I] do
    if AnsiSameText(KeyFields, Self.ForeignKeyFields) and
       AnsiSameText(ForeignKeyFields, Self.KeyFields) and
       AnsiSameText(ForeignTable, Self.TableName) and
       (CaseInsensitive = Self.CaseInsensitive) and
       (((RelationKind = rkReference) and (Self.RelationKind = rkReference)) or
         ((RelationKind = rkChildren) and (Self.RelationKind = rkParent)) or
         ((RelationKind = rkParent) and (Self.RelationKind = rkChildren))) and
       (RelationType = OppositeRelationType[Self.RelationType])
    then
    begin
      Result := FrgnTableDef.Relations[I];
      exit;
    end;
end;

(*
function TRelation.GetPeerRelation(CreateIfNotExist: Boolean = True; SyncProps: Boolean = False): TRelation;
var
  FrgnTableDef: TTableDefinition;
  PeerCreated: Boolean;
begin
  Result := nil;
  // Do not do anything until it's loaded
  if GetSchema.FLoadingFromStream or (csLoading in GetSchema.ComponentState) then exit;

  if ForeignTable = '' then exit;
  PeerCreated := False;
  FrgnTableDef := TableDef.GetSchema.GetTableDef(nil, ForeignTable);
  if FrgnTableDef = nil then exit;
  if PeerRelationName <> '' then
    Result := FrgnTableDef.Relations.Find(FPeerRelationName);
  if Result = nil then
  begin
    Result := FindPeerRelation;
    if Result = nil then
    begin
      if not CreateIfNotExist then exit;

      Result := FrgnTableDef.Relations.Add;
      PeerCreated := True;

      if FPeerRelationName = '' then
        Result.FName := Result.GetAutoName(TableDef.Name)
      else Result.FName := FPeerRelationName;

      if RelationType <> rtManyToMany then
        Result.FRequireOneRecord := not RequireOneRecord;
      SyncProps := True;
    end else if RequireOneRecord then
    begin
      FDeleteAction := Result.FDeleteAction;
      FUpdateAction := Result.FUpdateAction;
    end;
    FPeerRelationName := Result.Name;
  end;
  if SyncProps then
  begin
    Result.FKeyFields := ForeignKeyFields;
    Result.FForeignKeyFields := KeyFields;
    Result.FForeignTable := TableDef.Name;
    Result.FCaseInsensitive := CaseInsensitive;
    Result.FRelationKind := OppositeRelationKind[RelationKind];
    Result.FRelationType := OppositeRelationType[RelationType];
    Result.FPeerRelationName := Name;
    Result.FDeleteAction := FDeleteAction;
    Result.FUpdateAction := FUpdateAction;
    Result.FForeignCondition := Condition;
    if PeerCreated then
      Result.FCondition := ForeignCondition
    else FForeignCondition := Result.FCondition;
    if RequireOneRecord then
      Result.FRequireOneRecord := False;

    Result.FAllowZeroRecords := Result.CalcAllowZeroRecords;
    FAllowZeroRecords := CalcAllowZeroRecords;
  end;
end;
*)

function TRelation.EffectiveDeleteAction: TRelationAction;
begin
  with Relationship do
    if EnforceForeignKey and (Side(Self) = sideMaster) then
      Result := DeleteAction
    else Result := raIgnore;
end;

function TRelation.EffectiveUpdateAction: TRelationAction;
begin
  with Relationship do
    if EnforceForeignKey and (Side(Self) = sideMaster) then
      Result := UpdateAction
    else Result := raIgnore;
end;

function TRelation.GetCaseInsensitive: Boolean;
begin
  Result := Relationship.CaseInsensitive;
end;

function TRelation.GetCondition: String;
begin
  with Relationship do
    Result := GetCondition(Side(Self));
end;

function TRelation.GetDeleteAction: TRelationAction;
begin
  Result := Relationship.DeleteAction;
end;

function TRelation.GetForeignCondition: String;
begin
  with Relationship do
    Result := GetCondition(OppositeSide[Side(Self)]);
end;

function TRelation.GetForeignKeyFields: String;
begin
  with Relationship do
    Result := GetKeyFields(OppositeSide[Side(Self)]);
end;

function TRelation.GetForeignTable: String;
begin
  with Relationship do
    Result := GetTableName(OppositeSide[Side(Self)]);
end;

function TRelation.GetKeyFields: String;
begin
  with Relationship do
    Result := GetKeyFields(Side(Self));
end;

function TRelation.GetPeerRelationName: String;
begin
  with Relationship do
    Result := GetRelationName(OppositeSide[Side(Self)]);
end;

function TRelation.GetRelationKind: TRelationKind;
begin
  with Relationship do
    if MasterOwnsDetails then
    begin
      if Side(Self) = sideDetail then
        Result := rkChildren
      else Result := rkParent;
    end else
      Result := rkReference;
end;

function TRelation.GetRelationType: TRelationType;
begin
  with Relationship do
    case DetailCardinality of
      dcOne: Result := rtOneToOne;
      dcMany:
        if Side(Self) = sideDetail then
          Result := rtManyToOne
        else Result := rtOneToMany;
      else Result := rtManyToMany;
    end
end;

function TRelation.GetRequireOneRecord: Boolean;
begin
  with Relationship do
    Result := EnforceForeignKey and (Side(Self) = sideDetail);
end;

function TRelation.GetUpdateAction: TRelationAction;
begin
  Result := Relationship.UpdateAction;
end;

procedure TRelation.SetDeleteErrorMessage(const Value: String);
begin
  Relationship.DeleteErrorMessage := Value;
end;

procedure TRelation.SetPeerRelationName(const Value: String);
begin
  with Relationship do
    SetRelationName(OppositeSide[Side(Self)], Value);
end;

procedure TRelation.SetRequireRecordErrorMessage(const Value: String);
begin
  Relationship.RequireRecordErrorMessage := Value;
end;

procedure TRelation.SetUpdateErrorMessage(const Value: String);
begin
  Relationship.UpdateErrorMessage := Value;
end;

function TRelation.GetDescription: String;
begin
  with Relationship do
    Result := GetRelDescription(Side(Self));
end;

procedure TRelation.SetDescription(const Value: String);
begin
  with Relationship do
    SetRelDescription(Side(Self), Value);
end;

function TRelation.IsDataTemporary: Boolean;
begin
  Result := (FRelationship <> nil) and (FRelationship.Collection = Schema.FTempRelationships);
end;

function TRelation.GetItemID: Integer;
begin
  Result := Relationship.ItemID;
end;

function TRelation.GetRelationshipName: String;
begin
  Result := Relationship.Name;
end;

procedure TRelation.SetRelationshipName(const Value: String);
begin
  Relationship.Name := Value;
end;

function TRelation.GetEnforceForeignKey: Boolean;
begin
  Result := Relationship.EnforceForeignKey;
end;

function TRelation.GetMultiplicity: TMultiplicity;
begin
  case RelationType of
    rtOneToMany, rtOneToOne:
      if (Relationship.Side(Self) = sideMaster) and not Relationship.MasterRecordOptional then
        Result := mOne
      else Result := mZeroOrOne;
    rtManyToOne: Result := mZeroOrMany;
    else Result := mUndefined;
  end;
end;

function TRelation.GetSide: TRelationSide;
begin
  Result := Relationship.Side(Self);
end;
{
function TRelation.GetAutoName(const Prefix: String): String;
begin
  Result := TSchemaItemsCollection(Collection).GetAutoName(GetSchemaClassName, Prefix);
end;
}
function TRelation.GetRelationIndex: Integer;
begin
  with Relationship do
    Result := FRelationIndex[Side(Self)];
end;

function TRelation.GetIsForeignKey: Boolean;
begin
  with Relationship do
    Result := EnforceForeignKey and (Side(Self) = sideDetail);
end;

{ TRelations }

function TRelations.Add: TRelation;
begin
  Result := TRelation(inherited Add);
end;

function TRelations.Find(const Name: String): TRelation;
begin
  Result := TRelation(inherited Find(Name));
end;

function TRelations.GetRelation(Index: Integer): TRelation;
begin
  Result := TRelation(inherited GetItem(Index));
end;

procedure TRelations.SetRelation(Index: Integer; Value: TRelation);
begin
  inherited SetItem(Index, Value);
end;

{ TTableConstraint }

procedure TTableConstraint.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source.InheritsFrom(TTableConstraint) then
    FCheck := TTableConstraint(Source).FCheck;
end;

function TTableConstraint.Compare(Dest: TCompareSchemaItem): Boolean;
begin
  Result := inherited Compare(Dest)
    and AnsiSameText(FCheck, (Dest.DestItem as TTableConstraint).Check);
  Dest.PropsEqual := Result;
end;

function TTableConstraint.GetSchemaClassName: String;
begin
  Result := 'Constraint';
end;

{ TTableConstraints }

function TTableConstraints.Add: TTableConstraint;
begin
  Result := TTableConstraint(inherited Add);
end;

function TTableConstraints.Find(const Name: String): TTableConstraint;
begin
  Result := TTableConstraint(inherited Find(Name));
end;

function TTableConstraints.GetConstraint(Index: Integer): TTableConstraint;
begin
  Result := TTableConstraint(inherited GetItem(Index));
end;

procedure TTableConstraints.SetConstraint(Index: Integer;
  Value: TTableConstraint);
begin
  inherited SetItem(Index, Value);
end;

{ TAggregateLink }

constructor TAggregateLink.CreateLink(Collection: TCollection;
  Relation: TRelation; ForeignFieldDef: TFieldDefinition);
begin
  inherited Create(Collection);
  FForeignKeyFields := Relation.KeyFields;
  FKeyFields := Relation.ForeignKeyFields;
  FForeignTable := Relation.TableDef.TableName;
  FRelationType := Relation.RelationType;
  FRelationKind := Relation.RelationKind;
  FCondition := Relation.ForeignCondition;
  FForeignCondition := Relation.Condition;
  FCaseInsensitive := Relation.CaseInsensitive;
  FAggregateExpression := ForeignFieldDef.AggregateExpression;
  FForeignFieldName := ForeignFieldDef.Name;
  FAggregateType := ForeignFieldDef.AggregateType;
end;

{ TAggregateLinkData }

constructor TAggregateLinkData.Create(AggregateLink: TAggregateLink);
begin
  inherited Create;
  FAggregateLink := AggregateLink;
  FUpdateKeys := nil;
end;

destructor TAggregateLinkData.Destroy;
begin
  inherited Destroy;
  ClearKeys;
end;

function TAggregateLinkData.GetUpdateKeys: TStrings;
begin
  Result := FUpdateKeys;
end;

procedure TAggregateLinkData.AddKey(const KeyValue: String);
begin
  if FUpdateKeys = nil then
  begin
    FUpdateKeys := TStringList.Create;
    FUpdateKeys.Duplicates := dupIgnore;
    FUpdateKeys.Sorted := True;
  end;
  FUpdateKeys.Add(KeyValue);
end;

procedure TAggregateLinkData.ClearKeys;
begin
  if FUpdateKeys <> nil then
    FreeAndNil(FUpdateKeys);
end;

function TAggregateLinkData.HasKeys: Boolean;
begin
  Result := Assigned(FUpdateKeys) and (FUpdateKeys.Count > 0);
end;

{ TAggregateLinks }

function TAggregateLinks.GetAggregateLink(Index: Integer): TAggregateLink;
begin
  Result := TAggregateLink(inherited Items[Index]);
end;

procedure TAggregateLinks.SetAggregateLink(Index: Integer;
  const Value: TAggregateLink);
begin
  inherited SetItem(Index, Value);
end;

{ TTableDefinition }

constructor TTableDefinition.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFieldDefs := TFieldDefinitions.Create(Self, TFieldDefinition);
  FIndexDefs := TIndexDefinitions.Create(Self, TIndexDefinition);
  FRelations := TRelations.Create(Self, TRelation);
  FAggregateLinks := TAggregateLinks.Create(Self, TAggregateLink);
  FTriggers := TTriggerDefinitions.Create(Self, TTriggerDefinition);
  FConstraints := TTableConstraints.Create(Self, TTableConstraint);
  FTypePrefix := '';
  FReplicate := True;
  FObjectType := '';
  FObjectKeyFields := '';
  FMasterRecord := True;
  FObjectKeyCaseInsensitive := False;
  FDefinition := TStringList.Create;
end;

(*
constructor TTableDefinition.CreateView(Collection: TCollection);
begin
  Create(Collection);
  FIsView := True;
end;
*)

destructor TTableDefinition.Destroy;
begin
  Schema.UnPrepare;
  FFieldDefs.Free;
  FIndexDefs.Free;
  FRelations.Free;
  FAggregateLinks.Free;
  FTriggers.Free;
  FConstraints.Free;
  FDefinition.Free;
  inherited Destroy;
end;

procedure TTableDefinition.ObjectRenamed(const OldName: String);
var
  I: Integer;
begin
  //  if not ObjectTypeAssigned then
  //    CheckObjectTypeUnique(NewTableName);
  if FName <> '' then
  begin
    // Update table name in relations
    with GetSchema.Relationships do
    for I := 0 to Count - 1 do
    begin
      if Items[I].MasterTableName = OldName then
        Items[I].MasterTableName := Name;
      if Items[I].DetailTableName = OldName then
        Items[I].DetailTableName := Name;
    end;
  end;
  Schema.UnPrepare;
end;

function TTableDefinition.GetPrimaryIndex: TIndexDefinition;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FIndexDefs.Count - 1 do
    if ixPrimary in FIndexDefs[I].Options then
    begin
      Result := FIndexDefs[I];
      exit;
    end;
end;

function TTableDefinition.PartOfIndex(const Fields: String; CaseInsensitive: Boolean): Boolean;
begin
  Result := IndexDefs.GetIndexForFields(Fields, CaseInsensitive) <> nil;
end;

function TTableDefinition.IsUniqueKey(const Fields: String; CaseInsensitive: Boolean): Boolean;
var
  PK: TIndexDefinition;
  Idx, I: Integer;
  FieldName: String;
begin
  Result := False;
  // Fields contains all fields, comprising one of unique indexes
  for Idx := 0 to IndexDefs.Count - 1 do
  begin
    PK := IndexDefs[Idx];
    if not ((ixUnique in PK.Options) or (ixPrimary in PK.Options)) then continue;
    if CaseInsensitive and not (ixCaseInsensitive in PK.Options) then continue;
    if PK.Fields = '' then continue;
    Result := True;
    I := 1;
    while I <= Length(PK.Fields) do
    begin
      FieldName := AnsiUpperCase(NextToken(PK.Fields, ';', I));
      if AnsiPos(';'+FieldName+';', ';'+AnsiUpperCase(Fields)+';') < 1 then
      begin
        Result := False;
        break;
      end;
    end;
    if Result then exit;
  end;
end;

procedure TTableDefinition.GetFieldList(List: TList; const FieldNames: String);
var
  Pos: Integer;
  Field: TFieldDefinition;
begin
  Pos := 1;
  while Pos <= Length(FieldNames) do
  begin
    Field := FieldDefs.Find(ExtractFieldName(FieldNames, Pos));
    if Assigned(List) then List.Add(Field);
  end;
end;

procedure TTableDefinition.GetFieldList(List: TStrings; const FieldNames: String);
var
  Pos: Integer;
  FieldName: String;
  Field: TFieldDefinition;
begin
  Pos := 1;
  while Pos <= Length(FieldNames) do
  begin
    FieldName := ExtractFieldName(FieldNames, Pos);
    Field := FieldDefs.Find(FieldName);
    if Assigned(List) then List.AddObject(FieldName, Field);
  end;
end;

procedure TTableDefinition.SetFieldDefs(const Value: TFieldDefinitions);
begin
  FFieldDefs.Assign(Value);
end;

procedure TTableDefinition.SetIndexDefs(const Value: TIndexDefinitions);
begin
  FIndexDefs.Assign(Value);
end;

procedure TTableDefinition.SetRelations(const Value: TRelations);
begin
  FRelations.Assign(Value);
end;

procedure TTableDefinition.SetTriggers(const Value: TTriggerDefinitions);
begin
  FTriggers.Assign(Value);
end;

procedure TTableDefinition.Prepare;
var
  I, J: Integer;
  Relation: TRelation;
  PK: TIndexDefinition;
begin
  FAggregateLinks.Clear;
  with Schema.TableDefs do
    for I := 0 to Count - 1 do
      with Items[I].FieldDefs do
        for J := 0 to Count - 1 do
          if Items[J].AggregateType <> aNone then
          begin
            Relation := Schema.TableDefs[I].Relations.Find(Items[J].RelationName);
            if (Relation <> nil) and AnsiSameText(Relation.ForeignTable, Self.TableName) then
              TAggregateLink.CreateLink(FAggregateLinks, Relation, Items[J]);
          end;
  PK := GetPrimaryIndex;
  if PK <> nil then
  begin
    FObjectKeyFields := PK.Fields;
    FObjectKeyCaseInsensitive := ixCaseInsensitive in PK.Options;
  end else begin
    FObjectKeyFields := '';
  end;
  FMasterRecord := True;
  for I := 0 to Relations.Count - 1 do
    if Relations[I].RelationKind = rkParent then
    begin
      FMasterRecord := False;
      break;
    end;
end;

procedure TTableDefinition.UnPrepare;
begin
  FAggregateLinks.Clear;
end;

function TTableDefinition.GetObjectType: String;
begin
  Result := FObjectType;
  if Result = '' then
    Result := FName;
end;

procedure TTableDefinition.SetObjectType(const Value: String);
begin
  if Value <> FObjectType then
  begin
    // Make sure it's unique...
    CheckObjectTypeUnique(Value);
    FObjectType := Value;
  end;
end;

procedure TTableDefinition.CheckObjectTypeUnique(const NewValue: String);
var I: Integer;
begin
  if NewValue = '' then exit;
  with Schema.TableDefs do
  for I := 0 to Count - 1 do
    if (Items[I] <> Self) and AnsiSameText(Items[I].FObjectType, NewValue) then
      DatabaseErrorFmt(SObjectTypeMustBeUnique, [NewValue]);
end;

function TTableDefinition.ObjectTypeAssigned: Boolean;
begin
  Result := FObjectType <> '';
end;

procedure TTableDefinition.SetPropValue(const PropName, Value: String);
begin
  if AnsiSameText(PropName, 'AddDefinition') then
    FDefinition.Text := FDefinition.Text + Value else
    inherited;
end;

procedure TTableDefinition.Assign(ASource: TPersistent);
begin
  if ASource.InheritsFrom(TTableDefinition) then
  begin
    inherited Assign(ASource);
    FCategory := TTableDefinition(ASource).FCategory;
    FTypePrefix := TTableDefinition(ASource).FTypePrefix;
    FReplicate := TTableDefinition(ASource).FReplicate;
    FObjectType := TTableDefinition(ASource).FObjectType;
    FObjectKeyFields := TTableDefinition(ASource).FObjectKeyFields;
    FMasterRecord := TTableDefinition(ASource).FMasterRecord;
    FObjectKeyCaseInsensitive := TTableDefinition(ASource).FObjectKeyCaseInsensitive;
    FAggregateLinks.Clear;
    FFieldDefs.Assign(TTableDefinition(ASource).FFieldDefs);
    FIndexDefs.Assign(TTableDefinition(ASource).FIndexDefs);
    if TTableDefinition(ASource).StoreRelations then
      FRelations.Assign(TTableDefinition(ASource).FRelations);
    FTriggers.Assign(TTableDefinition(ASource).FTriggers);
    FConstraints.Assign(TTableDefinition(ASource).FConstraints);
    FDefinition.Assign(TTableDefinition(ASource).FDefinition);
    FIsView := TTableDefinition(ASource).FIsView;
  end else
    inherited Assign(ASource);
  Schema.UnPrepare;
end;

procedure TTableDefinition.CopyFrom(ASource: TPersistent; AWithSubItems: boolean = True);
var
  TempSource: TTableDefinition;
begin
  if not (ASource is TTableDefinition) then
    Exit;
  TempSource := ASource as TTableDefinition;
  FName := TempSource.FName;
  FDescription := TempSource.FDescription;
  FProps.Assign(TempSource.FProps);
  FCategory := TempSource.FCategory;
  FTypePrefix := TempSource.FTypePrefix;
  FReplicate := TempSource.FReplicate;
  FObjectType := TempSource.FObjectType;
  FObjectKeyFields := TempSource.FObjectKeyFields;
  FMasterRecord := TempSource.FMasterRecord;
  FObjectKeyCaseInsensitive := TempSource.FObjectKeyCaseInsensitive;
  FAggregateLinks.Clear;

  if AWithSubItems then
  begin
    FFieldDefs.CopyFrom(TempSource.FFieldDefs);
    FIndexDefs.CopyFrom(TempSource.FIndexDefs);
    if TempSource.StoreRelations then
      FRelations.CopyFrom(TempSource.FRelations);
    FTriggers.CopyFrom(TempSource.FTriggers);
    FConstraints.CopyFrom(TempSource.FConstraints);
  end;

  FProduceSQL := TempSource.FProduceSQL;
  FDefinition.Assign(TempSource.FDefinition);
  FIsView := TempSource.FIsView;

  Schema.UnPrepare;
end;

function TTableDefinition.Compare(Dest: TCompareSchemaItem): Boolean;
var
  DestTable: TTableDefinition;
  ByName: Boolean;
begin
  DestTable := Dest.DestItem as TTableDefinition;
  with Dest do
  begin
    ByName := (DestTable <> nil) and (Schema.SchemaID <> DestTable.Schema.SchemaID);
    GetSubItems;
    if DestTable = nil then
    begin
      FieldDefs.Compare(nil, SubItems, ByName);
      IndexDefs.Compare(nil, SubItems, ByName);
      Relations.Compare(nil, SubItems, ByName);
      Triggers.Compare(nil, SubItems, ByName);
      Constraints.Compare(nil, SubItems, ByName);
    end else
    begin
      FieldDefs.Compare(DestTable.FieldDefs, SubItems, ByName);
      IndexDefs.Compare(DestTable.IndexDefs, SubItems, ByName);
      Relations.Compare(DestTable.Relations, SubItems, ByName);
      Triggers.Compare(DestTable.Triggers, SubItems, ByName);
      Constraints.Compare(DestTable.Constraints, SubItems, ByName);
    end;
    if SubItems.Count = 0 then
      FreeAndNil(SubItems);

    PropsEqual := inherited Compare(Dest) and
      FDefinition.Equals(DestTable.FDefinition);

    if PropsEqual and (DestTable <> nil) then
      PropsEqual := FIsView = DestTable.IsView;

    Result := PropsEqual and (SubItems = nil);
  end;
end;

function TTableDefinition.GetSchemaClassName: String;
begin
  if IsView then
    Result := 'View' else
    Result := 'Table';
end;

function TTableDefinition.StoreFieldDefs: Boolean;
begin
  Result := FieldDefs.Count > 0;
end;

function TTableDefinition.StoreIndexDefs: Boolean;
begin
  Result := IndexDefs.Count > 0;
end;

function TTableDefinition.StoreRelations: Boolean;
begin
  Result := (Relations.Count > 0) and (Schema.FTempRelationships.Count > 0);
end;

function TTableDefinition.StoreTriggers: Boolean;
begin
  Result := Triggers.Count > 0;
end;

function TTableDefinition.GetPrimaryKeyFields: String;
var
  PK: TIndexDefinition;
begin
  PK := GetPrimaryIndex;
  if PK <> nil then
    Result := PK.Fields
  else Result := '';
end;

procedure TTableDefinition.SetConstraints(const Value: TTableConstraints);
begin
  FConstraints.Assign(Value);
end;

function TTableDefinition.StoreConstraints: Boolean;
begin
  Result := FConstraints.Count > 0;
end;

function TTableDefinition.FindConstraint(
  const Name: String): TTableCollectionItem;
begin
  Result := IndexDefs.Find(Name);
  if (Result <> nil) and (ixPrimary in TIndexDefinition(Result).Options) then exit;
  Result := Relations.Find(Name);
  if Result = nil then
    Result := Constraints.Find(Name);
end;

procedure TTableDefinition.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Relations', ReadRelations, WriteRelations, StoreRelations);
end;

procedure TTableDefinition.WriteRelations(Writer: TWriter);
begin
  Writer.WriteCollection(FRelations);
end;

procedure TTableDefinition.ReadRelations(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FRelations);
end;

function TTableDefinition.GetHasIdentityFields: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FieldDefs.Count - 1 do
    if FieldDefs[I].Identity then exit;
  Result := False;
end;

procedure TTableDefinition.SetIsView(Value: boolean);
begin
  if FIsView <> Value then
   FIsView := Value;
end;

procedure TTableDefinition.SetDefinition(const Value: TStrings);
begin
  FDefinition.Assign(Value);
end;

procedure TTableDefinition.UpdateDefinition(const Value: String = '');
begin
  if IsView and (FDefinition.Count > 0) then
    FDefinition.Insert(0, 'CREATE VIEW ' + Schema.FormatName(Name, Value));
end;


{ TTableDefinitions }

function TTableDefinitions.Add(AIsView: boolean = False): TTableDefinition;
begin
  Result := TTableDefinition(inherited Add);
  Result.IsView := AIsView;
end;

function TTableDefinitions.GetItem(Index: Integer): TTableDefinition;
begin
  Result := TTableDefinition(inherited GetItem(Index));
end;

procedure TTableDefinitions.SetItem(Index: Integer;
  const Value: TTableDefinition);
begin
  inherited SetItem(Index, Value);
end;

function TTableDefinitions.Find(const Name: String): TTableDefinition;
begin
  Result := TTableDefinition(inherited Find(Name));
end;

{ TEnumeration }

constructor TEnumeration.Create(Collection: TCollection);
begin
  inherited;
  FItems := TStringList.Create;
  FDescriptions := TStringList.Create;
  FShortDescriptions := TStringList.Create;
  FName := '';
  FTypePrefix := '';
  FIntConsts := False;
end;

destructor TEnumeration.Destroy;
begin
  inherited;
  FItems.Free;
  FDescriptions.Free;
  FShortDescriptions.Free;
end;

procedure TEnumeration.Assign(ASource: TPersistent);
begin
  if ASource is TEnumeration then
  begin
    inherited Assign(ASource);
    FTypePrefix := TEnumeration(ASource).TypePrefix;
    FIntConsts := TEnumeration(ASource).IntConsts;
    FItems.Assign(TEnumeration(ASource).Items);
    FDescriptions.Assign(TEnumeration(ASource).Descriptions);
    FShortDescriptions.Assign(TEnumeration(ASource).ShortDescriptions);
  end else
    inherited Assign(ASource);
end;

function TEnumeration.GetDescriptions: TStrings;
begin
  Result := FDescriptions;
end;

function TEnumeration.GetShortDescriptions: TStrings;
begin
  Result := FShortDescriptions;
end;

function TEnumeration.GetDisplayName: String;
begin
  Result := FDescription;
  if Result = '' then Result := Name;
end;

function TEnumeration.GetItems: TStrings;
begin
  Result := FItems;
end;

procedure TEnumeration.ObjectRenamed(const OldName: String);
var
  I, J: Integer;
begin
  if OldName = '' then exit;
  // Rename it for all fields
  with Schema do
  for I := 0 to TableDefs.Count - 1 do
    for J := 0 to TableDefs[I].FieldDefs.Count - 1 do
    with TableDefs[I].FieldDefs[J] do
      if AnsiSameText(Enumeration, OldName) then
        Enumeration := Self.Name;
end;

function TEnumeration.GetDisplayLabel: String;
begin
  Result := GetDisplayName;
end;

procedure TEnumeration.SetDescriptions(const Value: TStrings);
begin
  FDescriptions.Assign(Value);
end;

procedure TEnumeration.SetShortDescriptions(const Value: TStrings);
begin
  FShortDescriptions.Assign(Value);
end;

procedure TEnumeration.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

function TEnumeration.Compare(Dest: TCompareSchemaItem): Boolean;
var
  DestEnum: TEnumeration;
begin
  DestEnum := Dest.DestItem as TEnumeration;
  Result := inherited Compare(Dest)
    and FItems.Equals(DestEnum.Items)
    and FShortDescriptions.Equals(DestEnum.ShortDescriptions)
    and FDescriptions.Equals(DestEnum.Descriptions);
  Dest.PropsEqual := Result;
end;

function TEnumeration.GetDelphiDecl: String;
var
  I: Integer;
  EnumConstName, EnumConstValue, EnumConstDescription: String;
begin
  Result := '';
  for I := 0 to Items.Count - 1 do
  begin
    EnumConstName := ValueFromIndex(Items, I);
    EnumConstValue := Items.Names[I];
    EnumConstDescription := Descriptions.Values[EnumConstValue];
    if EnumConstDescription = '' then
      EnumConstDescription := EnumConstName;
    if not IntConsts then
      EnumConstValue := AnsiQuotedStr(EnumConstValue, '''');
    Result := Result + Format('  %s%s = %s; { %s }'#13#10,
      [TypePrefix, EnumConstName, EnumConstValue, EnumConstDescription])
  end;
end;

{ TEnumerations }

function TEnumerations.GetEnumeration(Index: Integer): TEnumeration;
begin
  Result := TEnumeration(inherited GetItem(Index));
end;

function TEnumerations.Find(const Name: String): TEnumeration;
begin
  Result := TEnumeration(inherited Find(Name));
end;

procedure TEnumerations.SetEnumeration(Index: Integer;
  const Value: TEnumeration);
begin
  inherited SetItem(Index, Value);
end;

function TEnumerations.Add: TEnumeration;
begin
  Result := TEnumeration(inherited Add);
end;

{ TTriggerDefinition }

constructor TTriggerDefinition.Create(Collection: TCollection);
begin
  inherited;
  FDefinition := '';
  FName := '';
  FTriggerWhen := [];
  FTriggerActive := taAlways;
  FTriggerType := ttServerSide;
end;

procedure TTriggerDefinition.Assign(ASource: TPersistent);
begin
  if ASource.InheritsFrom(TTriggerDefinition) then
  begin
    inherited Assign(ASource);
    FTriggerType := TTriggerDefinition(ASource).FTriggerType;
    FDefinition := TTriggerDefinition(ASource).FDefinition;
    FTriggerWhen := TTriggerDefinition(ASource).FTriggerWhen;
    FTriggerActive := TTriggerDefinition(ASource).FTriggerActive;
  end else
    inherited Assign(ASource);
end;

function TTriggerDefinition.GetDisplayName: String;
begin
  Result := TableName + '.' + FName;
end;

function TTriggerDefinition.GetTableName: String;
begin
  Result := TableDef.TableName;
end;

function TTriggerDefinition.GetSchemaClassName: String;
begin
  Result := 'Trigger';
end;

procedure TTriggerDefinition.SetPropValue(const PropName, Value: String);
begin
  if AnsiSameText(PropName, 'AddDefinition') then
    FDefinition := FDefinition + #13#10 + Value
  else inherited;
end;

function TTriggerDefinition.GetTriggerWhen(Idx: TChangeType): boolean;
begin
  Result := Idx in FTriggerWhen;
end;

procedure TTriggerDefinition.SetTriggerWhen(Idx: TChangeType; Value: boolean);
begin
  if Value then
    Include(FTriggerWhen, Idx) else
    Exclude(FTriggerWhen, Idx);
end;

procedure TTriggerDefinition.UpdateDefinition(const Value: String = '');

  function ClientSide: Boolean;
  begin
    with Schema do
      Result := (TargetDB = '') or AnsiSameText(TargetDB, 'DBISAM3')
        or AnsiSameText(TargetDB, 'DBISAM4') or AnsiSameText(TargetDB, 'Nexus1');
  end;

begin
  if (FDefinition <> '') and not ClientSide then
    FDefinition := 'CREATE TRIGGER ' + Schema.FormatName(Name, Value) + #13#10 + FDefinition;
end;

{ TTriggerDefinitions }

function TTriggerDefinitions.Add: TTriggerDefinition;
begin
  Result := TTriggerDefinition(inherited Add);
end;

function TTriggerDefinitions.GetTableTriggers(Index: Integer): TTriggerDefinition;
begin
  Result := TTriggerDefinition(inherited Items[Index]);
end;

procedure TTriggerDefinitions.SetTableTriggers(Index: Integer;
  const Value: TTriggerDefinition);
begin
  inherited SetItem(Index, Value);
end;

{ TModuleDefinitions }

function TModuleDefinitions.Add: TModuleDefinition;
begin
  Result := TModuleDefinition(inherited Add);
end;

function TModuleDefinitions.Find(const Name: String): TModuleDefinition;
begin
  Result := TModuleDefinition(inherited Find(Name));
end;

function TModuleDefinitions.GetModuleDefinition(
  Index: Integer): TModuleDefinition;
begin
  Result := TModuleDefinition(inherited GetItem(Index));
end;

procedure TModuleDefinitions.SetModuleDefinition(Index: Integer;
  Value: TModuleDefinition);
begin
  inherited SetItem(Index, Value);
end;

{ TModuleDefinition }

function TModuleDefinition.GetSchemaClassName: String;
begin
  Result := 'Module';
end;

{ TCustomObject }

constructor TCustomObject.Create(Collection: TCollection);
begin
  inherited;
  FSchemaClassName := 'Custom Object';
end;

procedure TCustomObject.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source.InheritsFrom(TCustomObject) then
  begin
    FSchemaClassName := TCustomObject(Source).FSchemaClassName;
  end;
end;

function TCustomObject.Compare(Dest: TCompareSchemaItem): Boolean;
begin
  Result := inherited Compare(Dest);
  Dest.PropsEqual := Result;
end;

function TCustomObject.GetSchemaClassName: String;
begin
  Result := FSchemaClassName;
  if Result = '' then
    Result := 'Custom Object';
end;

procedure TCustomObject.SetSchemaClassName(const Value: String);
begin
  FSchemaClassName := Value;
  // validate rename +++
end;

{ TCustomObjects }

function TCustomObjects.Add(const ASchemaClassName: String = ''): TCustomObject;
begin
  Result := TCustomObject(inherited Add);
  Result.FSchemaClassName := ASchemaClassName;
end;

function TCustomObjects.Find(const Name: String): TCustomObject;
begin
  Result := TCustomObject(inherited Find(Name));
end;

function TCustomObjects.GetCustomObject(Index: Integer): TCustomObject;
begin
  Result := TCustomObject(inherited GetItem(Index));
end;

procedure TCustomObjects.SetCustomObject(Index: Integer;
  Value: TCustomObject);
begin
  inherited SetItem(Index, Value);
end;

{ TPersistentDatabaseSchema }

type
  TPersistentDatabaseSchema = class(TComponent)
  protected
    { Protected declarations }
    FDomains: TDomains;
    FSequences: TSequences;
    FViewDefs: TViewDefinitions;
    FStoredProcs: TStoredProcDefinitions;
    FModules: TModuleDefinitions;
    FCustomObjects: TCustomObjects;
    FRelationships: TRelationships;
    FTableDefs: TTableDefinitions;
    FEnumerations: TEnumerations;
    FUpdates: TDatabaseUpdates;
    FCustomFieldProps: TStrings;
    FDefaultValues: TStrings;

    FTargetDB: String;
    FSchemaName: String;
    FSchemaID: String;
    FVersion: TSchemaVersion;
    FDescription: String;
    FCompanyName: String;
    FLegalCopyright: String;
    FAuthor: String;
    FEncloseIdentifiersInQuotes: Boolean;
    FSysTable: String;

    procedure SetCustomObjects(const Value: TCustomObjects);
    procedure SetDefaultValues(const Value: TStrings);
    procedure SetSequences(const Value: TSequences);
    procedure SetStoredProcs(const Value: TStoredProcDefinitions);
    procedure SetViewDefs(const Value: TViewDefinitions);
    procedure SetDomains(const Value: TDomains);
    procedure SetRelationships(const Value: TRelationships);
    procedure SetTableDefs(const Value: TTableDefinitions);
    procedure SetEnumerations(const Value: TEnumerations);
    procedure SetUpdates(const Value: TDatabaseUpdates);
    procedure SetCustomFieldProps(const Value: TStrings);
    procedure SetModules(const Value: TModuleDefinitions);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property TargetDB: String read FTargetDB write FTargetDB;
    property SchemaID: String read FSchemaID write FSchemaID;
    property Domains: TDomains read FDomains write SetDomains;
    property Sequences: TSequences read FSequences write SetSequences;
    property ViewDefs: TViewDefinitions read FViewDefs write SetViewDefs;
    property StoredProcs: TStoredProcDefinitions read FStoredProcs write SetStoredProcs;
    property Modules: TModuleDefinitions read FModules write SetModules;
    property CustomObjects: TCustomObjects read FCustomObjects write SetCustomObjects;
    property Relationships: TRelationships read FRelationships write SetRelationships;
    property TableDefs: TTableDefinitions read FTableDefs write SetTableDefs;
    property Enumerations: TEnumerations read FEnumerations write SetEnumerations;
    property Updates: TDatabaseUpdates read FUpdates write SetUpdates;
    property CustomFieldProps: TStrings read FCustomFieldProps write SetCustomFieldProps;
    property DefaultValues: TStrings read FDefaultValues write SetDefaultValues;

    property SchemaName: String read FSchemaName write FSchemaName;
    property Description: String read FDescription write FDescription;
    property Author: String read FAuthor write FAuthor;
    property CompanyName: String read FCompanyName write FCompanyName;
    property LegalCopyright: String read FLegalCopyright write FLegalCopyright;
    property EncloseIdentifiersInQuotes: Boolean read FEncloseIdentifiersInQuotes write FEncloseIdentifiersInQuotes default True;
    property SystemTableName: String read FSysTable write FSysTable;

    { These properties are deprecated and obsolete. Left for backward compatibility only. }
    property MinorVersion: SmallInt read FVersion.Minor write FVersion.Minor;
    property MajorVersion: SmallInt read FVersion.Major write FVersion.Major;
  end;

{ TDatabaseSchemaDesigner }

constructor TDatabaseSchemaDesigner.Create(Owner: TDatabaseSchema);
begin
  inherited Create;
  FSchema := Owner;
  FDesignerForm := nil;
  Owner.FDesigner := Self;
end;

destructor TDatabaseSchemaDesigner.Destroy;
begin
  if FSchema <> nil then
    FSchema.FDesigner := nil;
  if FDesignerForm <> nil then
    FDesignerForm.Free;
  inherited Destroy;
end;

procedure TDatabaseSchemaDesigner.Modified;
begin
  // Implement to call IFormDesigner.Modified
end;

procedure TDatabaseSchemaDesigner.SchemaChanged;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

{ TPersistentDatabaseSchema }

constructor TPersistentDatabaseSchema.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDomains := nil;
  FSequences := nil;
  FViewDefs := nil;
  FStoredProcs := nil;
  FRelationships := nil;
  FTableDefs := nil;
  FEnumerations := nil;
  FUpdates := nil;
  FCustomFieldProps := nil;
  FRelationships := nil;
  FModules := nil;
  FCustomObjects := nil;
  FEncloseIdentifiersInQuotes := True;
  FDefaultValues := nil;
end;

procedure TPersistentDatabaseSchema.SetCustomFieldProps(
  const Value: TStrings);
begin
  FCustomFieldProps.Assign(Value);
end;

procedure TPersistentDatabaseSchema.SetDomains(const Value: TDomains);
begin
  FDomains.Assign(Value);
end;

procedure TPersistentDatabaseSchema.SetEnumerations(
  const Value: TEnumerations);
begin
  FEnumerations.Assign(Value);
end;

procedure TPersistentDatabaseSchema.SetStoredProcs(
  const Value: TStoredProcDefinitions);
begin
  FStoredProcs.Assign(Value);
end;

procedure TPersistentDatabaseSchema.SetViewDefs(
  const Value: TViewDefinitions);
begin
  FViewDefs.Assign(Value);
end;

procedure TPersistentDatabaseSchema.SetTableDefs(const Value: TTableDefinitions);
begin
  FTableDefs.Assign(Value);
end;

procedure TPersistentDatabaseSchema.SetUpdates(
  const Value: TDatabaseUpdates);
begin
  FUpdates.Assign(Value);
end;

procedure TPersistentDatabaseSchema.SetSequences(const Value: TSequences);
begin
  FSequences.Assign(Value);
end;

procedure TPersistentDatabaseSchema.SetRelationships(const Value: TRelationships);
begin
  FRelationships.Assign(Value);
end;

procedure TPersistentDatabaseSchema.SetModules(
  const Value: TModuleDefinitions);
begin
  FModules.Assign(Value);
end;

procedure TPersistentDatabaseSchema.SetCustomObjects(
  const Value: TCustomObjects);
begin
  FCustomObjects.Assign(Value);
end;

procedure TPersistentDatabaseSchema.SetDefaultValues(
  const Value: TStrings);
begin
  FDefaultValues.Assign(Value);
end;

{ TDatabaseSchema }

constructor TDatabaseSchema.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // FStoreSchemaID := True;
  FUpdates := TDatabaseUpdates.Create(Self, TDatabaseUpdate);
  FDomains := TDomains.Create(Self, TDomain);
  FSequences := TSequences.Create(Self, TSequence);
  FViewDefs := TViewDefinitions.Create(Self, TViewDefinition);
  FStoredProcs := TStoredProcDefinitions.Create(Self, TStoredProcDefinition);
  FModules := TModuleDefinitions.Create(Self, TModuleDefinition);
  FCustomObjects := TCustomObjects.Create(Self, TCustomObject);
  FTempRelationships := TRelationships.Create(Self, TRelationship);
  FRelationships := TRelationships.Create(Self, TRelationship);
  FTableDefs := TTableDefinitions.Create(Self, TTableDefinition);
  FEnumerations := TEnumerations.Create(Self, TEnumeration);
  FCustomProps := TStringList.Create;
  FDefaultValues := TStringList.Create;
  FCustomProps.Sorted := True;
  FCustomProps.Duplicates := dupIgnore;
  FSchemaName := 'Database';
  FVersion := SchemaVersion(0, 0);
  FDescription := '';
  FPrepared := False;
  FDesigner := nil;
  FNextItemID := 0;
  FEncloseIdentifiersInQuotes := True;
end;

destructor TDatabaseSchema.Destroy;
begin
  FDesigner.Free;
  FUpdates.Free;
  FTableDefs.Free;
  FRelationships.Free;
  FTempRelationships.Free;
  FDomains.Free;
  FSequences.Free;
  FViewDefs.Free;
  FStoredProcs.Free;
  FModules.Free;
  FCustomObjects.Free;
  FEnumerations.Free;
  FCustomProps.Free;
  FDefaultValues.Free;
  inherited Destroy;
end;

procedure TDatabaseSchema.SetUpdates(AValue: TDatabaseUpdates);
begin
  FUpdates.Assign(AValue);
end;

procedure TDatabaseSchema.SetTableDefs(const Value: TTableDefinitions);
begin
  FTableDefs.Assign(Value);
end;

procedure TDatabaseSchema.SetDomains(const Value: TDomains);
begin
  FDomains.Assign(Value);
end;

procedure TDatabaseSchema.SetSequences(const Value: TSequences);
begin
  FSequences.Assign(Value);
end;

procedure TDatabaseSchema.SetStoredProcs(
  const Value: TStoredProcDefinitions);
begin
  FStoredProcs.Assign(Value);
end;

procedure TDatabaseSchema.SetViewDefs(const Value: TViewDefinitions);
begin
  FViewDefs.Assign(Value);
end;

procedure TDatabaseSchema.SetRelationships(const Value: TRelationships);
begin
  FRelationships.Assign(Value);
end;

procedure TDatabaseSchema.SetModules(const Value: TModuleDefinitions);
begin
  FModules.Assign(Value);
end;

procedure TDatabaseSchema.SetCustomObjects(const Value: TCustomObjects);
begin
  FCustomObjects.Assign(Value);
end;

function TDatabaseSchema.StoreDomains: Boolean;
begin
  Result := FDomains.Count > 0;
end;

function TDatabaseSchema.StoreEnumerations: Boolean;
begin
  Result := FEnumerations.Count > 0;
end;

function TDatabaseSchema.StoreSequences: Boolean;
begin
  Result := FSequences.Count > 0;
end;

function TDatabaseSchema.StoreStoredProcs: Boolean;
begin
  Result := FStoredProcs.Count > 0;
end;

function TDatabaseSchema.StoreTableDefs: Boolean;
begin
  Result := FTableDefs.Count > 0;
end;

function TDatabaseSchema.StoreUpdates: Boolean;
begin
  Result := FUpdates.Count > 0;
end;

function TDatabaseSchema.StoreViewDefs: Boolean;
begin
  Result := FViewDefs.Count > 0;
end;

function TDatabaseSchema.StoreRelationships: Boolean;
begin
  Result := Relationships.Count > 0;
end;

function TDatabaseSchema.StoreModules: Boolean;
begin
  Result := FModules.Count > 0;
end;

function TDatabaseSchema.StoreCustomObjects: Boolean;
begin
  Result := FCustomObjects.Count > 0;
end;

procedure TDatabaseSchema.LoadFromStream(Stream: TStream);
var
  P: TPersistentDatabaseSchema;
  StreamPos: Integer;
  Reader: TReader;
begin
  P := TPersistentDatabaseSchema.Create(nil);
  BeginUpdate;
  try
    FRelationships.Clear;
    FTableDefs.Clear;
    FEnumerations.Clear;
    FUpdates.Clear;
    FDomains.Clear;
    FSequences.Clear;
    FViewDefs.Clear;
    FStoredProcs.Clear;
    FModules.Clear;
    FCustomObjects.Clear;

    P.FRelationships := FRelationships;
    P.FTableDefs := FTableDefs;
    P.FEnumerations := FEnumerations;
    P.FUpdates := FUpdates;
    P.FDomains := FDomains;
    P.FSequences := FSequences;
    P.FViewDefs := FViewDefs;
    P.FStoredProcs := FStoredProcs;
    P.FModules := FModules;
    P.FCustomObjects := FCustomObjects;
    P.FDefaultValues := FDefaultValues;
    FNextItemID := 0;

    P.FCustomFieldProps := FCustomProps;
    StreamPos := Stream.Position;
    try
      Stream.ReadResHeader;
    except
      // Maybe an old format. Return and try again without the header
      Stream.Position := StreamPos;
    end;
    Reader := TReader.Create(Stream, 4096);
    try
      Reader.OnError := DoOnReadError;
      Reader.ReadRootComponent(P);
    finally
      Reader.Free;
    end;
    FSchemaName := P.SchemaName;
    FDescription := P.Description;
    FSchemaID := P.FSchemaID;
    FTargetDB := P.FTargetDB;
    FAuthor := P.Author;
    FCompanyName := P.CompanyName;
    FLegalCopyright := P.LegalCopyright;
    FEncloseIdentifiersInQuotes := P.EncloseIdentifiersInQuotes;
    FSysTable := P.FSysTable;
  finally
    P.FRelationships := nil;
    P.FTableDefs := nil;
    P.FEnumerations := nil;
    P.FUpdates := nil;
    P.FDomains := nil;
    P.FSequences := nil;
    P.FViewDefs := nil;
    P.FStoredProcs := nil;
    P.FModules := nil;
    P.FCustomObjects := nil;
    P.FCustomFieldProps := nil;
    P.Free;
    EndUpdate;
  end;
end;

procedure TDatabaseSchema.SaveToStream(Stream: TStream);
var
  P: TPersistentDatabaseSchema;
begin
  P := TPersistentDatabaseSchema.Create(nil);
  try
    P.FRelationships := FRelationships;
    P.FTableDefs := FTableDefs;
    P.FEnumerations := FEnumerations;
    P.FUpdates := FUpdates;
    P.FDomains := FDomains;
    P.FSequences := FSequences;
    P.FViewDefs := FViewDefs;
    P.FStoredProcs := FStoredProcs;
    P.FModules := FModules;
    P.FCustomObjects := FCustomObjects;
    P.FCustomFieldProps := FCustomProps;
    P.FDefaultValues := FDefaultValues;
    P.SchemaName := FSchemaName;
    P.Description := FDescription;
    P.Author := FAuthor;
    P.CompanyName := FCompanyName;
    P.LegalCopyright := FLegalCopyright;
    P.EncloseIdentifiersInQuotes := FEncloseIdentifiersInQuotes;
    P.FSysTable := FSysTable;
    P.FSchemaID := GetSchemaID;
    P.FTargetDB := FTargetDB;

    Stream.WriteComponentRes(P.ClassName, P);

    P.FRelationships := nil;
    P.FTableDefs := nil;
    P.FEnumerations := nil;
    P.FDomains := nil;
    P.FSequences := nil;
    P.FViewDefs := nil;
    P.FStoredProcs := nil;
    P.FModules := nil;
    P.FCustomObjects := nil;
    P.FCustomFieldProps := nil;
    P.FDefaultValues := nil;
    P.FUpdates := nil;
  finally
    P.Free;
  end;
end;

procedure TDatabaseSchema.LoadFromFile(const FileName: String);
begin
  if AnsiSameText(ExtractFileExt(FileName), '.dsd') then
    LoadFromDSDFile(FileName)
  else LoadFromDBSFile(FileName);
end;

procedure TDatabaseSchema.LoadFromDBSFile(const FileName: String);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

type
  TDiagram = class (TComponent)
  public
    procedure DoOnReadError(Reader: TReader; const Message: String; var Handled: Boolean);
    procedure ReadFromResFile(const FileName: String);
  end;

procedure TDiagram.DoOnReadError(Reader: TReader; const Message: String;
  var Handled: Boolean);
begin
  Handled := True;
end;

procedure TDiagram.ReadFromResFile(const FileName: String);
var
  Stream: TFileStream;
  Reader: TReader;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Stream.ReadResHeader;
    Reader := TReader.Create(Stream, 4096);
    try
      Reader.OnError := DoOnReadError;
      Reader.ReadRootComponent(Self);
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TDatabaseSchema.LoadFromDSDFile(const FileName: String);
const
  STORED_DOCUMENT_NAME = 'StoredDocument';
var
  Diagram: TDiagram;
  StoredSchema: TComponent;
begin
  Diagram := TDiagram.Create(nil);
  try
    Diagram.ReadFromResFile(FileName);
    StoredSchema := Diagram.FindComponent(STORED_DOCUMENT_NAME);
    if (StoredSchema = nil) or not StoredSchema.InheritsFrom(TDatabaseSchema) then
      raise Exception.Create(FileName + ' does contain schema information.');
    Self.Assign(StoredSchema);
  finally
    Diagram.Free;
  end;
end;

procedure TDatabaseSchema.LoadFromStr(Value: String);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.Write(Value[1], Length(Value));
    Stream.Position := 0;
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TDatabaseSchema.SaveToStr: String;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream);
    SetLength(Result, Stream.Size);
    Stream.Position := 0;
    Stream.Read(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

procedure TDatabaseSchema.SaveToFile(const FileName: String);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

function TDatabaseSchema.GetVersionLabel: String;
begin
  Result := VersionToStr(FVersion);
end;

procedure TDatabaseSchema.SetVersionLabel(const Value: String);
begin
  // Ignore this...
end;

procedure TDatabaseSchema.SetEnumerations(const Value: TEnumerations);
begin
  FEnumerations.Assign(Value);
end;

procedure TDatabaseSchema.UpdateVersion;
var
  I: Integer;
begin
  FVersion := SchemaVersion(-1, -1);
  for I := 0 to Updates.Count - 1 do
    if CompareVersions(Updates[I].Version, FVersion) > 0 then
      FVersion := Updates[I].Version;
end;

function TDatabaseSchema.GetTableDef(Table: TDataSet;
  const TableName: String): TTableDefinition;
var
  TempTableName: String;
  Idx: Integer;
begin
  Result := nil;
  if Self = nil then exit;
  if TableName <> '' then
    TempTableName := TableName
  else if Table <> nil then begin
    if IsPublishedProp(Table, propTableName) then
      TempTableName := GetStrProp(Table, propTableName)
    else if Table.FieldCount > 0 then
      TempTableName := ChangeFileExt(Table.Fields[0].Origin, '')
    else TempTableName := '';
  end;

  Idx := TableDefs.IndexOf(TempTableName);
  if Idx >= 0 then
    Result := TableDefs[Idx];
end;

function TDatabaseSchema.GetTableDef(
  const TableName: String): TTableDefinition;
begin
  Result := GetTableDef(nil, TableName);
end;

function TDatabaseSchema.GetObjectTable(const ObjectType: String): TTableDefinition;
var
  I: Integer;
begin
  for I := 0 to TableDefs.Count - 1 do
  begin
    Result := TableDefs[I];
    if AnsiSameText(Result.GetObjectType, ObjectType) then exit;
  end;
  Result := nil;
end;

function TDatabaseSchema.GetNextItemID: Integer;
begin
  Inc(FNextItemID);
  Result := FNextItemID;
end;

procedure TDatabaseSchema.Assign(Source: TPersistent);
var
  Stream: TMemoryStream;
begin
  if Source = nil then
    Clear
  else if Source.InheritsFrom(TDBSchemaVersion) then
  begin
    // Assign from snapshot - it is not a valid database schema
    BeginUpdate;
    try
      Description := TDBSchemaVersion(Source).Description;
      Relationships := TDBSchemaVersion(Source).Relationships;
      TableDefs := TDBSchemaVersion(Source).TableDefs;
      Domains := TDBSchemaVersion(Source).Domains;
      Sequences := TDBSchemaVersion(Source).Sequences;
      ViewDefs := TDBSchemaVersion(Source).ViewDefs;
      StoredProcs := TDBSchemaVersion(Source).StoredProcs;
      Modules := TDBSchemaVersion(Source).Modules;
      Enumerations := TDBSchemaVersion(Source).Enumerations;
      CustomObjects := TDBSchemaVersion(Source).CustomObjects;
    finally
      EndUpdate;
    end;
  end else if Source.InheritsFrom(TDatabaseSchema) then
  begin
    if csLoading in TDatabaseSchema(Source).ComponentState then
      TDatabaseSchema(Source).Loaded;
    // Assign properties
    Stream := TMemoryStream.Create;
    try
      TDatabaseSchema(Source).SaveToStream(Stream);
      Stream.Position := 0;
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
    // We must update relations here, because loaded is not used for TDBSchemaVersion
    UpdateRelationships;
  end else
    inherited;
end;

procedure TDatabaseSchema.Clear;
begin
  // Cleanup instance
  FUpdates.Clear;
  FRelationships.Clear;
  FTempRelationships.Clear;
  FTableDefs.Clear;
  FEnumerations.Clear;
  FSequences.Clear;
  FDomains.Clear;
  FViewDefs.Clear;
  FStoredProcs.Clear;
  FCustomObjects.Clear;
  FModules.Clear;
  FDefaultValues.Clear;
  FNextItemID := 0;
  FSchemaName := 'Database';
  FSchemaID := '';
  FVersion := SchemaVersion(0, 0);
end;

procedure TDatabaseSchema.GetTableCategories(List: TStrings);
var
  I: Integer;
  TempList: TStringList;
begin
  TempList := TStringList.Create;
  try
    TempList.Duplicates := dupIgnore;
    TempList.Sorted := True;
    TempList.Add('Documents');
    TempList.Add('Journals');
    TempList.Add('References');
    TempList.Add('Settings');
    for I := 0 to TableDefs.Count - 1 do
      if TableDefs[I].Category <> '' then
        TempList.Add(TableDefs[I].Category);
    List.Assign(TempList);
  finally
    TempList.Free;
  end;
end;

procedure TDatabaseSchema.SetPrepared(const Value: Boolean);
begin
  if FPrepared <> Value then
  begin
    if Value then
      Prepare
    else UnPrepare;
  end;
end;

procedure TDatabaseSchema.Prepare;
var I: Integer;
begin
  if not FPrepared then
  begin
    // Process all tables and prepare them
    for I := 0 to TableDefs.Count - 1 do
      TableDefs[I].Prepare;
    FPrepared := True;
  end;
end;

procedure TDatabaseSchema.UnPrepare;
var I: Integer;
begin
  if FPrepared then
  begin
    // Process all tables and unprepare them
    for I := 0 to TableDefs.Count - 1 do
      TableDefs[I].UnPrepare;
    FPrepared := False;
  end;
end;

procedure TDatabaseSchema.DoOnReadError(Reader: TReader;
  const Message: String; var Handled: Boolean);
begin
  Handled := False;
  if Assigned(FOnReadError) then
    FOnReadError(Reader, Message, Handled);
end;

function TDatabaseSchema.GetCustomProps: TStrings;
begin
  Result := FCustomProps;
end;

procedure TDatabaseSchema.SetCustomProps(const Value: TStrings);
begin
  FCustomProps.Assign(Value);
end;

procedure TDatabaseSchema.UpdateFieldDefinitions;
var
  I, J: Integer;
begin
  for I := 0 to FTableDefs.Count - 1 do
    for J := 0 to FTableDefs[I].FieldDefs.Count - 1 do
      FTableDefs[I].FieldDefs[J].AssignFromDomain;
end;

function TDatabaseSchema.TopLevelUnique(Item: TSchemaCollectionItem; const NewName: String): Boolean;

  function UniqueInCollection(Collection: TSchemaItemsCollection): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to Collection.Count - 1 do
      if (Collection.Items[I] <> Item)
        and (AnsiSameText(NewName, TSchemaCollectionItem(Collection.Items[I]).Name))
      then
      begin
        Result := False;
        exit;
      end;
  end;

begin
  Result := UniqueInCollection(TableDefs) and UniqueInCollection(ViewDefs)
    and UniqueInCollection(StoredProcs);
end;

procedure TDatabaseSchema.Compare(Dest: TDatabaseSchema; List: TList);
var
  ByName: Boolean;
begin
  if Dest <> nil then
  begin
    ByName := SchemaID <> Dest.SchemaID;
    Enumerations.Compare(Dest.Enumerations, List, ByName);
    Domains.Compare(Dest.Domains, List, ByName);
    Sequences.Compare(Dest.Sequences, List, ByName);
    TableDefs.Compare(Dest.TableDefs, List, ByName);
    Relationships.Compare(Dest.Relationships, List, ByName);
    ViewDefs.Compare(Dest.ViewDefs, List, ByName);
    StoredProcs.Compare(Dest.StoredProcs, List, ByName);
    Modules.Compare(Dest.Modules, List, ByName);
    CustomObjects.Compare(Dest.CustomObjects, List, ByName);
  end else begin
    Enumerations.Compare(nil, List);
    Domains.Compare(nil, List);
    Sequences.Compare(nil, List);
    TableDefs.Compare(nil, List);
    Relationships.Compare(nil, List);
    ViewDefs.Compare(nil, List);
    StoredProcs.Compare(nil, List);
    Modules.Compare(nil, List);
    CustomObjects.Compare(nil, List);
  end;
end;

function TDatabaseSchema.GetSchemaID: String;
var
  Guid: TGUID;
begin
  if FSchemaID = '' then
  begin
    CreateGUID(Guid);
    FSchemaID := GUIDToString(Guid);
  end;
  Result := FSchemaID;
end;

procedure TDatabaseSchema.ResetSchemaID;
begin
  FSchemaID := '';
end;

procedure TDatabaseSchema.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('SchemaGUID', ReadSchemaID, WriteSchemaID, True {FStoreSchemaID});
end;

procedure TDatabaseSchema.ReadSchemaID(Reader: TReader);
begin
  FSchemaID := Reader.ReadString;
end;

procedure TDatabaseSchema.WriteSchemaID(Writer: TWriter);
begin
  Writer.WriteString(SchemaID);
end;

procedure TDatabaseSchema.Loaded;
begin
  inherited;
  if not FStreamContainsVersion then
    FReadLibVersion := 200;
  FStreamContainsVersion := False;
  if ReadLibVersion < LibVersion then
    UpdateLibVersion;
  InternalUpdate;
end;

function CompareRelationIndexes(Item1, Item2: Pointer): Integer;
begin
  Result := TRelation(Item1).RelationIndex - TRelation(Item2).RelationIndex;
  if Result < 0 then Result := -1
  else if Result > 0 then Result := 1;
end;

procedure TDatabaseSchema.UpdateRelationships;
var
  I: Integer;
  Rel, PeerRel: TRelation;
  RelSide, PeerSide: TRelationSide;
  NewRelationship, TempRelationship: TRelationship;
begin
  // Process temp relationships
  while FTempRelationships.Count > 0 do
  with FTempRelationships[0] do
  begin
    if Assigned(MasterRelation) and Assigned(DetailRelation) then
      InternalRelationshipError(FTempRelationships[0]);

    Rel := DetailRelation;
    if Rel = nil then
      Rel := MasterRelation;
    if Rel = nil then
      InternalRelationshipError(FTempRelationships[0]);

    PeerSide := OppositeSide[Side(Rel)];
    if FRelation[PeerSide] <> nil then
      InternalRelationshipError(FTempRelationships[0]);
    PeerRel := Rel.FindPeerRelation;

    if (PeerRel <> nil) and not AnsiSameText(PeerRel.PeerRelationName, Rel.Name) then
      PeerRel := nil;

    if Rel = PeerRel then
      InternalRelationshipError(FTempRelationships[0]);

    TempRelationship := nil;

    if (PeerRel <> nil) and PeerRel.IsDataTemporary and (PeerRel.GetPeerRelation = nil) then
      TempRelationship := PeerRel.FRelationship;

    if TempRelationship <> nil then
    begin
      RelSide := TempRelationship.Side(PeerRel);
      FRelation[PeerSide] := PeerRel;

      TempRelationship.FRelation[sideDetail] := nil;
      TempRelationship.FRelation[sideMaster] := nil;
      FTableName[PeerSide] := TempRelationship.FTableName[RelSide];
      FRelationName[PeerSide] := TempRelationship.FRelationName[RelSide];
      FKeyFields[PeerSide] := TempRelationship.FKeyFields[RelSide];
      FCondition[PeerSide] := TempRelationship.FCondition[RelSide];
      FRelDescription[PeerSide] := TempRelationship.FRelDescription[RelSide];
      EnforceForeignKey := EnforceForeignKey or TempRelationship.EnforceForeignKey;
    end;

    // Merge into Relationship
    NewRelationship := Relationships.Add;
    NewRelationship.Assign(FTempRelationships[0]);
    if Rel.FItemID > 0 then
      NewRelationship.ItemID := Rel.FItemID
    else NewRelationship.ItemID;
    NewRelationship.FRelation := FTempRelationships[0].FRelation;
    FTempRelationships[0].FRelation[sideDetail] := nil;
    FTempRelationships[0].FRelation[sideMaster] := nil;

    if NewRelationship.FRelation[sideDetail] <> nil then
    begin
      NewRelationship.FRelation[sideDetail].FRelationship := NewRelationship;
      NewRelationship.FRelation[sideDetail].FName := NewRelationship.FRelationName[sideDetail];
    end;

    if NewRelationship.FRelation[sideMaster] <> nil then
    begin
      NewRelationship.FRelation[sideMaster].FRelationship := NewRelationship;
      NewRelationship.FRelation[sideMaster].FName := NewRelationship.FRelationName[sideMaster];
    end;

    FTempRelationships[0].Free;
    TempRelationship.Free;
  end;

  // Update all relationships
  for I := 0 to Relationships.Count - 1 do
    Relationships[I].UpdateRelations;

  // MB: 04-30-07 - Sort relations according to their stored index
  for I := 0 to TableDefs.Count - 1 do
    TableDefs[I].Relations.Sort(@CompareRelationIndexes);
end;

function TDatabaseSchema.FindIndexGlobal(
  const IndexName: String): TIndexDefinition;
var
  I: Integer;
begin
  if IndexName <> '' then
  for I := 0 to TableDefs.Count - 1 do
  begin
    Result := TableDefs[I].IndexDefs.Find(IndexName);
    if Result <> nil then exit;
  end;
  Result := nil;
end;

procedure TDatabaseSchema.BeginUpdate;
begin
  Inc(FUpdateCounter);
  UnPrepare;
end;

procedure TDatabaseSchema.EndUpdate;
begin
  if FUpdateCounter > 0 then
    Dec(FUpdateCounter);
  if FUpdateCounter = 0 then
    InternalUpdate;
end;

procedure TDatabaseSchema.UpdateSQLFieldTypes;
var
  I, J: Integer;
begin
  //DB +++
  if not Assigned(GlobalSQLFieldTypeChanged) then exit;
  for I := 0 to TableDefs.Count - 1 do
  with TableDefs[I] do
    for J := 0 to FieldDefs.Count - 1 do
      GlobalSQLFieldTypeChanged(Self, FieldDefs[J], FieldDefs[J].SQLFieldType);
  
  for I := 0 to Domains.Count - 1 do
    GlobalSQLFieldTypeChanged(Self, Domains[I], Domains[I].SQLFieldType);
end;

procedure TDatabaseSchema.InternalUpdate;
begin
  UpdateSQLFieldTypes;
  UpdateRelationships;
  UpdateViews;
  Prepare;
end;

procedure TDatabaseSchema.UpdateViews;
var
  I: integer;
  V: TTableDefinition;
begin
  for I := 0 to ViewDefs.Count-1 do
  begin
    V := TTableDefinition.Create(TableDefs);
    V.IsView := True;
    V.Assign(ViewDefs[I]);
    V.Definition := ViewDefs[I].Definition;
  end;
  ViewDefs.Clear;
end;

function TDatabaseSchema.IsUpdating: Boolean;
begin
  Result := FUpdateCounter > 0;
end;

function TDatabaseSchema.GetLibVersion: Integer;
begin
  Result := dbSchemaLibVersion;
end;

procedure TDatabaseSchema.SetLibVersion(const Value: Integer);
begin
  if csReading in ComponentState then
  begin
    FReadLibVersion := Value;
    FStreamContainsVersion := True;
  end;
end;

procedure TDatabaseSchema.UpdateLibVersion;
var
  I, J: Integer;
begin
  if FReadLibVersion < 204 then
  begin
    // Iterate all views, stored procs and triggers and update their definitions
    for I := 0 to ViewDefs.Count - 1 do
      ViewDefs[I].UpdateDefinition;
    for I := 0 to StoredProcs.Count - 1 do
      StoredProcs[I].UpdateDefinition;
    for I := 0 to TableDefs.Count - 1 do
      for J := 0 to TableDefs[I].Triggers.Count - 1 do
        TableDefs[I].Triggers[J].UpdateDefinition;
  end;
  FReadLibVersion := dbSchemaLibVersion;
end;

procedure TDatabaseSchema.SetDSDFileName(const Value: String);
begin
  if FDSDFileName <> Value then
  begin
    if Designer <> nil then
      raise Exception.Create(SUnableToAssignDSDFileName);
    FDSDFileName := Value;
  end;
end;

procedure TDatabaseSchema.ForEachItem(OnSchemaItem: TOnSchemaItem;
  ItemClass: TSchemaCollectionItemClass = nil;
  ExactMatchClass: Boolean = False;
  Data: Pointer = nil);

  function ProcessCollection(Coll: TSchemaItemsCollection): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if ExactMatchClass then
    begin
      if Coll.ItemClass <> ItemClass then exit;
    end else begin
      if not Coll.ItemClass.InheritsFrom(ItemClass) then exit;
    end;

    for I := 0 to Coll.Count - 1 do
    begin
      OnSchemaItem(Self, Coll.Items[I], Result, Data);
      if Result then exit;
    end;
  end;

var
  J: Integer;
begin
  if ItemClass = nil then
    ItemClass := TSchemaCollectionItem;

  if ProcessCollection(Enumerations) then exit;
  if ProcessCollection(Sequences) then exit;
  if ProcessCollection(Domains) then exit;
  if ProcessCollection(TableDefs) then exit;
  for J := 0 to TableDefs.Count - 1 do
  begin
    if ProcessCollection(TableDefs[J].FieldDefs) then exit;
    if ProcessCollection(TableDefs[J].IndexDefs) then exit;
    if ProcessCollection(TableDefs[J].Triggers) then exit;
    if ProcessCollection(TableDefs[J].Constraints) then exit;
  end;
  if ProcessCollection(Relationships) then exit;
  if ProcessCollection(ViewDefs) then exit;
  if ProcessCollection(StoredProcs) then exit;
  if ProcessCollection(Modules) then exit;
  if ProcessCollection(CustomObjects) then exit;
  if ProcessCollection(Updates) then exit;
end;

procedure TDatabaseSchema.UpdateItemName(Schema: TDatabaseSchema;
  Item: TSchemaCollectionItem; var Abort: Boolean; Data: Pointer = nil);
begin
  if Item = nil then exit;
  if Item.Name = '' then
    Item.Name := Item.GetAutoName;
end;

procedure TDatabaseSchema.DoSQLFieldTypeChanged(
  Item: TSchemaCollectionItem; const Value: String);
begin
  if not ((csLoading in ComponentState) or IsUpdating) then
  begin
    if Assigned(GlobalSQLFieldTypeChanged) then
      GlobalSQLFieldTypeChanged(Self, Item, Value);
  end;
end;

function TDatabaseSchema.FormatName(const Name, Fmt: String): String;
begin
  if EncloseIdentifiersInQuotes then
    Result := dbSchema.FormatName(Name, Fmt)
  else Result := Name;
end;

type
  TFindData = record
    ItemID: Integer;
    Item: TSchemaCollectionItem;
  end;

procedure TDatabaseSchema.DoFindItemByID(Schema: TDatabaseSchema;
  Item: TSchemaCollectionItem; var Abort: Boolean; Data: Pointer);
begin
  Abort := Item.ItemID = TFindData(Data^).ItemID;
  if Abort then
    TFindData(Data^).Item := Item;
end;

function TDatabaseSchema.FindItemByID(ItemID: Integer): TSchemaCollectionItem;
var
  FindData: TFindData;
begin
  FindData.ItemID := ItemID;
  FindData.Item := nil;
  ForEachItem(DoFindItemByID, nil, False, @FindData);
  Result := FindData.Item;
end;

function TDatabaseSchema.GetDefaultValues: TStrings;
begin
  Result := FDefaultValues;
end;

procedure TDatabaseSchema.SetDefaultValues(const Value: TStrings);
begin
  FDefaultValues.Assign(Value);
end;

function TDatabaseSchema.GetEmpty: boolean;
begin
  Result := (Enumerations.Count = 0) and
    (Sequences.Count = 0) and
    (Domains.Count = 0) and
    (TableDefs.Count = 0) and
    (StoredProcs.Count = 0) and
    (RelationShips.Count = 0) and
    (CustomObjects.Count = 0);
end;

{ TDBSchemaVersion }

function CompareIndexes(Item1, Item2: Pointer): Integer;
var
  SI1, SI2: TSchemaCollectionItem;
begin
  SI1 := TCompareSchemaItem(Item1).SrcItem;
  if SI1 = nil then SI1 := TCompareSchemaItem(Item1).DestItem;
  SI2 := TCompareSchemaItem(Item2).SrcItem;
  if SI2 = nil then SI2 := TCompareSchemaItem(Item2).DestItem;
  if SI1.Index < SI2.Index then
    Result := -1
  else if SI1.Index > SI2.Index then
    Result := 1
  else Result := 0;
end;

constructor TDBSchemaVersion.CreateDifference(Src, Dest: TDatabaseSchema);
var
  Item: TCompareSchema;

  function FindDestItem(List: TList; ADestItemID: Integer): Integer;
  begin
    for Result := 0 to List.Count - 1 do
      with TCompareSchemaItem(List[Result]) do
        if (SrcItem <> nil) and (SrcItem.ItemID = ADestItemID) then exit;
    Result := -1;
  end;

  procedure ProcessList(Parent: TObject; List: TList);
  var
    I: Integer;
    C: TSchemaItemsCollection;
    NewItem: TSchemaCollectionItem;
  begin
    // All items must be added in the order of their old indexes
    List.Sort(@CompareIndexes);

    for I := 0 to List.Count - 1 do
    with TCompareSchemaItem(List[I]) do
    begin
      if GetItem.InheritsFrom(TRelation) then continue;
      C := GetItemCollection(Parent, TSchemaCollectionItemClass(GetItem.ClassType));
      if C = nil then continue;
      // Here's a place where we can avoid storing relations
      if SrcItem <> nil then
      begin
        NewItem := C.Add;
        // If an item has beed modified or deleted - we store its previous version
        NewItem.FItemID := SrcItem.ItemID;
        NewItem.FOldIndex := SrcItem.Index;
        // We have to avoid assignment for objects, that contains collections
        if NewItem is TTableDefinition then
        with TTableDefinition(NewItem) do
        begin
          FName := SrcItem.Name;
          FProps.Assign(SrcItem.Props);
          FDescription := SrcItem.Description;
          FObjectType := TTableDefinition(SrcItem).ObjectType;
          FReplicate := TTableDefinition(SrcItem).Replicate;
          FTypePrefix := TTableDefinition(SrcItem).TypePrefix;
          FCategory := TTableDefinition(SrcItem).Category;
          FObjectKeyFields := TTableDefinition(SrcItem).ObjectKeyFields;
          FMasterRecord := TTableDefinition(SrcItem).MasterRecord;
          FObjectKeyCaseInsensitive := TTableDefinition(SrcItem).ObjectKeyCaseInsensitive;
          FProduceSQL := SrcItem.ProduceSQL;
          FIsView := TTableDefinition(SrcItem).FIsView;
          FDefinition.Assign(TTableDefinition(SrcItem).Definition);

           // Only tables have subitems
          if SubItems <> nil then
            ProcessList(NewItem, SubItems);
        end else
          NewItem.Assign(SrcItem);
       end else begin
        // If an item has beed created - we code removing it
        if FindDestItem(List, DestItem.ItemID) < 0 then
        begin
          NewItem := C.Add;
          NewItem.FItemID := -DestItem.ItemID; // deleted
          NewItem.FName := DestItem.Name;
        end;
      end;
    end;
  end;

begin
  inherited Create(nil);
  // FStoreSchemaID := False;
  FDate := SysUtils.Date;
  FSchemaID := Src.SchemaID;
  FVersion := Src.Version;
  if Src is TDBSchemaVersion then
    FUpdateAdded := TDBSchemaVersion(Src).UpdateAdded;
  Item := TCompareSchema.Create(Src, Dest);
  try
    if Item.SubItems <> nil then
      ProcessList(Self, Item.SubItems);
  finally
    Item.Free;
  end;
end;

constructor TDBSchemaVersion.CreateCheckpoint(Schema: TDatabaseSchema);
begin
  inherited Create(nil);
  // FStoreSchemaID := False;
  FDate := SysUtils.Date;
  FSchemaID := Schema.SchemaID;
  Assign(Schema);
  // Invoke update relationships manually, otherwise it won't be invoked for
  // new checkpoints.
  UpdateRelationships;
  Updates.Clear;
  FVersion := Schema.FVersion;
end;

procedure TDBSchemaVersion.ApplyDifference(Dest: TDatabaseSchema);

  procedure ProcessCollection(SrcCol, DestCol: TSchemaItemsCollection);
  var
    I, ID: Integer;
    DestItem: TSchemaCollectionItem;
  begin
    for I := 0 to SrcCol.Count - 1 do
    begin
      ID := abs(SrcCol[I].ItemID);
      DestItem := DestCol.FindByItemID(ID);

      if SrcCol[I].ItemID < 0 then
      begin
        if (DestItem = nil) or (SrcCol.FindByItemID(ID) <> nil) then continue;
        if (SrcCol[I] is TRelation) and not TRelation(SrcCol[I]).IsDataTemporary then continue;
        DestItem.Free;
      end else
      begin
        if DestItem = nil then
          DestItem := DestCol.Add;
        DestItem.FItemID := ID;
        if SrcCol[I] is TTableDefinition then
        with TTableDefinition(SrcCol[I]) do
        begin
          TTableDefinition(DestItem).FName := Name;
          TTableDefinition(DestItem).FDescription := Description;
          TTableDefinition(DestItem).FProps.Assign(Props);
          TTableDefinition(DestItem).FTypePrefix := TypePrefix;
          TTableDefinition(DestItem).FReplicate := Replicate;
          TTableDefinition(DestItem).FCategory := Category;
          TTableDefinition(DestItem).FObjectType := ObjectType;
          TTableDefinition(DestItem).FObjectKeyFields := ObjectKeyFields;
          TTableDefinition(DestItem).FMasterRecord := MasterRecord;
          TTableDefinition(DestItem).FObjectKeyCaseInsensitive := ObjectKeyCaseInsensitive;
          TTableDefinition(DestItem).FProduceSQL := ProduceSQL;
          TTableDefinition(DestItem).FIsView := IsView;
          TTableDefinition(DestItem).Definition.Assign(Definition);

          ProcessCollection(FieldDefs, TTableDefinition(DestItem).FieldDefs);
          ProcessCollection(IndexDefs, TTableDefinition(DestItem).IndexDefs);
          if StoreRelations then
            ProcessCollection(Relations, TTableDefinition(DestItem).Relations);
          ProcessCollection(Triggers, TTableDefinition(DestItem).Triggers);
          ProcessCollection(Constraints, TTableDefinition(DestItem).Constraints);
        end else
          DestItem.Assign(SrcCol[I]);
      end;
    end;
    // Update ordering
    for I := 0 to SrcCol.Count - 1 do
      if (SrcCol[I].ItemID >= 0) and (SrcCol[I].FOldIndex >= 0) then
      begin
        DestItem := DestCol.FindByItemID(SrcCol[I].ItemID);
        if DestItem <> nil then
          DestItem.Index := Min(DestCol.Count - 1, SrcCol[I].FOldIndex);
      end;
  end;

begin
  // We need to iterate all objects, conatined in this schema and add/update/delete
  // them into Dest schema.
  Dest.BeginUpdate;
  try
    ProcessCollection(Domains, Dest.Domains);
    ProcessCollection(Relationships, Dest.Relationships);
    ProcessCollection(TableDefs, Dest.TableDefs);
    ProcessCollection(ViewDefs, Dest.ViewDefs);
    ProcessCollection(StoredProcs, Dest.StoredProcs);
    ProcessCollection(Modules, Dest.Modules);
    ProcessCollection(CustomObjects, Dest.CustomObjects);
    ProcessCollection(Sequences, Dest.Sequences);
    ProcessCollection(Enumerations, Dest.Enumerations);
    Dest.FVersion := Version;
    Dest.FDescription := Description;
    if Dest.InheritsFrom(TDBSchemaVersion) then
      TDBSchemaVersion(Dest).FDate := Date;
  finally
    Dest.EndUpdate;
  end;
end;

function TDBSchemaVersion.GetIndex: Integer;
begin
  Result := Owner.ComponentCount - 1 - ComponentIndex;
end;

procedure TDBSchemaVersion.UpdateVersion;
begin
  // Suppress any updates.
end;

procedure TDBSchemaVersion.InternalUpdate;
begin
  // Suppress any updates.
end;

{ TSchemaVersionHistory }

function TSchemaVersionHistory.GetChildOwner: TComponent;
begin
  Result := Self;
end;

procedure TSchemaVersionHistory.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to ComponentCount - 1 do
    Proc(Components[I]);
end;

function TSchemaVersionHistory.AddVersionCheckpoint(Schema: TDatabaseSchema): TDBSchemaVersion;
var
  Difference: TDBSchemaVersion;
begin
  if VersionCount > 0 then
    Difference := TDBSchemaVersion.CreateDifference(GetCheckpoint, Schema)
  else Difference := nil;
  Result := TDBSchemaVersion.CreateCheckpoint(Schema);
  AddVersion(Result, Difference);
end;

procedure TSchemaVersionHistory.AddVersion(NewVersion, Difference: TDBSchemaVersion);
var
  Counter: Integer;
  Temp: String;
begin
  if VersionCount > 0 then
  begin
    // Replace checkpoint with difference
    Difference.Name := Versions[0].Name;
    Difference.Version := Versions[0].Version;
    Difference.Date := Versions[0].Date;
    Difference.Description := Versions[0].Description;
    Versions[0].Free;
    InsertComponent(Difference);
  end;
  Counter := 1;
  repeat
    Temp := 'ver' + IntToStr(Counter);
    Inc(Counter);
  until FindComponent(Temp) = nil;
  NewVersion.Name := Temp;
  InsertComponent(NewVersion);
end;

function TSchemaVersionHistory.GetCheckpoint: TDBSchemaVersion;
begin
  if VersionCount > 0 then
    Result := Versions[0]
  else Result := nil;
end;

function TSchemaVersionHistory.GetVersionCount: Integer;
begin
  Result := ComponentCount;
end;

function TSchemaVersionHistory.GetVersions(Idx: Integer): TDBSchemaVersion;
begin
  Result := Components[ComponentCount - 1 - Idx] as TDBSchemaVersion;
end;

function TSchemaVersionHistory.CreateSnapshot(Version: TDBSchemaVersion;
  Snapshot: TDatabaseSchema = nil): TDatabaseSchema;
var
  I: Integer;
  Checkpoint: TDBSchemaVersion;
begin
  if Snapshot = nil then
    Result := TDatabaseSchema.Create(nil)
  else Result := Snapshot;
  if Version = nil then exit;
  Checkpoint := GetCheckpoint;
  if Checkpoint = nil then exit;
  Result.SchemaID := Checkpoint.SchemaID;
  Result.Assign(Checkpoint);
  Result.FVersion := Checkpoint.Version;
  I := 0;
  while (Version <> Versions[I]) and (I < VersionCount - 1) do
  begin
    Inc(I);
    Versions[I].ApplyDifference(Result);
  end;
end;

function TSchemaVersionHistory.GetLastAddedCheckpoint(ASchema: TDatabaseSchema): TDatabaseSchema;
var
  I: Integer;
begin
  Result := TDatabaseSchema.Create(nil); // MB 12/09/05 +++ TDBSchemaVersion.Create(nil);
  try
    Result.SchemaID := ASchema.SchemaID;
    if GetCheckpoint <> nil then
    begin
      Result.Assign(GetCheckpoint);
      Result.FVersion := GetCheckpoint.Version;
      I := 1;
      while not Versions[I-1].UpdateAdded do
      begin
        if I < VersionCount then
          Versions[I].ApplyDifference(Result)
        else begin
          Result.Clear;
          Result.SchemaID := ASchema.SchemaID;
          break;
        end;
        Inc(I);
      end;
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

constructor TSchemaVersionHistory.Create(AOwner: TComponent);
begin
  inherited;
  FHistoryVersion := 1;
end;

function TSchemaVersionHistory.GetCurHistoryVersion: Integer;
begin
  Result := 2;
end;

procedure TSchemaVersionHistory.Loaded;
begin
  inherited;
  if GetCheckpoint <> nil then
    GetCheckpoint.UpdateRelationships;
end;

{ TDataSetReference }

constructor TDataSetReference.Create(DataSet: TDataSet; OwnDataSet: Boolean = False);
begin
  inherited Create;
  FOwnDataSet := OwnDataSet;
  AssignDataSet(DataSet);
end;

destructor TDataSetReference.Destroy;
begin
  AssignDataSet(nil);
  inherited Destroy;
end;

procedure TDataSetReference.AssignDataSet(DataSet: TDataSet);
begin
  if DataSet <> FDataSet then
  begin
    if (FDataSet <> nil) and OwnDataSet then
      FreeAndNil(FDataSet);
    FDataSet := DataSet;
    UpdateFields;
  end;
end;

procedure TDataSetReference.IterateFields(OnField: TOnFieldProc; Data: Pointer);
var
  I, Count: Integer;
  PropInfo: PPropInfo;
  TempList: PPropList;
  TypeInfo: PTypeInfo;
begin
  // Iterate published properties and assign corresponding
  // fields to them. All fields must exist in the DataSet, otherwise
  // an exception will be thrown by FieldByName method
  if not Assigned(OnField) then exit;
  TypeInfo := PTypeInfo(Self.ClassType.ClassInfo);
  Count := GetTypeData(TypeInfo)^.PropCount;
  if Count > 0 then
  begin
    GetMem(TempList, Count * SizeOf(Pointer));
    try
      GetPropInfos(TypeInfo, TempList);
      for I := 0 to Count - 1 do
      begin
        PropInfo := TempList^[I];
        if (PropInfo^.PropType^.Kind = tkClass) then
          OnField(PropInfo, Data);
      end;
    finally
      FreeMem(TempList, Count * SizeOf(Pointer));
    end;
  end;
end;

procedure TDataSetReference.UpdateFields;
begin
  IterateFields(UpdateField, nil);
end;

procedure TDataSetReference.CreatePersistentFields;
begin
  if FDataSet <> nil then
    IterateFields(CreatePersistentField, nil);
end;

procedure TDataSetReference.UpdateField(PropInfo: PPropInfo; Data: Pointer);
var
  TypeData: PTypeData;
  Fld: TField;
  PropName: String;
begin
  TypeData := GetTypeData(PropInfo^.PropType^);
  if (TypeData <> nil) and TypeData^.ClassType.InheritsFrom(TField) then
  begin
    if (FDataSet <> nil) and (FDataSet.FieldCount > 0) then
    begin
      PropName := String(PropInfo^.Name);
      Fld := FDataSet.FindField(PropName);
      if (Fld = nil) and (PropName[1] = '_') then
        Fld := FDataSet.FindField(copy(PropName, 2, MaxInt));
      if Fld = nil then
        DatabaseErrorFmt(SDataSetRefFieldNotFound, [PropName, FDataSet.Name]);

      SetObjectProp(Self, PropInfo, Fld)
    end else
      SetObjectProp(Self, PropInfo, nil);
  end;
end;

procedure TDataSetReference.CreatePersistentField(PropInfo: PPropInfo; Data: Pointer);
var
  TypeData: PTypeData;
begin
  TypeData := GetTypeData(PropInfo^.PropType^);
  if (TypeData <> nil) and TypeData^.ClassType.InheritsFrom(TField) then
    SetObjectProp(Self, PropInfo,
      CreateField(FDataSet, TFieldClass(TypeData^.ClassType), String(PropInfo^.Name)));
end;

procedure TDataSetReference.Refresh;
begin
  if FDataSet <> nil then
    FDataSet.Refresh;
end;

{ TDBRangeCursor }

procedure TDBRangeCursor.UpdateFrom(SrcCursor: TDBRangeCursor);
begin
  SetSuppressAutoIncValues(True);
  try
    CopyDataSet(SrcCursor.DataSet, DataSet);
  finally
    SetSuppressAutoIncValues(False);
  end;
end;

procedure TDBRangeCursor.SetSuppressAutoIncValues(Value: Boolean); 
begin
end;

{ TActiveTransaction }

constructor TActiveTransaction.Create;
begin
  inherited Create;
  ChangedObjects := TList.Create;
  ReplicationID := 0;
  SnapshotID := 0;
  Timestamp := Now;
  Aggregates := TObjectList.Create(True);
end;

destructor TActiveTransaction.Destroy;
begin
  FreeObjects(ChangedObjects);
  ChangedObjects.Free;
  Aggregates.Free;
  inherited Destroy;
end;

function TActiveTransaction.GetAggregateLinkData(
  AggregateLink: TAggregateLink): TAggregateLinkData;
var
  I: Integer;
begin
  for I := 0 to Aggregates.Count - 1 do
    if TAggregateLinkData(Aggregates[I]).AggregateLink = AggregateLink then
    begin
      Result := TAggregateLinkData(Aggregates[I]);
      exit;
    end;
  Result := TAggregateLinkData.Create(AggregateLink);
  Aggregates.Add(Result);
end;

function TActiveTransaction.WriteChange(TableDef: TTableDefinition;
  ObjectKey: Variant; AChangeType: TChangeType; SnapshotID: Integer = -1;
  const UserName: String = ''; ChangeStatus: TChangeStatus = csActive): Boolean;
var
  StrKey, StrObjType: String;
  I: Integer;
  State: TObjectState;
begin
  Result := True; // True if the state has been updated
  // Convert to upper case for cose insensitive search
  StrKey := EncodeVariant(ObjectKey);
  StrObjType := AnsiUpperCase(TableDef.GetObjectType);
  if TableDef.ObjectKeyCaseInsensitive then
    StrKey := AnsiUpperCase(StrKey);

  if SnapshotID < 0 then
    SnapshotID := Self.SnapshotID;

  // This for loop cries for optimization!!!
  // Locate Changed object if it has already been modified
  State := nil;
  for I := 0 to ChangedObjects.Count - 1 do
  with TObjectState(ChangedObjects[I]) do
    if (ObjectType = StrObjType) and (ObjectStrKey = StrKey) then
    begin
      State := TObjectState(ChangedObjects[I]);
      // True if the state has to be updated
      Result := (AChangeType in [ctDeleted, ctInserted]) or (State.ChangeType < AChangeType);
      if Result then
        State.ChangeType := AChangeType;
      break;
    end;

  // Insert new object or adjust ChangeType for existing object
  if State = nil then
  begin
    State := TObjectState.Create;
    try
      State.ObjectType := StrObjType;
      State.ObjectKey := ObjectKey;
      State.ObjectStrKey := StrKey;
      State.ChangeType := AChangeType;
      ChangedObjects.Add(State);
    except
      State.Free;
      raise;
    end;
  end;
  // Assigning/Updating additional info
  State.SnapshotID := SnapshotID;
  State.ReplicationID := ReplicationID;
  State.Timestamp := Timestamp;
  State.UserName := UserName;
  State.ChangeStatus := ChangeStatus;
end;


end.


