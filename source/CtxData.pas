(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Classes that implement data container infrastructure.
(*
(*  Copyright (c) 2005-2009, Context Software LLC
(*
(******************************************************************************)
unit CtxData;

{$I CtxVer.inc}

interface

uses SysUtils, Classes, Contnrs, CtxDataTypes;

type
  PObject = ^TObject;

  TCtxDataContainer = class;
  TCtxDataTables = class;
  TCtxDataTable = class;
  TCtxDataColumn = class;
  TCtxDataColumns = class;
  TCtxDataRow = class;

  TCtxDataRelation = class;
  TCtxDataRelations = class;

  TCtxDataAdapter = class;

  (*$HPPEMIT 'namespace Ctxdata' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  class DELPHICLASS TCtxDataColumn;' *)
  (*$HPPEMIT '}'*)

  TCtxColumnArray = array of TCtxDataColumn;

  TCtxCompareOption = (coDescending, coCaseInsensitive, coPartialKey, coNullsLast);
  {$NODEFINE TCtxCompareOption}
  TCtxCompareOptions = set of TCtxCompareOption;
  {$NODEFINE TCtxCompareOptions}

  TCtxCompareOptionsArray = array of TCtxCompareOptions;

  {:$ TCtxDataEventType enumerates different types of events which could be triggered }
  {:$ by data container objects. }
  TCtxDataEventType = (
    { DataContainer events }
    cdeContainerActivated,        // Notifies that container has changed structure
    cdeContainerDeactivated,      // All references to container objects must be released
    cdeContainerChanged,          // Structure changed during BeginUpdate\EndUpdate
    cdeContainerDataChanged,      // Data changed during BeginUpdate\EndUpdate
    cdeTableListChanged,          // These events only occur when container
    cdeRelationListChanged,       // is inactive

    { DataTable events }
    cdeTableDataChanged,          // Table data changed during BeginUpdate\EndUpdate on table level
    cdeTableChanged,              // Table parameters (name) changed
    cdeColumnListChanged,         // This event only occurs when container inacitve

    { Cursor events }
    cdeCursorClosed,
    cdeCursorBeforeOpen,

    { DataRow events }
    cdeRowInserted,
    cdeRowModified,
    cdeRowDeleted
  );

  {:$ TCtxDataRowState enumerates different states bits which can be assigned to a data row. }
  TCtxDataRowState = (drsInserted, drsUpdated, drsDeleted, drsStored, drsEditing, drsNeedRefresh);
  {$NODEFINE TCtxDataRowState}

  (*$HPPEMIT 'namespace Ctxdata' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef int TCtxDataRowState;' *)
  (*$HPPEMIT '  #pragma option push -b-' *)
  (*$HPPEMIT '  enum TCtxDataRowStateValues {drsInserted=0, drsUpdated=1, drsDeleted=2, drsStored=3, drsEditing=4, drsNeedRefresh=5};' *)
  (*$HPPEMIT '  #pragma option pop' *)
  (*$HPPEMIT '}'*)

  {:$ TCtxDataRowStates is a set of states which can be assigned to a data row. }
  TCtxDataRowStates = set of TCtxDataRowState;

  TCtxDataEvent = procedure (Context: TObject; DataEvent: TCtxDataEventType) of object;
  TCtxOnRowEvent = procedure (ARow: TCtxDataRow; Data: Pointer; var AbortIteration: Boolean);

  TCtxOrderByColumn = record
    FColumn: TCtxDataColumn;
    FOptions: TCtxCompareOptions;
  end;
  {$NODEFINE TCtxOrderByColumn}
 
  TCtxOrderByColumns = array of TCtxOrderByColumn;
  {$NODEFINE TCtxOrderByColumns}

  (*$HPPEMIT 'namespace Ctxdata' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  #pragma option push -b-' *)
  (*$HPPEMIT '  enum TCtxCompareOption { coDescending, coCaseInsensitive, coPartialKey, coNullsLast };' *)
  (*$HPPEMIT '  #pragma option pop' *)
  (*$HPPEMIT '  typedef Set<TCtxCompareOption, coDescending, coNullsFirst>  TCtxCompareOptions;' *)
  (*$HPPEMIT '  struct TCtxOrderByColumn' *)
  (*$HPPEMIT '  {' *)
  (*$HPPEMIT '  public:' *)
  (*$HPPEMIT '    TCtxDataColumn* FColumn;' *)
  (*$HPPEMIT '    TCtxCompareOptions FOptions;' *)
  (*$HPPEMIT '  } ;' *)
  (*$HPPEMIT '  typedef DynamicArray<TCtxOrderByColumn >  TCtxOrderByColumns;' *)
  (*$HPPEMIT '}'*)

  TCtxDataContainerDesigner = class
  protected
    FDataContainer: TCtxDataContainer;
    procedure NotifyEvent(Context: TObject; DataEvent: TCtxDataEventType); virtual;
  public
    constructor Create(ADataContainer: TCtxDataContainer);
    destructor Destroy; override;

    procedure GetDataObjects(List: TStringList); virtual; abstract;
    procedure Show; virtual; abstract;

    property DataContainer: TCtxDataContainer read FDataContainer;
  end;

  TCtxCalcFieldsEvent = procedure (Sender: TObject; ARow: TCtxDataRow) of object;

  {:$ TCtxDataContainer is a component, which represents a small memory database, }
  {:$ containing data tables as well as relations between them. TCtxDataContainer }
  {:$ can be created and populated in design-time and store data in form's dfm file }
  {:$ (see StoreDataInDfm property). It can also preserve data while in inactive }
  {:$ state (see PersistentData property). If data container has been restructured }
  {:$ while in inactive state, then the data will be reloaded back if possible next time }
  {:$ it is activated. }
  TCtxDataContainer = class (TComponent)
  protected
    { Designer }
    FDesigner: TCtxDataContainerDesigner;
    FTables: TCtxDataTables;
    FRelations: TCtxDataRelations;
    FParams: TCtxParameters;
    FCursors: TList;
    FActive: Boolean;
    FEventConsumers: TList;
    FUpdateCounter: Integer;
    FStoreDataInDfm: Boolean;
    FPersistentData: Boolean;
    FWritingToStream: Boolean; // temporary state using only then saving to stream/file
    FDataAdapter: TCtxDataAdapter;
    FNotifiedDeactivate: Boolean;
    FStructureChanged: Boolean;
    FRollbackRows: TList;
    FTransactionCounter: Integer;

    FSavedData: TStream;
    FLoadingFromStream: Boolean;

    FOnAfterUpdate: TNotifyEvent;
    FOnBeforeUpdate: TNotifyEvent;
    FOnUpdateFailed: TNotifyEvent;
    FOnCalcFields: TCtxCalcFieldsEvent;

    procedure SetStoreDataInDfm(const Value: Boolean);
    procedure SetDataAdapter(const Value: TCtxDataAdapter);
    function GetParam(const Name: String): Variant;
    procedure SetParam(const Name: String; const Value: Variant);
    procedure SetActive(Value: Boolean);
    procedure SetTables(const Value: TCtxDataTables);
    procedure SetRelations(const Value: TCtxDataRelations);
    procedure SetParams(const Value: TCtxParameters);

    procedure NotifyEvent(Context: TObject; DataEvent: TCtxDataEventType);
    procedure DefineProperties(Filer: TFiler); override;

    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoBeforeUpdate; virtual;
    procedure DoAfterUpdate; virtual;
    procedure DoOnUpdateFailed; virtual;
    procedure ClearData;
    procedure Prepare;
    procedure UnPrepare;
    procedure AddRollbackRow(ARow: TCtxDataRow);
    procedure DoCalcFields(ARow: TCtxDataRow); virtual;
  public
    {:$ Creates an instance of TCtxDataContainer component. }
    constructor Create(AOwner: TComponent); override;
    {:$ Destroys an instance of TCtxDataContainer component and clears all resources. }
    destructor Destroy; override;
    {:$ Assigns one data container or stream to another. }
    procedure Assign(Source: TPersistent); override;

    {:$ Returns assigned data adapter. Raises exception if data adapter is not found. }
    function CheckDataAdapter(ADataAdapter: TCtxDataAdapter): TCtxDataAdapter;
    {:$ Raises exception if data container is in transaction. }
    procedure CheckNotInTransaction;

    {:$ Saves the content of data container object to stream in binary format. }
    procedure SaveToStream(Stream: TStream; StoreStructure: Boolean = True; StoreData: Boolean = True);
    {:$ Loads data container from stream previously created by SaveToStream method. }
    procedure LoadFromStream(Stream: TStream);

    {:$ Saves the content of data container object to a file in binary format. }
    procedure SaveToFile(const FileName: String; StoreStructure: Boolean = True; StoreData: Boolean = True);
    {:$ Loads data container from a file previously created by SaveToFile method. }
    procedure LoadFromFile(const FileName: String);

    {:$ Fills data container from the the database using DataAdapter. }
    {:$ If DataAdapter is not specified, then the  }
    {:$ default one (see DataAdapter property) is used instead. }
    procedure Fill(ADataAdapter: TCtxDataAdapter = nil); virtual;
    {:$ Update the changes made to data container during offline editing back to }
    {:$ the database using DataAdapter. If DataAdapter is not specified, then the  }
    {:$ default one (see DataAdapter property) is used instead. }
    procedure Update(ADataAdapter: TCtxDataAdapter = nil); virtual;
    {:$ Refreshes (reloads) data container from the the database using DataAdapter. }
    {:$ If DataAdapter is not specified, then the }
    {:$ default one (see DataAdapter property) is used instead. }
    procedure Refresh(ADataAdapter: TCtxDataAdapter = nil); virtual;

    {:$ Begin atomic update of the data container. This is necessary to }
    {:$ process cascade and error constraints and be able to restore the }
    {:$ data container to the valid state (rollback) if the operation has failed. }
    {:$ Transactions work only for atomic edit and delete operations initiated in }
    {:$ ValidateRow method of TCtxDataTable. }
    procedure StartTransaction;
    {:$ End atomic update of the data container and commit changes. }
    procedure CommitTransaction;
    {:$ End atomic update of the data container and rollback changes. }
    procedure RollbackTransaction;
    {:$ Returns True if data container is in transaction. }
    function InTransaction: Boolean;

    {:$ Clear data container. All tables are emptied. The strcture of data }
    {:$ container is not affected by this method. This method will NOT mark }
    {:$ deleted rows as updated so Update method will not write anything to the }
    {:$ database. }
    procedure Clear; virtual;
    {:$ Clear data container entirely, including data and structure (table definitions). }
    procedure ClearStructure; virtual;
    {:$ Empty all tables in data container. The strcture of data }
    {:$ container is not affected by this method. This method will mark all rows }
    {:$ as deleted, so Update method will subsequently delete them all from the database. }
    procedure EmptyTables;
    {:$ Removes all invalid relations for the data container. }
    procedure CleanupRelations;

    {:$ Accept all changes made to data container since it's been filled, updated or since }
    {:$ last call to AcceptChanges or RejectChanges. }
    procedure AcceptChanges;
    {:$ Reject all changes made to data container since it's been filled, updated or since }
    {:$ last call to AcceptChanges or RejectChanges. This method will return data container }
    {:$ to initial state. }
    procedure RejectChanges;

    {:$ Begin batch update of data container and block all notifications and referential }
    {:$ integrity checks. }
    procedure BeginUpdate; virtual;
    {:$ End batch update of data container and notify recepients about all changes }
    {:$ which happens within BeginUpdate\EndUpdate block. }
    procedure EndUpdate; virtual;
    {:$ Returns True if data container is being updated (i.e. within BeginUpdate\EndUpdate block. }
    function IsUpdating: Boolean;

    {:$ Close all cursors for this data container. }
    procedure CloseCursors;

    property Designer: TCtxDataContainerDesigner read FDesigner;

    {:$ Provides array-like access to data container parameters as name-value pairs. }
    property Param[const Name: String]: Variant read GetParam write SetParam; default;
  published
    {:$ Tables collection contain list of tables (TCtxDataTable item) in the data container. }
    property Tables: TCtxDataTables read FTables write SetTables;
    {:$ Relations collection contain list of relations (TCtxDataRelation item) between tables in the data container. }
    property Relations: TCtxDataRelations read FRelations write SetRelations;
    {:$ Specifies whether or not a data container is open. Deactivate data container }
    {:$ before modifying its structure. }
    property Active: Boolean read FActive write SetActive;
    {:$ Specifies whether or not a data will be preserved in memory when }
    {:$ data container closes (Active := False). }
    property PersistentData: Boolean read FPersistentData write FPersistentData default False;
    {:$ Specifies whether or not data container will store its data populated in design-time }
    {:$ in form's DFM. }
    property StoreDataInDfm: Boolean read FStoreDataInDfm write SetStoreDataInDfm default False;
    {:$ This event is fired immediately before the data container it to be updated to database }
    {:$ using Update method. }
    property OnBeforeUpdate: TNotifyEvent read FOnBeforeUpdate write FOnBeforeUpdate;
    {:$ This event is fired immediately after the data container is updated to database }
    {:$ using Update method. }
    property OnAfterUpdate: TNotifyEvent read FOnAfterUpdate write FOnAfterUpdate;
    {:$ This event is fired if update to database using Update method has failed.}
    property OnUpdateFailed: TNotifyEvent read FOnUpdateFailed write FOnUpdateFailed;
    {:$ This event is fired to calcualte fields for data tables. }
    property OnCalcFields: TCtxCalcFieldsEvent read FOnCalcFields write FOnCalcFields;

    {:$ Contains collection of named parameters used to fill data container via adapter. }
    property Params: TCtxParameters read FParams write SetParams;
    {:$ References default data adapter component, used to fill and update this data }
    {:$ container from and to database or other types of data provider. }
    property DataAdapter: TCtxDataAdapter read FDataAdapter write SetDataAdapter;
  end;

  TCtxDataRowClass = class of TCtxDataRow;
  TCtxDataCollection = class;

  {:$ TCtxDataCollectionItem is an ancestor for all collection items in data container. }
  TCtxDataCollectionItem = class (TCollectionItem)
  protected
    FName: String;
    FProps: TStrings;

    function GetCollection: TCtxDataCollection;
    procedure SetProps(const Value: TStrings);
    procedure SetName(const Value: String); virtual;
    function GetItemIndex: Integer;
    procedure SetItemIndex(Value: Integer);
  public
    {:$ Creates an instance of TCtxDataCollectionItem object. }
    constructor Create(Collection: TCollection); override;
    {:$ Destroys the instance of TCtxDataCollectionItem object. }
    destructor Destroy; override;

    {:$ Specifies name of collection item. }
    property Name: String read FName write SetName;
    property Collection: TCtxDataCollection read GetCollection;
  published
    {:$ Specifies 1-based index of a collection item. This property is not stored and only used }
    {:$ to display item number in a collection view grid. }
    property ItemIndex: Integer read GetItemIndex write SetItemIndex stored False;
    {:: This property contains a generic list of extended parameters (as Key\Value pairs) }
    {:: which can be assigned to each data object (e.g. TCtxDataTable, TCtxDataColumn, TCtxDataRelation, etc. }
    property Props: TStrings read FProps write SetProps;
  end;

  {:$ TCtxDataCollection is an ancestor for all collections in data container. }
  TCtxDataCollection = class (TOwnedCollection)
  public
    {:$ Returns an item in TCtxDataCollection by Name. If item is not found an }
    {:$ exception occurs. }
    function Get(const AName: String): TCtxDataCollectionItem;
    {:$ Locates an item in TCtxDataCollection by Name. }
    function Find(const AName: String): TCtxDataCollectionItem;
    {:$ Returns True if AName is not found in the collection, except for an item passed as Item parameter. }
    function IsUnique(const AName: String; Item: TCtxDataCollectionItem = nil): Boolean;
    {:$ Generates an unique item name by appending Counter to the ProposedName. }
    function GetAutoName(const ProposedName: String; Counter: Integer = 0): String; virtual;
    {:$ Returns list of all item Names in the collection. }
    procedure GetNames(List: TStrings);
    {:$ Returns index of an item in the collection}
    function IndexOf(Item: Pointer): Integer;
  end;

  TCtxDataTableValidation = (dtvRequiredColumns, dtvPrimaryKey, dtvForeignKeys, dtvReferencedKeys, dtvCalcFields);
  TCtxDataTableValidations = set of TCtxDataTableValidation;

  TCtxColumnMapInfo = class
    Column: TCtxDataColumn;
    DataType: TCtxDataType;
  end;

  {:$ TCtxDataTable is a collection item representing data table in data container. }
  TCtxDataTable = class (TCtxDataCollectionItem)
  protected
    { Common }
    FRowClass: TCtxDataRowClass;
    FPrepared: Boolean;
    FUpdateCounter: Integer;
    FRows: TObjectList;
    FBufferSize: Integer;
    FDeletedCount: Integer;
    FNullRow: TCtxDataRow;
    FDefaultRow: TCtxDataRow;
    FValidationsRequired: TCtxDataTableValidations;
    { PK }
    FPKColumns: TCtxColumnArray;
    { Columns }
    FColumns: TCtxDataColumns;
    { Rows }
    procedure SetName(const Value: String); override;
    function GetDataContainer: TCtxDataContainer;
    function  GetRow(Idx: Integer): TCtxDataRow;
    function  GetPhysicalRowCount: Integer;
    function  GetRowCount: Integer;
    { Data Columns }
    procedure SetColumns(Value: TCtxDataColumns);
    procedure GetColumnList(AList: TList);
    { PK }
    function  GetPKExists: Boolean;
    function  FindRowPos(ARow: TCtxDataRow; var Pos: Integer): Boolean;
    function  UpdateRowPos(ARow: TCtxDataRow): Boolean;
    procedure DoSort(iLo, iHi: Integer);
    procedure Sort;

    procedure RowChanged(ARow: TCtxDataRow; AColumn: TCtxDataColumn; DataEvent: TCtxDataEventType); virtual;

    procedure DeleteRow(ARow: TCtxDataRow);
    function UnDeleteRow(ARow: TCtxDataRow): TCtxDataRow;
    procedure CheckRequiredColumns(ARow: TCtxDataRow);
    procedure CheckForeignKeys(ARow: TCtxDataRow);
    function CheckFKErrorConstrants(ARow: TCtxDataRow; IsDelete: Boolean): TCtxDataRelation;
    procedure ProcessFKUpdateConstraints(ARow: TCtxDataRow);
    procedure ProcessFKDeleteConstraints(ARow: TCtxDataRow);
    function ValidateRow(ARow: TCtxDataRow; Operation: TCtxDataRowState): Integer; virtual;
    procedure InternalMoveRow(OldIndex, NewIndex: Integer);
    function RowIndex(ARow: TCtxDataRow): Integer;

    procedure NotifyEvent(Context: TObject; DataEvent: TCtxDataEventType);

    procedure CalculateFields(ARow: TCtxDataRow); virtual;
    procedure Prepare; virtual;
    procedure UnPrepare; virtual;
    procedure ClearData;

    function GetDisplayName: String; override;
  public
    {:$ Creates an instance of TCtxDataTable object. }
    constructor Create(ACollection: TCollection); override;
    {:$ Destroys the instance of TCtxDataTable object. }
    destructor Destroy; override;

    {:$ Raises exception if this data table is prepared (i.e. ready to be populated by data). }
    procedure CheckUnPrepared;
    {:$ Raises exception if this data table is not prepared. }
    procedure CheckPrepared;
    {:$ Assigns one instance of TCtxDataTable to another. The data is NOT copied. }
    procedure Assign(Source: TPersistent); override;

    {:$ This column map will contain DestTable columns corresponding to our column indexes. }
    {:$ The number of items in list is equal to number of columns in this table. }
    function CreateColumnMap(DestTable: TCtxDataTable): TList;

    {:$ This function returns true if data table has one or more column marked at Primary Key.}
    function HasPKColumns: Boolean;

    {:$ Begin batch update of data table and block all notifications. Updating data }
    {:$ table does not block referential integrity checks. In order to do that, use }
    {:$ BeginUpdate\EndUpdate methods of data container. }
    procedure BeginUpdate;
    {:$ End batch update of data table and notify recepients about all changes }
    {:$ which happens within BeginUpdate\EndUpdate block. }
    procedure EndUpdate;
    {:$ Returns True if data table is being updated (i.e. within BeginUpdate\EndUpdate block. }
    function IsUpdating: Boolean;

    {:$ Accept all changes made to data table since it's been filled, updated or since }
    {:$ last call to AcceptChanges or RejectChanges. }
    procedure AcceptChanges;
    {:$ Reject all changes made to data table since it's been filled, updated or since }
    {:$ last call to AcceptChanges or RejectChanges. This method will return data container }
    {:$ to initial state. }
    procedure RejectChanges;

    {:$ Clears the contents of data table. This method does not affect the structure }
    {:$ of data table. This method also ignores referrential integrity checks. }
    procedure Clear;
    {:$ Clears the contents of data table. This method does not affect the structure }

    {:$ Fills data table from the the database using DataAdapter. }
    {:$ If DataAdapter is not specified, then the  }
    {:$ default one (see data container's DataAdapter property) is used instead. }
    procedure Fill(ADataAdapter: TCtxDataAdapter = nil);
    {:$ Update the changes made to data table during offline editing back to }
    {:$ the database using DataAdapter. If DataAdapter is not specified, then the  }
    {:$ default one (see data container's DataAdapter property) is used instead. }
    procedure Update(ADataAdapter: TCtxDataAdapter = nil);
    {:$ Refreshes (reloads) data table from the the database using DataAdapter. }
    {:$ If DataAdapter is not specified, then the }
    {:$ default one (see data container's DataAdapter property) is used instead. }
    procedure Refresh(ADataAdapter: TCtxDataAdapter = nil);
    {:$ Refreshes (reloads) data table from the the database using DataAdapter. }
    {:$ If DataAdapter is not specified, then the }
    {:$ default one (see data container's DataAdapter property) is used instead. }
    procedure RefreshRow(ADataRow: TCtxDataRow); virtual;

    { Rows }
    {:$ Creates an empty instance of data row (TCtxDataRow) corresponding to this table's structure. }
    {:$ The data row is not added to table. Use Insert method to insert it into the data table. }
    function NewRow: TCtxDataRow; virtual;
    {:$ Creates an instance of data row (TCtxDataRow) corresponding to this table's structure and }
    {:$ initializes it with default column values. }
    {:$ The data row is not added to table. Use Insert method to insert it into the data table. }
    function New: TCtxDataRow;
    {:$ Initializes data row with default column values. }
    procedure InitDefaultValues(ARow: TCtxDataRow);
    {:$ Inserts data row into this table at given index. The index is ignored if this table }
    {:$ has primary key columns defined. If ARow parameter is nil, then an empty row is }
    {:$ created and added to the table. This method returns the row that has been inserted }
    {:$ into the data table or raises exception if insertion fails. }
    function Insert(const ARow: TCtxDataRow = nil; const AIndex: Integer = -1): TCtxDataRow;
    {:$ Inserts an empty primary key into this data table. All other field values are }
    {:$ not assigned and integrity checks will be ignored for this row until it is }
    {:$ reloaded/refreshed from the database. This method effectively creates a placeholder }
    {:$ for the row in NeedRefresh state pending actual refresh from the database. }
    function InsertKey(const KeyValue: Variant): TCtxDataRow;
    {:$ Deletes row from this data table. The row is not physically deleted, just marked }
    {:$ with Deleted state and placed beyond the end of data table. Deleted rows can be }
    {:$ accessed by iterating data from from RowCount to PhysicalRowCount - 1 range of row indexes. }
    {:$ If this table has referential integrity constraints, then they will be executed }
    {:$ according to thier DeleteAction property. This operation is always Atomic. }
    {:$ In case of Cascade operation, this means that either all child rows will be }
    {:$ deleted successfully or the whole operation will be aborted with exception and }
    {:$ current state will be restored throughout the whole data container. }
    function Delete(ARow: TCtxDataRow): TCtxDataRow;
    {:$ Delete all records in table one by one respecting all referential integrity }
    {:$ constraints, including Cascade deletion of child records. Once Updated (using Update method) }
    {:$ the data will then be deleted from the database (i.e. every record is marked as "deleted"). }
    {:$ In order to simply clear the table, use Clear method instead. }
    procedure EmptyTable;
    {:$ Move row from OldIndex position to NewIndex. This method will only work with }
    {:$ tables, which has no primary key columns defined. Otherwise the exception will }
    {:$ be thrown. }
    procedure MoveRow(OldIndex, NewIndex: Integer);

    {:$ Returns true if referential constraints allows this record to be deleted }
    {:$ (i.e. it has no related records). }
    function CanDeleteRow(ARow: TCtxDataRow): Boolean;
    {:$ Returns true if this table has a N:1 relation with OwnRows = True.}
    function HasOwnerTable: Boolean;

    {:$ Locate row in the data table with values in Cols1 columns equal to values of ARow in Cols2 columns.}
    function FindRow(ARow: TCtxDataRow; const Cols1, Cols2: TCtxColumnArray; Options: TCtxCompareOptions): TCtxDataRow; overload;
    {:$ Locate row in the data table with values in Cols columns equal to values of ARow in Cols columns.}
    function FindRow(ARow: TCtxDataRow; const Cols: TCtxColumnArray; Options: TCtxCompareOptions): TCtxDataRow; overload;
    {:$ Locate row in the data table with values in PK columns equal to values of ARow in PK columns.}
    function FindRow(ARow: TCtxDataRow): TCtxDataRow; overload;
    {:$ Locate row in the data table with values in KeyFields columns equal to KeyValues.}
    function Locate(const KeyFields: String; const KeyValues: Variant; Options: TCtxCompareOptions): TCtxDataRow;

    { Streaming }
    {:$ Read table's data from TReader object. }
    procedure ReadData(Reader: TReader);
    {:$ Write table's data to TWriter object. }
    procedure WriteData(Writer: TWriter);

    {:$ Returns the size of row buffer in bytes. }
    property BufferSize: Integer read FBufferSize;
    {:$ Returns True if this data table is prepared. }
    property Prepared: Boolean read FPrepared;

    {:$ Returns Row (TCtxDataRow object) for given index to provide array-like random }
    {:$ access iteration of the data table. }
    property Rows[Idx: Integer]: TCtxDataRow read GetRow;
    {:$ Returns number of rows stored in the data table, including deleted rows, which are }
    {:$ always stored at the end of Rows array. PhysicalRowCount is always greater or equal }
    {:$ to RowCount. }
    property PhysicalRowCount: Integer read GetPhysicalRowCount;
    {:$ Returns number of actual valid rows stored in the data table. }
    property RowCount: Integer read GetRowCount;

    {:$ Returns reference to parent data container, which contains this data table. }
    property DataContainer: TCtxDataContainer read GetDataContainer;
    {:$ Returns class of data row for this data table. }
    property RowClass: TCtxDataRowClass read FRowClass;
    {:$ Returns array of PrimaryKey columns. This property is only valid after a }
    {:$ data table is prepared (see Prepared property and Prepare method). }
    property PKColumns: TCtxColumnArray read FPKColumns;
    {:$ Returns True if Primary Key columns exist for this data table. }
    property PKExists: Boolean read GetPKExists;
    {:$ Returns an instance of Row for this data table with NULL values in all columns. }
    property NullRow: TCtxDataRow read FNullRow;
    {:$ Returns an instance of Row for this data table with default values in all columns. }
    property DefaultRow: TCtxDataRow read FDefaultRow;
    {:$ Returns set of validations required for the data table. }
    property ValidationsRequired: TCtxDataTableValidations read FValidationsRequired;
  published
    {:$ Contains collection of data columns. }
    property Columns: TCtxDataColumns read FColumns write SetColumns;
    property DataColumns: TCtxDataColumns read FColumns write SetColumns stored False; 
    {:$ Specifies the name of this data table in data container. This name must be unique }
    {:$ within the data container. }
    property Name;
  end;

  {:$ TCtxDataTables is a collection of TCtxDataTable items. A collection of this }
  {:$ type if owned by TCtxDataContainer component (see Tables property). }
  TCtxDataTables = class (TCtxDataCollection)
  protected
    procedure SetTable(Idx: Integer; const Value: TCtxDataTable);
    function GetDataContainer: TCtxDataContainer;
    function GetTable(Idx: Integer): TCtxDataTable;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    {:$ Returns a data table by Name. If the table is not found an exception occurs. }
    function Get(const AName: String): TCtxDataTable;
    {:$ Locates a data table by Name. }
    function Find(const AName: String): TCtxDataTable;
    {:$ Adds new data table to the collection. }
    function Add: TCtxDataTable;
    {:$ Provides array-like access to elements of DataTables collection. }
    property Tables[Idx: Integer]: TCtxDataTable read GetTable write SetTable; default;
    {:$ Returns reference to the data container, owning this collection. }
    property DataContainer: TCtxDataContainer read GetDataContainer;
  end;

  {:$ Describes different column attributes. }
  TCtxDataColumnAttribute = (caRequired, caReadOnly, caPrimaryKey, caForeignKey, caCalculated, caReferencedKey);
  {:$ Describes set of column attributes. }
  TCtxDataColumnAttributes = set of TCtxDataColumnAttribute;

  {:$ TCtxDataColumn is a collection item representing columns in data table. }
  TCtxDataColumn = class (TCtxDataCollectionItem)
  protected
    FDataType: TCtxDataType;
    FDataSize: Word;
    FOffset: Word;
    FIndex: Integer;
    FAttributes: TCtxDataColumnAttributes;
    {Lookup support}
    // FLinkColumn: TCtxDataColumn;
    FRelationName: String;
    FRelation: TCtxDataRelation;
    FDisplayLabel: String;
    FRequireValidation: Boolean;

    function GetDataTypeName: String;
    procedure SetDataTypeName(const Value: String);
    procedure SetName(const Value: String); override;
    procedure SetDataType(Value: TCtxDataType);
    function GetDataSize: word;
    function GetDisplayName: String; override;
    function GetDataLength: word;
    procedure SetDataLength(Value: word);
    function GetAttr(const Index: Integer): Boolean;
    procedure SetAttr(const Index: Integer; const Value: Boolean);
    function GetDataTable: TCtxDataTable;
    function GetRelation: TCtxDataRelation;
  public
    {:$ Creates an instance of TCtxDataColumn item. }
    constructor Create(Collection: TCollection); override;
    {:$ Destroys the instance of TCtxDataColumn item. }
    destructor Destroy; override;
    {:$ Assign one instance of TCtxDataColumn item to another. }
    procedure Assign(Source: TPersistent); override;

    {:$ Returns reference to the data table containing this column. }
    property DataTable: TCtxDataTable read GetDataTable;
    {:$ Returns Index of this column in DataColumns collection. This property is }
    {:$ only valid after the data table has been prepared and is used for faster }
    {:$ access to column's index. The columns cannot be reorganized after a table }
    {:$ has been prepared. In order to accomplish that one would need to }
    {:$ set data container's Active property to False, perform restructure and then }
    {:$ reset it back to True. }
    property Index: Integer read FIndex;
    {:$ Size of data buffer for this column's value. }
    property BufSize: word read GetDataSize;
    {:$ Returns reference to a relation (foreign key) if this column is of type cdtReference. }
    {:$ In this case it is assumed that the value of this column is Row related to }
    {:$ a given row by that relation. }
    property Relation: TCtxDataRelation read GetRelation;
    {:$ Returns True if this column has any constraints which would require validating }
    {:$ its value if it is changing. }
    property RequireValidation: Boolean read FRequireValidation;

    {:$ Specifies set of column attrubutes. }
    property Attributes: TCtxDataColumnAttributes read FAttributes write FAttributes;
  published
    {:$ Specifies the data type for this column. }
    property DataType: TCtxDataType read FDataType write SetDataType;
    {:$ Specifies the length associated with variable length data types for this column. }
    property DataLength: word read GetDataLength write SetDataLength;
    {:$ Specifies column name. Identical to Name property. This property is deprecated. }
    property ColumnName: String read FName write SetName stored False;
    {:$ Specifies column name. The names of all columns within a table must be unique. }
    property Name;

    {:$ Specifies whether this column must have not NULL value assigned. }
    property Required: Boolean index caRequired read GetAttr write SetAttr;
    {:$ Specifies whether this column is read only and cannot be edited. }
    property ReadOnly: Boolean index caReadOnly read GetAttr write SetAttr;
    {:$ Specifies whether this column is part of Primary Key. }
    property PrimaryKey: Boolean index caPrimaryKey read GetAttr write SetAttr;
    {:$ Specifies whether this column is calculated. }
    property Calculated: Boolean index caCalculated read GetAttr write SetAttr;

    {:$ Specifies the dsiplay label for this column. }
    property DisplayLabel: String read FDisplayLabel write FDisplayLabel;
    {:: Define relation for the columns of type cdtReference. }
    property RelationName: String read FRelationName write FRelationName;
    {:$ Returns displayable name for column's data type. }
    property DataTypeName: String read GetDataTypeName write SetDataTypeName stored False;
  end;

  {:$ TCtxDataColumns is a collection of TCtxDataColumn items. A collection of this }
  {:$ type if owned by TCtxDataTable component (see Columns property). }
  TCtxDataColumns = class (TCtxDataCollection)
  protected
    function GetDataTable: TCtxDataTable;
    function GetColumn(Idx: Integer): TCtxDataColumn;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
    procedure Update(Item: TCollectionItem); override;
  public
    {:$ Creates an instance of TCtxDataColumns collection. }
    constructor Create(AOwner: TPersistent); virtual;

    {:$ Adds a column to TCtxDataColumns collection. }
    function Add: TCtxDataColumn;
    {:$ Adds a column to TCtxDataColumns collection and assigns values to its properties. }
    function AddColumn(const AColumnName: String; ADataType: TCtxDataType;
      ARequired: Boolean = False; ADataLength: word = 0): TCtxDataColumn;
    {:$ Finds a column in the TCtxDataColumns collection by Name. }
    function Find(const AName: String): TCtxDataColumn;
    {:$ Finds a column in the TCtxDataColumns collection by Name. This method raises }
    {:$ exception if the column is not found. }
    function Get(const AName: String): TCtxDataColumn;
    {:$ Returns list of column items from the list of semi-colon (;) delimited ColumnNames. }
    procedure GetColumnList(AList: TList; ColumnNames: String);
    {:$ Returns array of column items from the list of semi-colon (;) delimited ColumnNames. }
    function GetColumnArray(ColumnNames: String): TCtxColumnArray;
    {:$ Returns an array of sorting descriptors from the list of semi-colon (;) delimited ColumnNames. }
    function GetOrderByColumns(AColumnNames: String): TCtxOrderByColumns;

    {:$ Provides array-like access to column objects contained in this collection. }
    property Columns[Idx: Integer]: TCtxDataColumn read GetColumn; default;
    {:$ Returns reference to the data table owning this collection of columns. }
    property DataTable: TCtxDataTable read GetDataTable;
  end;

  {:$ TCtxDataRow class represents a data row object which can be contained in a data table. }
  TCtxDataRow = class (TObject)
  protected
    FData: Pointer;                 // Current data buffer
    FOriginalRow: TCtxDataRow;      // Row that contains original data buffer as loaded from database
    FOldRow: TCtxDataRow;           // Row to contain data buffer saved before editing
    FRollbackRow: TCtxDataRow;      // Row to contain data buffer and state preserved during transaction in a container
    FState: TCtxDataRowStates;      // Row state flags
    FDataTable: TCtxDataTable;      // Reference to the data table (effectively, row class type)

    function GetIndex: Integer;
    function GetBufferSize: Integer;
    function GetDataContainer: TCtxDataContainer;

    function GetDataPtr(const Column: TCtxDataColumn; ForWriting: Boolean = False): Pointer;
    function GetAssignedValue(Column: TCtxDataColumn): Boolean;
    procedure SetAssignedValue(Column: TCtxDataColumn; ASet: Boolean);

    function GetOriginalValue(Column: TCtxDataColumn): Variant;
    function GetValue(Column: TCtxDataColumn): Variant;
    function GetValues(const Columns: TCtxColumnArray): Variant;
    function GetReferencedRow(Column: TCtxDataColumn): TCtxDataRow;

    procedure SetValues(const Columns: TCtxColumnArray; Value: Variant);
    procedure SetValue(Column: TCtxDataColumn; const Value: Variant);

    function GetRowValues: Variant;
    procedure SetRowValues(Value: Variant);

    function GetState(Idx: TCtxDataRowState): Boolean;
    procedure SetState(Idx: TCtxDataRowState; ASet: Boolean);

    function  GetAsVariant(const ColumnName: String): Variant;
    procedure SetAsVariant(const ColumnName: String; Value: Variant);

    function GetAsString(Column: TCtxDataColumn): String;
    procedure SetAsString(Column: TCtxDataColumn; Value: String);

    { Buffer access methods }
    procedure InitBuffer(var Buffer: Pointer);
    procedure FreeBuffer(var Buffer: Pointer);

    procedure InternalClear(Buffer: Pointer);
    function InternalGetAssigned(Buffer: Pointer; const Column: TCtxDataColumn): Boolean;
    procedure InternalSetAssigned(Buffer: Pointer; const Column: TCtxDataColumn; Value: Boolean);
    function InternalGetDataPtr(var Buffer: Pointer; const Column: TCtxDataColumn; ForWriting: Boolean): Pointer;
    procedure InternalSetColumnData(const Column: TCtxDataColumn; var DataBuffer: Pointer; ValueBuffer: Pointer);
    procedure InternalGetColumnData(const Column: TCtxDataColumn; var DataBuffer: Pointer; ValueBuffer: Pointer);
    procedure InternalCopyBuffer(SrcBuf, DestBuf: Pointer);

    procedure AssignColumn(Source: TCtxDataRow; Column: TCtxDataColumn); overload;
    procedure AssignColumn(Source: TCtxDataRow; SrcColumn, DestColumn: TCtxDataColumn); overload;

    procedure SaveOriginalValue;
    function IsModified(const Columns: TCtxColumnArray): Boolean;
    function AssignedValues(const Columns: TCtxColumnArray): Boolean;

    procedure TransactionSave;
    procedure TransactionRollback;
    procedure TransactionDiscardRollback;
    function ModifiedByTransaction: Boolean;
  public
    {:$ Creates an instance TCtxDataRow object for the given data table. }
    constructor Create(ADataTable: TCtxDataTable); virtual;
    {:$ Destroys the instance TCtxDataRow object. }
    destructor Destroy; override;
    {:$ Commits all the changes made to this row since the last time AcceptChanges or RejectChanges was called. }
    procedure AcceptChanges;
    {:$ Rejects all changes made to the row since AcceptChanges or RejectChanges was last called. }
    procedure RejectChanges;

    {:$ Begins edit operation on the data row. }
    procedure BeginEdit;
    {:$ Ends edit operation for the data row. }
    procedure EndEdit;
    {:$ Cancels current edit operation on the data row and returns it to the previous state. }
    procedure CancelEdit;
    {:$ Deletes the data row. If this data row belogs to a table, then this object will }
    {:$ not be physically destroyed, but instead moved to the end of the table and marked }
    {:$ as deleted. It will no longer appear in any data set, however it can be restored }
    {:$ (i.e. undeleted) by calling RejectChanges method of a container, table or this row. }
    {:$ In case this row has not yet been inserted into a data table (see TCtxDataTable.Insert method), }
    {:$ the object will be physically destroyed. }
    procedure Delete;

    {:$ Inserts this row into its data table. }
    procedure Store;

    {:$ Checks whether this row is marked for refreshing and if so preforms }
    {:$ refresh using default DataAdapter assigned to the data container this row }
    {:$ and its table belong to. }
    procedure CheckNeedRefresh;

    { Streaming }
    {:$ Read row data from stream using Reader object with a given column map (ColMap) }
    {:$ which specifies the order in which columns appear in stream. }
    procedure ReadData(Reader: TReader; ColMap: TList);
    {:$ Writes row data to stream using Writer object. }
    procedure WriteData(Writer: TWriter);

    {:$ Assigns one instance of row from another. }
    procedure Assign(Source: TCtxDataRow); overload;
    {:$ Assigns one instance of row from another using column map. The Source  row }
    {:$ must belong to the same data table. }
    procedure Assign(Source: TCtxDataRow; ColumnMap: TList); overload;
    {:$ Assigns a value of the Column from the Source row to the same column in this row. }
    {:$ The Source row must belong to the same data table. }
    procedure Assign(Source: TCtxDataRow; Column: TCtxDataColumn); overload;
    {:$ Clears (set to NULL) the value of Column for this row. }
    procedure Clear(const Column: TCtxDataColumn = nil);

    {:$ Returns Column's value into provided Buffer. }
    procedure GetColumnData(const Column: TCtxDataColumn; Buffer: Pointer);
    {:$ Assigns Column's value from the value contained in provided Buffer. If the Buffer is nil }
    {:$ then the Column will be assigned NULL value, i.e. cleared. }
    procedure SetColumnData(const Column: TCtxDataColumn; Buffer: Pointer);

    {:$ Specifies whether this row has been deleted since last call to AcceptChanges or RejectChanges methods. }
    property Deleted: Boolean index drsDeleted read GetState;
    {:$ Specifies whether this row has been inserted since last call to AcceptChanges or RejectChanges methods. }
    property Inserted: Boolean index drsInserted read GetState write SetState;
    {:$ Specifies whether this row has been updated since last call to AcceptChanges or RejectChanges methods. }
    property Updated: Boolean index drsUpdated read GetState;
    {:$ Specifies whether this row needs to be refreshed from the database. }
    property NeedRefresh: Boolean index drsNeedRefresh read GetState write SetState;
    {:$ Specifies whether this row is currently in editing state initianed by call to BeginEdit method. }
    property Editing: Boolean index drsEditing read GetState;
    {:$ Specifies whether this row is contained in a table. The rows created by data table's New and NewRow }
    {:$ methods are not inserted into the table and must be inserted there explicitly by call to }
    {:$ Store method or data table's Insert method. }
    property Stored: Boolean index drsStored read GetState;

    {:$ Provides read-only access to row's state (TCtxDataRowStates set of flags). }
    property State: TCtxDataRowStates read FState;
    {:$ Provides read-only access to row's physical data buffer. You should never access }
    {:$ row's data directly, use Get\SetColumnData methods or Value property instead. }
    property Data: Pointer read FData;
    {:$ Returns the index of this row in the data table. This property returns -1 if this row is not Stored. }
    property Index: Integer read GetIndex;
    {:$ Returns True if the Column has value assigned to it (i.e. NOT NULL). }
    property AssignedValue[Column: TCtxDataColumn]: Boolean read GetAssignedValue write SetAssignedValue;
    {:$ Returns BufferSize for this row. It's the same as data table's BufferSize. }
    property BufferSize: Integer read GetBufferSize;
    {:$ Returns reference to this row's data table. }
    property DataTable: TCtxDataTable read FDataTable;
    {:$ Returns reference to this row's data container. }
    property DataContainer: TCtxDataContainer read GetDataContainer;

    {:$ Returns reference to row as before beginning of editing. }
    property OldRow: TCtxDataRow read FOldRow;
    {:$ Returns reference to the original row as it was initially loaded from the database. }
    property OriginalRow: TCtxDataRow read FOriginalRow;
    {:$ Provides array-like access to column values as Variants. }
    property Value[Column: TCtxDataColumn]: Variant read GetValue write SetValue; // default;
    {:$ Provides array-like access to original column values as Variants. }
    property OriginalValue[Column: TCtxDataColumn]: Variant read GetOriginalValue;
    {:$ Provides array-like access to column values as String. }
    property AsString[Column: TCtxDataColumn]: String read GetAsString write SetAsString;
    {:$ Provides array-like access to multi-column values returned and assigned as variant array. }
    property Values[const Columns: TCtxColumnArray]: Variant read GetValues write SetValues;
    {:$ Provides array-like access to column's value by column name. }
    property Column[const ColumnName: String]: Variant read GetAsVariant write SetAsVariant; default;
    {:$ Returns values of all columns in a single variant array. }
    property RowValues: Variant read GetRowValues write SetRowValues;
    {:$ Returns row referenced by the value in Column of type cdtReference. }
    property ReferencedRow[Column: TCtxDataColumn]: TCtxDataRow read GetReferencedRow;
  end;

  {:$ Defines possible relation types. }
  TCtxDataRelationType = (drtReference, drtOwner);

  {:$ Defines possible foreign key actions, which may apply to Delete and Update operations. }
  TCtxDataRelationAction = (draIgnore, draError, draCascade, draNullify);

  {:$ TCtxDataRelation represents a relation between two tables in a data container. }
  TCtxDataRelation = class (TCtxDataCollectionItem)
  protected
    FPrepared: Boolean;
    FParentColumnNames: String;
    FChildColumnNames: String;
    FParentTableName: String;
    FChildTableName: String;
    FOwnRows: Boolean;
    FDeleteAction: TCtxDataRelationAction;
    FUpdateAction: TCtxDataRelationAction;

    FParentTable: TCtxDataTable;
    FChildTable: TCtxDataTable;
    FParentColumns: TCtxColumnArray;
    FChildColumns: TCtxColumnArray;

    function GetConstraintActive: Boolean;
    function  GetDataContainer: TCtxDataContainer;
    procedure SetName(const Value: String); override;
    procedure SetChildColumnNames(const Value: String);
    procedure SetChildTableName(const Value: String);
    procedure SetParentColumnNames(const Value: String);
    procedure SetParentTableName(const Value: String);
    function GetIdentifying: Boolean;

    procedure Prepare;
    procedure UnPrepare;
  public
    {:$ Creates an instance of TCtxDataRelation item. }
    constructor Create(Collection: TCollection); override;
    {:$ Destroys the instance of TCtxDataRelation item. }
    destructor Destroy; override;
    {:$ Returns reference to the relation's data container. }
    property DataContainer: TCtxDataContainer read GetDataContainer;
    {:$ Assigns one instance of TCtxDataRelation item to another. }
    procedure Assign(Source: TPersistent); override;

    {:$ References the parent table for this relation. This property is only }
    {:$ valid when the data container is Active (i.e. relation and all tables are prepared). }
    property ParentTable: TCtxDataTable read FParentTable;
    {:$ References the child table for this relation. This property is only }
    {:$ valid when the data container is Active (i.e. relation and all tables are prepared). }
    property ChildTable: TCtxDataTable read FChildTable;
    {:$ References key columns in the parent table for this relation. This property is only }
    {:$ valid when the data container is Active (i.e. relation and all tables are prepared). }
    property ParentColumns: TCtxColumnArray read FParentColumns;
    {:$ References key columns in the child table for this relation. This property is only }
    {:$ valid when the data container is Active (i.e. relation and all tables are prepared). }
    property ChildColumns: TCtxColumnArray read FChildColumns;
    {:$ Returns True if this relation object is prepared (usually, when data container is Active). }
    property Prepared: Boolean read FPrepared;
    {:$ Returns True if this relation is a constraint (i.e. one of actions is not set to Ignore). }
    property ConstraintActive: Boolean read GetConstraintActive;
    {:$ Returns True if this relation references primary key in the parent table. }
    property Identifying: Boolean read GetIdentifying;
  published
    {:$ Specifies the name for this relation. This name must be unique within data container. }
    property Name;
    {:$ Specifies the name of the parent table. }
    property ParentTableName: String read FParentTableName write SetParentTableName;
    {:$ Specifies the name of the child table. }
    property ChildTableName: String read FChildTableName write SetChildTableName;
    {:$ Specifies names of key columns in the parent table for this relation. }
    property ParentColumnNames: String read FParentColumnNames write SetParentColumnNames;
    {:$ Specifies names of key columns in the child table for this relation. }
    property ChildColumnNames: String read FChildColumnNames write SetChildColumnNames;
    {:$ Specifies whether parent row owns child rows. This is generally true for }
    {:$ relations that describe containment (which means that child rows are part or
    {:$ conatined within the parent row. e.g. document lines within a document). }
    property OwnRows: Boolean read FOwnRows write FOwnRows;
    {:$ Specifies the action to be taken when the parent row is deleted. }
    property DeleteAction: TCtxDataRelationAction read FDeleteAction write FDeleteAction default draIgnore;
    {:$ Specifies the action to be taken when key columns in the parent row are updated. }
    property UpdateAction: TCtxDataRelationAction read FUpdateAction write FUpdateAction default draIgnore;
  end;

  {:$ TCtxDataRelations is a collection of TCtxDataRelation items. A collection of this }
  {:$ type if owned by TCtxDataContainer component (see Relations property). }
  TCtxDataRelations = class (TCtxDataCollection)
  protected
    FActive: Boolean;
    function  GetRelation(Idx: Integer): TCtxDataRelation;
    procedure SetRelation(Idx: Integer; const Value: TCtxDataRelation);
    function  GetDataContainer: TCtxDataContainer;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    {:$ Finds a relation in the TCtxDataRelations collection by Name. }
    function Find(const AName: String): TCtxDataRelation;
    {:$ Finds a relation in the TCtxDataRelations collection by Name. This method raises }
    {:$ exception if the column is not found. }
    function Get(const AName: String): TCtxDataRelation;
    {:$ Adds a relation to TCtxDataRelations collection. }
    function Add: TCtxDataRelation;
    {:$ Provides array-like access to relations in the TCtxDataRelations collection. }
    property Relations[Idx: Integer]: TCtxDataRelation read GetRelation write SetRelation; default;
    {:$ Returns reference to the data container, owning this collection. }
    property DataContainer: TCtxDataContainer read GetDataContainer;
  end;

  TFilterDataRowEvent = procedure (ARow: TCtxDataRow; var Accept: Boolean) of object;

  {:$ TCtxRowSet represents a selected, filtered and sorted subset of rows from a single data table. }
  TCtxRowSet = class (TObject)
  protected
    FDataTable: TCtxDataTable;
    FRows: TList;
    { State }
    FActive: Boolean;
    FValid: Boolean;
    FFiltered: Boolean;
    { Filter & Key}
    FKeyValue: Variant;
    FKeyCols: TCtxColumnArray;
    FKeyColNames: String;
    FKeyColCount: Integer;
    FMasterRow: TCtxDataRow;
    FRelation: TCtxDataRelation;

    { Sorting }
    FSortCols: TCtxColumnArray;
    FSortOptions: TCtxCompareOptionsArray;
    FSortCount: Integer;

    FOrderByColumns: TCtxOrderByColumns;
    FStartIndex: Integer;
    FRowCount: Integer;

    FOnNotifyDataEvent: TCtxDataEvent;
    FOnFilterDataRow: TFilterDataRowEvent;
    FConsumerObject: TObject;
    { override }
    function  GetRow(Idx: Integer): TCtxDataRow; virtual;
    function  GetRowCount: Integer; virtual;
    function  NativeAccess: Boolean; virtual;
    procedure SetFiltered(Value: Boolean);
    procedure CheckRow(ARow: TCtxDataRow; DataEvent: TCtxDataEventType); virtual;
    procedure BuildList; virtual;
    procedure UpdateRange; virtual;
    procedure ClearList; virtual;
    procedure DoSort(iLo, iHi: Integer);
    function  DoFilterRow(ARow: TCtxDataRow): Boolean;
    function  DoCompareRows(Row1, Row2: TCtxDataRow): Integer;
    procedure DoNotify(Context: TObject; DataEvent: TCtxDataEventType);
    function  SetKeyColumns(const AColumnNames: String): Boolean;
    function  SetKeyValue(AKeyValue: Variant): Boolean;
    function  GetColumn(Idx: Integer): TCtxDataColumn;
    procedure SetActive(Value: Boolean); virtual;
    procedure SetRelation(const Value: TCtxDataRelation);
    procedure SetDataTable(Value: TCtxDataTable);
    function GetDataContainer: TCtxDataContainer;
    function GetOrderByColumns(Index: Integer): TCtxOrderByColumn;
    function GetOrderByColumnCount: Integer;
    function UpdateSorting: Boolean; virtual;
    procedure ResetSorting;
  public
    {:$ Creates an instance of TCtxRowSet object. }
    constructor Create(ADataTable: TCtxDataTable; ARelation: TCtxDataRelation = nil);
    {:$ Destroys the instance of TCtxRowSet object. }
    destructor Destroy; override;

    {:$ Raises an exception if Relation this row set is based on is not valid. }
    procedure CheckRelation;
    {:$ Raises an exception if table is not assigned or not prepared. }
    procedure CheckTable;

    {:$ Assigns master row to select all its detail rows for the relation specified by Relation property. }
    procedure SetMasterRow(Value: TCtxDataRow);
    {:$ Assigns column names and key values to filter the rows in this row set. }
    function SetMasterKey(const AColumnNames: String; AKeyValue: Variant): Boolean; overload;
    {:$ Assigns key values to filter the rows in this row set. The column names must be }
    {:$ previously assigned by call to overloaded SetMasterKey method. }
    function SetMasterKey(AKeyValue: Variant): Boolean; overload;
    {:$ Assigns an array of TCtxOrderByColumn descriptors. }
    procedure SetOrderByColumns(Value: TCtxOrderByColumns);

    {:$ Returns 0-based index of a row in this row set. }
    function IndexOfRow(ARow: TCtxDataRow): Integer;
    {:$ Locate row in this row set. }
    function Locate(const KeyFields: String; const KeyValues: Variant;
      Options: TCtxCompareOptions): TCtxDataRow;
    {:$ Find next row in this row set satisfying OnFilterDataRow event. }
    function FindNext(Restart: Boolean = True; GoForward: Boolean = True;
      ACurPos: Integer = 0): TCtxDataRow;
    function FindRowPos(ARow: TCtxDataRow; var Pos: Integer): Boolean;
    {:$ Refresh (reload, filter and resort rows) row set from the data table. }
    {:$ Refresh only works if the row set is active. }
    procedure Refresh;
    {:$ Returns true if this row set has one or more order by descriptors assigned. }
    function IsSorted: Boolean;
    {:$ Returns true if this row set has OnFilterDataRow assigned and Filtered property is set to True. }
    function IsFiltered: Boolean;

    { Properties }

    {:$ Specifies data table for this row set. }
    property DataTable: TCtxDataTable read FDataTable write SetDataTable;
    {:$ Returns data container for this row set (same as DataTable.DataContainer). }
    property DataContainer: TCtxDataContainer read GetDataContainer;
    {:$ Specifies master row for this row set. The rows in this row set will }
    {:$ be selected from the DataTable based on a relation specified by Relation property. }
    property MasterRow: TCtxDataRow read FMasterRow write SetMasterRow;
    {:$ Specifies whether the row set is active and will be automatically populated and }
    {:$ updated from the referenced data table. }
    property Active: Boolean read FActive write SetActive;
    {:$ Specifies whether the row set is filtered in which case OnFilterDataRow event }
    {:$ will occur for each row to determine whether it should be a part of this row set. }
    property Filtered: Boolean read FFiltered write SetFiltered;
    {:$ Specifies the relation used to form master-detail link. }
    property Relation: TCtxDataRelation read FRelation write SetRelation;
    {:$ Provides random array-like access to rows in this row set. }
    property Rows[Idx: Integer]: TCtxDataRow read GetRow; default;
    {:$ Returns the number of rows in this row set. }
    property RowCount: Integer read GetRowCount;
    {:$ Provides array-style access to the TCtxOrderByColumn descriptors. }
    property OrderByColumns[Index: Integer]: TCtxOrderByColumn read GetOrderByColumns;
    {:$ Returns the number of TCtxOrderByColumn descriptors. }
    property OrderByColumnCount: Integer read GetOrderByColumnCount;

    { Events }

    {:$ This event occurs when data in the underlying table or data container has changed. }
    property OnNotifyDataEvent: TCtxDataEvent read FOnNotifyDataEvent write FOnNotifyDataEvent;
    {:$ This event occurs to filter rows being read from data table. }
    property OnFilterDataRow: TFilterDataRowEvent read FOnFilterDataRow write FOnFilterDataRow;
  end;

  TCtxDataAdapter = class (TComponent)
  protected
    FDisplayName: String;
    FDescription: String;
    FUpdateCounter: Integer;
  public
    { Container level operations }
    {:$ Fills tables in data container from database or other source of data. This method is abstract, so }
    {:$ the descendants must override it. }
    procedure Fill(DataContainer: TCtxDataContainer); virtual; abstract;
    {:$ Updates changes made to the tables in data container to the database or other source of data. This method is abstract, so }
    {:$ the descendants must override it. }
    procedure Update(DataContainer: TCtxDataContainer); virtual; abstract;
    {:$ Refreshes tables in data container from database or other source of data. }
    {:$ This method is implemented by calling Clear and Fill for the given data container. }
    procedure Refresh(DataContainer: TCtxDataContainer); virtual;

    {:$ Begins batch update operation. }
    procedure BeginUpdate; virtual;
    {:$ Ends batch update operation. }
    procedure EndUpdate; virtual;
    {:$ Returns true if batch update operation is in progress. }
    function IsUpdating: Boolean;

    { Table level operations }
    {:$ Fills a given table from database or other source of data. This method is abstract, so }
    {:$ the descendants must override it. }
    procedure FillTable(DataTable: TCtxDataTable; ParentRow: TCtxDataRow = nil); virtual; abstract;
    {:$ Refreshes a given table from database or other source of data. This method is abstract, so }
    {:$ the descendants must override it. }
    procedure RefreshTable(DataTable: TCtxDataTable); virtual; abstract;
    {:$ Updates changes made to the given table into the database or other source of data. This method is abstract, so }
    {:$ the descendants must override it. }
    procedure UpdateTable(DataTable: TCtxDataTable; ParentRow: TCtxDataRow = nil); virtual; abstract;

    { Row level operations }

    {:$ Refreshes a given row from database or other source of data. This method is abstract, so }
    {:$ the descendants must override it. }
    procedure RefreshRow(DataRow: TCtxDataRow); virtual; abstract;
    {:$ Updates changes made to the given row into the database or other source of data. This method is abstract, so }
    {:$ the descendants must override it. }
    procedure UpdateRow(Row: TCtxDataRow); virtual; abstract;

    {:$ Specifies Data Adapter's display name. }
    property DisplayName: String read FDisplayName write FDisplayName;
    {:$ Specifies Data Adapter's description. }
    property Description: String read FDescription write FDescription;
  end;

  procedure SetBit(AData: Pointer; Index: Integer; Value: Boolean);
  function GetBit(AData: Pointer; Index: Integer): Boolean;
  function _CompareRow(Row1, Row2: TCtxDataRow; Col1, Col2: TCtxDataColumn; Options: TCtxCompareOptions): Integer; overload;
  function _CompareRow(Row1, Row2: TCtxDataRow; Col: TCtxDataColumn; Options: TCtxCompareOptions): Integer; overload;
  function CompareRows(Row1, Row2: TCtxDataRow; const Cols1, Cols2: TCtxColumnArray; Options: TCtxCompareOptions): Integer;
  function ForEachRow(ChildTable: TCtxDataTable; ParentRow: TCtxDataRow;
    const ParentColumns, ChildColumns: TCtxColumnArray; OnRowEvent: TCtxOnRowEvent; Data: Pointer): Integer;
  function _VarIsNull(Value: Variant): Boolean;

  function DataTypeNameToType(const Value: String): TCtxDataType;

  procedure InitSchemaContainer(DataContainer: TCtxDataContainer);
  procedure DumpContainerState(Container: TCtxDataContainer; Output: TStrings);

resourcestring
  SItemNotFound = 'Item not found: %s';
  SParameterNotFound = 'Parameter not found: %s';

  SEmptyTableName = 'Table name should not be empty';
  SDuplicateTableName = 'Duplicate table name: %s';
  SDataTableNotFound = 'Table not found: %s';

  SEmptyRelationName = 'Relation name should not be empty';
  SDuplicateRelationName = 'Duplicate relation name: %s';
  SDataRelationNotFound = 'Data relation not found: %s';
  SInvalidRelationDefintion = 'Invalid relation definition: %s';

  SEmptyColumnName = 'Column name should not be empty';
  SDuplicateColumnName = 'Duplicate column name: %s';
  SColumnNotFound = 'Column %s not found in table %s';
  SInvalidColumnLink = 'Invalid reference definition for column %s in table %s';
  SEmptyColumnLink = 'Relation name is not assigned to column %s in table %s';
  SColumnRequired = 'Column %s is required and must be assigned a value';

  SRowNotInEditingState = 'Row is not in editing state';
  SRowInEditingState = 'Row is already begin edited';

  SUnableToPerformOnActiveDataTable = 'Unable to perform this operation on an active data table';
  SUnableToPerformOnInActiveDataTable = 'Unable to perform this operation on inactive data table';

  SUnableToCastValueToVariant = 'Unable to cast column value to variant. Table: %s; Column: %s';
  SUnableToAssignValueAsVariant = 'Unable to assign column value as variant. Table: %s; Column: %s';
  SUnableToAssignRowFromDifferentTable = 'Unable to assign row from a different table.';
  SDataAdapterIsNotAssigned = 'Unable to preform this operation. Data adapter is not assigned';
  SNotSupportedMethod = 'Method is not supported for this data type';

  SErrorOpeningDataTable = 'Error opening data table';
  SInvalidRelation = 'Invalid relation assigned';
  SDataTableNotAssigned = 'Data table not assigned';

  SPrimaryKeyViolation = 'Primary key violation in table "%s"';
  SForeignKeyViolation = 'Related row not found in table "%s" (relation: "%s")';
  SForeignKeyDeleteFailed = 'Unable to delete row. It has related rows in table "%s" (relation "%s")';
  SForeignKeyUpdateFailed = 'Unable to update row. It has related rows in table "%s" (relation "%s")';

  SUnableToPerformWithinTransaction = 'Unable to perform this operation within active transaction';
  SPrimaryKeyNotDefined = 'Primary key is required, but not defined for table "%s"';
  SUnableToMoveRowDueToPrimaryKeyDefined = 'Unable to rearrange rows due to primary key defined for table "%s".'#13#10
    + 'If primary key is defined the rows must be stored in primary key order and cannot be rearranged manually.';

  SEmptyColumnArray = 'Empty column array cannot be used to assign values to';

  SInvalidRowIndex = 'Invalid row index: %d';

const
  DataTypeNames: array [TCtxDataType] of String = (
    'Unknown', 'SmallInt', 'LargeInt', 'Boolean', 'Integer', 'Float',
    'DateTime', 'Date', 'Time', 'String', 'WideString', 'Guid', 'Memo', 'Blob', 'Reference');

const
  // Variant types
  VarTypes: array [TCtxDataType] of Integer =
    (varNULL, varSmallInt, varInt64, varBoolean, varInteger, varDouble,
     varDate, varDate, varDate, varString, varOleStr, varString, varNULL, varNULL, varNULL);

  DataTypeHasSize: array [TCtxDataType] of Boolean =
    (False, False, False, False, False, False, False, False, False, True, True, False, False, False, False);

implementation

uses Variants;

const
  ByRefDataType = [cdtString, cdtWideString, cdtMemo, cdtBlob];
  cdtWideStringTypes = [cdtWideString];
  cdtStringTypes = [cdtString, cdtMemo, cdtBlob];

  DataTypeSizes: array [TCtxDataType] of Integer = (0,
     SizeOf(SmallInt),
     SizeOf(Int64),
     SizeOf(WordBool),
     SizeOf(Integer),
     SizeOf(Double),
     SizeOf(TDateTime),
     SizeOf(TDateTime),
     SizeOf(TDateTime),
     SizeOf(String),
     SizeOf(WideString),
     SizeOf(TGuid),
     SizeOf(String),
     SizeOf(String),
     SizeOf(Pointer)
  );

function DataTypeNameToType(const Value: String): TCtxDataType;
begin
  for Result := Low(TCtxDataType) to High(TCtxDataType) do
    if AnsiSameText(DataTypeNames[Result], Value) then
      exit;
  Result := cdtUnknown;
end;

function ExtractColumnName(const Columns: String; var Pos: Integer; Delimiter: Char = ';'): String;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(Columns)) and (Columns[I] <> Delimiter) do Inc(I);
  Result := Trim(Copy(Columns, Pos, I - Pos));
  if (I <= Length(Columns)) and (Columns[I] = Delimiter) then Inc(I);
  Pos := I;
end;

procedure InitSchemaContainer(DataContainer: TCtxDataContainer);
begin
  with DataContainer do
  begin
    BeginUpdate;
    try
      Tables.Clear;
      Relations.Clear;
      with Tables.Add do
      begin
        Name := 'Tables';
        Columns.AddColumn('Name', cdtString, True, 255);
      end;
      with Tables.Add do
      begin
        Name := 'Columns';
        Columns.AddColumn('Name', cdtString, True, 255);
        Columns.AddColumn('TableName', cdtString, True, 255);
        Columns.AddColumn('ColumnType', cdtInteger, True);
        Columns.AddColumn('DataLength', cdtInteger, True);
        Columns.AddColumn('Required', cdtBoolean, True);
      end;
      with Tables.Add do
      begin
        Name := 'Relations';
        Columns.AddColumn('Name', cdtString, True, 255);
        Columns.AddColumn('ChildTableName', cdtString, True, 255);
        Columns.AddColumn('ChildColumnNames', cdtString, True, 255);
        Columns.AddColumn('ParentTableName', cdtString, True, 255);
        Columns.AddColumn('ParentColumnNames', cdtString, True, 255);
        Columns.AddColumn('OwnRows', cdtInteger, True);
      end;
      with Relations.Add do
      begin
        Name := 'Table_Columns';
        ParentTableName := 'Tables';
        ChildTableName := 'Columns';
        ParentColumnNames := 'Name';
        ChildColumnNames := 'TableName';
        OwnRows := True;
      end;
    finally
      EndUpdate;
      Active := True;
    end;
  end;
end;

procedure AddToColumns(var AColumns: TCtxColumnArray; AColumn: TCtxDataColumn);
var
  I: Integer;
begin
  I := Length(AColumns);
  SetLength(AColumns, I + 1);
  AColumns[I] := AColumn;
end;

procedure AddOrderByColumn(var AOrderByColumns: TCtxOrderByColumns;
  AColumn: TCtxDataColumn; AOptions: TCtxCompareOptions);
var
  I: Integer;
begin
  I := Length(AOrderByColumns);
  SetLength(AOrderByColumns, I + 1);
  AOrderByColumns[I].FColumn := AColumn;
  AOrderByColumns[I].FOptions := AOptions;
end;

procedure SetBit(AData: Pointer; Index: Integer; Value: Boolean); assembler;
asm
        OR      Value,Value
        JZ      @@1
        BTS     [EAX],Index
        RET
@@1:    BTR     [EAX],Index
        RET
end;

function GetBit(AData: Pointer; Index: Integer): Boolean; assembler;
asm
        BT      [EAX],Index
        SBB     EAX,EAX
        AND     EAX,1
end;

(******************************************************************************
  Comparison routines

  Compare data P1, P2 - Pointer on data,
  DataType - type of data,
  Options - compare options
  if P1 > P2 then Result = 1
  if P1 < P2 then Result = -1
  if P1 = P2 then Result = 0}
  if Null1 then Result = 1
  if Null2 then Result = -1
  if Null1 and Null2 then Result = 0
  Null is smallest value
(******************************************************************************)

function _CompareData(P1, P2: Pointer; DataType: TctxDataType; Options: TCtxCompareOptions): Integer;
var
  D: Double;
  P: AnsiString;
  PW: WideString;
  I: Integer;
  L: Int64;
  IsNull: Boolean;

  { compare Null1 and Null2. Set IsNull=True where both are null }
  procedure _CheckNulls;
  begin
    IsNull := True;
    if (P1 = nil) and (P2 = nil) then
      Result := 0
    else if P1 = nil then
      Result := -1
    else if P2 = nil then
      Result := 1
    else
      IsNull := False;
  end;

begin
  Result := 0;
  _CheckNulls;

  if IsNull then
  begin
    if coNullsLast in Options then
      Result := - Result;
    Exit;
  end;

  case DataType of
    cdtFloat, cdtDateTime, cdtDate, cdtTime:
      begin
        D := PDouble(P1)^ - PDouble(P2)^;
        if D > 0 then Result := 1 else
        if D < 0 then Result := -1;
      end;
    cdtSmallInt:
      begin
        I := PSmallint(P1)^ - PSmallint(P2)^;
        if I > 0 then Result := 1 else
        if I < 0 then Result := -1;
      end;
    cdtLargeInt:
      begin
        L := PInt64(P1)^ - PInt64(P2)^;
        if L > 0 then Result := 1 else
        if L < 0 then Result := -1;
      end;
    cdtInteger:
      begin
        I := PInteger(P1)^ - PInteger(P2)^;
        if I > 0 then Result := 1 else
        if I < 0 then Result := -1;
      end;
    cdtBoolean:
      if PWordBool(P1)^ > PWordBool(P2)^ then
        Result := 1 else
      if PWordBool(P1)^ < PWordBool(P2)^ then
        Result := -1;
    cdtGuid: // +++
      Result := CompareStr(GUIDToString(PGuid(P1)^), GUIDToString(PGuid(P2)^));
    cdtString, cdtMemo:
      begin
        if coPartialKey in Options then
        begin
          I := Length(PAnsiString(P1)^);
          P := copy(PAnsiString(P2)^, 1, I);
        end else
          P := PAnsiString(P2)^;
        if coCaseInsensitive in Options then
          Result := AnsiCompareText(String(PAnsiString(P1)^), String(P))
        else
          Result := AnsiCompareStr(String(PAnsiString(P1)^), String(P));
        if Result < 0 then
          Result := -1 else
        if Result > 0 then
          Result := 1;
      end;
    cdtWideString:
      begin
        if coPartialKey in Options then
        begin
          I := Length(PWideString(P1)^);
          PW := copy(PWideString(P2)^, 1, I);
        end else
          PW := PWideString(P2)^;

        {$IFDEF D2009_ORLATER}
        if coCaseInsensitive in Options then
          Result := AnsiCompareText(PWideString(P1)^, PW)
        else
          Result := AnsiCompareStr(PWideString(P1)^, PW);
        {$ELSE}
        if coCaseInsensitive in Options then
          Result := WideCompareText(PWideString(P1)^, PW)
        else
          Result := WideCompareStr(PWideString(P1)^, PW);
        {$ENDIF}

        if Result < 0 then
          Result := -1 else
        if Result > 0 then
          Result := 1;
      end;
  end;
  if coDescending in Options then
    Result := - Result;
end;

function _CompareRow(Row1, Row2: TCtxDataRow; Col1, Col2: TCtxDataColumn;
  Options: TCtxCompareOptions): Integer;
var
  P1, P2: Pointer;
begin
  Result := 0;
  if (Row1 = nil) or (Row2 = nil) or (Col1.FDataType <> Col2.FDataType) then
    Exit;
  P1 := Row1.GetDataPtr(Col1);
  P2 := Row2.GetDataPtr(Col2);
  Result := _CompareData(P1, P2, Col1.DataType, Options);
end;

function _CompareRow(Row1, Row2: TCtxDataRow; Col: TCtxDataColumn;
  Options: TCtxCompareOptions): Integer;
begin
  Result := _CompareRow(Row1, Row2, Col, Col, Options);
end;

function CompareRows(Row1, Row2: TCtxDataRow; const Cols1, Cols2: TCtxColumnArray; Options: TCtxCompareOptions): Integer;
var
  I: Integer;
begin
  Result := 0;
  // MB: Simply exist if is is the same row
  if (Row1 = Row2) and (Cols1 = Cols2) then exit;

  for I := Low(Cols1) to High(Cols1) do
  begin
    Result := _CompareRow(Row1, Row2, Cols1[I], Cols2[I], Options);
    if Result <> 0 then
      Break;
  end;
end;

function ForEachRow(ChildTable: TCtxDataTable; ParentRow: TCtxDataRow;
  const ParentColumns, ChildColumns: TCtxColumnArray; OnRowEvent: TCtxOnRowEvent; Data: Pointer): Integer;
var
  I: Integer;
  Row: TCtxDataRow;
  AbortIteration: Boolean;
begin
  Result := 0;
  I := 0;
  AbortIteration := False;
  while I < ChildTable.RowCount do
  begin
    Row := ChildTable.Rows[I];
    if CompareRows(ParentRow, Row, ParentColumns, ChildColumns, []) = 0 then
    begin
      Inc(Result);
      OnRowEvent(Row, Data, AbortIteration);
    end;
    if not Row.Deleted then
      Inc(I);
  end;
end;

function _VarIsNull(Value: Variant): Boolean;
var
  I: Integer;
begin
  if VarIsArray(Value) then
  begin
    Result := False;
    for I := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
      if not _VarIsNull(Value[I]) then exit;
    Result := True;
  end else
    Result := VarIsNull(Value) or VarIsEmpty(Value);
end;

procedure DumpContainerState(Container: TCtxDataContainer; Output: TStrings);
var
  I, R, C: Integer;
  T: TCtxDataTable;
  Line: String;
begin
  with Container do
  for I := 0 to Tables.Count - 1 do
  begin
    T := Tables[I];
    Output.Add(Format('<table name="%s" PhysicalRowCount=%d RowCount=%d>', [T.Name, T.PhysicalRowCount, T.RowCount]));
    for R := 0 to T.PhysicalRowCount - 1 do
    begin
      Line := '<row';
      if T.Rows[R].NeedRefresh then
        Line := Line + ' need_refresh';
      if T.Rows[R].Deleted then
        Line := Line + ' deleted';
      if T.Rows[R].Inserted then
        Line := Line + ' inserted';
      if T.Rows[R].Updated then
        Line := Line + ' updated';
      if T.Rows[R].Editing then
        Line := Line + ' editing';
      Line := Line + '>';
      Output.Add(Line);
      Line := '';
      for C := 0 to T.Columns.Count - 1 do
      begin
        if Line <> '' then
          Line := Line + ', ';
        if T.Rows[R].AssignedValue[T.Columns[C]] then
          Line := Line + AnsiQuotedStr(T.Rows[R].GetAsString(T.Columns[C]), '"')
        else Line := Line + '{NULL}'
      end;
      Output.Add('  ' + Line);

      if T.Rows[R].Updated then
      begin
        Line := '';
        for C := 0 to T.Columns.Count - 1 do
        begin
          if Line <> '' then
            Line := Line + ', ';
          Line := Line + AnsiQuotedStr(T.Rows[R].OriginalValue[T.Columns[C]], '"')
        end;
        Output.Add('  ORIG:' + Line);
      end;

      if T.Rows[R].FRollbackRow <> nil then
      begin
        Line := '';
        for C := 0 to T.Columns.Count - 1 do
        begin
          if Line <> '' then
            Line := Line + ', ';
          Line := Line + AnsiQuotedStr(T.Rows[R].FRollbackRow.Value[T.Columns[C]], '"')
        end;
        Output.Add('  ROLLBACK:' + Line);
      end;

      Output.Add('</row>');
    end;
    Output.Add('</table>');
  end;
end;

{ TCtxDataContainerDesigner }

constructor TCtxDataContainerDesigner.Create(ADataContainer: TCtxDataContainer);
begin
  inherited Create;
  FDataContainer := ADataContainer;

  // Destroy previous instance of the designer
  FDataContainer.FDesigner.Free;
  // Assign self to the containers designer
  FDataContainer.FDesigner := Self;
end;

destructor TCtxDataContainerDesigner.Destroy;
begin
  FDataContainer.FDesigner := nil;
  inherited Destroy;
end;

procedure TCtxDataContainerDesigner.NotifyEvent(Context: TObject; DataEvent: TCtxDataEventType);
begin
  // Implement in descendants
end;

{ TCtxDataColumns }

constructor TCtxDataColumns.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TCtxDataColumn);
end;

function TCtxDataColumns.Add: TCtxDataColumn;
begin
  Result := TCtxDataColumn(inherited Add);
end;

function TCtxDataColumns.GetDataTable: TCtxDataTable;
begin
  Result := TCtxDataTable(GetOwner);
end;

function TCtxDataColumns.GetColumn(Idx: Integer): TCtxDataColumn;
begin
  Result := TCtxDataColumn(inherited GetItem(Idx));
end;

function TCtxDataColumns.Find(const AName: String): TCtxDataColumn;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    if AnsiSameText(Columns[I].Name, AName) then
    begin
      Result := Columns[I];
      Exit;
    end;
  Result := nil;
end;

procedure TCtxDataColumns.GetColumnList(AList: TList; ColumnNames: String);
var
  I: Integer;
begin
  ASSERT(AList <> nil);
  AList.Clear;
  I := 1;
  while I <= Length(ColumnNames) do
    AList.Add(Get(ExtractColumnName(ColumnNames, I)));
end;

function TCtxDataColumns.GetColumnArray(ColumnNames: String): TCtxColumnArray;
var
  I, P: Integer;
  F: TCtxDataColumn;
begin
  SetLength(Result, 0);
  I := 1;
  P := 0;
  while I <= Length(ColumnNames) do
  begin
    F := Get(ExtractColumnName(ColumnNames, I));
    SetLength(Result, P+1);
    Result[P] := F;
    inc(P);
  end;
end;

function TCtxDataColumns.GetOrderByColumns(AColumnNames: String): TCtxOrderByColumns;
var
  I, P: Integer;
  ColName: String;
  CompareOptions: TCtxCompareOptions;
  F: TCtxDataColumn;
begin
  SetLength(Result, 0);
  I := 1;
  P := 0;
  while I <= Length(AColumnNames) do
  begin
    ColName := ExtractColumnName(AColumnNames, I, ',');
    CompareOptions := [];
    F := Get(ColName);
    SetLength(Result, P + 1);
    Result[P].FColumn := F;
    Result[P].FOptions := CompareOptions; // [coDescending];
    inc(P);
  end;
end;

procedure TCtxDataColumns.Notify(Item: TCollectionItem; Action: TCollectionNotification);
begin
  DataTable.DataContainer.Active := False;
  inherited;
end;

function TCtxDataColumns.Get(const AName: String): TCtxDataColumn;
begin
  Result := Find(AName);
  if Result = nil then
    raise Exception.CreateFmt(SColumnNotFound, [AName, DataTable.Name]);
end;

function TCtxDataColumns.AddColumn(const AColumnName: String;
  ADataType: TCtxDataType; ARequired: Boolean;
  ADataLength: word): TCtxDataColumn;
begin
  Result := Add;
  with Result do
  begin
    Name := AColumnName;
    DataType := ADataType;
    if ARequired then
      Include(FAttributes, caRequired);
    DataLength := ADataLength;
  end;
end;

procedure TCtxDataColumns.Update(Item: TCollectionItem);
begin
  DataTable.NotifyEvent(DataTable, cdeColumnListChanged);
  inherited;
end;

{ TCtxDataCollectionItem }

constructor TCtxDataCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FProps := TStringList.Create;
end;

destructor TCtxDataCollectionItem.Destroy;
begin
  FProps.Free;
  inherited Destroy;
end;

function TCtxDataCollectionItem.GetCollection: TCtxDataCollection;
begin
  Result := (inherited Collection) as TCtxDataCollection;
end;

function TCtxDataCollectionItem.GetItemIndex: Integer;
begin
  Result := Index + 1;
end;

procedure TCtxDataCollectionItem.SetItemIndex(Value: Integer);
begin
  if Value > Collection.Count - 1 then
    Value := Collection.Count - 1;
  if Value < 0 then
    Value := 0;
  Index := Value;
end;

procedure TCtxDataCollectionItem.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TCtxDataCollectionItem.SetProps(const Value: TStrings);
begin
  FProps.Assign(Value);
end;

{ TCtxDataTable }

constructor TCtxDataTable.Create(ACollection: TCollection);
begin
  FName := '';
  FRowClass := TCtxDataRow;
  FColumns := TCtxDataColumns.Create(Self);
  FUpdateCounter := 0;
  FPrepared := False;
  FRows := TObjectList.Create;
  inherited Create(ACollection);
end;

destructor TCtxDataTable.Destroy;
begin
  DataContainer.Active := False;
  FRows.Free;
  FColumns.Clear;
  FColumns.Free;
  FColumns := nil;
  inherited;
end;

procedure TCtxDataTable.InitDefaultValues(ARow: TCtxDataRow);
var
  I: Integer;
  Col: TCtxDataColumn;
begin
  for I := 0 to Columns.Count - 1 do
  begin
    Col := Columns[I];
    if Col.Required and not ARow.AssignedValue[Col] then
    case Col.DataType of
      cdtSmallInt, cdtLargeInt, cdtInteger, cdtFloat: ARow.Value[Col] := 0;
      cdtBoolean: ARow.Value[Col] := False;
      cdtString, cdtWideString: ARow.Value[Col] := '';
    end;
  end;
end;

function TCtxDataTable.NewRow: TCtxDataRow;
begin
  Result := FRowClass.Create(Self);
end;

function TCtxDataTable.New: TCtxDataRow;
begin
  Result := NewRow;
  if FDefaultRow.FData <> nil then
  begin
    Result.InitBuffer(Result.FData);
    Result.InternalCopyBuffer(FDefaultRow.FData, Result.FData);
  end;
  // InitDefaultValues(Result);
end;

function TCtxDataTable.Insert(const ARow: TCtxDataRow = nil; const AIndex: Integer = -1): TCtxDataRow;
var
  NewPos: Integer;
begin
  if ARow = nil then
    Result := New else
    Result := ARow;

  NewPos := ValidateRow(Result, drsInserted);
  if NewPos < 0 then
  begin
    if Result.Deleted then
      NewPos := FRows.Count
    else
      if AIndex < 0 then
        NewPos := FRows.Count - FDeletedCount else
        NewPos := AIndex;
  end;
  FRows.Insert(NewPos, Result);
  Result.SetState(drsStored, True);
  // Save the fact that this row is inserted
  // Rollback row will not be created so we will know to delete this record on rollback
  DataContainer.AddRollbackRow(Result);

  if not IsUpdating then
    Result.SetState(drsInserted, True);
  if not IsUpdating then
    RowChanged(Result, nil, cdeRowInserted);
end;

function TCtxDataTable.InsertKey(const KeyValue: Variant): TCtxDataRow;
begin
  Result := NewRow;
  try
    if not PKExists then
      raise Exception.CreateFmt(SPrimaryKeyNotDefined, [Name]);

    Result.Values[FPKColumns] := KeyValue;
    if FindRow(Result, FPKColumns, []) = nil then
    begin
      Result.NeedRefresh := True;
      Insert(Result);
    end else
      FreeAndNil(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TCtxDataTable.UnDeleteRow(ARow: TCtxDataRow): TCtxDataRow;
begin
  Result := ARow;
  if Result.Deleted then
  begin
    InternalMoveRow(Result.Index, RowCount);
    Dec(FDeletedCount);
    if PKExists then
      UpdateRowPos(Result);
    Result.SetState(drsDeleted, False);
  end;
end;

function TCtxDataTable.Delete(ARow: TCtxDataRow): TCtxDataRow;
begin
  ASSERT(ARow <> nil);
  ASSERT(ARow.DataTable = Self);
  ASSERT(not ARow.Deleted);

  Result := ARow;
  Result.TransactionSave;
  ValidateRow(Result, drsDeleted);

  if ARow <> nil then
  begin
    InternalMoveRow(ARow.Index, PhysicalRowCount - 1);
    Inc(FDeletedCount);
    Result.SetState(drsDeleted, True);
    RowChanged(Result, nil, cdeRowDeleted);
  end;
end;

procedure TCtxDataTable.MoveRow(OldIndex, NewIndex: Integer);
begin
  if PKExists then
    raise Exception.CreateFmt(SUnableToMoveRowDueToPrimaryKeyDefined, [Name]);
  if (OldIndex < 0) or (OldIndex >= RowCount) then
    raise Exception.CreateFmt(SInvalidRowIndex, [OldIndex]);
  if (NewIndex < 0) or (NewIndex >= RowCount) then
    raise Exception.CreateFmt(SInvalidRowIndex, [NewIndex]);
  InternalMoveRow(OldIndex, NewIndex);
end;

function TCtxDataTable.CanDeleteRow(ARow: TCtxDataRow): Boolean;
begin
  // Check foreign key constraints 
  Result := not (ARow.Deleted) and (CheckFKErrorConstrants(ARow, True {Delete}) = nil);
end;

procedure TCtxDataTable.DeleteRow(ARow: TCtxDataRow);
begin
  FRows.Delete(ARow.Index);
end;

procedure TCtxDataTable.CheckRequiredColumns(ARow: TCtxDataRow);
var
  I: Integer;
  Column: TCtxDataColumn;
begin
  for I := 0 to Columns.Count - 1 do
  begin
    Column := Columns[I];
    if Column.Required and not ARow.AssignedValue[Column] then
      raise Exception.CreateFmt(SColumnRequired, [Column.Name]);
  end;
end;

procedure TCtxDataTable.CheckForeignKeys(ARow: TCtxDataRow);
var
  I: Integer;
begin
  // Raises exception if related rows not found
  for I := 0 to DataContainer.Relations.Count - 1 do
  with DataContainer.Relations[I] do
  begin
    if (ParentTable <> nil) and (ChildTable = Self)
      and ARow.IsModified(ChildColumns)
      and ARow.AssignedValues(ChildColumns)
      and (ParentTable.FindRow(ARow, ParentColumns, ChildColumns, []) = nil)
    then
      raise Exception.CreateFmt(SForeignKeyViolation, [ParentTableName, Name]);
  end;
end;

function TCtxDataTable.CheckFKErrorConstrants(ARow: TCtxDataRow; IsDelete: Boolean): TCtxDataRelation;
var
  I: Integer;
begin
  // Returns first relation, that fails (i.e. related rows exsists for a relation)
  for I := 0 to DataContainer.Relations.Count - 1 do
  begin
    Result := DataContainer.Relations[I];
    with Result do
    if (ParentTable = Self) and (ChildTable <> nil)
      and (UpdateAction = draError)
      and (IsDelete or ARow.IsModified(ParentColumns))
      and (ChildTable.FindRow(ARow.OldRow, ChildColumns, ParentColumns, []) <> nil)
    then
      exit;
  end;
  Result := nil;
end;

procedure TCtxDataTable.ProcessFKUpdateConstraints(ARow: TCtxDataRow);
var
  I: Integer;
  J: Integer;
  ChildRow: TCtxDataRow;
  FailedRelation: TCtxDataRelation;
begin
  FailedRelation := CheckFKErrorConstrants(ARow, False {Update});
  if FailedRelation <> nil then
    raise Exception.CreateFmt(SForeignKeyUpdateFailed, [FailedRelation.ChildTableName, FailedRelation.Name]);

  // Update related rows or nullify keys for each relation
  for I := 0 to DataContainer.Relations.Count - 1 do
  with DataContainer.Relations[I] do
  begin
    if (ParentTable = Self) and (ChildTable <> nil)
      and (UpdateAction in [draCascade, draNullify])
      and ARow.IsModified(ParentColumns)
    then
      for J := 0 to ChildTable.RowCount - 1 do
      begin
        ChildRow := ChildTable.Rows[J];
        if CompareRows(ARow.OldRow, ChildRow, ParentColumns, ChildColumns, []) = 0 then
        begin
          if UpdateAction = draCascade then
            ChildRow.Values[ChildColumns] := ARow.Values[ParentColumns]
          else ChildRow.Values[ChildColumns] := NullRow.Values[ParentColumns];
        end;
      end;
  end;
end;

procedure TCtxDataTable.ProcessFKDeleteConstraints(ARow: TCtxDataRow);
var
  I: Integer;
  J: Integer;
  ChildRow: TCtxDataRow;
  FailedRelation: TCtxDataRelation;
begin
  FailedRelation := CheckFKErrorConstrants(ARow, True {Delete});
  if FailedRelation <> nil then
    raise Exception.CreateFmt(SForeignKeyDeleteFailed, [FailedRelation.ChildTableName, FailedRelation.Name]);

  // Delete related rows or nullify keys for each relation
  for I := 0 to DataContainer.Relations.Count - 1 do
  with DataContainer.Relations[I] do
    if (ParentTable = Self) and (ChildTable <> nil)
      and (DeleteAction in [draCascade, draNullify])
    then
    begin
      J := 0;
      while J < ChildTable.RowCount do
      begin
        ChildRow := ChildTable.Rows[J];
        if CompareRows(ARow.OldRow, ChildRow, ParentColumns, ChildColumns, []) = 0 then
        begin
          if DeleteAction = draCascade then
            ChildRow.Delete
          else ChildRow.Values[ChildColumns] := NullRow.Values[ParentColumns];
        end;
        if not ChildRow.Deleted then
          Inc(J);
      end;
    end;
end;

function TCtxDataTable.ValidateRow(ARow: TCtxDataRow; Operation: TCtxDataRowState): Integer;
var
  PKViolation: Boolean;
begin
  // ValidateRow performs transactional processing of related rows
  Result := 0;

  // We do not do anything about rows, that need refreshing
  if ARow.NeedRefresh then exit;

  if (dtvCalcFields in FValidationsRequired) and (Operation in [drsInserted, drsUpdated]) then
    CalculateFields(ARow);

  // Update position of changed row in the primary key
  PKViolation := False;
  case Operation of
    drsUpdated:
      if PKExists and ARow.IsModified(FPKColumns) then
        PKViolation := UpdateRowPos(ARow);
    drsInserted:
      if PKExists then
        PKViolation := FindRowPos(ARow, Result)
      else Result := -1; // we will position this record in insert
  end;

  // Do not validate anything if data container is updating
  if DataContainer.IsUpdating or (FValidationsRequired = []) then exit;

  if (dtvPrimaryKey in FValidationsRequired) and PKViolation then
    raise Exception.CreateFmt(SPrimaryKeyViolation, [Name]);

  // Check required columns
  if (dtvRequiredColumns in FValidationsRequired) and (Operation in [drsInserted, drsUpdated]) then
    CheckRequiredColumns(ARow);

  // Check foreign key constraints
  if (dtvForeignKeys in FValidationsRequired) and (Operation in [drsInserted, drsUpdated]) then
    CheckForeignKeys(ARow);

  if (dtvReferencedKeys in FValidationsRequired) and (Operation in [drsUpdated, drsDeleted]) then
  begin
    // Begin transaction
    DataContainer.StartTransaction;
    try
      // Process foreign key cascade and nullify constraints
      if Operation = drsUpdated then
        ProcessFKUpdateConstraints(ARow)
      else
        ProcessFKDeleteConstraints(ARow);
      // Commit changes
      DataContainer.CommitTransaction;
    except
      // Rollback changes
      DataContainer.RollbackTransaction;
      raise;
    end;
  end;
end;

procedure TCtxDataTable.EmptyTable;
begin
  // This method should respect referential integrity
  BeginUpdate;
  try
    while RowCount > 0 do
      Rows[0].Delete;
    (*
    for I := 0 to RowCount - 1 do
      Rows[I].SetState(drsDeleted, True);
    FDeletedCount := PhysicalRowCount;
    *)
  finally
    EndUpdate;
  end;
end;

function TCtxDataTable.GetRow(Idx: Integer): TCtxDataRow;
begin
  Result := TCtxDataRow(FRows[Idx]);
end;

function TCtxDataTable.GetPhysicalRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TCtxDataTable.GetRowCount: Integer;
begin
  Result := PhysicalRowCount - FDeletedCount;
end;

function TCtxDataTable.RowIndex(ARow: TCtxDataRow): Integer;
begin
  Result := FRows.IndexOf(ARow);
end;

procedure TCtxDataTable.ClearData;
begin
  FRows.Clear;
  FDeletedCount := 0;
end;

function TCtxDataTable.FindRow(ARow: TCtxDataRow; const Cols: TCtxColumnArray; Options: TCtxCompareOptions): TCtxDataRow;
var
  I: Integer;
begin
  for I := 0 to RowCount-1 do
    if CompareRows(TCtxDataRow(FRows[I]), ARow, Cols, Cols, Options) = 0 then
    begin
      Result := TCtxDataRow(FRows[I]);
      Exit;
    end;
  Result := nil;
end;

function TCtxDataTable.FindRow(ARow: TCtxDataRow; const Cols1, Cols2: TCtxColumnArray; Options: TCtxCompareOptions): TCtxDataRow;
var
  I: Integer;
begin
  for I := 0 to RowCount-1 do
    if CompareRows(TCtxDataRow(FRows[I]), ARow, Cols1, Cols2, Options) = 0 then
    begin
      Result := TCtxDataRow(FRows[I]);
      Exit;
    end;
  Result := nil;
end;

function TCtxDataTable.FindRow(ARow: TCtxDataRow): TCtxDataRow;
var
  I: Integer;
begin
  if not PKExists then
    raise Exception.Create(SPrimaryKeyNotDefined);
  if FindRowPos(ARow, I) then
    Result := Rows[I]
  else Result := nil;
end;

function TCtxDataTable.Locate(const KeyFields: String; const KeyValues: Variant; Options: TCtxCompareOptions): TCtxDataRow;
var
  FldList: TCtxColumnArray;
  TempRow: TCtxDataRow;
begin
  CheckPrepared;
  TempRow := NewRow;
  try
    FldList := Columns.GetColumnArray(KeyFields);
    TempRow.Values[FldList] := KeyValues;
    Result := FindRow(TempRow, FldList, Options);
  finally
    TempRow.Free;
  end;
end;

procedure TCtxDataTable.InternalMoveRow(OldIndex, NewIndex: Integer);
begin
  if (OldIndex < 0) or (OldIndex >= PhysicalRowCount) then
    raise Exception.CreateFmt(SInvalidRowIndex, [OldIndex]);

  if NewIndex < 0 then
    NewIndex := 0
  else if NewIndex > PhysicalRowCount then
    NewIndex := PhysicalRowCount;

  FRows.Move(OldIndex, NewIndex);
end;

procedure TCtxDataTable.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TCtxDataTable) then
  begin
    DataContainer.Active := False;
    if not AnsiSameText(FName, TCtxDataTable(Source).Name) then
      FName := TCtxDataCollection(Collection).GetAutoName(TCtxDataTable(Source).Name);
    Columns := (Source as TCtxDataTable).Columns;
    // Do not assign data here
  end else
  inherited Assign(Source);
end;

function TCtxDataTable.CreateColumnMap(DestTable: TCtxDataTable): TList;
var
  I: Integer;
  DestColumn: TCtxDataColumn;
begin
  Result := TList.Create;
  try
    Result.Count := Columns.Count;
    for I := 0 to Columns.Count - 1 do
    begin
      DestColumn := DestTable.Columns.Find(Columns[I].Name);
      if DestColumn <> nil then
        Result[I] := DestColumn
      else Result[I] := nil;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure TCtxDataTable.SetColumns(Value: TCtxDataColumns);
begin
  FColumns.Assign(Value);
end;

procedure TCtxDataTable.GetColumnList(AList: TList);
var
  I: Integer;
begin
  if AList <> nil then
    for I := 0 to Columns.Count-1 do
      AList.Add(Columns[I]);
end;

procedure TCtxDataTable.RowChanged(ARow: TCtxDataRow; AColumn: TCtxDataColumn; DataEvent: TCtxDataEventType);
begin
  NotifyEvent(ARow, DataEvent);
end;

function TCtxDataTable.FindRowPos(ARow: TCtxDataRow; var Pos: Integer): Boolean;
var
  M, L, H, R: Integer;
begin
  ASSERT(PKExists);

  Result := False; // Exact match
  L := 0;
  H := RowCount - 1;
  while L <= H do
  begin
    M := (L + H) shr 1;
    R := CompareRows(Rows[M], ARow, FPKColumns, FPKColumns, []);
    if R < 0 then
      L := M + 1 else
      begin
        H := M - 1;
        if R = 0 then
        begin
          L := M;
          Result := True;
        end;
      end;
  end;
  Pos := L;
end;

function TCtxDataTable.UpdateRowPos(ARow: TCtxDataRow): Boolean;
var
  RowIdx: Integer;
  CmpValue: Integer;
  NeedReposition: Boolean;
begin
  ASSERT(PKExists);

  // Updates the position of row in the primary key
  // Optimization: check if we need to change position!
  Result := False;

  RowIdx := ARow.Index;
  NeedReposition := RowIdx >= RowCount;

  if (not NeedReposition) and (RowIdx > 0) then
  begin
    CmpValue := CompareRows(ARow, Rows[RowIdx - 1], FPKColumns, FPKColumns, []);
    NeedReposition := CmpValue < 0;
    Result := Result or (CmpValue = 0); // primary key violation
  end;

  if (not NeedReposition) and (RowIdx < RowCount - 1) then
  begin
    CmpValue := CompareRows(ARow, Rows[RowIdx + 1], FPKColumns, FPKColumns, []);
    NeedReposition := CmpValue > 0;
    Result := Result or (CmpValue = 0); // primary key violation
  end;

  if not NeedReposition then exit;

  FRows.Extract(ARow);
  try
    Result := FindRowPos(ARow, RowIdx);
    FRows.Insert(RowIdx, ARow);
  except
    FRows.Insert(RowCount - 1, ARow);
    raise;
  end;
end;

function TCtxDataTable.GetPKExists: Boolean;
begin
  Result := Length(FPKColumns) > 0;
end;

procedure TCtxDataTable.DoSort(iLo, iHi: Integer);
var
  Lo, Hi: Integer;
  MidObj: TCtxDataRow;

  procedure SwapPos(Pos1, Pos2: Integer);
  var
    T: Pointer;
    O: Boolean;
  begin
    O := FRows.OwnsObjects;
    FRows.OwnsObjects := False;
    try
      T := FRows[Pos1];
      FRows[Pos1] := FRows[Pos2];
      FRows[Pos2] := T;
    finally
      FRows.OwnsObjects := O;
    end;
  end;

begin
  if iHI > iLo then
  repeat
    Lo := iLo;
    Hi := iHi;
    MidObj := Rows[(iLo+iHi) shr 1];
    repeat
      while CompareRows(Rows[Lo], MidObj, FPKColumns, FPKColumns, []) < 0 do Inc(Lo);
      while CompareRows(Rows[Hi], MidObj, FPKColumns, FPKColumns, []) > 0 do Dec(Hi);
      if Lo <= Hi then
      begin
        SwapPos(Lo, Hi);
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if iLo < Hi then
      DoSort(iLo, Hi);
    iLo := Lo;
  until Lo >= iHi;
end;

procedure TCtxDataTable.Sort;
begin
  if PKExists then
    DoSort(0, RowCount-1);
end;

procedure TCtxDataTable.Prepare;
var
  I, C: Integer;
  R: TCtxDataRelation;
  PKDesc: TCtxOrderByColumns;
  Relation: TCtxDataRelation;
begin
  if FPrepared then exit;
  try
    FPrepared := True;

    FBufferSize := Columns.Count div 8;
    if Columns.Count mod 8 <> 0 then
      inc(FBufferSize);

    // Optimization: we need to create flag that this table
    // require any validation and if not, then skip it alltogether
    // This way we won't have to iterate all relations when updating each
    // record.
    FValidationsRequired := [];

    SetLength(PKDesc, 0); // MB: was not initialized;
    for I := 0 to Columns.Count-1 do
    with Columns[I] do
    begin
      if DataType = cdtReference then
      begin
        if FRelationName = '' then
          raise Exception.CreateFmt(SEmptyColumnLink, [Name, Self.Name]);
        R := DataContainer.Relations.Get(FRelationName);
        R.Prepare;
        if R.ChildTable <> Self then
          raise Exception.CreateFmt(SInvalidColumnLink, [Name, Self.Name])
        // else
        // for J := 0 to Length(R.FChildColumns) - 1 do
          ; // R.ChildColumns[J].FLinkColumn := Columns[I]; ???
      end;
      if Calculated then
        Include(FValidationsRequired, dtvCalcFields);
    end;

    for I := 0 to DataContainer.Relations.Count - 1 do
    begin
      Relation := DataContainer.Relations[I];
      if Relation.ConstraintActive and (Relation.ChildTable = Self) then
      begin
        Include(FValidationsRequired, dtvForeignKeys);
        for C := Low(Relation.ChildColumns) to High(Relation.ChildColumns) do
          Include(Relation.ChildColumns[C].FAttributes, caForeignKey);
      end;

      if Relation.ConstraintActive and (Relation.ParentTable = Self) then
      begin
        Include(FValidationsRequired, dtvReferencedKeys);
        for C := Low(Relation.ParentColumns) to High(Relation.ParentColumns) do
          Include(Relation.ParentColumns[C].FAttributes, caReferencedKey);
      end;
    end;

    SetLength(FPKColumns, 0);
    for I := 0 to Columns.Count-1 do
      with Columns[I] do
      begin
        FIndex := I;
        FOffset := FBufferSize;
        Inc(FBufferSize, BufSize);

        { This field determines whether any validation will occur when assigning value to this column }
        FRequireValidation := Required or (caPrimaryKey in FAttributes)
          or (caForeignKey in FAttributes) or (caReferencedKey in FAttributes);

        if caRequired in FAttributes then
          Include(FValidationsRequired, dtvRequiredColumns);
        if caPrimaryKey in FAttributes then
          Include(FValidationsRequired, dtvPrimaryKey);

        if DataType = cdtReference then
          FRelation := DataContainer.Relations.Get(FRelationName);
        if PrimaryKey then
          AddToColumns(FPKColumns, Columns[I]);
      end;

      // Create null row which can be returned if no row found
      FNullRow := NewRow;
      // Create row with only default values 
      FDefaultRow := NewRow;
      InitDefaultValues(FDefaultRow);
  except
    UnPrepare;
    raise;
  end;
end;

procedure TCtxDataTable.UnPrepare;
var
  I: Integer;
begin
  if FPrepared then
  begin
    SetLength(FPKColumns, 0);
    FRows.Clear;
    FreeAndNil(FNullRow);
    FreeAndNil(FDefaultRow);
    for I := 0 to Columns.Count - 1 do
      Columns[I].FRelation := nil;
    FPrepared := False;
  end;
end;

function TCtxDataTable.GetDataContainer: TCtxDataContainer;
begin
  Result := TCtxDataTables(Collection).GetDataContainer;
end;

procedure TCtxDataTable.SetName(const Value: String);
var
  TmpStr: String;
begin
  if FName <> Value then
  begin
    // Validate rename +++
    TmpStr := Trim(Value);
    if TmpStr = '' then
      raise Exception.Create(SEmptyTableName);

    if not DataContainer.Tables.IsUnique(TmpStr, Self) then
      raise Exception.CreateFmt(SDuplicateTableName, [TmpStr]);

    DataContainer.Active := False;
    FName := TmpStr;

    NotifyEvent(Self, cdeTableChanged);
  end;
end;

procedure TCtxDataTable.CheckUnPrepared;
begin
  if Prepared then
    raise Exception.Create(SUnableToPerformOnActiveDataTable);
end;

procedure TCtxDataTable.CheckPrepared;
begin
  if not Prepared then
    raise Exception.Create(SUnableToPerformOnInActiveDataTable);
end;

procedure TCtxDataTable.ReadData(Reader: TReader);
var
  R: TCtxDataRow;
  C: TCtxColumnMapInfo;
  ColMap: TObjectList;
begin
  BeginUpdate;
  try
    if Reader.ReadBoolean then
    begin
      DataContainer.Active := True;
      ColMap := TObjectList.Create;
      try
        Reader.ReadListBegin;
        while not Reader.EndOfList do
        begin
          // locate column by name
          C := TCtxColumnMapInfo.Create;
          try
            C.Column := Columns.Find(Reader.ReadString);
            C.DataType := TCtxDataType(Reader.ReadInteger);
            // make sure that data type is the same
            if (C.Column <> nil) and (C.Column.DataType <> C.DataType) then
              C.Column := nil;
            ColMap.Add(C);
          except
            C.Free;
            raise;
          end;
        end;
        Reader.ReadListEnd;

        Reader.ReadListBegin;
        while not Reader.EndOfList do
        begin
          R := NewRow;
          R.ReadData(Reader, ColMap);
          Insert(R);
        end;
        Reader.ReadListEnd;

      finally
        ColMap.Free; // All map objects will be disposed
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCtxDataTable.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteBoolean(Prepared);
  if Prepared then
  begin
    Writer.WriteListBegin;
    for I := 0 to Columns.Count - 1 do
    begin
      Writer.WriteString(Columns[I].Name);
      Writer.WriteInteger(Ord(Columns[I].DataType));
    end;
    Writer.WriteListEnd;

    Writer.WriteListBegin;
    for I := 0 to RowCount - 1 do
      Rows[I].WriteData(Writer);
    Writer.WriteListEnd;
  end;
end;

procedure TCtxDataTable.BeginUpdate;
begin
  if FUpdateCounter = 0 then
    DataContainer.Active := True;
  Inc(FUpdateCounter);
end;

procedure TCtxDataTable.EndUpdate;
begin
  Dec(FUpdateCounter);
  if FUpdateCounter = 0 then
    NotifyEvent(Self, cdeTableDataChanged);
end;

function TCtxDataTable.IsUpdating: Boolean;
begin
  Result := FUpdateCounter > 0;
end;

function TCtxDataTable.GetDisplayName: String;
begin
  Result := FName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TCtxDataTable.Fill(ADataAdapter: TCtxDataAdapter);
begin
  BeginUpdate;
  try
    DataContainer.CheckDataAdapter(ADataAdapter).FillTable(Self);
  finally
    EndUpdate;
  end;
end;

procedure TCtxDataTable.Update(ADataAdapter: TCtxDataAdapter = nil);
begin
  DataContainer.CheckDataAdapter(ADataAdapter).UpdateTable(Self);
end;

procedure TCtxDataTable.Refresh(ADataAdapter: TCtxDataAdapter);
begin
  BeginUpdate;
  try
    DataContainer.CheckDataAdapter(ADataAdapter).RefreshTable(Self);
  finally
    EndUpdate;
  end;
end;

procedure TCtxDataTable.Clear;
begin
  // This method does not respect referrential integrity
  ClearData;
  NotifyEvent(Self, cdeTableDataChanged);
end;

procedure TCtxDataTable.AcceptChanges;
var
  R: Integer;
begin
  DataContainer.CheckNotInTransaction;

  for R := 0 to RowCount - 1 do
    Rows[R].AcceptChanges;
  FRows.Count := RowCount;
  FDeletedCount := 0;
end;

procedure TCtxDataTable.RejectChanges;
var
  I: Integer;
  Row: TCtxDataRow;
begin
  DataContainer.CheckNotInTransaction;

  BeginUpdate;
  try
    // First process all updated and inserted records up to RowCount
    I := 0;
    while I < RowCount do
    begin
      Row := Rows[I];
      Row.RejectChanges;
      if (I < RowCount) and (Row = Rows[I]) then
        Inc(I);
    end;
    // Go in reverse and restore deleted records
    while FDeletedCount > 0 do
    begin
      Row := Rows[FRows.Count - 1]; // restore last row
      if Row.Editing then
        Row.CancelEdit;
      Row.RejectChanges; // this will UnDelete the row and place it in its position
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCtxDataTable.NotifyEvent(Context: TObject;
  DataEvent: TCtxDataEventType);
begin
  if not IsUpdating then
    DataContainer.NotifyEvent(Context, DataEvent);
end;

procedure TCtxDataTable.RefreshRow(ADataRow: TCtxDataRow);
var
  SaveValidationsRequired: TCtxDataTableValidations;
begin
  // We save and restore validation options. We do not need any validation here
  // however, we also do not want to enter beginupdate\endupdate state for the
  // container or table.
  SaveValidationsRequired := FValidationsRequired;
  try
    FValidationsRequired := [];
    DataContainer.CheckDataAdapter(nil).RefreshRow(ADataRow);
  finally
    FValidationsRequired := SaveValidationsRequired;
  end;
end;

function TCtxDataTable.HasOwnerTable: Boolean;
var
  I: Integer;
begin
  Result := True;
  with DataContainer do
  for I := 0 to Relations.Count - 1 do
    if Relations[I].OwnRows and AnsiSameText(Relations[I].ChildTableName, Self.Name) then
      exit;
  Result := False;
end;

procedure TCtxDataTable.CalculateFields(ARow: TCtxDataRow);
begin
  // Calculate fields for the data row
  DataContainer.DoCalcFields(ARow);
end;

function TCtxDataTable.HasPKColumns: Boolean;
var
  I: Integer;
begin
  for I := 0 to DataColumns.Count - 1 do
    if DataColumns[I].PrimaryKey then
    begin
      Result := True;
      exit;
    end;
  Result := False;
end;

{ TCtxDataColumn }

constructor TCtxDataColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TCtxDataColumn.Destroy;
begin
  inherited Destroy;
end;

procedure TCtxDataColumn.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TCtxDataColumn) then
  begin
    DataTable.DataContainer.Active := False;
    if FName <> TCtxDataColumn(Source).Name then
      FName := TCtxDataCollection(Collection).GetAutoName(TCtxDataColumn(Source).Name);
    FDataType := (Source as TCtxDataColumn).FDataType;
    FDataSize := (Source as TCtxDataColumn).FDataSize;
    FAttributes := (Source as TCtxDataColumn).FAttributes;
    FDisplayLabel := (Source as TCtxDataColumn).FDisplayLabel;
    FRelationName := (Source as TCtxDataColumn).FRelationName;
  end else
    inherited Assign(Source);
end;

function TCtxDataColumn.GetDisplayName: String;
begin
  Result := FName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

procedure TCtxDataColumn.SetName(const Value: String);
var
  TmpStr: String;
begin
  if FName <> Value then
  begin
    // Validate rename
    TmpStr := Trim(Value);
    if TmpStr = '' then
      raise Exception.Create(SEmptyColumnName);

    if not DataTable.Columns.IsUnique(TmpStr, Self) then
      raise Exception.CreateFmt(SDuplicateColumnName, [TmpStr]);

    DataTable.CheckUnPrepared;
    FName := Value;

    DataTable.NotifyEvent(Self, cdeColumnListChanged);
  end;
end;

procedure TCtxDataColumn.SetDataType(Value: TCtxDataType);
begin
  if FDataType <> Value then
  begin
    DataTable.CheckUnPrepared;
    FDataType := Value;
    if FDataType in [cdtString, cdtWideString] then
      FDataSize := 20
    else FDataSize := 0;

    DataTable.NotifyEvent(Self, cdeColumnListChanged);
  end;
end;

function TCtxDataColumn.GetDataSize: word;
begin
  Result := DataTypeSizes[FDataType];
end;

function TCtxDataColumn.GetDataLength: word;
begin
  if FDataType in [cdtString, cdtWideString] then
    Result := FDataSize else
    Result := DataTypeSizes[FDataType];
end;

procedure TCtxDataColumn.SetDataLength(Value: word);
begin
  if FDataSize <> Value then
  begin
    DataTable.CheckUnPrepared;
    if FDataType in [cdtString, cdtWideString] then
      FDataSize := Value;

    DataTable.NotifyEvent(Self, cdeColumnListChanged);
  end;
end;

function TCtxDataColumn.GetAttr(const Index: Integer): Boolean;
begin
  Result := TCtxDataColumnAttribute(Index) in FAttributes;
end;

procedure TCtxDataColumn.SetAttr(const Index: Integer;
  const Value: Boolean);
begin
  if Value <> GetAttr(Index) then
  begin
    if Value then
      Include(FAttributes, TCtxDataColumnAttribute(Index))
    else Exclude(FAttributes, TCtxDataColumnAttribute(Index));

    DataTable.NotifyEvent(Self, cdeColumnListChanged);
  end;
end;


function TCtxDataColumn.GetDataTable: TCtxDataTable;
begin
  Result := TCtxDataTable(Collection.Owner);
end;

function TCtxDataColumn.GetRelation: TCtxDataRelation;
begin
  if FRelation <> nil then
    Result := FRelation else
    begin
      FRelation := DataTable.DataContainer.Relations.Find(RelationName);
      Result := FRelation;
    end;
end;

function TCtxDataColumn.GetDataTypeName: String;
begin
  if DataType = cdtReference then
    Result := '->' + RelationName
  else Result := DataTypeNames[DataType];
end;

procedure TCtxDataColumn.SetDataTypeName(const Value: String);
begin
  if Value = '' then
    DataType := cdtUnknown
  else if copy(Value, 1, 2) = '->' then
  begin
    DataType := cdtReference;
    RelationName := copy(Value, 3, System.Length(Value));
  end else
    DataType := DataTypeNameToType(Value);
end;


{ TCtxDataRow }

constructor TCtxDataRow.Create(ADataTable: TCtxDataTable);
begin
  inherited Create;
  FState := [];
  FDataTable := ADataTable;
  if FDataTable <> nil then
    FDataTable.Prepare;
  FOldRow := nil;
  FOriginalRow := nil;
  FRollbackRow := nil;
end;

destructor TCtxDataRow.Destroy;
begin
  // We cannot delete data row if it is maintained. If it is not mainatined
  // then there's no reason to notify anyone about it being deleted. +++
  FreeBuffer(FData);
  FreeAndNil(FOldRow);
  FreeAndNil(FRollbackRow);
  FreeAndNil(FOriginalRow);
  inherited;
end;

function TCtxDataRow.GetState(Idx: TCtxDataRowState): Boolean;
begin
  Result := Idx in FState;
end;

procedure TCtxDataRow.SetState(Idx: TCtxDataRowState; ASet: Boolean);
begin
  if ASet then
    Include(FState, Idx)
  else Exclude(FState, Idx);
end;

function TCtxDataRow.GetIndex: Integer;
begin
  if Stored then
    Result := FDataTable.RowIndex(Self)
  else Result := -1;
end;

{ Internal buffer handling methods }

function TCtxDataRow.GetBufferSize: Integer;
begin
  Result := FDataTable.FBufferSize;
end;

procedure TCtxDataRow.InitBuffer(var Buffer: Pointer);
begin
  if Buffer = nil then
  begin
    ReallocMem(Buffer, FDataTable.FBufferSize);
    FillChar(Buffer^, BufferSize, 0);
  end;
end;

procedure TCtxDataRow.FreeBuffer(var Buffer: Pointer);
begin
  if Buffer <> nil then
  begin
    InternalClear(Buffer);
    FreeMem(Buffer, BufferSize);
    Buffer := nil;
  end;
end;

function TCtxDataRow.InternalGetAssigned(Buffer: Pointer; const Column: TCtxDataColumn): Boolean;
begin
  if Buffer <> nil then
    Result := GetBit(Buffer, Column.FIndex)
  else Result := False;
end;

procedure TCtxDataRow.InternalSetAssigned(Buffer: Pointer; const Column: TCtxDataColumn; Value: Boolean);
begin
  if Buffer <> nil then
    SetBit(Buffer, Column.FIndex, Value);
end;

function TCtxDataRow.InternalGetDataPtr(var Buffer: Pointer; const Column: TCtxDataColumn; ForWriting: Boolean): Pointer;
begin
  // Create buffer if necessary
  if (Buffer = nil) and ForWriting then
    InitBuffer(Buffer);
  if Buffer <> nil then
    Result := Pointer(Integer(Buffer)+ Column.FOffset)
  else Result := nil;
end;

procedure TCtxDataRow.InternalClear(Buffer: Pointer);
var
  I: Integer;
begin
  if Buffer <> nil then
    for I := 0 to FDataTable.Columns.Count-1 do
      InternalSetColumnData(FDataTable.Columns[I], Buffer, nil);
end;

procedure TCtxDataRow.InternalSetColumnData(const Column: TCtxDataColumn; var DataBuffer: Pointer; ValueBuffer: Pointer);
var
  P: Pointer;
begin
  if Column = nil then
    Exit;
  P := InternalGetDataPtr(DataBuffer, Column, True);

  InternalSetAssigned(DataBuffer, Column, ValueBuffer <> nil);

  if ValueBuffer = nil then
    case Column.FDataType of
      cdtString: PAnsiString(P)^ := '';
      cdtWideString: PWideString(P)^ := '';
      cdtMemo: PAnsiString(P)^ := '';
      cdtBlob: PAnsiString(P)^ := '';
      cdtReference: PObject(P)^ := nil;
    end
  else
  begin
    case Column.DataType of
      cdtString: PAnsiString(P)^ := PAnsiString(ValueBuffer)^;
      cdtWideString: PWideString(P)^ := PWideString(ValueBuffer)^;
      cdtMemo: PAnsiString(P)^ := PAnsiString(ValueBuffer)^;
      cdtBlob: PAnsiString(P)^ := PAnsiString(ValueBuffer)^;
      cdtReference: PObject(P)^ := nil;
      else begin
        Move(ValueBuffer^, P^, Column.BufSize);
        if Column.DataType = cdtDate then
          ReplaceTime(PDateTime(P)^, 0)
        else if Column.DataType = cdtTime then
          ReplaceDate(PDateTime(P)^, 0);
      end;
    end;
  end;
end;

procedure TCtxDataRow.InternalGetColumnData(const Column: TCtxDataColumn; var DataBuffer: Pointer; ValueBuffer: Pointer);
var
  P: Pointer;
begin
  if Column = nil then
    Exit;
  if not InternalGetAssigned(DataBuffer, Column) then
    FillChar(ValueBuffer^, Column.BufSize, 0) else
  begin
    P := InternalGetDataPtr(DataBuffer, Column, False);
    case Column.DataType of
      cdtString: PAnsiString(ValueBuffer)^ := PAnsiString(P)^;
      cdtWideString: PWideString(ValueBuffer)^ := PWideString(P)^;
      cdtBlob: PAnsiString(ValueBuffer)^ := PAnsiString(P)^;
      cdtMemo: PAnsiString(ValueBuffer)^ := PAnsiString(P)^;
      cdtReference: PObject(ValueBuffer)^ := PObject(P)^;
      else Move(P^, ValueBuffer^, Column.BufSize);
    end;
  end;
end;

procedure TCtxDataRow.InternalCopyBuffer(SrcBuf, DestBuf: Pointer);
var
  I: Integer;
  P: Pointer;
begin
  if SrcBuf = nil then exit;
  ASSERT(DestBuf <> nil);
  with FDataTable do
  for I := 0 to Columns.Count - 1 do
  begin
    if InternalGetAssigned(SrcBuf, Columns[I]) then
      P := InternalGetDataPtr(SrcBuf, Columns[I], False)
    else P := nil;
    InternalSetColumnData(Columns[I], DestBuf, P);
  end;
end;

{ Assign methods }

procedure TCtxDataRow.AssignColumn(Source: TCtxDataRow; Column: TCtxDataColumn);
begin
  if Source.AssignedValue[Column] then
    SetColumnData(Column, Source.GetDataPtr(Column))
  else SetColumnData(Column, nil);
end;

procedure TCtxDataRow.AssignColumn(Source: TCtxDataRow; SrcColumn, DestColumn: TCtxDataColumn);
begin
  if (SrcColumn <> nil) and Source.AssignedValue[SrcColumn] then
    SetColumnData(DestColumn, Source.GetDataPtr(SrcColumn))
  else SetColumnData(DestColumn, nil);
end;

procedure TCtxDataRow.Assign(Source: TCtxDataRow; Column: TCtxDataColumn);
begin
  if Source = nil then
    Clear(Column)
  else if FDataTable <> Source.FDataTable then
    raise Exception.Create(SUnableToAssignRowFromDifferentTable)
  else
    AssignColumn(Source, Column);
end;

procedure TCtxDataRow.Assign(Source: TCtxDataRow; ColumnMap: TList);
var
  I: Integer;
begin
  { ColumnMap contains our columns corresponding to Source table column indexes. }

  if Source = Self then exit;
  if Source = nil then
    Clear else
  with FDataTable do
  begin
    if Stored and not Editing then
    begin
      BeginEdit;
      try
        for I := 0 to ColumnMap.Count - 1 do
          if ColumnMap[I] <> nil then
            Value[TCtxDataColumn(ColumnMap[I])] := Source.Value[Source.DataTable.Columns[I]];
        EndEdit;
      except
        CancelEdit;
        raise;
      end;
    end else
      for I := 0 to ColumnMap.Count - 1 do
        if ColumnMap[I] <> nil then
          // AssignColumn(Source, Source.DataTable.Columns[I], TCtxDataColumn(ColumnMap[I]));
          Value[TCtxDataColumn(ColumnMap[I])] := Source.Value[Source.DataTable.Columns[I]];
  end;
end;

procedure TCtxDataRow.Assign(Source: TCtxDataRow);
var
  I: Integer;
begin
  if Source = Self then exit;
  if Source = nil then
    Clear else
  if FDataTable <> Source.FDataTable then
    raise Exception.Create(SUnableToAssignRowFromDifferentTable) else
  with FDataTable do
  begin
    if Stored and not Editing then
    begin
      BeginEdit;
      try
        for I := 0 to Columns.Count - 1 do
          AssignColumn(Source, Columns[I]);
        EndEdit;
      except
        CancelEdit;
        raise;
      end;
    end else
      for I := 0 to Columns.Count - 1 do
        AssignColumn(Source, Columns[I]);
  end;
end;

procedure TCtxDataRow.Clear(const Column: TCtxDataColumn = nil);
var
  I: Integer;
begin
  if FData = nil then
    exit;

  if Column <> nil then
    SetColumnData(Column, nil)
  else
  begin
    BeginEdit;
    try
      with DataTable do
      for I := 0 to Columns.Count - 1 do
        SetColumnData(Columns[I], nil);
      EndEdit;
    except
      CancelEdit;
      raise;
    end;
  end;
end;

procedure TCtxDataRow.CheckNeedRefresh;
begin
  if NeedRefresh then
  begin
    InitBuffer(FData);
    NeedRefresh := False;
    DataTable.RefreshRow(Self);
  end;
end;

function TCtxDataRow.IsModified(const Columns: TCtxColumnArray): Boolean;
var
  I: Integer;
begin
  if (FOldRow = nil) or (FOldRow.FData = nil) then
    Result := False
  else
  begin
    Result := True;
    for I := 0 to Length(Columns) - 1 do
      if _CompareData(
        GetDataPtr(Columns[I]), InternalGetDataPtr(FOldRow.FData, Columns[I], False),
        Columns[I].DataType, []) <> 0
      then exit;
    Result := False;
  end;
end;

function TCtxDataRow.AssignedValues(const Columns: TCtxColumnArray): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Length(Columns) - 1 do
    if AssignedValue[Columns[I]] then exit;
  Result := False;
end;

procedure TCtxDataRow.SaveOriginalValue;
begin
  // Save values into OriginalRow
  if (FData <> nil) and (FOriginalRow = nil)
    and not Inserted
    and Stored
    // and (not DataTable.IsUpdating) loading?
  then
  begin
    FOriginalRow := DataTable.NewRow;
    // FOriginalRow.Assign(Self);
    InitBuffer(FOriginalRow.FData);
    InternalCopyBuffer(Self.FData, FOriginalRow.FData);
  end;

  TransactionSave;
end;

function TCtxDataRow.GetDataPtr(const Column: TCtxDataColumn; ForWriting: Boolean = False): Pointer;
begin
  if Column <> nil then
  begin
    CheckNeedRefresh;
    if ForWriting and (FOriginalRow = nil) then
      SaveOriginalValue;
    Result := InternalGetDataPtr(FData, Column, ForWriting);
  end else
    Result := nil;
end;

procedure TCtxDataRow.GetColumnData(const Column: TCtxDataColumn; Buffer: Pointer);
begin
  CheckNeedRefresh;
  InternalGetColumnData(Column, FData, Buffer);
end;

procedure TCtxDataRow.SetColumnData(const Column: TCtxDataColumn; Buffer: Pointer);
begin
  if Stored and not Editing then
  begin
    CheckNeedRefresh;
    SaveOriginalValue; // This will preserve old state

    if Column.RequireValidation or ((dtvCalcFields in DataTable.ValidationsRequired) and not Column.Calculated) then
    begin
      BeginEdit;
      try
        InternalSetColumnData(Column, FData, Buffer);
        EndEdit;
      except
        CancelEdit;
        raise;
      end;
    end else
    begin
      // This is a shortcut around editing for simple columns
      InternalSetColumnData(Column, FData, Buffer);
      SetState(drsUpdated, True);
      DataTable.RowChanged(Self, Column, cdeRowModified);
    end;
  end else
    InternalSetColumnData(Column, FData, Buffer);
end;

function TCtxDataRow.GetAssignedValue(Column: TCtxDataColumn): Boolean;
begin
  Result := InternalGetAssigned(FData, Column);
end;

procedure TCtxDataRow.SetAssignedValue(Column: TCtxDataColumn; ASet: Boolean);
begin
  InternalSetAssigned(FData, Column, ASet);
end;

{ Streaming methods }

procedure TCtxDataRow.WriteData(Writer: TWriter);
var
  I: Integer;
  P: Pointer;
  Column: TCtxDataColumn;
  NotNull: Boolean;
begin
  Writer.WriteBoolean(FData <> nil);
  if FData <> nil then
  with DataTable do
    for I := 0 to Columns.Count - 1 do
    begin
      Column := Columns[I];
      NotNull := AssignedValue[Column];
      Writer.WriteBoolean(NotNull);
      if NotNull then
      begin
        P := GetDataPtr(Column);
        case Column.DataType of
          cdtSmallInt: Writer.WriteInteger(PSmallInt(P)^);
          cdtLargeInt: Writer.WriteInteger(PInt64(P)^);
          cdtBoolean: Writer.WriteBoolean(PWordBool(P)^);
          cdtInteger: Writer.WriteInteger(PInteger(P)^);
          cdtFloat: Writer.WriteFloat(PDouble(P)^);
          cdtDateTime, cdtDate, cdtTime: Writer.WriteDate(PDateTime(P)^);
          cdtString, cdtMemo, cdtBlob: Writer.WriteString(String(PAnsiString(P)^));
          cdtWideString: Writer.WriteWideString(PWideString(P)^);
        end;
      end;
    end;
end;

procedure TCtxDataRow.ReadData(Reader: TReader; ColMap: TList);
var
  I: Integer;
  P: Pointer;
  Column: TCtxDataColumn;
  NotNull: Boolean;
begin
  if Reader.ReadBoolean then
  begin
    InitBuffer(FData);
    InternalClear(FData);
    with DataTable do
      for I := 0 to ColMap.Count - 1 do
      begin
        Column := TCtxColumnMapInfo(ColMap[I]).Column;
        NotNull := Reader.ReadBoolean;
        if Column = nil then
        begin
          // Skip stored value
          if NotNull then
          case TCtxColumnMapInfo(ColMap[I]).DataType of
            cdtSmallInt: Reader.ReadInteger;
            cdtLargeInt: Reader.ReadInt64;
            cdtBoolean: Reader.ReadBoolean;
            cdtInteger: Reader.ReadInteger;
            cdtFloat: Reader.ReadFloat;
            cdtDateTime, cdtDate, cdtTime: Reader.ReadDate;
            cdtString, cdtMemo, cdtBlob: Reader.ReadString;
            cdtWideString: Reader.ReadWideString;
          end;
        end else
        begin
          // Read stored value
          AssignedValue[Column] := NotNull;
          if NotNull then
          begin
            P := GetDataPtr(Column, True);
            case Column.DataType of
              cdtSmallInt: PSmallInt(P)^ := Reader.ReadInteger;
              cdtLargeInt: PInt64(P)^ := Reader.ReadInt64;
              cdtBoolean: PWordBool(P)^ := Reader.ReadBoolean;
              cdtInteger: PInteger(P)^ := Reader.ReadInteger;
              cdtFloat: PDouble(P)^ := Reader.ReadFloat;
              cdtDateTime, cdtDate, cdtTime: PDateTime(P)^ := Reader.ReadDate;
              cdtString, cdtMemo, cdtBlob: PAnsiString(P)^ := AnsiString(Reader.ReadString);
              cdtWideString: PWideString(P)^ := Reader.ReadWideString;
            end;
          end;
        end;
      end;
  end;
end;

{ Column access methods }

function TCtxDataRow.GetAsVariant(const ColumnName: String): Variant;
begin
  Result := GetValue(DataTable.Columns.Get(ColumnName));
end;

procedure TCtxDataRow.SetAsVariant(const ColumnName: String; Value: Variant);
begin
  SetValue(DataTable.Columns.Get(ColumnName), Value);
end;

function TCtxDataRow.GetValues(const Columns: TCtxColumnArray): Variant;
var
  I, C: Integer;
begin
  C := Length(Columns);
  if C > 1 then
  begin
    Result := VarArrayCreate([0, C-1], varVariant);
    for I := 0 to C - 1 do
      Result[I] := GetValue(Columns[I]);
  end else if C = 1 then
    Result := GetValue(Columns[0]);
end;

function TCtxDataRow.GetRowValues: Variant;
var
  I, C: Integer;
begin
  C := DataTable.Columns.Count;
  if C > 1 then
  begin
    Result := VarArrayCreate([0, C - 1], varVariant);
    for I := 0 to C - 1 do
      Result[I] := GetValue(DataTable.Columns[I]);
  end else if C = 1 then
    Result := GetValue(DataTable.Columns[0]);
end;

procedure TCtxDataRow.SetValues(const Columns: TCtxColumnArray; Value: Variant);
var
  I, C: Integer;
begin
  C := Length(Columns);
  if C = 0 then
    raise Exception.Create(SEmptyColumnArray);

  if C > 1 then
  begin
    BeginEdit;
    try
      for I := 0 to C-1 do
        SetValue(Columns[I], Value[I]);
      EndEdit;
    except
      CancelEdit;
      raise;
    end;
  end else if C = 1 then
    SetValue(Columns[0], Value);
end;

procedure TCtxDataRow.SetRowValues(Value: variant);
var
  I, C: Integer;
begin
  C := DataTable.Columns.Count - 1;
  if C > 1 then
  begin
    BeginEdit;
    try
      for I := 0 to C-1 do
        SetValue(DataTable.Columns[I], Value[I]);
    except
      CancelEdit;
      raise;
    end;
  end else if C = 1 then
    SetValue(DataTable.Columns[0], Value);
end;

function TCtxDataRow.GetValue(Column: TCtxDataColumn): Variant;
var
  P: Pointer;
begin
  Result := Null;
  ASSERT(Column <> nil);
  if AssignedValue[Column] then
  begin
    P := GetDataPtr(Column);
    case Column.FDataType of
      cdtSmallInt:
        Result := PSmallInt(P)^;
      cdtLargeInt:
        Result := PInt64(P)^;
      cdtInteger:
        Result := PInteger(P)^;
      cdtBoolean:
        Result := PWordBool(P)^;
      cdtFloat:
        Result := PDouble(P)^;
      cdtDateTime, cdtDate, cdtTime:
        Result := VarFromDateTime(PDateTime(P)^);
      cdtString, cdtMemo, cdtBlob:
        Result := PAnsiString(P)^;
      cdtWideString:
        Result := PWideString(P)^;
      else
        raise Exception.CreateFmt(SUnableToCastValueToVariant, [DataTable.Name, Column.Name]);
    end;
  end;
end;

procedure TCtxDataRow.SetValue(Column: TCtxDataColumn;
  const Value: Variant);
var
  Buffer: array [0..128] of AnsiChar;
  P: Pointer;
begin
  ASSERT(Column <> nil);
  if VarIsEmpty(Value) or VarIsNull(Value) then
    Clear(Column)
  else
  begin
    P := @Buffer[0];
    FillChar(P^, Column.BufSize, 0);
    case Column.DataType of
      cdtSmallInt:
        PSmallInt(P)^ := Value;
      cdtLargeInt:
        PInt64(P)^ := Value;
      cdtInteger:
        PInteger(P)^ := Value;
      cdtBoolean:
        PWordBool(P)^ := Value;
      cdtFloat:
        PDouble(P)^ := Value;
      cdtDateTime, cdtDate, cdtTime:
        PDateTime(P)^ := VarToDateTime(Value);
      cdtString, cdtMemo, cdtBlob:
        PAnsiString(P)^ := AnsiString(VarToStr(Value));
      cdtWideString:
        PWideString(P)^ := VarToWideStr(Value);
      else
        raise Exception.CreateFmt(SUnableToAssignValueAsVariant, [DataTable.Name, Column.Name]);
    end;
    SetColumnData(Column, P);
    if Column.DataType in cdtStringTypes then
      PAnsiString(P)^ := ''
    else if Column.DataType in cdtWideStringTypes then
      PWideString(P)^ := '';
  end;
end;

function TCtxDataRow.GetAsString(Column: TCtxDataColumn): String;
var
  P: Pointer;
begin
  ASSERT(Column <> nil);
  Result := '';
  if AssignedValue[Column] then
  begin
    P := GetDataPtr(Column);
    case Column.FDataType of
      cdtUnknown: Result := '{N\A}';
      cdtLargeInt: Result := IntToStr(PInt64(P)^);
      cdtSmallInt: Result := IntToStr(PSmallInt(P)^);
      cdtInteger: Result := IntToStr(PInteger(P)^);
      cdtBoolean: Result := BoolToStr(PWordBool(P)^, True);
      cdtFloat: Result := FloatToStr(PDouble(P)^);
      cdtDateTime: Result := DateTimeToStr(PDateTime(P)^);
      cdtDate: Result := DateToStr(PDateTime(P)^);
      cdtTime: Result := TimeToStr(PDateTime(P)^);      
      cdtString: Result := String(PAnsiString(P)^);
      cdtWideString: Result := PWideString(P)^;
      cdtGuid: Result := GUIDToString(PGuid(P)^);
      cdtMemo: Result := String(PAnsiString(P)^);
      cdtBlob: Result := '{BLOB}';
      cdtReference: Result := '{ROW}';
    end;
  end;
end;

procedure TCtxDataRow.SetAsString(Column: TCtxDataColumn; Value: String);
var
  P: Pointer;
  Buffer: array [0..128] of AnsiChar;
begin
  ASSERT(Column <> nil);
  P := @Buffer[0];
  case Column.FDataType of
    cdtLargeInt: PInt64(P)^ := StrToInt64(Value);
    cdtSmallInt: PSmallInt(P)^ := StrToInt(Value);
    cdtInteger: PInteger(P)^ := StrToInt(Value);
    cdtBoolean: PWordBool(P)^ := StrToBool(Value);
    cdtFloat: PDouble(P)^ := StrToFloat(Value);
    cdtDateTime: PDateTime(P)^ := StrToDateTime(Value);
    cdtDate: PDateTime(P)^ := StrToDate(Value);
    cdtTime: PDateTime(P)^ := StrToTime(Value);
    cdtString: PAnsiString(P)^ := AnsiString(Value);
    cdtWideString: PWideString(P)^ := Value;
    cdtGuid: PGuid(P)^ := StringToGUID(Value);
    cdtMemo: PAnsiString(P)^ := AnsiString(Value);
    cdtBlob: PAnsiString(P)^ := AnsiString(Value);
    else
      raise Exception.CreateFmt(SUnableToAssignValueAsVariant, [DataTable.Name, Column.Name]);
  end;
  SetColumnData(Column, P);
  if Column.DataType in cdtStringTypes then
    PAnsiString(P)^ := ''
  else if Column.DataType in cdtWideStringTypes then
    PWideString(P)^ := '';
end;

function TCtxDataRow.GetOriginalValue(Column: TCtxDataColumn): Variant;
begin
  if FOriginalRow <> nil then
    Result := FOriginalRow.Value[Column]
  else
    Result := GetValue(Column);
end;

{ Editing state methods }

procedure TCtxDataRow.AcceptChanges;
begin
  CheckNeedRefresh;
  // Preserv current state, reset all modification flags
  FreeAndNil(FOriginalRow);
  FState := FState - [drsInserted, drsUpdated, drsDeleted];
end;

procedure TCtxDataRow.RejectChanges;
begin
  DataContainer.CheckNotInTransaction;

  // First lets cancel any editing
  if Editing then
    CancelEdit;
  if Inserted then
    // Delete self from data table if was inserted
    DataTable.DeleteRow(Self)
  else
  begin
    // Restore original state, reset all modification flags
    if FOriginalRow <> nil then
    begin
      Assign(FOriginalRow);
      FreeAndNil(FOriginalRow);
    end;
    // Undelete row if it was deleted
    if Deleted then
      DataTable.UnDeleteRow(Self);
    FState := FState - [drsInserted, drsUpdated, drsDeleted, drsNeedRefresh];
    DataTable.NotifyEvent(Self, cdeRowModified);
  end;
end;

procedure TCtxDataRow.BeginEdit;
begin
  if not Stored then exit;
  if Editing then
    raise Exception.Create(SRowInEditingState);

  CheckNeedRefresh;
  SaveOriginalValue; // This will preserve old state

  // Copy current Data buffer to RollbackData
  if FData <> nil then
  begin
    FOldRow := DataTable.NewRow;
    InitBuffer(FOldRow.FData);
    InternalCopyBuffer(FData, FOldRow.FData);
  end;
  SetState(drsEditing, True);
end;

procedure TCtxDataRow.CancelEdit;
begin
  if not Stored then exit;
  if not Editing then
    raise Exception.Create(SRowNotInEditingState);

  if FOldRow <> nil then
  begin
    InternalCopyBuffer(FOldRow.FData, FData);
    FreeAndNil(FOldRow);
  end else
    if FData <> nil then
      FreeBuffer(FData);
  SetState(drsEditing, False);
end;

procedure TCtxDataRow.EndEdit;
begin
  if not Stored then exit;
  if not Editing then
    raise Exception.Create(SRowNotInEditingState);

  // ValidateRow performs transactional processing of related rows
  FDataTable.ValidateRow(Self, drsUpdated);

  SetState(drsEditing, False);
  SetState(drsUpdated, True);
  try
    DataTable.RowChanged(Self, nil, cdeRowModified);
  finally
    FreeAndNil(FOldRow);
  end;  
end;

procedure TCtxDataRow.Delete;
begin
  if Stored then
    DataTable.Delete(Self)
  else Free;
end;

procedure TCtxDataRow.Store;
begin
  DataTable.Insert(Self);
end;

function TCtxDataRow.GetReferencedRow(Column: TCtxDataColumn): TCtxDataRow;
begin
  Result := nil;
  if Column.DataType <> cdtReference then exit;
  with Column.Relation do
  begin
    Result := ParentTable.FindRow(Self, ParentColumns, ChildColumns, []);
    if Result = nil then
      Result := ParentTable.NullRow;
  end;
end;

function TCtxDataRow.ModifiedByTransaction: Boolean;
begin
  Result := FRollbackRow <> nil;
end;

procedure TCtxDataRow.TransactionRollback;
begin
  if FRollbackRow <> nil then
  begin
    InternalCopyBuffer(FRollbackRow.FData, FData);
    if Deleted then
      DataTable.UnDeleteRow(Self);
    FState := FRollbackRow.FState; // restore state
    FreeAndNil(FRollbackRow);
  end else
    DataTable.DeleteRow(Self);
end;

procedure TCtxDataRow.TransactionDiscardRollback;
begin
  if FRollbackRow <> nil then
    FreeAndNil(FRollbackRow);
end;

procedure TCtxDataRow.TransactionSave;
begin
  if DataContainer.InTransaction and (FRollbackRow = nil) and not Deleted then
  begin
    if Stored then
    begin
      FRollbackRow := DataTable.NewRow;
      InitBuffer(FRollbackRow.FData);
      InternalCopyBuffer(FData, FRollbackRow.FData);
      FRollbackRow.FState := FState; // save state
    end;
    // In case of insert we only add self. Then when rolling back transaction
    // we will delete this type of row if RollbackRow = nil
    DataContainer.AddRollbackRow(Self);
  end;
end;

function TCtxDataRow.GetDataContainer: TCtxDataContainer;
begin
  Result := FDataTable.DataContainer;
end;

{ TCtxDataContainer }

constructor TCtxDataContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRollbackRows := nil;
  FTransactionCounter := 0;
  FTables := TCtxDataTables.Create(Self, TCtxDataTable);
  FRelations := TCtxDataRelations.Create(Self, TCtxDataRelation);
  FParams := TCtxParameters.Create(Self, TCtxParameter);
  FCursors := TList.Create;
  FEventConsumers := nil;
  FDesigner := nil;
  FUpdateCounter := 0;
  FStoreDataInDfm := False;
  FPersistentData := False;
  FSavedData := nil;
end;

destructor TCtxDataContainer.Destroy;
begin
  FDesigner.Free; // kill the designer and it will kill the form
  FDesigner := nil;
  DataAdapter := nil;
  CloseCursors;

  // No need to store anything here
  FStoreDataInDfm := False;
  FPersistentData := False;
  Active := False;

  FCursors.Free;
  FCursors := nil;
  FRelations.Free;
  FRelations := nil;
  FTables.Free;
  FTables := nil;
  FreeAndNil(FParams);
  FreeAndNil(FSavedData);
  // Disconnect all consumers
  inherited Destroy;
end;

procedure TCtxDataContainer.Prepare;
var
  I: Integer;
begin
  if FActive then exit;
  try
    FActive := True;
    // Activate tables and relations
    for I := 0 to Relations.Count - 1 do
      Relations[I].Prepare;
    for I := 0 to Tables.Count - 1 do
      Tables[I].Prepare;

    // Restore data if any
    if (FSavedData <> nil) and not FLoadingFromStream then
    try
      FSavedData.Position := 0;
      LoadFromStream(FSavedData);
    except
      // Ignore errors on loading
    end;
    FreeAndNil(FSavedData);

    NotifyEvent(Self, cdeContainerActivated);
  except
    UnPrepare;
    raise;
  end;
end;

procedure TCtxDataContainer.UnPrepare;
var
  I: Integer;
begin
  if not FActive then exit;

  // Preserve data if any
  if not FLoadingFromStream then
    if StoreDataInDfm or PersistentData then
    for I := 0 to Tables.Count - 1 do
    if Tables[I].Prepared and (Tables[I].RowCount > 0) then
    begin
      FSavedData := TMemoryStream.Create;
      SaveToStream(FSavedData, False, True);
      FSavedData.Position := 0;
      break;
    end;

  FActive := False;
  NotifyEvent(Self, cdeContainerDeactivated);
  // Deactivate tables and relations
  for I := 0 to Relations.Count - 1 do
    Relations[I].UnPrepare;
  for I := 0 to Tables.Count - 1 do
    Tables[I].UnPrepare;
end;

procedure TCtxDataContainer.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    if Value then
      Prepare
    else UnPrepare;
  end;
end;

procedure TCtxDataContainer.NotifyEvent(Context: TObject; DataEvent: TCtxDataEventType);
var
  I: Integer;
  C: TCtxRowSet;
begin
  if not IsUpdating
    or (not FNotifiedDeactivate and (DataEvent = cdeContainerDeactivated))
  then
  begin
    if FDesigner <> nil then
      FDesigner.NotifyEvent(Context, DataEvent);

    if (FCursors <> nil) and (FCursors.Count > 0) then
    begin
      I := 0;
      while I < FCursors.Count do
      begin
        C := TCtxRowSet(FCursors[I]);
        if C <> nil then
          C.DoNotify(Context, DataEvent);
        if FCursors.IndexOf(C) >= 0 then
          Inc(I);
      end;
    end;

    if IsUpdating and (DataEvent = cdeContainerDeactivated) then
      FNotifiedDeactivate := True;
  end else
    if DataEvent in [cdeTableListChanged, cdeRelationListChanged, cdeTableChanged, cdeColumnListChanged] then
      FStructureChanged := True;
end;

procedure TCtxDataContainer.SetTables(const Value: TCtxDataTables);
begin
  FTables.Assign(Value);
end;

procedure TCtxDataContainer.SetRelations(const Value: TCtxDataRelations);
begin
  FRelations.Assign(Value);
end;

procedure TCtxDataContainer.ClearData;
var
  I: Integer;
begin
  if Active then
  for I := 0 to Tables.Count - 1 do
    Tables[I].ClearData;
end;

procedure TCtxDataContainer.BeginUpdate;
begin
  if FUpdateCounter = 0 then
  begin
    FNotifiedDeactivate := False;
    FStructureChanged := False;
  end;
  Inc(FUpdateCounter);
end;

procedure TCtxDataContainer.EndUpdate;
begin
  Dec(FUpdateCounter);
  if FUpdateCounter = 0 then
  begin
    if FStructureChanged then
      NotifyEvent(Self, cdeContainerChanged);

    if FNotifiedDeactivate and Active then
      NotifyEvent(Self, cdeContainerActivated);

    NotifyEvent(Self, cdeContainerDataChanged);
    FNotifiedDeactivate := False;
    FStructureChanged := False;
  end;
end;

function TCtxDataContainer.IsUpdating: Boolean;
begin
  Result := FUpdateCounter > 0;
end;

procedure TCtxDataContainer.LoadFromFile(const FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCtxDataContainer.ReadData(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TCtxDataContainer.LoadFromStream(Stream: TStream);
var
  Reader: TReader;
  WasActive: Boolean;
  TableName: String;
begin
  ASSERT(not FLoadingFromStream);

  WasActive := Active;
  BeginUpdate;
  try
    FLoadingFromStream := True;
    Reader := TReader.Create(Stream, 4096);
    try
      { Read Structure }
      if Reader.ReadBoolean then
      begin
        Active := False;
        Reader.ReadValue;
        Reader.ReadCollection(Tables);
        Reader.ReadValue;
        Reader.ReadCollection(Relations);
      end;
      // Load data
      Reader.ReadListBegin;
      while not Reader.EndOfList do
      begin
        Active := True;
        // Read table name
        TableName := Reader.ReadString;
        // Read data with field map
        Tables.Get(TableName).ReadData(Reader);
      end;
      Reader.ReadListEnd;
    finally
      Reader.Free;
    end;

    Active := WasActive or Active;
  finally
    // Make sure to dispose saved data here so that container is not loaded
    // again upon activation below
    FreeAndNil(FSavedData);
    FLoadingFromStream := False;
    EndUpdate;
  end;
end;

procedure TCtxDataContainer.SaveToFile(const FileName: String; StoreStructure: Boolean = True; StoreData: Boolean = True);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, StoreStructure, StoreData);
  finally
    Stream.Free;
  end;
end;

procedure TCtxDataContainer.WriteData(Stream: TStream);
begin
  if not Active and (FSavedData <> nil) then
    Stream.CopyFrom(FSavedData, 0)
  else SaveToStream(Stream, False, True);
end;

procedure TCtxDataContainer.SaveToStream(Stream: TStream; StoreStructure: Boolean = True; StoreData: Boolean = True);
var
  Writer: TWriter;
  I: Integer;
begin
  Writer := TWriter.Create(Stream, 4096);
  try
    FWritingToStream := True;
    Writer.WriteBoolean(StoreStructure);
    if StoreStructure then
    begin
      Writer.WriteCollection(Tables);
      Writer.WriteCollection(Relations);
    end;
    Writer.WriteListBegin;
    if StoreData then
      for I := 0 to Tables.Count - 1 do
      begin
        Writer.WriteString(Tables[I].Name);
        Tables[I].WriteData(Writer);
      end;
    Writer.WriteListEnd;
  finally
    FWritingToStream := False;
    Writer.Free;
  end;
end;

procedure TCtxDataContainer.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, FStoreDataInDfm);
end;

procedure TCtxDataContainer.CloseCursors;
begin
  while FCursors.Count > 0 do
    TCtxRowSet(FCursors.Last).Active := False;
end;

procedure TCtxDataContainer.ClearStructure;
begin
  BeginUpdate;
  try
    Active := False;
    Tables.Clear;
    Relations.Clear;
  finally
    EndUpdate;
  end;
end;

procedure TCtxDataContainer.Clear;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Tables.Count - 1 do
      Tables[I].ClearData;
    // Cleanup saved data also
    FreeAndNil(FSavedData);
  finally
    EndUpdate;
  end;
end;

procedure TCtxDataContainer.EmptyTables;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Tables.Count - 1 do
      Tables[I].EmptyTable;
  finally
    EndUpdate;
  end;
end;

function TCtxDataContainer.CheckDataAdapter(ADataAdapter: TCtxDataAdapter): TCtxDataAdapter;
begin
  if ADataAdapter = nil then
    Result := FDataAdapter
  else Result := ADataAdapter;
  if Result = nil then
    raise Exception.Create(SDataAdapterIsNotAssigned);
end;

procedure TCtxDataContainer.Fill(ADataAdapter: TCtxDataAdapter = nil);
begin
  CheckDataAdapter(ADataAdapter).Fill(Self);
end;

procedure TCtxDataContainer.Refresh(ADataAdapter: TCtxDataAdapter = nil);
begin
  CheckDataAdapter(ADataAdapter).Refresh(Self);
end;

procedure TCtxDataContainer.Update(ADataAdapter: TCtxDataAdapter = nil);
begin
  DoBeforeUpdate;
  try
    CheckDataAdapter(ADataAdapter).Update(Self);
    // Update journals here in descendants
    DoAfterUpdate;
  except
    DoOnUpdateFailed;
    raise;
  end;
end;

procedure TCtxDataContainer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FDataAdapter) then
    DataAdapter := nil;
  inherited;
end;

procedure TCtxDataContainer.DoAfterUpdate;
begin
  // Implement in descendants. Update journals for example.
  if Assigned(OnAfterUpdate) then
    OnAfterUpdate(Self);
end;

procedure TCtxDataContainer.DoBeforeUpdate;
begin
  // Implement in descendants.
  if Assigned(OnBeforeUpdate) then
    OnBeforeUpdate(Self);
end;

procedure TCtxDataContainer.DoOnUpdateFailed;
begin
  // Implement in descendants.
  if Assigned(OnUpdateFailed) then
    OnUpdateFailed(Self);
end;

procedure TCtxDataContainer.SetParams(const Value: TCtxParameters);
begin
  FParams.Assign(Value);
end;

function TCtxDataContainer.GetParam(const Name: String): Variant;
var
  P: TCtxParameter;
begin
  P := FParams.Get(Name);
  if P <> nil then
    Result := P.Value
  else Result := NULL;
end;

procedure TCtxDataContainer.SetParam(const Name: String;
  const Value: Variant);
var
  P: TCtxParameter;
begin
  P := FParams.Get(Name);
  if P <> nil then
    P.Value := Value;
end;

procedure TCtxDataContainer.SetDataAdapter(const Value: TCtxDataAdapter);
begin
  if FDataAdapter <> Value then
  begin
    if FDataAdapter <> nil then
      FDataAdapter.RemoveFreeNotification(Self);
    FDataAdapter := Value;
    if FDataAdapter <> nil then
      FDataAdapter.FreeNotification(Self);
  end;
end;

procedure TCtxDataContainer.AcceptChanges;
var
  I: Integer;
begin
  CheckNotInTransaction;
  for I := 0 to Tables.Count - 1 do
    Tables[I].AcceptChanges;
end;

procedure TCtxDataContainer.RejectChanges;
var
  I: Integer;
begin
  CheckNotInTransaction;
  BeginUpdate;
  try
    for I := 0 to Tables.Count - 1 do
      Tables[I].RejectChanges;
  finally
    EndUpdate;
  end;
end;

procedure TCtxDataContainer.Assign(Source: TPersistent);
begin
  if Source.InheritsFrom(TCtxDataContainer) then
  begin
    FLoadingFromStream := True;
    try
      Clear; // cleanup all data, including the one saved in container
      Active := False;
      ClearStructure;
      FTables.Assign(TCtxDataContainer(Source).Tables);
      FRelations.Assign(TCtxDataContainer(Source).Relations);
      PersistentData := TCtxDataContainer(Source).PersistentData;
      StoreDataInDfm := TCtxDataContainer(Source).StoreDataInDfm;
      Params.Assign(TCtxDataContainer(Source).Params);
      // Active := TCtxDataContainer(Source).Active;
      // DataAdapter := TCtxDataContainer(Source).DataAdapter;
    finally
      FLoadingFromStream := False;
    end;
  end else
  if Source.InheritsFrom(TStream) then
  begin
    LoadFromStream(TStream(Source));
  end else
    inherited;
end;

procedure TCtxDataContainer.CleanupRelations;
var
  I: Integer;
begin
  UnPrepare;
  I := 0;
  while I < Relations.Count do
  begin
    if (Tables.Find(Relations[I].ParentTableName) = nil) or
      (Tables.Find(Relations[I].ChildTableName) = nil)
    then
      Relations[I].Free
    else Inc(I);
  end;
end;

procedure TCtxDataContainer.CommitTransaction;
var
  I: Integer;
  R: TCtxDataRow;
begin
  if FTransactionCounter = 0 then exit;
  Dec(FTransactionCounter);
  if FTransactionCounter = 0 then
  begin
    for I := FRollbackRows.Count - 1 downto 0 do
    begin
      R := FRollbackRows[I];
      R.TransactionDiscardRollback;
    end;
    FreeAndNil(FRollbackRows);
  end;
end;

procedure TCtxDataContainer.RollbackTransaction;
var
  I: Integer;
  R: TCtxDataRow;
begin
  if FTransactionCounter = 0 then exit;
  for I := FRollbackRows.Count - 1 downto 0 do
  begin
    R := FRollbackRows[I];
    R.TransactionRollback;
  end;
  FreeAndNil(FRollbackRows);
  FTransactionCounter := 0;
end;

procedure TCtxDataContainer.StartTransaction;
begin
  if FTransactionCounter = 0 then
    FRollbackRows := TList.Create;
  Inc(FTransactionCounter);
end;

function TCtxDataContainer.InTransaction: Boolean;
begin
  Result := FTransactionCounter > 0;
end;

procedure TCtxDataContainer.CheckNotInTransaction;
begin
  if InTransaction then
    raise Exception.Create(SUnableToPerformWithinTransaction);
end;

procedure TCtxDataContainer.AddRollbackRow(ARow: TCtxDataRow);
begin
  if (FRollbackRows <> nil) and (FRollbackRows.IndexOf(ARow) < 0) then
    FRollbackRows.Add(ARow);
end;

procedure TCtxDataContainer.DoCalcFields(ARow: TCtxDataRow);
begin
  if Assigned(FOnCalcFields) then
    FOnCalcFields(Self, ARow);
end;

procedure TCtxDataContainer.SetStoreDataInDfm(const Value: Boolean);
begin
  FStoreDataInDfm := Value;
end;

{ TCtxDataCollection }

function TCtxDataCollection.Find(const AName: String): TCtxDataCollectionItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TCtxDataCollectionItem(Items[I]);
    if AnsiSameText(Result.Name, AName) then exit;
  end;
  Result := nil;
end;

function TCtxDataCollection.Get(const AName: String): TCtxDataCollectionItem;
begin
  Result := Find(AName);
  if Result = nil then
    raise Exception.CreateFmt(SItemNotFound, [AName]);
end;

function TCtxDataCollection.GetAutoName(const ProposedName: String; Counter: Integer = 0): String;
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

procedure TCtxDataCollection.GetNames(List: TStrings);
var
  I: Integer;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := 0 to Count - 1 do
      List.Add(TCtxDataCollectionItem(Items[I]).Name);
  finally
    List.EndUpdate;
  end;
end;

{$HINTS OFF}
type
  _TCollection = class(TPersistent)
  private
    FItemClass: TCollectionItemClass;
    FItems: TList;
  end;
{$HINTS ON}

function TCtxDataCollection.IndexOf(Item: Pointer): Integer;
begin
  Result := _TCollection(Self).FItems.IndexOf(Item)
end;

function TCtxDataCollection.IsUnique(const AName: String;
  Item: TCtxDataCollectionItem): Boolean;
var
  Item2: TCtxDataCollectionItem;
begin
  Item2 := Find(AName);
  Result := (Item2 = nil) or (Item2 = Item);
end;

{ TCtxDataTables }

function TCtxDataTables.Add: TCtxDataTable;
begin
  Result := TCtxDataTable(inherited Add);
end;

function TCtxDataTables.Find(const AName: String): TCtxDataTable;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    if AnsiSameText(Tables[I].Name, AName) then
    begin
      Result := Tables[I];
      Exit;
    end;
  Result := nil;
end;

function TCtxDataTables.Get(const AName: String): TCtxDataTable;
begin
  Result := Find(AName);
  if Result = nil then
    raise Exception.CreateFmt(SDataTableNotFound, [AName]);
end;

function TCtxDataTables.GetDataContainer: TCtxDataContainer;
begin
  Result := TCtxDataContainer(GetOwner);
end;

function TCtxDataTables.GetTable(Idx: Integer): TCtxDataTable;
begin
  Result := TCtxDataTable(inherited GetItem(Idx));
end;

procedure TCtxDataTables.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  DataContainer.Active := False;
  inherited;
end;

procedure TCtxDataTables.SetTable(Idx: Integer;
  const Value: TCtxDataTable);
begin
  inherited SetItem(Idx, Value);
end;

procedure TCtxDataTables.Update(Item: TCollectionItem);
begin
  DataContainer.NotifyEvent(DataContainer, cdeTableListChanged);
  inherited;
end;

{ TCtxDataRelation }

constructor TCtxDataRelation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TCtxDataRelation.Destroy;
begin
  DataContainer.Active := False;
  inherited Destroy;
end;

function TCtxDataRelation.GetDataContainer: TCtxDataContainer;
begin
  Result := TCtxDataRelations(Collection).GetDataContainer;
end;

procedure TCtxDataRelation.UnPrepare;
begin
  if not FPrepared then exit;
  FParentTable := nil;
  FChildTable := nil;
  SetLength(FParentColumns, 0);
  SetLength(FChildColumns, 0);
  FPrepared := False;
end;

procedure TCtxDataRelation.Prepare;
begin
  if FPrepared then exit;
  try
    FPrepared := True;
    FParentTable := DataContainer.Tables.Get(FParentTableName);
    FChildTable := DataContainer.Tables.Get(FChildTableName);
    FParentColumns := FParentTable.Columns.GetColumnArray(FParentColumnNames);
    FChildColumns := FChildTable.Columns.GetColumnArray(FChildColumnNames);
    if Length(FParentColumns) <> Length(FChildColumns) then
      raise Exception.CreateFmt(SInvalidRelationDefintion, [Name]);
  except
    UnPrepare;
    raise;
  end;
end;

procedure TCtxDataRelation.SetName(const Value: String);
var
  TmpStr: String;
begin
  if FName <> Value then
  begin
    // Validate rename
    TmpStr := Trim(Value);
    if TmpStr = '' then
      raise Exception.Create(SEmptyRelationName);

    if not DataContainer.Relations.IsUnique(TmpStr, Self) then
      raise Exception.CreateFmt(SDuplicateRelationName, [TmpStr]);

    DataContainer.Active := False;
    FName := Value;
  end;
end;

procedure TCtxDataRelation.SetChildColumnNames(const Value: String);
begin
  if FChildColumnNames <> Value then
  begin
    DataContainer.Active := False;
    FChildColumnNames := Value;
  end;
end;

procedure TCtxDataRelation.SetChildTableName(const Value: String);
begin
  if FChildTableName <> Value then
  begin
    DataContainer.Active := False;
    FChildTableName := Value;
  end;
end;

procedure TCtxDataRelation.SetParentColumnNames(const Value: String);
begin
  if FParentColumnNames <> Value then
  begin
    DataContainer.Active := False;
    FParentColumnNames := Value;
  end;
end;

procedure TCtxDataRelation.SetParentTableName(const Value: String);
begin
  if FParentTableName <> Value then
  begin
    DataContainer.Active := False;
    FParentTableName := Value;
  end;
end;

procedure TCtxDataRelation.Assign(Source: TPersistent);
begin
  if Source.InheritsFrom(TCtxDataRelation) then
  begin
    DataContainer.Active := False;
    if FName <> TCtxDataRelation(Source).Name then
      FName := TCtxDataCollection(Collection).GetAutoName(TCtxDataRelation(Source).Name);
    FParentTableName := TCtxDataRelation(Source).FParentTableName;
    FChildTableName := TCtxDataRelation(Source).FChildTableName;
    FParentColumnNames := TCtxDataRelation(Source).FParentColumnNames;
    FChildColumnNames := TCtxDataRelation(Source).FChildColumnNames;
    FDeleteAction := TCtxDataRelation(Source).FDeleteAction;
    FUpdateAction := TCtxDataRelation(Source).FDeleteAction;
  end else
    inherited;
end;

function TCtxDataRelation.GetConstraintActive: Boolean;
begin
  Result := (DeleteAction <> draIgnore) or (UpdateAction <> draIgnore);
end;

function TCtxDataRelation.GetIdentifying: Boolean;
var
  I: Integer;
begin
  Result := False;
  if (ParentTable = nil) or (Length(ParentColumns) <> Length(ParentTable.PKColumns)) then exit;
  for I := 0 to Length(ParentColumns) - 1 do
    if ParentColumns[I] <> ParentTable.PKColumns[I] then exit;
  Result := True;
end;

{ TCtxDataRelations }

function TCtxDataRelations.Add: TCtxDataRelation;
begin
  Result := TCtxDataRelation(inherited Add);
end;

function TCtxDataRelations.Find(const AName: String): TCtxDataRelation;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
  begin
    Result := Relations[I];
    if AnsiSameText(Result.Name, AName) then
      Exit;
  end;
  Result := nil;
end;

function TCtxDataRelations.Get(const AName: String): TCtxDataRelation;
begin
  Result := Find(AName);
  if Result = nil then
    raise Exception.CreateFmt(SDataRelationNotFound, [AName]);
end;

function TCtxDataRelations.GetDataContainer: TCtxDataContainer;
begin
  Result := TCtxDataContainer(GetOwner);
end;

function TCtxDataRelations.GetRelation(Idx: Integer): TCtxDataRelation;
begin
  Result := TCtxDataRelation(inherited GetItem(Idx));
end;

procedure TCtxDataRelations.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
  DataContainer.NotifyEvent(DataContainer, cdeRelationListChanged);
end;

procedure TCtxDataRelations.SetRelation(Idx: Integer;
  const Value: TCtxDataRelation);
begin
  inherited SetItem(Idx, Value);
end;

{ TCtxRowSet }

constructor TCtxRowSet.Create(ADataTable: TCtxDataTable; ARelation: TCtxDataRelation);
begin
  Inherited Create;
  FDataTable := ADataTable;
  FRelation := ARelation;
  FRows := TList.Create;
  FKeyValue := Null;
  ResetSorting;
  FFiltered := False;
  FStartIndex := 0;
  FRowCount := 0;
  if FRelation <> nil then
    SetKeyColumns(FRelation.FChildColumnNames);
end;

destructor TCtxRowSet.Destroy;
begin
  Active := False;
  ClearList;
  FDataTable := nil;
  FRows.Free;
  Inherited Destroy;
end;

procedure TCtxRowSet.CheckTable;
begin
  if (FDataTable = nil) or not FDataTable.Prepared then
    raise Exception.Create(SErrorOpeningDataTable);
end;

procedure TCtxRowSet.CheckRelation;
begin
  if (FRelation = nil) or (not FRelation.Prepared) or (FRelation.ChildTable <> FDataTable) then
    raise Exception.Create(SInvalidRelation);
end;

procedure TCtxRowSet.SetDataTable(Value: TCtxDataTable);
begin
  if FDataTable = Value then
    Exit;
  Active := False;
  FDataTable := Value;
end;

function TCtxRowSet.GetRow(Idx: Integer): TCtxDataRow;
begin
  if FDataTable = nil then
    Result := nil
  else if NativeAccess then
    Result := FDataTable.Rows[Idx]
  else if (Idx >= 0) and (Idx < FRowCount) then
    Result := FRows[Idx + FStartIndex]
  else Result := nil;
end;

function TCtxRowSet.GetRowCount: Integer;
begin
  if FDataTable = nil then
    Result := 0
  else if NativeAccess then
    Result := FDataTable.RowCount
  else Result := FRowCount;
end;

procedure TCtxRowSet.SetActive(Value: Boolean);
begin
  if Active = Value then
    Exit;
  if not Value then
  begin
    ClearList;
    DoNotify(Self, cdeCursorClosed);
    DataContainer.FCursors.Remove(Self);
    FDataTable := nil;
    FRelation := nil;
    ResetSorting;
  end else
  begin
    if FDataTable = nil then
      raise Exception.Create(SDataTableNotAssigned);
    DataContainer.Active := True;
    DataContainer.NotifyEvent(Self, cdeCursorBeforeOpen);
    if FRelation <> nil then
    begin
      CheckRelation;
      SetKeyColumns(FRelation.FChildColumnNames);
    end;
    DataContainer.FCursors.Add(Self);
    BuildList;
  end;
  FActive := Value;
end;

function TCtxRowSet.NativeAccess: Boolean;
begin
  Result := not (IsSorted or IsFiltered);
end;

function TCtxRowSet.IsSorted: Boolean;
begin
  Result := FSortCount > 0;
end;

function TCtxRowSet.IsFiltered: Boolean;
begin
  Result := FFiltered and Assigned(OnFilterDataRow);
end;

procedure TCtxRowSet.SetFiltered(Value: Boolean);
begin
  if FFiltered <> Value then
  begin
    FFiltered := Value;
    Refresh;
  end;
end;

function TCtxRowSet.IndexOfRow(ARow: TCtxDataRow): Integer;
begin
  if ARow = nil then
    Result := -1
  else if NativeAccess then
    Result := ARow.Index
  else Result := FRows.IndexOf(ARow) - FStartIndex;
end;

function TCtxRowSet.FindNext(Restart: Boolean = True; GoForward: Boolean = True;
  ACurPos: Integer = 0): TCtxDataRow;
var
  C,E: Integer;
  Accept: Boolean;
begin
  Result := nil;
  if not Assigned(OnFilterDataRow) then
    Exit;
  E := RowCount;    
  C := 0;
  if Restart and GoForward then
  begin
    ACurPos := 0;
    C := E-1;
  end else
  if Restart and not GoForward then
  begin
    ACurPos := E-1;
    C := 0;
  end else
  if not Restart and GoForward then
  begin
    inc(ACurPos);
    C := E-1;
  end else
  if not Restart and not GoForward then
  begin
    dec(ACurPos);
    C := 0;
  end;
  while (ACurPos <> C) and (ACurPos >= 0) and (ACurPos < E) do
  begin
    Accept := True;
    OnFilterDataRow(Rows[ACurPos], Accept);
    if Accept then
    begin
      Result := Rows[ACurPos];
      Exit;
    end;
    if GoForward then
      inc(ACurPos) else
      dec(ACurPos);
  end;
end;

function TCtxRowSet.FindRowPos(ARow: TCtxDataRow; var Pos: Integer): Boolean;
var
  M, L, H, R: Integer;
begin
  ASSERT(Length(FSortCols) > 0);

  Result := False; // Exact match
  L := 0;
  H := RowCount - 1;
  while L <= H do
  begin
    M := (L + H) shr 1;
    // R := CompareRows(Rows[M], ARow, FSortCols, FSortCols, []);
    R := DoCompareRows(Rows[M], ARow);
    if R < 0 then
      L := M + 1 else
      begin
        H := M - 1;
        if R = 0 then
        begin
          L := M;
          Result := True;
        end;
      end;
  end;
  Pos := L;
end;

function TCtxRowSet.Locate(const KeyFields: String; const KeyValues: Variant;
  Options: TCtxCompareOptions): TCtxDataRow;
var
  FldList: TCtxColumnArray;
  TempRow: TCtxDataRow;
  I: Integer;
begin
  // Optimize by using existing sort order
  Result := nil;
  if (not Active) or (RowCount = 0) then
    Exit;
  SetLength(FldList, 0);
  TempRow := FDataTable.NewRow;
  try
    FldList := FDataTable.Columns.GetColumnArray(KeyFields);
    TempRow.Values[FldList] := KeyValues;
    for I := 0 to RowCount-1 do
    begin
      if CompareRows(TempRow, Rows[I], FldList, FldList, Options) = 0 then
      begin
        Result := Rows[I];
        Exit;
      end;
    end;  
  finally
    TempRow.Free;
  end;
end;

procedure TCtxRowSet.ClearList;
begin
  FRows.Clear;
  FStartIndex := 0;
  FRowCount := 0;
end;

function TCtxRowSet.UpdateSorting: Boolean;
var
  I: Integer;
begin
  // Update Sorting will return true if sorting has actually changed
  Result := True;

  // ToDo: Remove duplicated columns from sorting
  FSortCount := OrderByColumnCount + FKeyColCount;

  SetLength(FSortCols, FSortCount);
  SetLength(FSortOptions, FSortCount);

  for I := 0 to FKeyColCount - 1 do
  begin
    FSortCols[I] := FKeyCols[I];
    FSortOptions[I] := [];
  end;

  for I := FKeyColCount to FSortCount - 1 do
  begin
    FSortCols[I] := FOrderByColumns[I - FKeyColCount].FColumn;
    FSortOptions[I] := FOrderByColumns[I - FKeyColCount].FOptions;
  end;

end;

procedure TCtxRowSet.UpdateRange;
var
  Idx, P, SaveSortCount: Integer;
  FilterRow: TCtxDataRow;
begin
  // Update FStartIndex and FRowCount
  FStartIndex := 0;
  FRowCount := FRows.Count;
  if FKeyColCount = 0 then exit;

  // Assign key column values for filtering
  SaveSortCount := FSortCount;
  FilterRow := FDataTable.NewRow;
  try
    FilterRow.SetValues(FKeyCols, FKeyValue);
    FSortCount := FKeyColCount;

    if FindRowPos(FilterRow, P) then
    begin
      Idx := P - 1;
      while (Idx >= 0) and (DoCompareRows(FilterRow, Rows[Idx]) = 0) do
        Dec(Idx);

      Inc(P);
      while (P < FRows.Count) and (DoCompareRows(FilterRow, Rows[P]) = 0) do
        Inc(P);

      FStartIndex := Idx + 1;
      FRowCount := P - FStartIndex;
    end else
      FRowCount := 0;
  finally
    FSortCount := SaveSortCount;
    FreeAndNil(FilterRow);
  end;
end;

procedure TCtxRowSet.BuildList;
var
  I: Integer;
  R: TCtxDataRow;
begin
  ClearList;
  CheckTable;

  if NativeAccess or (FDataTable.RowCount = 0) then
    Exit;

  // Input:
  // FSortCols - contains sort columns only to pass to Row.Modified call. Not used otheriwse.
  // FKeyCols - Not used for sorting only for filtering
  // FOrderByColumns - array of ordering options
  // FOrderByColumnCount - size of array of FOrderByColumns

  if IsFiltered then
  begin
    // Process all rows
    for I := 0 to FDataTable.RowCount - 1 do
    begin
      R := FDataTable.Rows[I];
      if DoFilterRow(R) then
        FRows.Add(R);
    end;

  end else
  begin
    // Simply copy all rows
    FRows.Assign(FDataTable.FRows);
    FRows.Count := FDataTable.RowCount;
    (*
    for I := 0 to FDataTable.RowCount - 1 do
      FRows.Add(FDataTable.Rows[I]);
    *)
  end;

  FStartIndex := 0;
  FRowCount := FRows.Count;

  if IsSorted then
    DoSort(0, RowCount-1);

  UpdateRange;
end;

procedure TCtxRowSet.DoSort(iLo, iHi: Integer);
var
  Lo, Hi: Integer;
  MidObj: TCtxDataRow;
  T: Pointer;
begin
  if iLo < iHi then
  repeat
    Lo := iLo;
    Hi := iHi;
    MidObj := Rows[(iLo+iHi) shr 1];
    repeat
      while DoCompareRows(Rows[Lo], MidObj) < 0 do Inc(Lo);
      while DoCompareRows(Rows[Hi], MidObj) > 0 do Dec(Hi);
      if Lo <= Hi then
      begin
        T := FRows[Lo];
        FRows[Lo] := FRows[Hi];
        FRows[Hi] := T;
        Inc(Lo);
        Dec(Hi);
      end;
    until Lo > Hi;
    if iLo < Hi then
      DoSort(iLo, Hi);
    iLo := Lo;
  until Lo >= iHi;
end;

function TCtxRowSet.DoCompareRows(Row1, Row2: TCtxDataRow): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FSortCount - 1 do
  begin
    Result := _CompareRow(Row1, Row2, FSortCols[I], FSortOptions[I]);
    if Result <> 0 then
      Break;
  end;
end;

procedure TCtxRowSet.Refresh;
begin
  if Active then
    BuildList;
end;

function TCtxRowSet.DoFilterRow(ARow: TCtxDataRow): Boolean;
begin
  Result := True;
  if IsFiltered then
    OnFilterDataRow(ARow, Result)
end;

procedure TCtxRowSet.CheckRow(ARow: TCtxDataRow; DataEvent: TCtxDataEventType);

  function FindNewPos(L, H: Integer): Integer;
  var M, R: Integer;
  begin
    while L <= H do
    begin
      M := (L + H) shr 1;
      R := DoCompareRows(Rows[M], ARow);
      if R < 0 then
        L := M + 1 else
        begin
          H := M - 1;
          if R = 0 then L := M;
        end;
    end;
    Result := L;
  end;

var
  RIdx, NewRIdx: Integer;
begin
  if (ARow = nil) or NativeAccess or
     (not (DataEvent in [cdeRowInserted, cdeRowModified, cdeRowDeleted])) then
    Exit;

  RIdx := -1; // +++ MB: was not initialized
  FStartIndex := 0;
  FRowCount := FRows.Count;
  try
    if DataEvent <> cdeRowInserted then
      RIdx := IndexOfRow(ARow);

    if (DataEvent = cdeRowDeleted) or (not DoFilterRow(ARow)) then
    begin
      if RIdx >= 0 then
        FRows.Delete(RIdx);
    end else if DataEvent = cdeRowInserted then
    begin
      if IsSorted then
        NewRIdx := FindNewPos(0, RowCount - 1)
      else NewRIdx := RowCount;
      FRows.Insert(NewRIdx, ARow);
    end else if IsSorted and ARow.IsModified(FSortCols) and (RowCount > 1) and (RIdx >= 0) then
    begin
      FRows.Move(RIdx, FRowCount - 1);
      NewRIdx := FindNewPos(0, FRowCount - 2);
      FRows.Move(RowCount - 1, NewRIdx);
    end;
  finally
    UpdateRange;
  end;
end;

function TCtxRowSet.SetKeyColumns(const AColumnNames: String): Boolean;
begin
  Result := FKeyColNames <> AColumnNames;
  if Result then
  begin
    FKeyColNames := AColumnNames;
    FKeyCols := FDataTable.Columns.GetColumnArray(FKeyColNames);
    FKeyColCount := Length(FKeyCols);
    // Update Sorting will return true if sorting has actually changed
    Result := UpdateSorting;
  end;
end;

function TCtxRowSet.SetKeyValue(AKeyValue: Variant): Boolean;
begin
  FMasterRow := nil;
  Result := FKeyValue <> AKeyValue;
  if Result then
    FKeyValue := AKeyValue;
end;

function TCtxRowSet.SetMasterKey(const AColumnNames: String; AKeyValue: Variant): Boolean;
begin
  CheckTable;

  Result := SetKeyColumns(AColumnNames);
  if Result then
  begin
    SetKeyValue(AKeyValue);
    Refresh;
    exit;
  end;

  Result := SetKeyValue(AKeyValue);
  if Result and Active then
    UpdateRange;
end;

function TCtxRowSet.SetMasterKey(AKeyValue: Variant): Boolean;
begin
  Result := SetMasterKey(FKeyColNames, AKeyValue);
end;

procedure TCtxRowSet.SetMasterRow(Value: TCtxDataRow);
begin
  if (FMasterRow <> nil) and (FMasterRow <> Value) and (FRelation <> nil) then
    SetMasterKey(FKeyColNames, FMasterRow.GetValues(FRelation.FParentColumns))
  else SetMasterKey(FKeyColNames, DataTable.NullRow.GetValues(FRelation.FParentColumns));
end;

procedure TCtxRowSet.DoNotify(Context: TObject; DataEvent: TCtxDataEventType);
begin
  if Context <> nil then
  case DataEvent of
    cdeContainerDeactivated:
    begin
      if Assigned(FOnNotifyDataEvent) then
        FOnNotifyDataEvent(Context, DataEvent);
    end;
    cdeContainerDataChanged,
    cdeTableDataChanged:
      if (DataEvent = cdeContainerDataChanged) or (Context = FDataTable) then
      begin
        Refresh;
        if Assigned(FOnNotifyDataEvent) then
          FOnNotifyDataEvent(Context, DataEvent);
      end;
    cdeRowInserted,
    cdeRowModified,
    cdeRowDeleted:
      if TCtxDataRow(Context).DataTable = FDataTable then
      begin
        CheckRow(TCtxDataRow(Context), DataEvent);
        if Assigned(FOnNotifyDataEvent) then
          FOnNotifyDataEvent(Context, DataEvent);
      end;
  end;
end;

function TCtxRowSet.GetColumn(Idx: Integer): TCtxDataColumn;
begin
  Result := FDataTable.Columns[Idx];
end;

procedure TCtxRowSet.SetRelation(const Value: TCtxDataRelation);
begin
  if FRelation <> Value then
  begin
    Active := False;
    FRelation := Value;
  end;
end;

function TCtxRowSet.GetOrderByColumns(Index: Integer): TCtxOrderByColumn;
begin
  Result := FOrderByColumns[Index];
end;

procedure TCtxRowSet.SetOrderByColumns(Value: TCtxOrderByColumns);
begin
  FValid := False;
  ClearList;
  FOrderByColumns := Value;
  // Update Sorting will return true if sorting has actually changed
  if UpdateSorting then
    Refresh;
  FValid := True;
end;

function TCtxRowSet.GetDataContainer: TCtxDataContainer;
begin
  if FDataTable <> nil then
    Result := FDataTable.GetDataContainer
  else Result := nil;
end;

function TCtxRowSet.GetOrderByColumnCount: Integer;
begin
  Result := Length(FOrderByColumns);
end;

procedure TCtxRowSet.ResetSorting;
begin
  SetLength(FKeyCols, 0);
  FKeyColNames := '';
  SetLength(FSortCols, 0);
  SetLength(FSortOptions, 0);
  FSortCount := 0;
end;

{ TCtxDataAdapter }

procedure TCtxDataAdapter.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TCtxDataAdapter.EndUpdate;
begin
  Dec(FUpdateCounter);
end;

function TCtxDataAdapter.IsUpdating: Boolean;
begin
  Result := FUpdateCounter > 0;
end;

procedure TCtxDataAdapter.Refresh(DataContainer: TCtxDataContainer);
begin
  DataContainer.BeginUpdate;
  try
    DataContainer.Clear;
    Fill(DataContainer);
  finally
    DataContainer.EndUpdate;
  end;
end;

end.

