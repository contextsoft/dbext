(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Component implementing generic memory VCL DataSet
(*
(*  Contains:
(*              TDbMemDataSet = class(TDataSet)
(*
(*  Copyright (c) 2004-2007 Michael Baytalsky
(*
(*  ------------------------------------------------------------
(*  FILE        : dbMemDS.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 2.15
(*  DELPHI\BCB  : Delphi 5,6,7,2005,2006,2007
(*
(******************************************************************************)
unit dbMemDS;

interface

uses
  Classes, SysUtils, DB, dbExtParser;

const
  btSign = $DD;   // DEBUG
  flUsed = $EE;
  flFree = $DD;

  FlatBufferLimit = 32;

  DefaultPieceSize: longint = 64*1024 - 22;  // 64K

{ internal data types }

{ ! can't modify this consts }

  dtUnknown     = 0;
  dtString      = 1;

  dtInt8        = 2;
  dtInt16       = 3;
  dtSmallint    = dtInt16;
  dtInt32       = 4;

  dtInteger     = dtInt32;
  dtFloat       = 5;
  dtDate        = 6; // Date only
  dtTime        = 7; // Time only
  dtDateTime    = 8; // Date and time
  dtUInt16      = 9;
  dtWord        = dtUInt16;
  dtBoolean     = 10;
  dtInt64       = 11;
  dtLargeint    = dtInt64;
  dtCurrency    = 12;
  dtBlob        = 13;
  dtMemo        = 14;
  dtObject      = 15;
  dtReference   = 16;
  dtArray       = 17;
  dtTable       = 18;

  dtVariant     = 19;
  dtExtString   = 20;
  dtBytes       = 21;
  dtVarBytes    = 22; /// Cannot be deleted because "Fixed" flag not avaible on component level (MSAccess) GetFieldType(DataType: word): TFieldType
  dtExtVarBytes = 23;

  dtUInt32      = 24;
  dtLongword    = dtUInt32;

  dtWideString  = 25;
  dtExtWideString = 26;

  dtGraphic     = 27;
  dtFmtMemo     = 28;

{ StringHeap const }

const
  BlockSize = 16384;
  SmallSize = 2000;
  Align = 8;
  RefNull = 101;

type
  TDataType = word;

{ TBlockManager }

  PBlockHeader = ^TBlockHeader;
  TBlockHeader = packed record
    ItemCount: word;
    UsedItems: word;
    Prev: PBlockHeader;
    Next: PBlockHeader;
    Test: byte;       // DEBUG
  end;

  TItemStatus = (isUnmodified, isUpdated, isAppended, isDeleted);
  TItemTypes = set of TItemStatus;
  TUpdateRecAction = (urFail, urAbort, urSkip, urRetry, urApplied, urNone);

  PItemHeader = ^TItemHeader;
  TItemHeader = packed record
    Block: PBlockHeader;
    Prev: PItemHeader;
    Next: PItemHeader;
    Rollback: PItemHeader;
    Status: TItemStatus;
    UpdateResult: TUpdateRecAction;
    Order: longint;
    Flag: byte;
  end;

  TBlockManager = class
  private
  public
    FirstFree: PItemHeader;
    FirstBlock: PBlockHeader;
    RecordSize: longint;
    DefaultItemCount: word;

    constructor Create;
    destructor Destroy; override;

    procedure AllocBlock(var Block: PBlockHeader; ItemCount: word);
    procedure FreeBlock(Block: PBlockHeader);

    procedure AddFreeBlock;
    procedure FreeAllBlock;

    procedure AllocItem(var Item: PItemHeader);
    procedure FreeItem(var Item: PItemHeader);
    procedure InitItem(Item: PItemHeader);

    procedure PutRecord(Item: PItemHeader; Rec: pointer);
    procedure GetRecord(Item: PItemHeader; Rec: pointer);
    function GetRecordPtr(Item: PItemHeader): pointer;

    procedure CopyRecord(ItemSrc: PItemHeader; ItemDest: PItemHeader);
  end;

{ TStringHeap }

  PBlock = ^TBlock;
  TStrData = array [0..BlockSize - SizeOf(PBlock) - 1] of char;
  TBlock = packed record
    Next: PBlock;
    Data: TStrData;
  end;

  PSmallTab = ^TSmallTab;
  TSmallTab = array [1..SmallSize div Align] of PChar;

  TStringHeap = class
  private
    FSmallTab: PSmallTab;
    FFree: integer;
    FRoot: PBlock;
    FEmpty: boolean;
    FSysGetMem: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function NewBuf(Size: integer): pointer;
    function AllocStr(Str: PChar; Trim: boolean = false): PChar; overload;
    function AllocStr(Str: PWideChar; Trim: boolean = false): PWideChar; overload;
    function ReAllocStr(Str: PChar; Trim: boolean = false): PChar; overload;
    function ReAllocStr(Str: PWideChar; Trim: boolean = false): PWideChar; overload;
    procedure DisposeBuf(Buf: pointer);
    procedure AddRef(Buf: pointer);
    procedure Clear;
    property Empty: boolean read FEmpty;
    property SysGetMem: boolean read FSysGetMem;
  end;

{ TFieldDesc }

  TFieldTypeSet = set of byte;

  TDateFormat = (dfMSecs, dfDateTime, dfTime, dfDate);

  TObjectType = class;

  TFieldDesc = class
  protected
    FName: string;       // unique name in TData
    FActualName: string; // original name from source
    FTableName: string;      //table of name that holds this field
    FDataType: word;
    FSubDataType: word;
    FLength: word;       // precision for number
    FScale: word;
    FFieldNo: word;
    FActualFieldNo: word;
    FSize: word;         // size in rec buffer
    FDataSize: word;     // size in storage structure
    FOffset: longint;    // offset in rec buffer
    FDataOffset: longint;// offset in storage structure
    FRequired: boolean;
    FReadOnly: boolean;
    FIsKey: boolean;
    FFixed: boolean;     // indicates that the string field has a fixed size
    FHidden: boolean;
    FObjectType: TObjectType;
    FParentField: TFieldDesc;
    FHiddenObject: boolean;  // for hide Object field (child field is visible)
    FHandle: pointer;    // pointer to field specific data
    FReserved: boolean;  // reserved flag for perfomance optimization

    procedure SetObjectType(Value: TObjectType);

  public
    constructor Create;
    destructor Destroy; override;

    function HasParent: boolean;

    procedure Assign(FieldDesc: TFieldDesc);

    property Name: string read FName write FName;
    property ActualName: string read FActualName write FActualName ;
    property TableName: string read FTableName write FTableName;
    property DataType: word read FDataType write FDataType;
    property SubDataType: word read FSubDataType write FSubDataType;
    property Length: word read FLength write FLength;
    property Scale: word read FScale write FScale;
    property FieldNo: word read FFieldNo write FFieldNo;
    property ActualFieldNo: word read FActualFieldNo write FActualFieldNo; // for define
    property Size: word read FSize write FSize;
    property Offset: longint read FOffset write FOffset;
    property Required: boolean read FRequired write FRequired;
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property IsKey: boolean read FIsKey write FIsKey;
    property Fixed: boolean read FFixed write FFixed;
    property Hidden: boolean read FHidden write FHidden;
    property ObjectType: TObjectType read FObjectType write SetObjectType;
    property ParentField: TFieldDesc read FParentField write FParentField;
    property HiddenObject: boolean read FHiddenObject write FHiddenObject; // IncludeObject
    property Handle: pointer read FHandle write FHandle;
  end;

  TFieldDescs = class (TList)
  private
    function GetItems(Index: integer): TFieldDesc;

  public
    destructor Destroy; override;

    procedure Clear; override;

    function FindField(Name: string): TFieldDesc;
    function FieldByName(Name: string): TFieldDesc;

    property Items[Index: integer]: TFieldDesc read GetItems; default;
  end;

{ TSharedObject }

  TSharedObject = class
  protected
    FRefCount: integer;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Free;

    procedure CheckValid;

    procedure AddRef;
    procedure Release;

    property RefCount: integer read FRefCount;
  end;

{ TObjectType }

  TAttribute = class
  private
    FName: string;
    FDataType: word;
    FLength: word;
    FScale: word;
    FSize: word;       // size of got data
    FDataSize: word;   // size of stored data
    FOffset: word;     // stored offset
    FIndicatorOffset: word;  // indicator offset
    FAttributeNo: word;
    FObjectType: TObjectType;
    FOwner: TObjectType;
    FFixed: boolean;

    procedure SetObjectType(Value: TObjectType);

  public
    constructor Create;
    destructor Destroy; override;

    property Name: string read FName write FName;
    property DataType: word read FDataType write FDataType;
    property Fixed: boolean read FFixed write FFixed;
    property Length: word read FLength write FLength;
    property Scale: word read FScale write FScale;
    property Size: word read FSize write FSize;
    property DataSize: word read FDataSize write FDataSize;
    property Offset: word read FOffset write FOffset;
    property IndicatorOffset: word read FIndicatorOffset write FIndicatorOffset;
    property AttributeNo: word read FAttributeNo write FAttributeNo;
    property ObjectType: TObjectType read FObjectType write SetObjectType;
    property Owner: TObjectType read FOwner write FOwner;
  end;

  TObjectType = class (TSharedObject)
  private
    function GetAttributes(Index: integer): TAttribute;
    function GetAttributeCount: integer;

  protected
    FName: string;
    FDataType: word;
    FSize: integer;
    FAttributes: TList;

  protected
    procedure ClearAttributes;

  public
    constructor Create;
    destructor Destroy; override;

    function FindAttribute(Name: string): TAttribute;
    function AttributeByName(Name: string): TAttribute;

    property Name: string read FName;
    property DataType: word read FDataType;
    property Size: integer read FSize;
    property AttributeCount: integer read GetAttributeCount;

    property Attributes[Index: integer]: TAttribute read GetAttributes;
  end;

  TDBObject = class (TSharedObject)
  private
    FObjectType: TObjectType;

  protected
    procedure SetObjectType(Value: TObjectType);

    procedure GetAttributeValue(Name: string; Dest: pointer; var IsBlank: boolean); virtual;
    procedure SetAttributeValue(Name: string; Source: pointer); virtual;

  public
    constructor Create;

    property ObjectType: TObjectType read FObjectType;
  end;

  PCacheItem = ^TCacheItem;
  TCacheItem = record
    Item: PItemHeader;
    Next: PCacheItem;
  end;

  PRecBookmark = ^TRecBookmark;
  TRecBookmark = record
    RefreshIteration: longint;
    Item: PItemHeader;
    Order: longint
  end;

  TFilterFunc = function(RecBuf: pointer): boolean of object;

  TBlob = class;

{ TData }

  TUpdateRecKind = (ukUpdate, ukInsert, ukDelete);
  TOnModifyRecord = procedure of object;
  TOnApplyRecord = procedure (UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction) of object;

  TData = class
  private
    FRecordSize: longint;  // FDataSize + TIndicatorSize
    FCachedUpdates: boolean;
    FOnAppend: TOnModifyRecord;
    FOnDelete: TOnModifyRecord;
    FOnUpdate: TOnModifyRecord;
    FOnApplyRecord: TOnApplyRecord;
    FAutoInitFields: boolean; // initialization fields by InternalInitField
    FTrimFixedChar: boolean;

  { Filter }
    FFilterFunc: TFilterFunc;
    FFilterText: string;
    FFilterCaseInsensitive: boolean;
    FFilterNoPartialCompare: boolean;
    FFilterItemTypes: TItemTypes;

    Evaluator: TEvaluator;
    FilterRecBuf: pointer;
    FFiltered: boolean;

  /// if False then PutField set Null for string fields with empty value ('')
    FEnableEmptyStrings: boolean;
    FIsComplexFields: boolean;

    procedure CreateFilterExpression(const Text: string);
    procedure FreeFilterExpression;

    function GetFieldCount: word;
    procedure SetFilterText(Value: string);
    procedure SetCachedUpdates(Value: boolean);

  protected
    FRecordNoOffset: integer;

    FRecordCount: longint;
    FBOF: boolean;
    FEOF: boolean;
    DataSize: longint; // size of data

    FFields: TFieldDescs;

    StringHeap: TStringHeap;

  { Open/Close }
    procedure InternalPrepare; virtual;
    procedure InternalUnPrepare; virtual;
    procedure InternalOpen; virtual;
    procedure InternalClose; virtual;

  { Data }
    procedure InitData; virtual;
    procedure FreeData; virtual;

  { Fields }
    procedure InternalInitFields; virtual;
    procedure InitObjectFields(ObjectType: TObjectType; Parent: TFieldDesc);

    function GetIndicatorSize: word; virtual;

    procedure GetChildFieldInfo(Field: TFieldDesc; var RootField:TFieldDesc; var AttrName: string);
    procedure GetChildField(Field: TFieldDesc; RecBuf: pointer; Dest: pointer; var IsBlank: boolean);
    procedure PutChildField(Field: TFieldDesc; RecBuf: pointer; Source: pointer);

    function NeedConvertEOL: boolean; virtual;

  { Records }

  { Navigation }
    function GetEOF: boolean; virtual;
    function GetBOF: boolean; virtual;

    function GetRecordCount: longint; virtual;
    function GetRecordNo: longint; virtual;
    procedure SetRecordNo(Value: longint); virtual;

  { Edit }
    procedure InternalAppend(RecBuf: pointer); virtual;
    procedure InternalDelete; virtual;
    procedure InternalUpdate(RecBuf: pointer); virtual;

    property IndicatorSize: word read GetIndicatorSize;

  { Filter }
    function Filtered: boolean;

  { CachedUpdates }
    function GetUpdatesPending: boolean; virtual;
    procedure SetFilterItemTypes(Value: TItemTypes); virtual;
  public
    Active: boolean;
    Prepared: boolean;
    NewCacheRecBuf: pointer;
    OldCacheRecBuf: pointer;

    property FieldCount: word read GetFieldCount;
    property Fields: TFieldDescs read FFields;
    property Bof: boolean read GetBOF; // EOF: for CB case sensivity
    property Eof: boolean read GetEOF;

    constructor Create;
    destructor Destroy; override;

  { Open/Close }
    procedure Open; virtual;
    procedure Close; virtual;

    procedure Prepare; virtual;
    procedure UnPrepare; virtual;

    procedure Reopen; virtual;

  { Fields }
    procedure InitFields; virtual;
    procedure ClearFields;
    procedure GetField(FieldNo: word; RecBuf: pointer; Dest: pointer; var IsBlank: boolean);
    procedure GetFieldData(Field: TFieldDesc; RecBuf: pointer; Dest: pointer); virtual;
    procedure PutField(FieldNo: word; RecBuf: pointer; Source: pointer);
    procedure PutFieldData(Field: TFieldDesc; RecBuf: pointer; Source: pointer); virtual;
    function GetNull(FieldNo: word; RecBuf: pointer): boolean; virtual;
    procedure SetNull(FieldNo: word; RecBuf: pointer; Value: boolean); virtual;

    procedure GetFieldAsVariant(FieldNo: word; RecBuf: pointer; var Value: variant); virtual;
    procedure PutFieldAsVariant(FieldNo: word; RecBuf: pointer; const Value: variant); virtual;

    procedure GetDateFromBuf(Buf: pointer; Date: pointer; Format: TDateFormat); virtual;
    procedure PutDateToBuf(Buf: pointer; Date: pointer; Format: TDateFormat); virtual;

    function FindField(Name: string): TFieldDesc;
    function FieldByName(Name: string): TFieldDesc;

    function BlobFieldTypes: TFieldTypeSet; virtual; // TBlob descendants - dtBlob, dtMemo etc
    function ComplexFieldTypes: TFieldTypeSet; virtual; // All supported complex field types (BlobFieldTypes, ExtFieldTypes and TSharedObject descendants (not BLOB))

    function IsFields(FieldTypes: TFieldTypeSet): boolean;
    function IsBlobFields: boolean;
    function CheckIsComplexFields: boolean;

  { Records }
    function AllocRecBuf(var RecBuf: pointer): pointer;
    procedure FreeRecBuf(RecBuf: pointer);

    procedure InitRecord(RecBuf: pointer);
    //procedure FreeRecord(RecBuf: pointer);
    procedure GetRecord(RecBuf: pointer); virtual; abstract;
    procedure GetNextRecord(RecBuf: pointer); virtual; abstract;
    procedure GetPriorRecord(RecBuf: pointer); virtual; abstract;
    procedure PutRecord(RecBuf: pointer); virtual; abstract;
    procedure AppendRecord(RecBuf: pointer); virtual; abstract;
    procedure AppendBlankRecord;
    procedure InsertRecord(RecBuf: pointer); virtual; abstract;
    procedure UpdateRecord(RecBuf: pointer); virtual; abstract; // Modify
    procedure DeleteRecord; virtual; abstract;

    procedure EditRecord(RecBuf: pointer);
    procedure PostRecord(RecBuf: pointer);
    procedure CancelRecord(RecBuf: pointer); virtual;

    procedure CreateComplexFields(RecBuf: pointer; WithBlob: boolean); virtual;
    procedure FreeComplexFields(RecBuf: pointer; WithBlob: boolean); virtual;
    procedure CopyComplexFields(Source: pointer; Dest: pointer; WithBlob: boolean); virtual;  // copy content ComplexFields
    procedure AddRefComplexFields(RecBuf: pointer); virtual;
    procedure Sort(const FieldNames: string); virtual; abstract;

  { Navigation }
    procedure SetToBegin; virtual;
    procedure SetToEnd; virtual;

  { BookMarks }
    procedure GetBookmark(Bookmark: PRecBookmark); virtual;
    procedure SetToBookmark(Bookmark: PRecBookmark); virtual;
    function BookmarkValid(Bookmark: PRecBookmark): boolean; virtual;
    function CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer; virtual;

  { CachedUpdates }
    function GetUpdateStatus: TItemStatus; virtual;
    function GetUpdateResult: TUpdateRecAction; virtual;

    procedure SetCacheRecBuf(NewBuf: pointer; OldBuf: pointer); virtual;
    procedure ApplyUpdates; virtual;
    procedure CommitUpdates; virtual;
    procedure CancelUpdates; virtual;
    procedure RestoreUpdates; virtual;
    procedure RevertRecord; virtual;

    procedure ApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction); virtual;

    procedure GetOldRecord(RecBuf: pointer); virtual; // get rollback data

  { Filter }
    procedure FilterUpdated; virtual;

  { Blobs }
    function GetBlob(FieldNo: word; RecBuf: pointer): TBlob;
    function ReadBlob(FieldNo: word; RecBuf: pointer; Position: longint;
      Count: longint; Dest: pointer; FromRollback: boolean = false): longint;
    procedure WriteBlob(FieldNo: word; RecBuf: pointer; Position: longint;
      Count: longint; Source: pointer);
    procedure TruncateBlob(FieldNo: word; RecBuf: pointer; Size: longint);
    function GetBlobSize(FieldNo: word; RecBuf: pointer; FromRollback: boolean = false): longint;

    property RecordSize: longint read FRecordSize;
    property RecordCount: longint read GetRecordCount;//FRecordCount;
    property RecordNo: longint read GetRecordNo write SetRecordNo;
    property CachedUpdates: boolean read FCachedUpdates write SetCachedUpdates default False;
    property UpdatesPending: boolean read GetUpdatesPending;
    property FilterFunc: TFilterFunc read FFilterFunc write FFilterFunc;
    property FilterText: string read FFilterText write SetFilterText;
    property FilterCaseInsensitive: boolean read FFilterCaseInsensitive write FFilterCaseInsensitive;
    property FilterNoPartialCompare: boolean read FFilterNoPartialCompare write FFilterNoPartialCompare;
    property FilterItemTypes: TItemTypes read FFilterItemTypes write SetFilterItemTypes;
    property AutoInitFields: boolean read FAutoInitFields write FAutoInitFields;
    property TrimFixedChar: boolean read FTrimFixedChar write FTrimFixedChar;

  /// if False then PutField set Null for string fields with empty value ('')
    property EnableEmptyStrings: boolean read FEnableEmptyStrings write FEnableEmptyStrings;

    property OnAppend: TOnModifyRecord write FOnAppend;
    property OnDelete: TOnModifyRecord write FOnDelete;
    property OnUpdate: TOnModifyRecord write FOnUpdate;
    property OnApplyRecord: TOnApplyRecord write FOnApplyRecord;
    property IsComplexFields: boolean read FIsComplexFields write FIsComplexFields;
  end;

  TReorderOption = (roInsert,roDelete,roFull);

  TMemData = class (TData)
  private
    Cache: PCacheItem;
    LastCacheItem: PCacheItem;
    FRefreshIteration: longint;

  protected
    FirstItem: PItemHeader;
    LastItem: PItemHeader;
    CurrentItem: PItemHeader;
    NewItem: PItemHeader;

    BlockMan: TBlockManager;

  { Items/Data }
    function InsertItem: PItemHeader;
    function AppendItem: PItemHeader;
    procedure DeleteItem(Item: PItemHeader);
    procedure RevertItem(Item: PItemHeader);

    procedure InitData; override;
    procedure FreeData; override;

    procedure ReorderItems(Item: PItemHeader; ReorderOption: TReorderOption);

  { Navigation }
    function GetEOF: boolean; override;
    function GetBOF: boolean; override;

    function GetRecordCount: longint; override;
    function GetRecordNo: longint; override;
    procedure SetRecordNo(Value: longint); override;

  { Edit }
    procedure AddCacheItem(CacheItem: PCacheItem);

  { CachedUpdates }
    function GetUpdatesPending: boolean; override;
    procedure SetFilterItemTypes(Value: TItemTypes); override;

  public
    constructor Create;
    destructor Destroy; override;

  { Fields }
    procedure InitFields; override;

  { Records }
    procedure GetRecord(RecBuf: pointer); override;
    procedure GetNextRecord(RecBuf: pointer); override;
    procedure GetPriorRecord(RecBuf: pointer); override;
    procedure PutRecord(RecBuf: pointer); override;
    procedure AppendRecord(RecBuf: pointer); override;
    procedure InsertRecord(RecBuf: pointer); override;
    procedure UpdateRecord(RecBuf: pointer); override;
    procedure DeleteRecord; override;
    procedure RemoveRecord;  // remove record from memory

    function OmitRecord(Item: PItemHeader): boolean;
    procedure Sort(const FieldName: string); override;

  { Navigation }
    procedure SetToBegin; override;
    procedure SetToEnd; override;

  { BookMarks }
    procedure GetBookmark(Bookmark: PRecBookmark); override;
    procedure SetToBookmark(Bookmark: PRecBookmark); override;
    function BookmarkValid(Bookmark: PRecBookmark): boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer; override;

  { CachedUpdates }
    function GetUpdateStatus: TItemStatus; override;
    function GetUpdateResult: TUpdateRecAction; override;

    procedure SetCacheRecBuf(NewBuf: pointer; OldBuf: pointer); override;
    procedure ApplyUpdates; override;
    procedure CommitUpdates; override;
    procedure CancelUpdates; override;
    procedure RestoreUpdates; override;
    procedure RevertRecord; override;

    procedure GetOldRecord(RecBuf: pointer); override;
    
  { Filter }
    procedure FilterUpdated; override;

  end;

{ TBlob }

  PPieceHeader = ^TPieceHeader;
  TPieceHeader = packed record
    Blob: TBlob;
    Size: cardinal;
    Used: cardinal;
    Prev: PPieceHeader;
    Next: PPieceHeader;
    Test: word;
  end;

  TBlob = class (TSharedObject)
  private
    FFirstPiece: PPieceHeader;
    FNeedRollback: boolean;
    Rollback: TBlob;

    function GetAsString: string;
    procedure SetAsString(Value: string);

    function GetAsWideString: WideString;
    procedure SetAsWideString(Value: WideString);

    procedure AddCR_Unicode;
    procedure RemoveCR_Unicode;
    procedure AddCR_String;
    procedure RemoveCR_String;

  protected
    FIsUnicode: boolean;

    procedure CheckValid;   // DEBUG
    procedure CheckCached;

    procedure CheckValue; virtual;

    procedure SaveToRollback;

    function GetSize: cardinal; virtual;
    procedure SetIsUnicode(Value: boolean);

  public
    PieceSize: cardinal;
    Test: byte;   // DEBUG

    constructor Create(IsUnicode: boolean = False);
    destructor Destroy; override;

  { Pieces }
    procedure AllocPiece(var Piece: PPieceHeader; Size: cardinal);
    procedure ReallocPiece(var Piece: PPieceHeader; Size: cardinal);
    procedure FreePiece(Piece: PPieceHeader);
    procedure AppendPiece(Piece: PPieceHeader);
    procedure DeletePiece(Piece: PPieceHeader);
    procedure CompressPiece(var Piece: PPieceHeader);

    function FirstPiece: PPieceHeader;

    function Read(Position: cardinal; Count: cardinal; Dest: pointer): cardinal; virtual;
    procedure Write(Position: cardinal; Count: cardinal; Source: pointer); virtual;
    procedure Clear;
    procedure Truncate(NewSize: cardinal);
    procedure Compress;
    procedure Defrag; // Move all data to first piece
    procedure AddCR;
    procedure RemoveCR;

  { Stream/File }

    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream); virtual;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    procedure Assign(Source: TBlob);

  { Rollback }
    procedure EnableRollback;
    procedure Commit; virtual;
    procedure Cancel; virtual;
    function CanRollback: boolean;

    property Size: cardinal read GetSize;
    property AsString: string read GetAsString write SetAsString;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    property IsUnicode: boolean read FIsUnicode write SetIsUnicode;
  end;

{ TVariantObject }

  TVariantObject = class (TSharedObject)
  private
    FValue: Variant;

  public
    property Value: Variant read FValue write FValue;
  end;

  function NextPiece(Piece: PPieceHeader): PPieceHeader;
  function PieceData(Piece: PPieceHeader): pointer;
  function PieceUsedPtr(Piece: PPieceHeader): pointer;

  procedure DataError(Msg: string);

{ PWideChar routines }

  function StrLenW(const Str: PWideChar): Cardinal;
  function StrCopyW(Dest: PWideChar; const Source: PWideChar): PWideChar;
  procedure StrLCopyW(Dest: PWideChar; const Source: PWideChar; MaxLen{WideChars}: Cardinal);
  procedure StrTrim(const Str: PChar; Len: integer = -1);
  procedure StrTrimW(const Str: PWideChar; Len: integer = -1);

const
  MaxArrayItem: integer = 100; // Max count of fields from array type

{$IFDEF DEBUG}
  ShareObjectCnt: integer = 0;
{$ENDIF}

  varDecimal  = $000E;
  varLongWord = $0013;
{$IFNDEF VER6P}

type
  TVarDataD6 = packed record // TVarData from Delphi 6
    VType: word;
    case Integer of
      0: (Reserved1: Word;
        case Integer of
          0: (Reserved2, Reserved3: Word;
            case Integer of
              varLongWord: (VLongWord: LongWord);
              varDecimal: (VInt64: Int64);
          );
      );
  end;

{$ENDIF}

function AddCR(Source, Dest: PChar; Count: integer): integer; overload;
function RemoveCR(Source, Dest: PChar; DestLen, Count: integer): integer; overload;

function AddCR(Source, Dest: PWideChar; Count: integer): integer; overload;
function RemoveCR(Source, Dest: PWideChar; DestLen, Count: integer): integer; overload;

const
  uaDefault = 10; // TUpdateAction

type
  TUpdateRecordTypes = set of (rtModified, rtInserted, rtDeleted, rtUnmodified);
  TUpdateErrorEvent = procedure(DataSet: TDataSet; E: EDatabaseError;
    UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction) of object;
  TUpdateRecordEvent = procedure(DataSet: TDataSet; UpdateKind: TUpdateKind;
    var UpdateAction: TUpdateAction) of object;

  PRecInfo = ^TRecInfo;
  TRecInfo = record
    RecordNumber: longint;
    UpdateStatus: TUpdateStatus;
    BookmarkFlag: TBookmarkFlag;
    RefComplexFields: boolean;
  end;

//  TBlobData = string;

  TLocateExOption = (lxCaseInsensitive,lxPartialKey,lxNearest,lxNext,lxUp{,lxCharCompare});
  TLocateExOptions = set of TLocateExOption;

  TDbMemDataSetBase = class(TDataSet)
  private
    FOldRecBuf: PChar;
    FFilterBuffer: PChar;
    FCachedUpdates: boolean;
    FLocalUpdate: boolean;
    //FInDeferredPost: boolean; to protected
    FInInserting: boolean;
    FInEditing: boolean;

    FOnUpdateError: TUpdateErrorEvent;
    FOnUpdateRecord: TUpdateRecordEvent;
    FIndexFieldNames: string;

    function GetOldRecord: PChar;

    {function GetBlobData(Field:TField; Buffer: PChar):TBlobData;
    procedure SetBlobData(Field:TField; Buffer: PChar; Value:TBlobData);
    procedure ClearBlobCache(Buffer: PChar);}

    procedure SetCachedUpdates(Value: boolean);
    function GetUpdatesPending: boolean;
    function GetPrepared: boolean;
    procedure SetPrepared(Value: boolean);
    function GetUpdateRecordSet: TUpdateRecordTypes;
    procedure SetUpdateRecordSet(Value: TUpdateRecordTypes);
    procedure SetIndexFieldNames(const Value: string);

  protected
    Data: TData;  // FIRecordSet

    FBookmarkOfs: longint;
    FRecInfoOfs: longint;
    FRecBufSize: longint;
    FInCacheProcessing: boolean;
    FInDeferredPost: boolean;  // private
    NewCacheRecBuf: PChar;
    OldCacheRecBuf: PChar;
    FParentDataSet: TDbMemDataSetBase;
    FLocalConstraints: boolean;
    FNumberRange: boolean;
    FNeedAddRef: boolean;

    procedure CreateIRecordSet; virtual;
    procedure FreeIRecordSet;
    procedure SetIRecordSet(Value: TData{TRecordSet}); virtual;

  { Open/Close DataSet }
    procedure OpenCursor(InfoQuery: boolean); override;
    procedure CloseCursor; override;

    procedure InternalOpen; override;
    procedure InternalClose; override;
    function IsCursorOpen: boolean; override;
    procedure InternalRefresh; override;

  { Field Management }
    procedure InternalInitFieldDefs; override;
    procedure CreateFieldDefs; virtual;
    procedure ClearCalcFields(Buffer: PChar); override;

    procedure GetObjectTypeNames(Fields: TFields);
    function GetFieldType(DataType: word): TFieldType; overload; virtual;
    function GetFieldType(FieldDesc: TFieldDesc): TFieldType; overload; virtual;
    procedure SetFieldData(Field: TField; Buffer: pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean); override;

    procedure SetNumberRange(FieldDef: TFieldDef); virtual;

  { Buffer/Record Management }
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;

    procedure InitRecord(Buffer: PChar); override;
    procedure InternalInitRecord(Buffer: PChar); override;

    function GetActiveRecBuf(var RecBuf: PChar): boolean;
    function GetRecord(Buffer: PChar; GetMode:TGetMode; DoCheck: boolean): TGetResult; override;

    procedure BlockReadNext; override;
    procedure SetBlockReadSize(Value: integer); override;
    procedure FreeRefBuffers;
    procedure FreeRefComplexFields(Buffer: PChar);

  { Bookmarks }
    procedure GetBookmarkData(Buffer: PChar; Data: pointer); override;
    procedure SetBookmarkData(Buffer: PChar; Data: pointer); override;
    function GetBookmarkFlag(Buffer: PChar):TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: PChar; Value:TBookmarkFlag); override;
    function GetBookmarkStr: TBookmarkStr; override;

  { Navigation }
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalGotoBookmark(Bookmark: pointer); override;
    procedure InternalSetToRecord(Buffer: PChar); override;

  { Editing }
    procedure InternalAddRecord(Buffer: pointer; Append: boolean); override;
    procedure InternalInsert; override;
    procedure InternalDelete; override;
    procedure InternalEdit; override;

    procedure InternalPost; override;
    procedure InternalCancel; override;

    procedure InternalDeferredPost; virtual;

    function PerformAppend: boolean; virtual;
    function PerformDelete: boolean; virtual;
    function PerformUpdate: boolean; virtual;

    procedure DoPerformAppend;
    procedure DoPerformDelete;
    procedure DoPerformUpdate;

    procedure DoOnNewRecord; override;

  { Filter/Find/Locate }
    procedure ActivateFilters;
    procedure DeactivateFilters;
    function RecordFilter(RecBuf: pointer): boolean;
    procedure SetFilterData(const Text: string; Options:TFilterOptions);

    procedure SetFiltered(Value: boolean); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;

    procedure CopyFieldValue(const Value: variant; out ValuePtr: pointer; out ValueType: integer; FieldDesc: TFieldDesc); virtual;
    function CompareStrValues(const Value: PChar; const FieldValue: PChar; const Options: TLocateExOptions): boolean;
    function CompareWideStrValues(const Value: PWideChar; const FieldValue: PWideChar; const Options: TLocateExOptions): boolean;
    function CompareBinValues(const Value: PChar; const ValueLen: integer; const FieldValue: PChar; const FieldValueLen: integer; const Options: TLocateExOptions): boolean;
    function CompareFieldValue(ValuePtr: pointer; const ValueType: integer; FieldDesc: TFieldDesc; RecBuf: PChar; const Options: TLocateExOptions): boolean; virtual;
    function LocateRecord(const KeyFields: string; const KeyValues: variant;
      Options: TLocateExOptions; SavePos: boolean): boolean;
    function FindRecord(Restart, GoForward: boolean): boolean; override;

  { CachedUpdates }
    procedure CheckCachedUpdateMode;

    procedure DoApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction);

  { Blobs }
    procedure CloseBlob(Field: TField); override;

  { Misc }
    function GetRecordCount: integer; override;
    function GetRecordSize: word; override;

    function GetRecNo: integer; override;
    procedure SetRecNo(Value: integer); override;

    procedure InternalHandleException; override;

    procedure AssignTo(Dest: TPersistent); override;

    procedure DataEvent(Event:TDataEvent; Info: longint); override;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure Prepare; virtual;
    procedure UnPrepare; virtual;
    procedure CheckPrepared;

  { Fields }
    function GetFieldData(Field: TField; Buffer: pointer): boolean; overload; override;
    function Translate(Src,Dest: PChar; ToOem: boolean): integer; override;

    function GetFieldData(FieldNo: integer; Buffer: pointer): boolean; overload; override;
    function GetFieldData(Field: TField; Buffer: Pointer;
      NativeFormat: Boolean): Boolean; override;

  { Edit }
    procedure Cancel; override;
    procedure DeferredPost;

  { Bookmarks }
    function BookmarkValid(Bookmark: DB.TBookmark): boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: DB.TBookmark): integer; override;

    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    function Locate(const KeyFields: string; const KeyValues: variant;
      Options: TLocateOptions): boolean; override;
    function LocateEx(const KeyFields: string; const KeyValues: variant;
      Options: TLocateExOptions): boolean;
    function Lookup(const KeyFields: string; const KeyValues: variant;
      const ResultFields: string): variant; override;

  { CachedUpdates }
    function UpdateStatus: TUpdateStatus; override;
    function UpdateResult: TUpdateAction;
    procedure ApplyUpdates;
    procedure CommitUpdates;
    procedure CancelUpdates;
    procedure RestoreUpdates;
    procedure RevertRecord;

    function IsSequenced: boolean; override;

    property Prepared: boolean read GetPrepared write SetPrepared;
    property CachedUpdates: boolean read FCachedUpdates write SetCachedUpdates default False;
    property UpdatesPending: boolean read GetUpdatesPending;
    property LocalUpdate: boolean read FLocalUpdate write FLocalUpdate default False;
    property UpdateRecordTypes: TUpdateRecordTypes read GetUpdateRecordSet write SetUpdateRecordSet;
    property IndexFieldNames: string read FIndexFieldNames write SetIndexFieldNames;

  // obsolete
    property LocalConstraints: boolean read FLocalConstraints write FLocalConstraints default True;

    property OnUpdateError: TUpdateErrorEvent read FOnUpdateError write FOnUpdateError;
    property OnUpdateRecord: TUpdateRecordEvent read FOnUpdateRecord write FOnUpdateRecord;
  end;

{ TBlobStream }

  TBlobStream = class(TStream)
  protected
    FField: TBlobField;
    FDataSet: TDbMemDataSetBase;
    FBuffer: PChar;
    FMode: TBlobStreamMode;
    FFieldNo: integer;
    FOpened: boolean;
    FModified: boolean;
    FPosition: longint;
    FBlobData: TBlobData;
//    FCached: Boolean;
//    FCacheSize: Longint;
    function GetBlobSize: Longint;

  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    procedure Truncate;
  end;

  function GetFieldType(DataType: word): TFieldType;
  function GetDataType(FieldType: TFieldType): word;

type
  TDbMemDatasetOption = (voPersistentData, voStored);
  TDbMemDatasetOptions = set of TDbMemDatasetOption;

  TDbMemDataSet = class(TDbMemDataSetBase)
  private
    FOptions: TDbMemDatasetOptions;
    FStreamedActive: boolean;
    FAvoidRefreshData: boolean;
    FAvoidReload: boolean;

    procedure ReadBinaryData(Stream: TStream);
    procedure WriteBinaryData(Stream: TStream);

    function IsFieldDefsStored: boolean;
    function GetFieldDefs: TFieldDefs;
    procedure SetFieldDefs(Value: TFieldDefs);

  protected
    procedure CreateIRecordSet; override;

    procedure InternalClose; override;
    function IsCursorOpen: boolean; override;

    procedure CreateFieldDefs; override;
    procedure DefChanged(Sender: TObject); override;

    procedure DefineProperties(Filer: TFiler); override;

    procedure AssignDataSet(Source: TDataSet);

    procedure SetActive(Value:boolean); override;

  public
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;

    procedure Loaded; override;

    function IsSequenced: boolean; override;

    procedure AddField(Name: string; FieldType: TFieldType; Size: integer = 0; Required: boolean = False);
    procedure DeleteField(Name: string);
    procedure DeleteFields;

    procedure Clear;

  { Stream/File }
    procedure LoadFromStream(Stream: TStream; LoadFields: boolean = True);
    procedure SaveToStream(Stream: TStream; StoreFields: boolean = True);
    procedure LoadFromVariant(Data: Variant; LoadFields: Boolean = True);

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    procedure Assign(Source: TPersistent); override;

  published
    property Options: TDbMemDatasetOptions read FOptions write FOptions default [voPersistentData, voStored];
    property IndexFieldNames;

    property Active;
    property AutoCalcFields;
    property Filtered;
    property Filter;
    property FilterOptions;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;

    //property Fields stored False;
    property FieldDefs: TFieldDefs read GetFieldDefs write SetFieldDefs stored IsFieldDefsStored;
  end;

  procedure Register;

resourcestring
  SUnknownDataType       = 'Unknown data type.';
  SDataTypeNotSupported  = 'Data type is not supported.';
  SFieldNotFound         = 'Field %s not found.';
  SAttributeNotFount     = 'Attribute %s not found.';
  SCannotConvertType     = 'Cannot convert type.';
  SIllegalFilter         = 'Illegal filter expression.';
  SNeedBlobType          = 'Field is not BLOB.';
  SInvalidSharedObject   = 'Invalid SharedObject.';
  SInvalidBlob           = 'Invalid BLOB.';
  SBlobMustBeCached      = 'Blob must be in cached mode.';
  SCachedAlreadyEnabled  = 'Cached is already enabled.';

  SDataSetIsNotPrepared  = 'DataSet is not prepared.';
  SInvalidKeyField       = 'Error in KeyFields string.';
  SNotCachedUpdate       = 'Not in cached update mode.';

  SConnectionNotDefined   = 'Connection is not defined.';
  SCannotConnect          = 'Cannot connect to database.';
  SMacroNotFound          = 'Macro %s not found.';
  STransactionInProgress  = 'User transaction is already in progress.';
  SUpdateFailed           = 'Update failed. Found %d records.';
  SRefreshFailed          = 'Refresh failed. Found %d records.';
  SNoCorrespondParam      = 'Not found field corresponding parameter %s.';
  SUnknownParamDataType   = 'Unknown type of parameter %s.';
  SRecordChanged          = 'Record was changed by another user.';
  STableNameNotDefined    = 'TableName must be defined.';
  SStoredProcNotDefined   = 'StoredProcName must be defined.';
  SConnectionIsClosed     = 'Operation is not allowed when the connection is closed';

  SCannotChangeIsUnicode  = 'Cannot change IsUnicode if Size > 0';
  SColumnNotFound         = 'Column %s not found';

implementation

uses
  DBConsts, Math, Windows
{$IFNDEF VER130}
  , Variants
{$ENDIF}
  ;

var
  RefreshIteration: longint = 101;

procedure Register;
begin
  RegisterComponents('Database Extensions (Deprecated)', [TDbMemDataset]);
end;

procedure DataError(Msg: string);
begin
  raise Exception.Create(Msg);
end;

{ PWideChar routines }

function StrLenW(const Str: PWideChar): Cardinal; assembler;
asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        MOV     EAX,0FFFFFFFEH
        SUB     EAX,ECX
        MOV     EDI,EDX
end;

function StrCopyW(Dest: PWideChar; const Source: PWideChar): PWideChar;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     ECX,0FFFFFFFFH
        XOR     AX,AX
        REPNE   SCASW
        NOT     ECX
        SHL     ECX, 1  // Size := Len * sizeof(WideChar)
        MOV     EDI,ESI
        MOV     ESI,EDX
        MOV     EDX,ECX
        MOV     EAX,EDI
        SHR     ECX,2
        REP     MOVSD
        MOV     ECX,EDX
        AND     ECX,3
        REP     MOVSB
        POP     ESI
        POP     EDI
end;

procedure StrLCopyW(Dest: PWideChar; const Source: PWideChar; MaxLen{WideChars}: Cardinal);
var
  pwc: PWideChar;
begin
  pwc := Source;
  while (pwc^ <> #0) and (pwc < Source + MaxLen) do begin
    Dest^ := pwc^;
    Inc(Dest);
    Inc(pwc);
  end;
  Dest^ := #0;
end;

procedure StrTrim(const Str: PChar; Len: integer = -1);
var
  pc: PChar;
begin
  if Len = - 1 then // Detect length
    Len := StrLen(Str);

  pc := Str + Len - 1;

  while ((pc^ = ' ') or (pc^ = #0)) and (pc >= Str) do begin
    pc^ := #0;
    Dec(pc);
  end;
end;

procedure StrTrimW(const Str: PWideChar; Len: integer = -1);
var
  pwc: PWideChar;
begin
  if Len = - 1 then // Detect length
    Len := StrLenW(Str);

  pwc := Str + Len - 1;

  while ((pwc^ = ' ') or (pwc^ = #0)) and (pwc >= Str) do begin
    pwc^ := #0;
    Dec(pwc);
  end;
end;

{ TFieldDesc }

constructor TFieldDesc.Create;
begin
  inherited;
end;

destructor TFieldDesc.Destroy;
begin
  if FObjectType <> nil then
    FObjectType.Release;

  inherited;
end;

function TFieldDesc.HasParent: boolean;
begin
  Result := FParentField <> nil;
end;

procedure TFieldDesc.Assign(FieldDesc:TFieldDesc);
begin
  Name := FieldDesc.Name;
  ActualName := FieldDesc.ActualName;
  DataType := FieldDesc.DataType;
  Length := FieldDesc.Length;
  Scale := FieldDesc.Scale;
  Size := FieldDesc.Size;
  Offset := FieldDesc.Offset;
  Required := FieldDesc.Required;
  FieldNo := FieldDesc.FieldNo;
end;

procedure TFieldDesc.SetObjectType(Value:TObjectType);
begin
  if Value <> FObjectType then begin
    if FObjectType <> nil then
      FObjectType.Release;

    FObjectType := Value;

    if FObjectType <> nil then
      FObjectType.AddRef;
  end;
end;

{ TFieldDescs }

destructor TFieldDescs.Destroy;
begin
  Clear;

  inherited;
end;

procedure TFieldDescs.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i] <> nil then
      TFieldDesc(Items[i]).Free;

  inherited Clear;
end;

function TFieldDescs.FindField(Name: string):TFieldDesc;
var
  i: integer;
  ComplexField: boolean;
  Found: boolean;
begin
  Result := nil;
  ComplexField := (Pos('.', Name) > 0) or (Pos('[', Name) > 0);
  for i := 0 to Count - 1 do
    if (Items[i] <> nil) then begin
      if ComplexField then
        Found := AnsiCompareText(TFieldDesc(Items[i]).ActualName, Name) = 0
      else
        Found := AnsiCompareText(TFieldDesc(Items[i]).Name, Name) = 0;

      if Found then begin
        Result := Items[i];
        Exit;
      end;
    end;
end;

function TFieldDescs.FieldByName(Name: string): TFieldDesc;
begin
  Result := FindField(Name);

  if Result = nil then
    raise Exception.Create(Format(SFieldNotFound, [Name]));
end;

function TFieldDescs.GetItems(Index: integer): TFieldDesc;
begin
  Result := inherited Items[Index];
end;

{ TAttribute }

constructor TAttribute.Create;
begin
  inherited;
end;

destructor TAttribute.Destroy;
begin
  if (FObjectType <> nil) and (FOwner.Name <> FObjectType.Name) then
    FObjectType.Release;

  inherited;
end;

procedure TAttribute.SetObjectType(Value:TObjectType);
begin
  if Value <> FObjectType then begin
    if FObjectType <> nil then
      FObjectType.Release;

    FObjectType := Value;

    if (FObjectType <> nil) and (FOwner.Name <> FObjectType.Name) then
      FObjectType.AddRef;
  end;
end;

{ TObjectType }

constructor TObjectType.Create;
begin
  inherited;

  FAttributes := TList.Create;
end;

destructor TObjectType.Destroy;
begin
  ClearAttributes;
  FAttributes.Free;

  inherited;
end;

{function TObjectType.AddAttribute:TAttribute;
begin
  Result := TAttribute.Create;
  FAttributes.Add(Result);
end;}

procedure TObjectType.ClearAttributes;
var
  i: integer;
begin
  for i := 0 to FAttributes.Count - 1 do
    TAttribute(FAttributes[i]).Free;
  FAttributes.Clear;
end;

function TObjectType.FindAttribute(Name: string):TAttribute;
var
  St: string;
  iPos,IndexPos: integer;
  i: integer;
  OType:TObjectType;
begin
  Name := AnsiUpperCase(Name);
  OType := Self;

  repeat
    Name := TrimLeft(Name);

    iPos := Pos('.', Name);
    IndexPos := Pos('[', Name);
    if IndexPos = 1 then begin
      i := Pos(']', Name);
      if i = 0 then begin
        Result := nil;
        Exit;
      end;
      if Name[i + 1] = '.' then
        Inc(i);

      St := 'ELEMENT';
      Name := Copy(Name, i + 1, Length(Name));
    end
    else
      if (iPos > 0) and ((iPos < IndexPos) or (IndexPos = 0)) then begin
        St := Copy(Name, 1, iPos - 1);
        Name := Copy(Name, iPos + 1, Length(Name));
      end
      else
        if IndexPos > 0 then begin
          St := Copy(Name, 1, IndexPos - 1);
          Name := Copy(Name, IndexPos, Length(Name));
        end
        else
          St := Name;

    Result := nil;
    for i := 0 to OType.AttributeCount - 1 do
      if AnsiUpperCase(TAttribute(OType.Attributes[i]).Name) = St then begin
        Result := OType.Attributes[i];
        break;
      end;

    if (Result = nil) or not(Result.DataType in [dtObject,dtArray,dtTable,dtReference]) and
      (iPos <> 0)
    then begin
      Result := nil;
      Exit;
    end;

    OType := Result.ObjectType;
  until (iPos = 0) and ((IndexPos = 0) or (Name = ''));
end;

function TObjectType.AttributeByName(Name: string):TAttribute;
begin
  Result := FindAttribute(Name);
  if Result = nil then
    raise Exception.Create(Format(SAttributeNotFount, [Name]));
end;

function TObjectType.GetAttributes(Index: integer):TAttribute;
begin
  Result := TAttribute(FAttributes[Index]);
end;

function TObjectType.GetAttributeCount: integer;
begin
  Result := FAttributes.Count;
end;

{ TDBObject }

constructor TDBObject.Create;
begin
  inherited;
end;

procedure TDBObject.SetObjectType(Value:TObjectType);
begin
  if FObjectType <> nil then
    FObjectType.Release;

  FObjectType := Value;

  if FObjectType <> nil then
    FObjectType.AddRef;
end;

procedure TDBObject.GetAttributeValue(Name: string; Dest: pointer; var IsBlank: boolean);
begin
  IsBlank := True;
end;

procedure TDBObject.SetAttributeValue(Name: string; Source: pointer);
begin
end;

{ TData }

{$IFDEF DEBUG}
const
  DataCnt: integer = 0;
{$ENDIF}

constructor TData.Create;
begin
  FEOF := True;
  FBOF := True;
  FFields := TFieldDescs.Create;
  FAutoInitFields := True;
  FEnableEmptyStrings := False;

{$IFDEF DEBUG} Inc(DataCnt); {$ENDIF}
  StringHeap := TStringHeap.Create;

  Evaluator := TEvaluator.Create('', nil, TExpressionContext);
end;


destructor TData.Destroy;
begin
  Close;

  ClearFields;
  FFields.Free;
  StringHeap.Free;
  FreeAndNil(Evaluator);
  inherited;

{$IFDEF DEBUG} Dec(DataCnt); {$ENDIF}
end;

{ Data }

procedure TData.InitData;
begin
  FBOF := True;
  FEOF := True;
  FRecordCount := 0;
  FRecordNoOffset := 0;
end;

procedure TData.FreeData;
begin
  InitData;
end;

{ Open / Close }

procedure TData.InternalPrepare;
begin
end;

procedure TData.Prepare;
begin
  InternalPrepare;
  Prepared := True; // lost connection
end;

procedure TData.InternalUnPrepare;
begin
end;

procedure TData.UnPrepare;
begin
  if Prepared then begin
    Prepared := False;
    if FAutoInitFields then
      ClearFields;
    InternalUnPrepare;
  end;
end;

procedure TData.InternalOpen;
begin
end;

procedure TData.Open;
begin
  if not Active then begin
    InitData;
    try
      InternalOpen;
      CreateFilterExpression(FFilterText); // ???
    except
      FreeData;
      FreeFilterExpression;
      raise;
    end;

    Active := True;
  end;
end;

procedure TData.InternalClose;
begin
end;

procedure TData.Close;
begin
  try
    if Active then
      InternalClose;
  finally
    Active := False;
    FreeData;         // FreeData after for multithreads

    if FAutoInitFields and not Prepared then // After FreeData!
      ClearFields;

    FreeFilterExpression;
  end;
end;

procedure TData.Reopen;
begin
  Close;
  Open;
end;

{ Field }

function TData.GetFieldCount: word;
begin
  Result := FFields.Count;
end;

function TData.GetIndicatorSize: word;
begin
  Result := FieldCount;
end;

procedure TData.InternalInitFields;
begin
end;

procedure TData.InitObjectFields(ObjectType:TObjectType; Parent:TFieldDesc);
var
  i: integer;
  Field:TFieldDesc;
  Item,CountItem: integer;
begin
  for i := 0 to ObjectType.AttributeCount - 1 do begin
    if ObjectType.DataType in [dtObject,dtTable] then
      CountItem := 1
    else begin
      CountItem := ObjectType.Size;
      if CountItem > MaxArrayItem then  // Restriction of array length
        CountItem := MaxArrayItem;
    end;

    for Item := 0 to CountItem - 1 do begin
      Field := TFieldDesc.Create;
      Field.ParentField := Parent;
      if ObjectType.DataType in [dtObject,dtTable] then begin
        Field.Name := ObjectType.Attributes[i].Name;
        if Parent = nil then
          Field.ActualName := Field.Name
        else
          Field.ActualName := Parent.ActualName + '.' + Field.Name;
      end
      else begin
        Field.Name := '[' + IntToStr(Item) + ']';
        if Parent = nil then
          Field.ActualName := Field.Name
        else
          Field.ActualName := Parent.ActualName + Field.Name;
      end;

      Field.DataType := ObjectType.Attributes[i].DataType;
      Field.Size := 0;// ObjectType.Attributes[i].Size;
      Field.Fixed := ObjectType.Attributes[i].Fixed;
      Field.Length := ObjectType.Attributes[i].Length;
      Field.FieldNo := FFields.Count + 1;
      Field.ObjectType := ObjectType.Attributes[i].ObjectType;
      FFields.Add(Field);

      if Field.DataType in [dtObject,dtArray] then
        InitObjectFields(Field.ObjectType, Field);
    end;
  end;
end;

function CompareAlias(Field1, Field2: Pointer): integer;
begin
  if Field1 = Field2 then
    result := 0
  else begin
    Result := AnsiCompareText(TFieldDesc(Field1).Name, TFieldDesc(Field2).Name);
    if Result = 0 then begin
      Result := TFieldDesc(Field1).FieldNo - TFieldDesc(Field2).FieldNo;
      TFieldDesc(Field1).FReserved := True;
      TFieldDesc(Field2).FReserved := True;
    end;
  end
end;

procedure TData.InitFields;
var
  i: integer;

  // perfomance optimization for many fields set aliases
  procedure InitAliases;
  var
    AliasNum, AliasLen: integer;
    AFields: TList;
    i: integer;
    s: string;
  begin
    AFields := TList.Create;
    try
      AFields.Capacity := FFields.Capacity;
      for i := 0 to FFields.Count - 1 do
        if (FFields[i] <> nil) and (TFieldDesc(FFields[i]).ParentField = nil) then
          AFields.Add(FFields[i]);

      AFields.Sort(CompareAlias);
      AliasNum := 0;
      for i := 0 to AFields.Count - 1 do
        if TFieldDesc(AFields[i]).FReserved then begin
          if (AliasNum > 1) then begin
            s := TFieldDesc(AFields[i-1]).Name;
            AliasLen := 1 {'_'} + Length(IntToStr((AliasNum - 1)));
            SetLength(s, Length(s) - AliasLen);
            if (AnsiCompareText(s, TFieldDesc(AFields[i]).Name) <> 0) then
              AliasNum := 0;
          end;
          if AliasNum <> 0 then
            TFieldDesc(AFields[i]).Name := TFieldDesc(AFields[i]).Name + '_' + IntToStr(AliasNum);
          Inc(AliasNum);
        end else
          AliasNum := 0;
    finally
      AFields.Free;
    end;
  end;

var
  Off, AlignOff: integer;

begin
  if FAutoInitFields then begin
    ClearFields;
    InternalInitFields;
    InitAliases;
  end;

  DataSize := 0;
  for i := 0 to FieldCount - 1 do begin
    Fields[i].Offset := DataSize;
    if Fields[i].DataType = dtWideString then begin
      Off := Fields[i].Offset;
      AlignOff := Off and 1; // Fields[i].Offset mod 2;
      Fields[i].Offset := Off + AlignOff; // align WideString field offset
    end
    else
      AlignOff := 0;
    DataSize := DataSize + Fields[i].Size + AlignOff;
  end;

  FRecordSize := DataSize + IndicatorSize;
  FRecordSize := FRecordSize + (FRecordSize + 1) mod 2; //align
  CheckIsComplexFields;
end;

procedure TData.ClearFields;
begin
  FFields.Clear;
end;

procedure TData.GetDateFromBuf(Buf: pointer; Date: pointer; Format: TDateFormat);
begin
  case Format of
    dfMSecs:
      TDateTime(Date^) := TimeStampToMSecs(DateTimeToTimeStamp(TDateTime(Buf^)));
    dfDateTime:
      TDateTime(Date^) := TDateTime(Buf^);
    dfDate: 
      Longint(Date^) := DateTimeToTimeStamp(TDateTime(Buf^)).Date; // see TDateTimeRec.Date (DB, DB.NativeToDateTime)
    dfTime: 
      Longint(Date^) := DateTimeToTimeStamp(TDateTime(Buf^)).Time; // see TDateTimeRec.Time (DB, DB.NativeToDateTime)
  end;
end;

procedure TData.PutDateToBuf(Buf: pointer; Date: pointer; Format: TDateFormat);
var
  ts: TTimeStamp;
begin
  case Format of
    dfMSecs:
      TDateTime(Buf^) := TimeStampToDateTime(MSecsToTimeStamp(TDateTime(Date^)));
    dfDateTime:
      TDateTime(Buf^) := TDateTime(Date^);

    dfDate: begin
      ts.Date := Longint(Date^); // see TDateTimeRec.Date (DB, DB.DateTimeToNative)
      ts.Time := 0;
      TDateTime(Buf^) := TimeStampToDateTime(ts);
    end;
    dfTime: begin
      ts.Date := DateDelta;
      ts.Time := Longint(Date^); // see TDateTimeRec.Time (DB, DB.DateTimeToNative)
      TDateTime(Buf^) := TimeStampToDateTime(ts);
    end;
  end;
end;

procedure TData.GetChildFieldInfo(Field: TFieldDesc; var RootField: TFieldDesc; var AttrName: string);
begin
  AttrName := '';
  repeat
    if AttrName = '' then
      AttrName := Field.Name
    else
      if Field.DataType = dtArray then
        AttrName := Field.Name + AttrName
      else
        AttrName := Field.Name + '.' + AttrName;
    Field := Field.ParentField;
  until not Field.HasParent;
  RootField := Field;
end;

procedure TData.GetChildField(Field: TFieldDesc; RecBuf: pointer; Dest: pointer; var IsBlank: boolean);
var
  DBObject: TDBObject;
  AttrName: string;
begin
  GetChildFieldInfo(Field, Field, AttrName);
  DBObject := TDBObject(Pointer(PChar(RecBuf) + Field.Offset)^);
  if DBObject <> nil then
    DBObject.GetAttributeValue(AttrName, Dest, IsBlank)
  else
    IsBlank := True;
end;

procedure TData.PutChildField(Field: TFieldDesc; RecBuf: pointer; Source: pointer);
var
  DBObject: TDBObject;
  AttrName: string;
begin
  GetChildFieldInfo(Field, Field, AttrName);
  DBObject := TDBObject(Pointer(PChar(RecBuf) + Field.Offset)^);
  if DBObject <> nil then
    DBObject.SetAttributeValue(AttrName, Source);
end;

const
  CRLF = $0A0D;
  LF   = $0A;
  CRLF_UTF16 = $000A000D;
  LF_UTF16   = $000A;

function AddCR(Source, Dest: PChar; Count: integer): integer;
var
  SourceEnd: PChar;
  w: word;
  b: byte;
begin
  Result := Count;
  SourceEnd := Source + Count;
  while Source < SourceEnd do begin
    w := Word(Pointer(Source)^);
    if w = CRLF then begin
      Word(Pointer(Dest)^) := w;
      Inc(Source, 2);
      Inc(Dest, 2);
    end
    else begin
      b := Lo(w);
      if b = 0 then begin
        Dec(Result, SourceEnd - Source);
        break;
      end
      else
      if b = LF then begin
        Word(Pointer(Dest)^) := CRLF;
        Inc(Source);
        Inc(Dest, 2);
        Inc(Result);
      end
      else begin
        Dest^ := Char(b);
        Inc(Source);
        Inc(Dest);
      end;
    end;
  end;
  Dest^ := #0;
end;

function RemoveCR(Source, Dest: PChar; DestLen, Count: integer): integer;
var
  SourceEnd: PChar;
  DestStart: PChar;
  w: word;
begin
  Result := Count;
  SourceEnd := Source + Count;
  DestStart := Dest;
  while (Source < SourceEnd) and (Dest - DestStart < DestLen) do begin
    w := Word(Pointer(Source)^);
    if w = CRLF then begin
      Dest^ := Char(LF);
      Inc(Source, 2);
      Dec(Result);
      Inc(Dest);
    end
    else
    begin
      Dest^ := Char(Lo(w));
      Inc(Source);
      Inc(Dest);
    end;
  end;
  Dest^ := #0;
end;

function AddCR(Source, Dest: PWideChar; Count: integer): integer;
var
  SourceEnd: PWideChar;
  w: LongWord;
  b: word;
begin
  Result := Count;
  SourceEnd := Source + Count;
  while Source < SourceEnd do begin
    w := LongWord(Pointer(Source)^);
    if w = CRLF_UTF16 then begin
      LongWord(Pointer(Dest)^) := w;
      Inc(Source, 2);
      Inc(Dest, 2);
    end
    else begin
      b := Word(w);
      if b = 0 then begin
        Dec(Result, SourceEnd - Source);
        break;
      end
      else
      if b = LF_UTF16 then begin
        LongWord(Pointer(Dest)^) := CRLF_UTF16;
        Inc(Source);
        Inc(Dest, 2);
        Inc(Result);
      end
      else begin
        Dest^ := WideChar(b);
        Inc(Source);
        Inc(Dest);
      end;
    end;
  end;
  Dest^ := #0;
end;

function RemoveCR(Source, Dest: PWideChar; DestLen, Count: integer): integer;
var
  SourceEnd: PWideChar;
  DestStart: PWideChar;
  w: LongWord;
begin
  Result := Count;
  SourceEnd := Source + Count;
  DestStart := Dest;
  while (Source < SourceEnd) and (Dest - DestStart < DestLen) do begin
    w := LongWord(Pointer(Source)^);
    if w = CRLF_UTF16 then begin
      Dest^ := WideChar(LF_UTF16);
      Inc(Source, 2);
      Dec(Result);
      Inc(Dest);
    end
    else
    begin
      Dest^ := WideChar(Word(w));
      Inc(Source);
      Inc(Dest);
    end;
  end;
  Dest^ := #0;
end;

function TData.NeedConvertEOL: boolean;
begin
  Result := False;
end;

procedure TData.GetFieldData(Field: TFieldDesc; RecBuf: pointer; Dest: pointer);
var
  Data: PChar;
begin
  case Field.DataType of
    dtUInt32:
      int64(Dest^) := PLongWord(PChar(RecBuf) + Field.Offset)^;
    dtDateTime:
      GetDateFromBuf(PChar(RecBuf) + Field.Offset, Dest, dfMSecs);
    dtDate:
      GetDateFromBuf(PChar(RecBuf) + Field.Offset, Dest, dfDate);
    dtTime:
      GetDateFromBuf(PChar(RecBuf) + Field.Offset, Dest, dfTime);
    dtVariant:
      Variant(Dest^) := TVariantObject(Pointer(PChar(RecBuf) + Field.Offset)^).Value;
    dtExtString: begin
      Assert(PChar(Pointer(PChar(RecBuf) + Field.Offset)^) <> nil);
      if NeedConvertEOL then
        AddCR(PChar(Pointer(PChar(RecBuf) + Field.Offset)^), Dest, MaxInt)
      else
        StrCopy(Dest, PChar(Pointer(PChar(RecBuf) + Field.Offset)^));
    end;
    dtExtWideString: begin
      Assert(PWideChar(Pointer(PChar(RecBuf) + Field.Offset)^) <> nil);
      StrCopyW(Dest, PWideChar(Pointer(PChar(RecBuf) + Field.Offset)^));
    end;
    dtExtVarBytes: begin
      Data := PChar(Pointer(PChar(RecBuf) + Field.Offset)^);
      Move(Data^, Dest^, Word(Pointer(Data)^) + SizeOf(Word));
    end;
    dtString:
      if NeedConvertEOL then
        AddCR((PChar(RecBuf) + Field.Offset), Dest, Field.Size)
      else
        Move((PChar(RecBuf) + Field.Offset)^, Dest^, Field.Size);
    dtWideString:
      Move((PChar(RecBuf) + Field.Offset)^, Dest^, Field.Size);
  else
    Move((PChar(RecBuf) + Field.Offset)^, Dest^, Field.Size);
  end;
end;

procedure TData.GetField(FieldNo: word; RecBuf: pointer; Dest: pointer; var IsBlank: boolean);
var
  Field: TFieldDesc;

begin
  Assert((FieldNo <= FieldCount) and (FieldNo > 0));

  IsBlank := GetNull(FieldNo, RecBuf);

  if (Dest = nil) or IsBlank and not (Fields[FieldNo - 1].DataType in (ComplexFieldTypes - [dtExtString, dtExtWideString, dtExtVarBytes]))
  then
    Exit;

  Field := Fields[FieldNo - 1];

  if not Field.HasParent then
    GetFieldData(Field, RecBuf, Dest)
  else
    GetChildField(Field, RecBuf, Dest, IsBlank);

  if not IsBlank and (Field.DataType in [dtString, dtWideString]) and Field.Fixed and FTrimFixedChar then // trim fixed char values
    if Field.DataType = dtString then
      StrTrim(Dest, Field.Length)
    else
      StrTrimW(Dest, Field.Length);
end;

function SetScale(F: double; Scale: integer): double;
begin
  if Scale > 0 then begin
    Result := StrToFloat(FloatToStrF(F, ffFixed, 18, Scale)); // 0.009
  end
  else
    Result := F;
end;

procedure TData.PutFieldData(Field: TFieldDesc; RecBuf: pointer; Source: pointer);
var
  Dest: pointer;
begin
  case Field.DataType of
    dtFloat:
      Double(Pointer((PChar(RecBuf) + Field.Offset))^) := SetScale(Double(Source^), Field.Scale);
    dtDateTime:
      PutDateToBuf(PChar(RecBuf) + Field.Offset, Source, dfMSecs);
    dtDate: 
      PutDateToBuf(PChar(RecBuf) + Field.Offset, Source, dfDate);
    dtTime: 
      PutDateToBuf(PChar(RecBuf) + Field.Offset, Source, dfTime);
    dtVariant:
      TVariantObject(Pointer(PChar(RecBuf) + Field.Offset)^).Value := Variant(Source^);
    dtWideString: 
      StrLCopyW(PWideChar((PChar(RecBuf) + Field.Offset)), PWideChar(WideString(Source^)), Field.Length);
    dtExtString: begin
      Dest := Pointer(PChar(RecBuf) + Field.Offset);
      StringHeap.DisposeBuf(PChar(Dest^));
      PChar(Dest^) := StringHeap.AllocStr(PChar(Source));
    end;
    dtExtWideString: begin
      Dest := Pointer(PChar(RecBuf) + Field.Offset);
      StringHeap.DisposeBuf(PChar(Dest^));
      PWideChar(Dest^) := StringHeap.AllocStr(PWideChar(WideString(Source^)));
    end;
    dtExtVarBytes: begin
      Dest := Pointer(PChar(RecBuf) + Field.Offset);
      StringHeap.DisposeBuf(PChar(Dest^));
      if Source <> nil then begin
        PChar(Dest^) := StringHeap.NewBuf(Word(Source^) + SizeOf(Word));
        Move(Source^, PChar(Dest^)^, Word(Source^) + SizeOf(Word));
      end
      else
        PChar(Dest^) := nil;
    end
  else
    Move(Source^, (PChar(RecBuf) + Field.Offset)^, Field.Size);
  end;
end;

procedure TData.PutField(FieldNo: word; RecBuf: pointer; Source: pointer);
var
  Field: TFieldDesc;
begin
  if Source = nil then begin
    SetNull(FieldNo, RecBuf, True);
    Exit;
  end;

  Field := Fields[FieldNo - 1];

  if not Field.HasParent then begin
    if (not FEnableEmptyStrings) and
      ((Field.DataType in [dtString, dtExtString]) and (PChar(Source)^ = #0) or
       (Field.DataType in [dtWideString, dtExtWideString]) and (PWideChar(Source)^ = #0)) then
      SetNull(FieldNo, RecBuf, True)
    else
    begin
      PutFieldData(Field, RecBuf, Source);
      SetNull(FieldNo, RecBuf, False);
    end;
  end
  else
    PutChildField(Field, RecBuf, Source);
end;

function TData.GetNull(FieldNo: word; RecBuf: pointer): boolean;
var
  Field:TFieldDesc;
begin
  Field := Fields[FieldNo - 1];
  if not Field.HasParent then
    Result := (PChar(RecBuf) + DataSize + FieldNo - 1)^ = #1
  else
    GetChildField(Field, RecBuf, nil, Result);
end;

procedure TData.SetNull(FieldNo: word; RecBuf: pointer; Value: boolean);
var
  Flag: char;
  Field: TFieldDesc;
  Blob: TBlob;
begin
  Field := Fields[FieldNo - 1];
  if not Field.HasParent then begin
    if Value then
      Flag := #1
    else
      Flag := #0;

    (PChar(RecBuf) + DataSize + FieldNo - 1)^ := Flag;

    if Value and (Field.DataType in BlobFieldTypes) then begin // clear Blob value
      Blob := TBlob(Pointer(PChar(RecBuf) + Field.Offset)^);
      if Blob <> nil then
        Blob.Clear;
    end;
  end
  else
    PutChildField(Field, RecBuf, nil);
end;

procedure TData.GetFieldAsVariant(FieldNo: word; RecBuf: pointer; var Value: variant);
var
  Field: TFieldDesc;
  FieldData: pointer;
  Date: TDateTime;
  Buf: array [0..4000] of byte;
  IsBlank: boolean;
begin
  if GetNull(FieldNo, RecBuf) then begin
    Value := Null;
    Exit;
  end;

  Value := Unassigned; // Delphi bug
  Field := Fields[FieldNo - 1];

  if not Field.HasParent then
    FieldData := PChar(RecBuf) + Field.Offset
  else begin
    FieldData := @Buf;
    GetChildField(Field, RecBuf, FieldData, IsBlank);
  end;

  case Field.DataType of
    dtString:
      if Field.Fixed and FTrimFixedChar then
      // trim fixed char values
        Value := TrimRight(String(PChar(FieldData)))
      else
        Value := String(PChar(FieldData));
    dtWideString:
      if Field.Fixed and FTrimFixedChar then
      // trim fixed char values
        Value := TrimRight(WideString(PWideChar(FieldData)))
      else
        Value := WideString(PWideChar(FieldData));
    dtInt8:
      Value := Shortint(FieldData^);
    dtSmallint:
      Value := Smallint(FieldData^);
    dtInt64: begin
    {$IFDEF VER6P}
      Value := Int64(FieldData^);
    {$ELSE}
      TVarData(Value).VType := varDecimal;
      TVarDataD6(Value).VInt64 := Int64(FieldData^);
    {$ENDIF}
    end;
    dtUInt32: begin
    {$IFDEF VER6P}
      Value := LongWord(FieldData^);
    {$ELSE}
      TVarData(Value).VType := varLongWord;
      TVarDataD6(Value).VLongword := LongWord(FieldData^);
    {$ENDIF}
    end;
    dtInteger:
      Value := Integer(FieldData^);
    dtWord:
      Value := Word(FieldData^);
    dtBoolean:
      Value := WordBool(FieldData^);
    dtFloat,dtCurrency:
      Value := Double(FieldData^);
    dtDateTime, dtDate, dtTime: begin
      if Field.HasParent then
        Date := TimeStampToDateTime(MSecsToTimeStamp(TDateTime(FieldData^)))
      else
        GetDateFromBuf(FieldData, @Date, dfDateTime);
      Value := Date;
    end;
    dtBlob, dtMemo, dtGraphic, dtFmtMemo: begin
      Value := TBlob(FieldData^).AsString;
    end;
    dtVariant:
      Value := TVariantObject(FieldData^).Value;
    dtExtString:
      Value := String(PChar(FieldData^));
    dtExtWideString:
      Value := WideString(PWideChar(FieldData^));
    dtBytes: begin
      Value := VarArrayCreate([0, Field.Length - 1], varByte);
      Move(FieldData^, TVarData(Value).VArray.Data^, Field.Length);
    end;
    dtVarBytes: begin
      Value := VarArrayCreate([0, Word(FieldData^) - 1], varByte);
      Move((PChar(FieldData) + sizeof(word))^, TVarData(Value).VArray.Data^, Word(FieldData^));
    end;
    dtExtVarBytes: begin
      Value := VarArrayCreate([0, Word(Pointer(FieldData^)^) - 1], varByte);
      Move(TVarData(Value).VArray.Data^, (PChar(Pointer(FieldData^)) + sizeof(word))^, Word(Pointer(FieldData^)^));
    end;
  else
    raise EConvertError.Create(SCannotConvertType);
  end;
end;

procedure TData.PutFieldAsVariant(FieldNo: word; RecBuf: pointer; const Value: variant);
var
  FieldData: pointer;
  Date: TDateTime;
  b: boolean;

  ws: WideString;
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then begin
    SetNull(FieldNo, RecBuf, True);
    Exit;
  end;
  with Fields[FieldNo - 1] do begin
    FieldData := PChar(RecBuf) + Offset;
    case DataType of
      dtString:
        StrLCopy(FieldData, PChar(VarToStr(Value)), Size);
      dtWideString:
      begin
        ws := Value;
        StrLCopyW(FieldData, PWideChar(ws), Size div 2 - 1);
      end;
      dtInt8:
        Shortint(FieldData^) := Value;
      dtSmallint:
        case VarType(Value) of
          varSmallint,varInteger,varByte{$IFDEF VER6P}, varWord{$ENDIF}:
            Smallint(FieldData^) := Value;
        else
          raise EConvertError.Create(SCannotConvertType);
        end;
      dtInteger:
        case VarType(Value) of
          varString,varOleStr:
            Integer(FieldData^) := StrToInt(Value);
          varSmallint,varInteger,varByte,{$IFDEF VER6P}varWord,{$ENDIF}
          varSingle,varDouble,varCurrency:
            Integer(FieldData^) := Value;
        else
          raise EConvertError.Create(SCannotConvertType);
        end;
      dtInt64:
        case VarType(Value) of
        {$IFDEF VER6P}
          varInt64:
            Int64(FieldData^) := Value;
        {$ELSE}
          varDecimal:
            Int64(FieldData^) := TVarDataD6(Value).VInt64;
        {$ENDIF}
        else
          raise EConvertError.Create(SCannotConvertType);
        end;
      dtUInt32:
        case VarType(Value) of
          varLongWord:
            LongWord(FieldData^) := {$IFDEF VER6P}TVarData{$ELSE}TVarDataD6{$ENDIF}(Value).VLongWord;
        else
          raise EConvertError.Create(SCannotConvertType);
        end;
      dtWord:
        case VarType(Value) of
          varSmallint,varInteger,varByte{$IFDEF VER6P},varWord{$ENDIF}:
            Word(FieldData^) := Value;
        else
          raise EConvertError.Create(SCannotConvertType);
        end;
      dtBoolean:
        case VarType(Value) of
          varBoolean:
          begin
            b := Value;
            WordBool(FieldData^) := b;
          end
        else
          raise EConvertError.Create(SCannotConvertType);
        end;
      dtFloat, dtCurrency:
        case VarType(Value) of
          varString,varOleStr:
            Double(FieldData^) := SetScale(StrToFloat(Value), Scale);
          varSmallint,varInteger,varByte:
            Double(FieldData^) := Value;
          varSingle,varDouble,varCurrency:
            Double(FieldData^) := SetScale(Value, Scale);
        else
          raise EConvertError.Create(SCannotConvertType);
        end;
      dtDateTime, dtDate, dtTime: begin
        Date := VarToDateTime(Value);
        PutDateToBuf(FieldData, @Date, dfDateTime);
      end;
      dtMemo,dtBlob,dtGraphic,dtFmtMemo: begin  // used to refresh String as Memo
        TBlob(FieldData^).AsString := VarToStr(Value);
      end;
      dtVariant:
        TVariantObject(FieldData^).Value := Value;
      dtExtString: begin
        StringHeap.DisposeBuf(PChar(FieldData^));
        PChar(FieldData^) := StringHeap.AllocStr(PChar(String(Value)));
      end;
      dtExtWideString: begin
        StringHeap.DisposeBuf(PChar(FieldData^));
        PWideChar(FieldData^) := StringHeap.AllocStr(PWideChar(WideString(Value)));
      end;
      dtBytes: begin
        Assert(TVarData(Value).VType = varArray + varByte, 'Invalid VType');
        Assert(TVarData(Value).VArray.Bounds[0].ElementCount = Length, 'Invalid data size');
        Move(TVarData(Value).VArray.Data^, FieldData^, Length);
      end;
      dtVarBytes: begin
        Assert(TVarData(Value).VType = varArray + varByte, 'Invalid VType');
        Assert(TVarData(Value).VArray.Bounds[0].ElementCount <= Length, 'Invalid data size');

        Word(FieldData^) := TVarData(Value).VArray.Bounds[0].ElementCount;
        Move(TVarData(Value).VArray.Data^, (PChar(FieldData) + sizeof(word))^, Word(FieldData^));
      end;
      dtExtVarBytes: begin
        Assert(TVarData(Value).VType = varArray + varByte, 'Invalid VType');
        Assert(TVarData(Value).VArray.Bounds[0].ElementCount <= Length, 'Invalid data size');

        StringHeap.DisposeBuf(PChar(FieldData^));
        PChar(FieldData^) := StringHeap.NewBuf(TVarData(Value).VArray.Bounds[0].ElementCount + sizeof(Word));

        Word(Pointer(FieldData^)^) := TVarData(Value).VArray.Bounds[0].ElementCount;
        Move(TVarData(Value).VArray.Data^, (PChar(FieldData^) + sizeof(word))^, Word(Pointer(FieldData^)^));
      end;
    else
      raise EConvertError.Create(SCannotConvertType);
    end;
  end;

  SetNull(FieldNo, RecBuf, False);
end;

function TData.FindField(Name: string):TFieldDesc;
begin
  Result := FFields.FindField(Name);
end;

function TData.FieldByName(Name: string):TFieldDesc;
begin
  Result := FFields.FieldByName(Name);
end;

function TData.BlobFieldTypes: TFieldTypeSet; // TBlob descendants - dtBlob, dtMemo etc
begin
  Result := [dtBlob, dtMemo, dtGraphic, dtFmtMemo];
end;

function TData.ComplexFieldTypes: TFieldTypeSet; // All supported complex field types (BlobFieldTypes, ExtFieldTypes and TSharedObject descendants (not BLOB))
begin
  Result := BlobFieldTypes + [dtExtString, dtExtWideString, dtExtVarBytes, dtVariant];
end;

function TData.IsFields(FieldTypes: TFieldTypeSet): boolean;
var
  i: integer;
begin
  i := 0;
  while (i < FieldCount) and not (Fields[i].DataType in FieldTypes) do
    Inc(i);
  Result := i < FieldCount;
end;

function TData.IsBlobFields: boolean;
var
  i: integer;
begin
  i := 0;
  while (i < FieldCount) and not (Fields[i].DataType in BlobFieldTypes) do
    Inc(i);
  Result := i < FieldCount;
end;

function TData.CheckIsComplexFields: boolean;
var
  i: integer;
begin
  i := 0;
  while (i < FieldCount) and not (Fields[i].DataType in ComplexFieldTypes) do
    Inc(i);
  Result := i < FieldCount;
  FIsComplexFields := Result;
end;

{ Records }

function TData.AllocRecBuf(var RecBuf: pointer): pointer;
begin
  GetMem(RecBuf, RecordSize);
  Result := RecBuf;
end;

procedure TData.FreeRecBuf(RecBuf: pointer);
begin
  FreeMem(RecBuf);
end;

procedure TData.CreateComplexFields(RecBuf: pointer; WithBlob: boolean);
var
  i: integer;
  Blob: TSharedObject;
begin
  for i := 0 to FieldCount - 1 do
    case Fields[i].DataType of
    dtBlob, dtMemo, dtGraphic, dtFmtMemo:
      if WithBlob then begin
        Blob := TBlob.Create;
        Pointer(Pointer(PChar(RecBuf) + Fields[i].Offset)^) := Blob;
      end;
  {$IFDEF VER5P}
    dtVariant:
      begin
        Blob := TVariantObject.Create;
        Pointer(Pointer(PChar(RecBuf) + Fields[i].Offset)^) := Blob;
      end;
  {$ENDIF}
    dtExtString, dtExtWideString, dtExtVarBytes:
      Pointer(Pointer(PChar(RecBuf) + Fields[i].Offset)^) := nil;
    end;
end;

procedure TData.AddRefComplexFields(RecBuf: pointer);
var
  i: integer;
begin
  for i := 0 to FieldCount - 1 do
    if Fields[i].DataType in [dtExtString, dtExtWideString, dtExtVarBytes] then
      StringHeap.AddRef(PChar(Pointer(PChar(RecBuf) + Fields[i].Offset)^))
    else
    if (Fields[i].DataType in ComplexFieldTypes) and not Fields[i].HasParent then
      TSharedObject(Pointer(PChar(RecBuf) + Fields[i].Offset)^).AddRef;
end;

procedure TData.FreeComplexFields(RecBuf: pointer; WithBlob: boolean);
var
  i: integer;
  Blob: TSharedObject;
begin
  for i := 0 to FieldCount - 1 do
    case Fields[i].DataType of
    dtBlob, dtMemo, dtGraphic, dtFmtMemo:
      if WithBlob then begin
        Blob := Pointer(Pointer(PChar(RecBuf) + Fields[i].Offset)^);
        Blob.Free;
      end;
  {$IFDEF VER5P}
    dtVariant:
      begin
        Blob := Pointer(Pointer(PChar(RecBuf) + Fields[i].Offset)^);
        Blob.Free;
      end;
  {$ENDIF}      
    dtExtString, dtExtWideString, dtExtVarBytes:
      if not StringHeap.Empty then
        StringHeap.DisposeBuf(PChar(Pointer(PChar(RecBuf) + Fields[i].Offset)^));
    end;
end;

procedure TData.CopyComplexFields(Source: pointer; Dest: pointer; WithBlob: boolean);
var
  i: integer;
  SrcPtr: PChar;
  DestPtr: pointer;
begin
  if WithBlob then
    Assert(False);

  for i := 0 to FieldCount - 1 do
    case Fields[i].DataType of
      dtExtString: begin
        SrcPtr := PChar(Pointer(PChar(Source) + Fields[i].Offset)^);
        DestPtr := PChar(Dest) + Fields[i].Offset;
        PChar(DestPtr^) := StringHeap.AllocStr(SrcPtr);
      end;
      dtExtWideString: begin
        SrcPtr := PChar(Pointer(PChar(Source) + Fields[i].Offset)^);
        DestPtr := PChar(Dest) + Fields[i].Offset;
        PWideChar(DestPtr^) := StringHeap.AllocStr(PWideChar(SrcPtr));
      end;
      dtExtVarBytes: begin
        SrcPtr := PChar(Pointer(PChar(Source) + Fields[i].Offset)^);
        DestPtr := PChar(Dest) + Fields[i].Offset;
        if SrcPtr = nil then 
          PChar(DestPtr^) := nil
        else begin
          PChar(DestPtr^) := StringHeap.NewBuf(Word(Pointer(SrcPtr)^) + SizeOf(Word));
          Move(SrcPtr^, PChar(DestPtr^)^, Word(Pointer(SrcPtr)^) + SizeOf(Word));
        end
      end;
    {$IFDEF VER5P}
      dtVariant: 
        TVariantObject(Pointer(Pointer(PChar(Dest) + Fields[i].Offset)^)).Value := 
          TVariantObject(Pointer(Pointer(PChar(Source) + Fields[i].Offset)^)).Value;
    {$ENDIF}
    end;
end;

procedure TData.InitRecord(RecBuf: pointer);
var
  i: integer;
begin
// Complex fields need create later
  if IsComplexFields then  // clear pointer to complex field
    FillChar(RecBuf^, RecordSize, #0);

  for i := 1 to FieldCount do
    SetNull(i, RecBuf, True);
end;

procedure TData.AppendBlankRecord;
var
  RecBuf: pointer;
begin
  AllocRecBuf(RecBuf);
  try
    InitRecord(RecBuf);
    AppendRecord(RecBuf);
  finally
    FreeRecBuf(RecBuf);
  end;
end;

procedure TData.EditRecord(RecBuf: pointer);
var
  TempBuf: pointer;
begin
  AllocRecBuf(TempBuf);
  try
    GetRecord(TempBuf);
    CreateComplexFields(TempBuf, False);  // Blobs uses internal cache
    CopyComplexFields(RecBuf, TempBuf, False);
    PutRecord(TempBuf);

    {if IsBlobFields then
      for i:= 0 to FieldCount - 1 do
        if Fields[i].DataType in BlobFieldTypes then
          TBlob(Pointer(PChar(RecBuf) + Fields[i].Offset)^).EnableRollback;}
  finally
    FreeRecBuf(TempBuf);
  end;
end;

procedure TData.PostRecord(RecBuf: pointer);
var
  i: integer;
  TempBuf: pointer;
begin
  AllocRecBuf(TempBuf);
  try
    GetRecord(TempBuf);

    UpdateRecord(RecBuf);

    if IsBlobFields then
      for i := 0 to FieldCount - 1 do
        if Fields[i].DataType in BlobFieldTypes then
          GetBlob(Fields[i].FieldNo, RecBuf).Commit;

    FreeComplexFields(TempBuf, False);
  finally
    FreeRecBuf(TempBuf);
  end;
end;

procedure TData.CancelRecord(RecBuf: pointer);
var
  i: integer;
begin
  if IsBlobFields then
    for i := 0 to FieldCount - 1 do
      if Fields[i].DataType in BlobFieldTypes then
        GetBlob(Fields[i].FieldNo, RecBuf).Cancel;

  FreeComplexFields(RecBuf, False);
end;

{ Edit }

procedure TData.InternalAppend(RecBuf: pointer);
begin
  if Assigned(FOnAppend) then
    FOnAppend;
end;

procedure TData.InternalDelete;
begin
  if Assigned(FOnDelete) then
    FOnDelete;
end;

procedure TData.InternalUpdate(RecBuf: pointer);
begin
  if Assigned(FOnUpdate) then
    FOnUpdate;
end;

procedure TData.ApplyRecord(UpdateKind:TUpdateRecKind; var Action:TUpdateRecAction);
begin
  if Assigned(FOnApplyRecord) then
    FOnApplyRecord(UpdateKind, Action);
end;

{ Navigation }

function TData.GetEOF: boolean;
begin
  Result := FEOF;
end;

function TData.GetBOF: boolean;
begin
  Result := FBOF;
end;

procedure TData.SetToBegin;
begin
  FBOF := True;
  FEOF := False;
end;

procedure TData.SetToEnd;
begin
  FEOF := True;
  FBOF := False;
end;

function TData.GetRecordCount: longint;
begin
  Result := -1;
end;

function TData.GetRecordNo: longint;
begin
  Result := -1;
end;

procedure TData.SetRecordNo(Value: longint);
begin
end;

{ BookMarks }

procedure TData.GetBookmark(Bookmark: PRecBookmark);
begin
  Bookmark.Order := RecordNo;
end;

procedure TData.SetToBookmark(Bookmark: PRecBookmark);
begin
  if Bookmark.Order <> -1 then
    SetRecordNo(Bookmark.Order);
end;

function TData.BookmarkValid(Bookmark: PRecBookmark): boolean;
begin
  if Bookmark <> nil then
    Result := Bookmark.Order <> -1
  else
    Result := False;
end;

function TData.CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2,-1),(1,0));
begin
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then begin
    if Bookmark1.Order >= Bookmark2.Order then
      if Bookmark1.Order = Bookmark2.Order then
        Result := 0
      else
        Result := 1
    else
      Result := -1
  end;
end;

{ CachedUpdates }

function TData.GetUpdateStatus: TItemStatus;
begin
  Result := isUnmodified;
end;

function TData.GetUpdateResult: TUpdateRecAction;
begin
  Result := urNone;
end;

procedure TData.SetCacheRecBuf(NewBuf: pointer; OldBuf: pointer);
begin
end;

procedure TData.ApplyUpdates;
begin
end;

procedure TData.CommitUpdates;
begin
end;

procedure TData.CancelUpdates;
begin
end;

procedure TData.RestoreUpdates;
begin
end;

procedure TData.RevertRecord;
begin
end;

function TData.GetUpdatesPending: boolean;
begin
  Result := False;
end;

procedure TData.GetOldRecord(RecBuf: pointer);
begin
end;

type
  TDataContext = class(TExpressionContext)
    class function GetItem(context: pointer; const name: string): variant; override;
    class function GetSubContext(context: pointer; const name: string; out contextClass: TContextClass): pointer; override;
  end;

{ TDataContext }

class function TDataContext.GetItem(context: pointer;
  const name: string): variant;
var
  FieldDesc: TFieldDesc;
begin
  with TMemData(context) do begin
    FieldDesc := FindField(name);
    GetFieldAsVariant(FieldDesc.FieldNo, FilterRecBuf, result);
  end;
end;

class function TDataContext.GetSubContext(context: pointer;
  const name: string; out contextClass: TContextClass): pointer;
begin
  result := nil;
end;

procedure TData.CreateFilterExpression(const Text: string);
begin
  FreeFilterExpression;
  if Trim(Text) <> '' then begin
    Evaluator.Create(Text, self, TDataContext);
    FFiltered := true;
  end;
end;

procedure TData.FreeFilterExpression;
begin
  FFiltered := false;
end;

procedure TData.FilterUpdated;
begin
end;

function TData.Filtered: boolean;
begin
  Result := Assigned(FFilterFunc) or FFiltered;
end;

{ Blobs }

function TData.GetBlob(FieldNo: word; RecBuf: pointer): TBlob;
var
  IsBlank: boolean;
begin
  if not (Fields[FieldNo - 1].DataType in BlobFieldTypes) then
    raise Exception.Create(SNeedBlobType);

  GetField(FieldNo, RecBuf, @Result, IsBlank);
  Assert(Result <> nil);
end;

function TData.ReadBlob(FieldNo: word; RecBuf: pointer; Position: longint;
  Count: longint; Dest: pointer; FromRollback: boolean = false): longint;
var
  Blob: TBlob;
  ws: WideString;
  s: string;
  ReadedBytesCount: longint;
begin
  Blob := GetBlob(FieldNo, RecBuf);
  if FromRollback and (Blob.Rollback <> nil) then
    Blob := Blob.Rollback;

  if not Blob.FIsUnicode then
    Result := Blob.Read(Position, Count, Dest)
  else
  begin
    if Count = 0 then
      SetLength(ws, longint(Blob.Size) - Position)
    else
      SetLength(ws, Count);
    ReadedBytesCount := Blob.Read(Position shl 1, Count shl 1, PWideChar(ws));
    Result := ReadedBytesCount shr 1;
    SetLength(ws, Result); // Truncate ws to real size
    s := ws;
    Move(PChar(s)^, Dest^, Length(s));
  end;
end;

procedure TData.WriteBlob(FieldNo: word; RecBuf: pointer; Position: longint;
  Count: longint; Source: pointer);
var
  Blob: TBlob;
  ws: WideString;
  s: string;
begin
  Blob := GetBlob(FieldNo, RecBuf);

  Blob.EnableRollback;
  if not Blob.FIsUnicode then
    Blob.Write(Position, Count, Source)
  else
  begin
    SetLength(s, Count);
    Move(Source^, PChar(s)^, Length(s));
    ws := s;
    Blob.Write(Position shl 1, Count shl 1, PWideChar(ws));
  end;

  SetNull(FieldNo, RecBuf, False);
end;

procedure TData.TruncateBlob(FieldNo: word; RecBuf: pointer; Size: longint);
var
  Blob:TBlob;
begin
  Blob := GetBlob(FieldNo, RecBuf);

  Blob.EnableRollback;
  Blob.Truncate(Size);

  if Size = 0 then
    SetNull(FieldNo, RecBuf, True);
end;

function TData.GetBlobSize(FieldNo: word; RecBuf: pointer; FromRollback: boolean = false): longint;
var
  Blob: TBlob;
begin
  Blob := GetBlob(FieldNo, RecBuf);
  if FromRollback and (Blob.Rollback <> nil) then
    Blob := Blob.Rollback;

  Result := Blob.Size;
  if Blob.FIsUnicode then
    Result := Result shr 1;
end;

procedure TData.SetCachedUpdates(Value: boolean);
begin
  if Value <> FCachedUpdates then begin
    if FCachedUpdates then
      CancelUpdates;

    FCachedUpdates := Value;

    if FCachedUpdates then
      FFilterItemTypes := [isUnmodified, isUpdated, isAppended];
  end;
end;

procedure TData.SetFilterText(Value: string);
begin
  if Value <> FFilterText then begin
    if Active then
      CreateFilterExpression(Value);
    FFilterText := Value;
  end;
end;

procedure TData.SetFilterItemTypes(Value:TItemTypes);
begin
  FFilterItemTypes := Value;
end;

{ TMemData }

constructor TMemData.Create;
begin
  inherited;

  BlockMan := TBlockManager.Create;

  InitData;
end;

destructor TMemData.Destroy;
begin
  inherited;

  BlockMan.Free;
end;

{ Items / Data }

function TMemData.InsertItem: PItemHeader;
var
  Item: PItemHeader;
begin
  if EOF then begin
    Result := AppendItem;
    Exit;
  end;

  if BOF then
    CurrentItem := FirstItem;

  BlockMan.AllocItem(Item);

  Item.Next := CurrentItem;

  if CurrentItem <> nil then begin
    Item.Prev := CurrentItem.Prev;
    if CurrentItem.Prev <> nil then
      CurrentItem.Prev.Next := Item;
    CurrentItem.Prev := Item
  end
  else begin
    Item.Prev := nil;
  end;

  if FirstItem = CurrentItem then
    FirstItem := Item;

  if LastItem = nil then
    LastItem := Item;

  Result := Item;
end;

function TMemData.AppendItem: PItemHeader;
var
  Item: PItemHeader;
begin
  BlockMan.AllocItem(Item);

  if FirstItem = nil then begin
    FirstItem := Item;
    Item.Order := 1;
  end
  else begin
    LastItem.Next := Item;
    Item.Order := LastItem.Order + 1;
  end;

  Item.Prev := LastItem;
  Item.Next := nil;
  LastItem := Item;

  Result := Item;
end;

procedure TMemData.DeleteItem(Item: PItemHeader);
begin
  if Item <> nil then begin
    if Item = FirstItem then
      if Item = LastItem then begin
        CurrentItem := nil;
        FirstItem := nil;
        LastItem := nil;
        FBOF := True;
        FEOF := True;
      end
      else begin
        FirstItem := Item.Next;
        FirstItem.Prev := nil;
        if Item = CurrentItem then
          CurrentItem := FirstItem;
      end
    else
      if Item = LastItem then begin
        LastItem := Item.Prev;
        LastItem.Next := nil;
        if Item = CurrentItem then
          CurrentItem := LastItem;
      end
      else begin
        if Item = CurrentItem then
          CurrentItem := Item.Next;

        Item.Prev.Next := Item.Next;
        Item.Next.Prev := Item.Prev;
      end;

    {if IsComplexFields then
      FreeComplexFields(PChar(Item) + sizeof(TItemHeader), True);}

    BlockMan.FreeItem(Item);
  end;
end;

procedure TMemData.InitData;
begin
  FirstItem := nil;
  LastItem := nil;
  CurrentItem := nil;
  Cache := nil;
  LastCacheItem := nil;

  FBOF := True;
  FEOF := True;
  FRecordCount := 0;
  FRecordNoOffset := 0;

  BlockMan.FirstFree := nil;
  Inc(DbMemDS.RefreshIteration);
  FRefreshIteration := DbMemDS.RefreshIteration;
end;

procedure TMemData.FreeData;
var
  CacheItem: PCacheItem;
  AComplexFieldTypes: TFieldTypeSet;
  Item: PItemHeader;
begin
  if not StringHeap.SysGetMem then begin
    AComplexFieldTypes := ComplexFieldTypes - [dtExtString, dtExtWideString, dtExtVarBytes];
    StringHeap.Clear;
  end
  else
    AComplexFieldTypes := ComplexFieldTypes;

  if IsFields(AComplexFieldTypes) then begin
  // Free complex fields
    Item := FirstItem;
    while Item <> nil do begin
      FreeComplexFields(PChar(Item) + sizeof(TItemHeader), True);
      Item := Item.Next;
    end;
    CacheItem := Cache;
    while CacheItem <> nil do begin
      Item := CacheItem.Item.Rollback;
      if Item <> nil then
        FreeComplexFields(PChar(Item) + sizeof(TItemHeader), True);
      CacheItem := CacheItem.Next;
    end;
  end;

// Free cache
  while Cache <> nil do begin
    CacheItem := Cache;
    Cache := Cache.Next;
    Dispose(CacheItem);
  end;
  
  StringHeap.Clear;
  BlockMan.FreeAllBlock;

  InitData;
end;

procedure TMemData.ReorderItems(Item: PItemHeader; ReorderOption: TReorderOption);
var
  No: longint;
  Item1: PItemHeader;
begin
  if (Item <> nil) or (ReorderOption = roFull) and (FirstItem <> nil)
  then begin
    if ReorderOption = roFull then begin
      Item := FirstItem;
      No := 1;
    end
    else
      if Item.Next <> nil then
        No := Item.Next.Order
      else
        if Item.Prev <> nil then
          No := Item.Prev.Order
        else begin
          Item.Order := 1;
          FRecordNoOffset := 0;
        { $IFDEF LINUX
          No := 0; // Kylix 1 anti warning
         $ENDIF}
          Exit;
        end;

    if (ReorderOption = roFull) or (No > (FRecordCount + FRecordNoOffset) div 2)
    then begin
      Item1 := Item.Prev;
      while (Item1 <> nil) and OmitRecord(Item1) do
        Item1 := Item1.Prev;
      if Item1 <> nil then
        No := Item1.Order + 1
      else begin
        No := 1;
        FRecordNoOffset := 0;
      end;

      while Item <> nil do begin

        if not OmitRecord(Item) then begin
          Item.Order := No;
          Inc(No);
        end;
        Item := Item.Next;
      end;
    end
    else begin
      Item1 := Item.Next;
      while (Item1 <> nil) and OmitRecord(Item1) do
        Item1 := Item1.Next;
      if Item1 <> nil then begin
        No := Item1.Order - 1;
        if ReorderOption = roInsert then
          Inc(FRecordNoOffset)
        else
          Dec(FRecordNoOffset)
      end
      else begin
        No := FRecordCount;
        FRecordNoOffset := 0;
      end;

      while Item <> nil do begin
        if not OmitRecord(Item) then begin
          Item.Order := No;
          Dec(No);
        end;
        Item := Item.Prev;
      end;
    end;

    if ReorderOption = roFull then
      FRecordCount := No - 1;
  end;
end;

{ Fields }

procedure TMemData.InitFields;
begin
  inherited;

  BlockMan.RecordSize := RecordSize;
end;

{ Records }

function TMemData.OmitRecord(Item: PItemHeader): boolean;
begin
  if Item <> nil then begin
    FilterRecBuf := BlockMan.GetRecordPtr(Item);
    Result := FCachedUpdates and not (Item.Status in FFilterItemTypes) or
      Assigned(FFilterFunc) and not FFilterFunc(FilterRecBuf) or
      FFiltered and (Evaluator.Evaluate <> true);
  end
  else
    Result := True; //False;
end;

procedure TMemData.GetRecord(RecBuf: pointer);
begin
  if not(EOF or BOF or (CurrentItem = nil)) then
    if OmitRecord(CurrentItem) then
      GetNextRecord(RecBuf)
    else
      BlockMan.GetRecord(CurrentItem, RecBuf);
end;

procedure TMemData.GetNextRecord(RecBuf: pointer);
  procedure OmitRecords;
  begin
    while (CurrentItem <> nil) and OmitRecord(CurrentItem) do
      CurrentItem := CurrentItem.Next;
  end;
begin
  if not EOF then begin
    if BOF then begin
      FBOF := False;
      CurrentItem := FirstItem;
    end
    else
      if CurrentItem <> nil then
        CurrentItem := CurrentItem.Next
      else
        CurrentItem := FirstItem;

    OmitRecords;
    if CurrentItem = nil then
      FEOF := True
    else
      if RecBuf <> nil then
        GetRecord(RecBuf);
  end;
end;

procedure TMemData.GetPriorRecord(RecBuf: pointer);
  procedure OmitRecords;
  begin
    while (CurrentItem <> nil) and OmitRecord(CurrentItem) do
      CurrentItem := CurrentItem.Prev;
  end;
begin
  if not BOF then begin
    if EOF then begin
      FEOF := False;
      CurrentItem := LastItem;
    end
    else
      if CurrentItem <> nil then
        CurrentItem := CurrentItem.Prev
      else
        CurrentItem := LastItem;

    OmitRecords;
    if CurrentItem = nil then
      FBOF := True
    else
      if RecBuf <> nil then
        GetRecord(RecBuf);
  end;
end;

procedure TMemData.PutRecord(RecBuf: pointer);
begin
  Assert(CurrentItem <> nil);
  BlockMan.PutRecord(CurrentItem, RecBuf);
end;

procedure TMemData.InsertRecord(RecBuf: pointer);
var
  CacheItem: PCacheItem;
  OldCurrentItem: PItemHeader;
begin
  OldCurrentItem := CurrentItem;
  if not FCachedUpdates then begin
    NewItem := InsertItem;
    try
      InternalAppend(RecBuf);
    except
      DeleteItem(NewItem);

      CurrentItem := OldCurrentItem;
      raise;
    end;
    CurrentItem := NewItem;
  end
  else begin
    CurrentItem := InsertItem;
    try
      New(CacheItem);
      CacheItem.Item := CurrentItem;
      AddCacheItem(CacheItem);

      CurrentItem.Status := isAppended;
      CurrentItem.UpdateResult := urNone;
    except
      DeleteItem(CurrentItem);

      CurrentItem := OldCurrentItem;
      raise;
    end;
  end;
  PutRecord(RecBuf);
  Inc(FRecordCount);
  ReorderItems(CurrentItem, roInsert);
end;

procedure TMemData.AppendRecord(RecBuf: pointer);
begin
  SetToEnd;
  InsertRecord(RecBuf);
end;

procedure TMemData.UpdateRecord(RecBuf: pointer);
var
  CacheItem: PCacheItem;
  Rollback: PItemHeader;
  i: integer;
  Blob: TBlob;
  RollbackRecBuf: PChar;
  ItemRecBuf: PChar;
begin
  Assert(CurrentItem <> nil);

  if not FCachedUpdates then
    InternalUpdate(RecBuf)
  else begin
    if CurrentItem.Status = isUnmodified then begin
    // add to cache
      New(CacheItem);
      CacheItem.Item := CurrentItem;
      AddCacheItem(CacheItem);
    end;

    if (CurrentItem.Status <> isAppended) or (CurrentItem.UpdateResult = urApplied)
    then begin
      CurrentItem.Status := isUpdated;

      if CurrentItem.Rollback = nil then begin
      // create rollback record
        BlockMan.AllocItem(Rollback);
        CurrentItem.Rollback := Rollback;
        BlockMan.CopyRecord(CurrentItem, Rollback);
        AddRefComplexFields(PChar(Rollback) + sizeof(TItemHeader));
      end;
      if IsBlobFields then begin
        RollbackRecBuf := PChar(CurrentItem.Rollback) + sizeof(TItemHeader);
        ItemRecBuf := PChar(CurrentItem) + sizeof(TItemHeader);
        for i := 0 to FieldCount - 1 do
          if Fields[i].DataType in BlobFieldTypes then begin
            Blob := TBlob(Pointer(RollbackRecBuf + Fields[i].Offset)^);
            if (Blob.Rollback <> nil) and (Blob = TBlob(Pointer(ItemRecBuf + Fields[i].Offset)^)) then begin
              TBlob(Pointer(RollbackRecBuf + Fields[i].Offset)^) := Blob.Rollback;
              Blob.Rollback := nil;
              Blob.Release;
            end;
          end;
      end;
    end;
    CurrentItem.UpdateResult := urNone;
  end;

  PutRecord(RecBuf);
end;

procedure TMemData.RemoveRecord;
var
  PermitDelete: boolean;
begin
  if FCachedUpdates then begin
    PermitDelete := CurrentItem.Status <> isAppended;
    RevertRecord;
  end
  else
    PermitDelete := True;

  if IsComplexFields then
    FreeComplexFields(PChar(CurrentItem) + sizeof(TItemHeader), True);

  if PermitDelete then
    DeleteItem(CurrentItem);

  Dec(FRecordCount);
  ReorderItems(CurrentItem, roDelete);
end;

procedure TMemData.DeleteRecord;
var
  CacheItem: PCacheItem;
  OldCacheItem: PCacheItem;
begin
  if not FCachedUpdates then begin
    InternalDelete;

    RemoveRecord;
  end
  else begin
    if CurrentItem.Status = isUnmodified then begin
    // add to cache
      New(CacheItem);
      CacheItem.Item := CurrentItem;
      AddCacheItem(CacheItem);

      CurrentItem.Status := isDeleted;
      CurrentItem.UpdateResult := urNone;
    end
    else
      case CurrentItem.Status of
        isAppended: begin
        // remove record from cache
          CacheItem := Cache;
          OldCacheItem := CacheItem;
          while CacheItem <> nil do begin
            if CacheItem.Item = CurrentItem then begin
              if CacheItem = LastCacheItem then
                if CacheItem = Cache then
                  LastCacheItem := nil
                else
                  LastCacheItem := OldCacheItem;

              if CacheItem = Cache then
                Cache := CacheItem.Next
              else
                OldCacheItem.Next := CacheItem.Next;

              Dispose(CacheItem);
              break;
            end;

            OldCacheItem := CacheItem;
            CacheItem := CacheItem.Next;
          end;

          if IsComplexFields then
            FreeComplexFields(PChar(CurrentItem) + sizeof(TItemHeader), True);

          DeleteItem(CurrentItem);
        end;
        isUpdated: begin
        // rollback record
          FreeComplexFields(PChar(CurrentItem) + sizeof(TItemHeader), True);
          BlockMan.CopyRecord(CurrentItem.Rollback, CurrentItem);
          BlockMan.FreeItem(CurrentItem.Rollback);
          CurrentItem.Rollback := nil;

          CurrentItem.Status := isDeleted;
          CurrentItem.UpdateResult := urNone;
        end;
      end;
      Dec(FRecordCount);
      ReorderItems(CurrentItem, roDelete);
  end;
end;

{ Edit }

{ Navigation }

function TMemData.GetBOF: boolean;
begin
  Result := (CurrentItem = nil) and FBOF;  // WAR
end;

function TMemData.GetEOF: boolean;
begin
  Result := (CurrentItem = nil) and FEOF;  // WAR
end;

procedure TMemData.SetToBegin;
begin
  CurrentItem := nil; //FirstItem;
  FBOF := True;
  if LastItem <> nil then
    FEOF := False;
end;

procedure TMemData.SetToEnd;
begin
  CurrentItem := nil; //LastItem;
  FEOF := True;
  if FirstItem <> nil then
    FBOF := False;
end;

function TMemData.GetRecordCount: longint;
begin
  Result := FRecordCount;
end;

function TMemData.GetRecordNo: longint;
begin
  if CurrentItem <> nil then
    Result := CurrentItem.Order + FRecordNoOffset
  else
    Result := 0;
end;

procedure TMemData.SetRecordNo(Value: longint);
var
  Item,CurrItem: PItemHeader;
  ForwardDir: boolean;
begin
  if (FirstItem <> nil) and (Value > 0) then begin
    if CurrentItem <> nil then
      CurrItem := CurrentItem
    else
      CurrItem := FirstItem;

    if (Value < Abs(LastItem.Order + FRecordNoOffset - Value)) and
      (Value < Abs(CurrItem.Order + FRecordNoOffset - Value))
    then begin
    // from first
      Item := FirstItem;
      ForwardDir := True;
    end
    else
      if Abs(LastItem.Order + FRecordNoOffset - Value) <
        Abs(CurrItem.Order + FRecordNoOffset - Value)
      then begin
      // from
        Item := LastItem;
        ForwardDir := LastItem.Order + FRecordNoOffset < Value;
      end
      else begin
      // from current
        Item := CurrItem;
        ForwardDir := CurrItem.Order + FRecordNoOffset < Value;
      end;

    while (Item <> nil) and (Item.Order + FRecordNoOffset <> Value) do
      if ForwardDir then begin
        Item := Item.Next
      end
      else
        Item := Item.Prev;

    if Item <> nil then
      CurrentItem := Item;
  end;
end;

{ BookMarks }

procedure TMemData.GetBookmark(Bookmark: PRecBookmark);
begin
  Bookmark.RefreshIteration := FRefreshIteration;
  Bookmark.Item := CurrentItem;
  if CurrentItem <> nil then
    Bookmark.Order := CurrentItem.Order + FRecordNoOffset
  else
    Bookmark.Order := -1;
end;

procedure TMemData.SetToBookmark(Bookmark: PRecBookmark);
var
  OldCurrentItem: PItemHeader;
begin
  if (Bookmark.RefreshIteration = FRefreshIteration) and
    (Bookmark.Item <> nil)
  then begin
    OldCurrentItem := CurrentItem;
    try // for freed item
      CurrentItem := Bookmark.Item;
      if CurrentItem.Flag = flUsed then begin
        FBOF := False;
        FEOF := False;
        Exit;
      end
      else
        CurrentItem := OldCurrentItem;
    except
      CurrentItem := OldCurrentItem;
    end;
  end;

// Set by order
  inherited;
end;

function TMemData.BookmarkValid(Bookmark: PRecBookmark): boolean;
begin
  if Bookmark <> nil then
    Result := (Bookmark.Order <> -1) or (Bookmark.Item <> nil)
  else
    Result := False;
end;

function TMemData.CompareBookmarks(Bookmark1, Bookmark2: PRecBookmark): integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2,-1),(1,0));
begin
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then
    if Bookmark1.RefreshIteration = Bookmark2.RefreshIteration then
      if Bookmark1.Item = Bookmark2.Item then begin
        Result := 0;
        Exit;
      end;

// Compare by order
  Result := inherited CompareBookmarks(Bookmark1, Bookmark2);
end;

{ CachedUpdates }

function TMemData.GetUpdateStatus: TItemStatus;
begin
  if CurrentItem <> nil then
    Result := CurrentItem.Status
  else
    Result := isUnmodified;
end;

function TMemData.GetUpdateResult: TUpdateRecAction;
begin
  if CurrentItem <> nil then
    Result := CurrentItem.UpdateResult
  else
    Result := urNone;
end;

procedure TMemData.AddCacheItem(CacheItem: PCacheItem);
begin
// add to end cache
  CacheItem.Next := nil;
  if Cache = nil then
    Cache := CacheItem
  else
    LastCacheItem.Next := CacheItem;

  LastCacheItem := CacheItem;
end;

procedure TMemData.SetCacheRecBuf(NewBuf: pointer; OldBuf: pointer);
begin
  NewCacheRecBuf := NewBuf;
  OldCacheRecBuf := OldBuf;
end;

procedure TMemData.ApplyUpdates;
var
  CacheItem, NextCacheItem, PrevCacheItem: PCacheItem;
  Action: TUpdateRecAction;
  OldCurrentItem: PItemHeader;

  function ValidateCacheItem: boolean;
    { On case of deleting current item from cache via RevertRecord in
      ApplyRecord call. Returns True if CacheItem was deleted. }
  begin
    Result := True;
    if PrevCacheItem <> nil then begin
      if PrevCacheItem.Next = NextCacheItem then begin
        CacheItem := NextCacheItem;
        Result := True;
      end;
    end
    else
      if NextCacheItem <> nil then begin
        if Cache.Next <> NextCacheItem then begin
          CacheItem := NextCacheItem;
          Result := False;
        end;
      end
      else
        if Cache = nil then begin
          CacheItem := nil;
          Result := False;
        end;
  end;

begin
  if FCachedUpdates then begin
    OldCurrentItem := CurrentItem;
    try
      PrevCacheItem := nil;
      CacheItem := Cache;
      while CacheItem <> nil do
        if CacheItem.Item.UpdateResult <> urApplied then begin
          NextCacheItem := CacheItem.Next;
          try
            CurrentItem := CacheItem.Item; // for refresh on applied
            Action := urFail;
            try
              case CacheItem.Item.Status of
                isAppended: begin
                  BlockMan.GetRecord(CacheItem.Item, NewCacheRecBuf);
                  BlockMan.GetRecord(CacheItem.Item, OldCacheRecBuf);
                  ApplyRecord(ukInsert, Action);
                  BlockMan.PutRecord(CacheItem.Item, NewCacheRecBuf); // for ReturnParams
                end;
                isUpdated: begin
                  BlockMan.GetRecord(CacheItem.Item, NewCacheRecBuf);
                  BlockMan.GetRecord(CacheItem.Item.Rollback, OldCacheRecBuf);
                  ApplyRecord(ukUpdate, Action);
                  BlockMan.PutRecord(CacheItem.Item, NewCacheRecBuf); // for ReturnParams
                end;
                isDeleted: begin
                  BlockMan.GetRecord(CacheItem.Item, NewCacheRecBuf);
                  BlockMan.GetRecord(CacheItem.Item, OldCacheRecBuf);
                  ApplyRecord(ukDelete, Action);
                end;
              else
                Assert(False);
              end;
            finally
              if ValidateCacheItem then begin
                CacheItem.Item.UpdateResult := Action;
                if not (Action = urRetry) then begin
                  PrevCacheItem := CacheItem;
                  CacheItem := NextCacheItem;
                end;
              end;
            end;
          except
            if CacheItem <> nil then
              OldCurrentItem := CacheItem.Item; // failed item is current
            raise;
          end;
        end
        else
          CacheItem := CacheItem.Next;
    finally
      CurrentItem := OldCurrentItem;
    end;
  end;
end;

procedure TMemData.CommitUpdates;
var
  CacheItem,CacheItem1: PCacheItem;
begin
  if UpdatesPending then
    ApplyUpdates;

  CacheItem := Cache;
  LastCacheItem := nil;
  while CacheItem <> nil do
    if CacheItem.Item.UpdateResult = urApplied then begin
      if CacheItem.Item.Rollback <> nil then begin
        FreeComplexFields(PChar(CacheItem.Item.Rollback) + sizeof(TItemHeader), True);
        BlockMan.FreeItem(CacheItem.Item.Rollback);
        CacheItem.Item.Rollback := nil;
      end;

      if CacheItem.Item.Status = isDeleted then begin
        if IsComplexFields then
          FreeComplexFields(PChar(CacheItem.Item) + sizeof(TItemHeader), True);

        DeleteItem(CacheItem.Item)
      end
      else begin
        CacheItem.Item.Status := isUnmodified;
        CacheItem.Item.UpdateResult := urNone;
      end;

      CacheItem1 := CacheItem;
      CacheItem := CacheItem.Next;
      if CacheItem1 = Cache then
        Cache := CacheItem;
      Dispose(CacheItem1);
    end
    else begin
      CacheItem := CacheItem.Next;
      if CacheItem <> nil then
        LastCacheItem := CacheItem;
    end;
end;

procedure TMemData.RevertItem(Item: PItemHeader);
begin
  case Item.Status of
    isAppended: begin
      if IsComplexFields then
        FreeComplexFields(PChar(Item) + sizeof(TItemHeader), True);

      DeleteItem(Item);
    end;
    isUpdated: begin
      FreeComplexFields(PChar(Item) + sizeof(TItemHeader), True);
      BlockMan.CopyRecord(Item.Rollback, Item);
      BlockMan.FreeItem(Item.Rollback);
      Item.Rollback := nil;
      Item.Status := isUnmodified;
      Item.UpdateResult := urNone;
    end;
    isDeleted: begin
      Item.Status := isUnmodified;
      Item.UpdateResult := urNone;
    end;
    isUnmodified:
      Item.UpdateResult := urNone;
  end;
end;

procedure TMemData.RevertRecord;
var
  CacheItem: PCacheItem;
  OldCacheItem: PCacheItem;
begin
  if Cache <> nil then begin
    CacheItem := Cache;
    OldCacheItem := CacheItem;
    while (CacheItem <> nil) and (CacheItem.Item <> CurrentItem) do begin
      OldCacheItem := CacheItem;
      CacheItem := CacheItem.Next;
    end;
    if CacheItem <> nil then begin
      if OldCacheItem <> CacheItem then
        OldCacheItem.Next := CacheItem.Next
      else
        Cache := CacheItem.Next;

      RevertItem(CacheItem.Item);
      Dispose(CacheItem);
    end;
  end;
end;

procedure TMemData.CancelUpdates;
var
  CacheItem: PCacheItem;
begin
  if Cache <> nil then begin
    while Cache <> nil do begin
      RevertItem(Cache.Item);
      CacheItem := Cache;
      Cache := Cache.Next;
      Dispose(CacheItem);
    end;

    LastCacheItem := nil;

    ReorderItems(nil, roFull);
  end;
end;

procedure TMemData.RestoreUpdates;
var
  CacheItem: PCacheItem;
begin
  if FCachedUpdates then begin
    CacheItem := Cache;
    while CacheItem <> nil do begin
      //CacheItem.Item.Status
      CacheItem.Item.UpdateResult := urNone;
      CacheItem := CacheItem.Next;
    end;
  end;
end;

function TMemData.GetUpdatesPending: boolean;
var
  CacheItem:PCacheItem;
begin
  Result := False;
  CacheItem := Cache;
  while (CacheItem <> nil) and not Result do begin
    Result := CacheItem.Item.UpdateResult <> urApplied;
    CacheItem := CacheItem.Next;
  end;
end;

procedure TMemData.GetOldRecord(RecBuf: pointer);
begin
  if not(EOF or BOF or (CurrentItem = nil)) then begin
    if OmitRecord(CurrentItem) then
      GetNextRecord(RecBuf);
    if CurrentItem <> nil then
      if CurrentItem.Rollback <> nil then
        BlockMan.GetRecord(CurrentItem.Rollback, RecBuf)
      else
        BlockMan.GetRecord(CurrentItem, RecBuf);
  end;
end;

{ Filter }

procedure TMemData.FilterUpdated;
begin
  ReorderItems(nil, roFull);
  FEOF := RecordCount = 0; // for correct navigation
end;

procedure TMemData.SetFilterItemTypes(Value: TItemTypes);
begin
  if Value <> FilterItemTypes then begin
    inherited;

    ReorderItems(nil, roFull);
    FEOF := RecordCount = 0; // for correct navigation
  end;
end;

{ TBlockManager }

constructor TBlockManager.Create;
begin
  DefaultItemCount := 10;  // WAR
end;

destructor TBlockManager.Destroy;
begin
  FreeAllBlock;

  inherited;
end;

procedure TBlockManager.AllocBlock(var Block: PBlockHeader; ItemCount: word);
var
  BlockSize: integer;
begin
  BlockSize := sizeof(TBlockHeader) + ItemCount*(sizeof(TItemHeader) + RecordSize);

  GetMem(Block, BlockSize);

  Block.ItemCount := ItemCount;
  Block.UsedItems := ItemCount;

  Block.Next := FirstBlock;
  Block.Prev := nil;

  Block.Test := btSign;         // DEBUG

  if FirstBlock <> nil then
    FirstBlock.Prev := Block;
  FirstBlock := Block;
end;

procedure TBlockManager.FreeBlock(Block: PBlockHeader);
var
  BlockSize: integer;
begin
  if Block = FirstBlock then begin
    FirstBlock := Block.Next;
    if FirstBlock <> nil then
      FirstBlock.Prev := nil;
  end
  else begin
    Block.Prev.Next := Block.Next;
    if Block.Next <> nil then
      Block.Next.Prev := Block.Prev;
  end;

  BlockSize := sizeof(TBlockHeader) + Block.ItemCount*(sizeof(TItemHeader)
    + RecordSize);

  Assert(Block.Test = btSign);

  FreeMem(Block, BlockSize);
end;

procedure TBlockManager.FreeAllBlock;
begin
  while FirstBlock <> nil do
    FreeBlock(FirstBlock);

  FirstFree := nil;
end;

procedure TBlockManager.AddFreeBlock;
var
  Block:PBlockHeader;
  Item: PItemHeader;
  i: word;
begin
  AllocBlock(Block, DefaultItemCount);

  Item := Pointer(PChar(Block) + sizeof(TBlockHeader));
  for i := 1 to DefaultItemCount do begin
    Item.Prev := nil;
    Item.Next := FirstFree;
    Item.Block := Block;
    Item.Flag := flFree;

    if FirstFree <> nil then
      FirstFree.Prev := Item;
    FirstFree := Item;

    Item := Pointer(PChar(Item) + sizeof(TItemHeader) + RecordSize);
  end;
  Block.UsedItems := 0;
end;

procedure TBlockManager.AllocItem(var Item: PItemHeader);
begin
  if FirstFree = nil then
    AddFreeBlock;

  Item := FirstFree;

  Assert(Item.Flag = flFree);
  Item.Flag := flUsed;

  FirstFree := FirstFree.Next;
  if FirstFree <> nil then
    FirstFree.Prev := nil;

  Item.Rollback := nil;
  Item.Status := isUnmodified;
  Item.UpdateResult := urNone;
  Item.Order := 0;

  Inc(Item.Block.UsedItems);
end;

procedure TBlockManager.FreeItem(var Item: PItemHeader);
var
  Free: PItemHeader;
  i: integer;
begin
  Assert(Item.Flag = flUsed);

  Item.Flag := flFree;

  if Item.Block.UsedItems =  1 then begin
  // Procesing Free List
    Free := Pointer(PChar(Item.Block) + sizeof(TBlockHeader));
    for i := 1 to Item.Block.ItemCount do begin
      if Free <> Item then begin
        Assert(Free.Flag = flFree);

        if Free = FirstFree then begin
          FirstFree := Free.Next;
          if FirstFree <> nil then
            FirstFree.Prev := nil;
        end
        else begin
          Free.Prev.Next := Free.Next;
          if Free.Next <> nil then
            Free.Next.Prev := Free.Prev;
        end;
      end;
      Free := Pointer(PChar(Free) + sizeof(TItemHeader) + RecordSize);
    end;
    FreeBlock(Item.Block);
  end
  else begin
    Item.Prev := nil;
    Item.Next := FirstFree;
    if FirstFree <> nil then
      FirstFree.Prev := Item;
    FirstFree := Item;
    Dec(Item.Block.UsedItems);
  end;
end;

procedure TBlockManager.InitItem(Item: PItemHeader);
begin
  Item.Rollback := nil;
  Item.Status := isUnmodified;
  Item.UpdateResult := urNone;
end;

procedure TBlockManager.PutRecord(Item: PItemHeader; Rec: pointer);
begin
  Move(Rec^, (PChar(Item) + sizeof(TItemHeader))^, RecordSize);
end;

procedure TBlockManager.GetRecord(Item: PItemHeader; Rec: pointer);
begin
  Move((PChar(Item) + sizeof(TItemHeader))^, Rec^, RecordSize);
end;

function TBlockManager.GetRecordPtr(Item: PItemHeader): pointer;
begin
  Result := PChar(Item) + sizeof(TItemHeader);
end;

procedure TBlockManager.CopyRecord(ItemSrc: PItemHeader; ItemDest: PItemHeader);
begin
  Move((PChar(ItemSrc) + sizeof(TItemHeader))^, (PChar(ItemDest) + sizeof(TItemHeader))^, RecordSize);
end;

{ TStringHeap }

constructor TStringHeap.Create;
begin
  FRoot := nil;
  New(FSmallTab);
  FillChar(FSmallTab^, SizeOf(TSmallTab), 0);
  FEmpty := True;
  FSysGetMem := False;
end;

destructor TStringHeap.Destroy;
begin
  Clear;
  Dispose(FSmallTab);

  inherited;
end;

procedure TStringHeap.Clear;
var
  P, Temp: PBlock;
begin
  if Empty then
    Exit;
  P := FRoot;
  while P <> nil do begin
    Temp := P;
    P := P.Next;
    Dispose(Temp);
  end;
  FRoot := nil;
  FFree := SizeOf(TStrData);
  FillChar(FSmallTab^, SizeOf(TSmallTab), 0);
  FEmpty := True;
  FSysGetMem := False;
end;

function TStringHeap.AllocStr(Str: PChar; Trim: boolean = false): PChar;
var
  Len: integer;
  EndPtr: PChar;
begin
  if Str = nil then
    Result := nil
  else begin
    Len := StrLen(Str);
    if Trim then begin
      EndPtr := Str + Len - 1;
      while (EndPtr^ = ' ') and (Len > 0) do begin
        Dec(EndPtr);
        Dec(Len);
      end;
    end;
    Result := NewBuf(Len + 1);
    Move(Str^, Result^, Len);
    (Result + Len)^ := #0;
  end;
end;

function TStringHeap.AllocStr(Str: PWideChar; Trim: boolean = false): PWideChar;
var
  Len: integer;
  EndPtr: PWideChar;
begin
  if Str = nil then
    Result := nil
  else begin
    Len := StrLenW(Str);
    if Trim then begin
      EndPtr := Str + Len - 1;
      while (EndPtr^ = ' ') and (Len > 0) do begin
        Dec(EndPtr);
        Dec(Len);
      end;
    end;
    Result := NewBuf((Len + 1) * sizeof(WideChar));
    Move(Str^, Result^, Len * sizeof(WideChar));
    (Result + Len)^ := #0;
  end;
end;

function TStringHeap.ReAllocStr(Str: PChar; Trim: boolean = false): PChar;
begin
  Result := AllocStr(Str, Trim);
  DisposeBuf(Str);
end;

function TStringHeap.ReAllocStr(Str: PWideChar; Trim: boolean = false): PWideChar;
begin
  Result := AllocStr(Str, Trim);
  DisposeBuf(Str);
end;

function TStringHeap.NewBuf(Size: integer): pointer;
var
  P: PChar;
  Temp: PBlock;
  PP: ^PChar;
  divSize: integer;
begin
  if Size <= 0 then begin
    Result := nil;
  end
  else begin
    FEmpty := False;
    divSize := (Size + Align - 1) div Align;
    if divSize <= SmallSize div Align then begin
      Result := FSmallTab[divSize];
      if Result <> nil then begin
        FSmallTab[divSize] := PChar(Pointer(Result)^);
        Inc(Word(Pointer(PChar(Result) - SizeOf(Word))^));
        Exit;
      end;
      Size := divSize * Align;
      if FRoot = nil then begin
        New(FRoot);
        FRoot.Next := nil;
        FFree := SizeOf(TStrData);
      end
      else
      if FFree < Size + SizeOf(Integer) + SizeOf(Word) then begin
        P := PChar(@FRoot.Data) + SizeOf(TStrData) - FFree;
        divSize := (FFree - SizeOf(Integer) - SizeOf(Word)) div Align;
        Integer(Pointer(P)^) := divSize * Align;
        Inc(P, SizeOf(Integer));
        Word(Pointer(P)^) := RefNull;
        Inc(P, SizeOf(Word));
        PP := @FSmallTab[divSize];
        PChar(Pointer(P)^) := PP^;
        PP^ := P;
        Temp := FRoot;
        New(FRoot);
        FRoot.Next := Temp;
        FFree := SizeOf(TStrData);
      end;
      Result := PChar(@FRoot.Data) + SizeOf(TStrData) - FFree;
      Integer(Pointer(Result)^) := Size;
      Dec(FFree, Size + SizeOf(Integer) + SizeOf(Word));
      if FFree < SizeOf(Integer) + SizeOf(Word) + Align then begin
        Inc(Integer(Pointer(Result)^), FFree and not (Align - 1));
        Temp := FRoot;
        New(FRoot);
        FRoot.Next := Temp;
        FFree := SizeOf(TStrData);
      end;
      Inc(PChar(Result), SizeOf(Integer));
    end
    else begin
      GetMem(Result, Size + SizeOf(Word));
      FSysGetMem := True;
    end;
    Word(Pointer(Result)^) := RefNull;
    Inc(PChar(Result), SizeOf(Word));
  end;
end;

procedure TStringHeap.DisposeBuf(Buf: pointer);
var
  Size: integer;
  PRefCount: pointer;
  PP: ^PChar;
  divSize: integer;
begin
  if (Buf <> nil) then begin
    PRefCount := Pointer(PChar(Buf) - SizeOf(Word));
    Assert(Word(PRefCount^) >= RefNull, 'DisposeBuf failed');
    if Word(PRefCount^) = RefNull then begin
      Dec(Word(PRefCount^));
      Size := Integer(Pointer(PChar(PRefCount) - SizeOf(Integer))^);
      divSize := (Size + Align - 1) div Align;
      if (divSize <= SmallSize div Align) then begin
        PP := @FSmallTab[divSize];
        PChar(Pointer(Buf)^) := PP^;
        PP^ := Buf;
      end
      else begin
        FreeMem(PRefCount);
      end;
    end
    else
      Dec(Word(PRefCount^));
  end;
end;

procedure TStringHeap.AddRef(Buf: pointer);
var
  PRefCount: pointer;
begin
  if (Buf <> nil) then begin
    PRefCount := Pointer(PChar(Buf) - SizeOf(Word));
    Assert(Word(PRefCount^) >= RefNull, 'AddRefStr failed');
    Inc(Word(PRefCount^));
  end;
end;

{ TShareObject }

constructor TSharedObject.Create;
begin
  AddRef;

{$IFDEF DEBUG} Inc(ShareObjectCnt); {$ENDIF}
end;

destructor TSharedObject.Destroy;
begin
  {$IFDEF DEBUG} Dec(ShareObjectCnt); {$ENDIF}
  FRefCount := 0;

  inherited;
end;

procedure TSharedObject.CheckValid;
begin
  if FRefCount = 0 then
    raise Exception.Create(SInvalidSharedObject);
end;

procedure TSharedObject.Free;
begin
  if Assigned(Self) then begin
    Assert(FRefCount > 0);

    if FRefCount = 1 then
      inherited Free
    else
      Dec(FRefCount);
  end;
end;

procedure TSharedObject.AddRef;
begin
  Inc(FRefCount);
end;

procedure TSharedObject.Release;
begin
  Free;
end;

{ TPiece }

function NextPiece(Piece: PPieceHeader): PPieceHeader;
begin
  if Piece <> nil then
    Result := Piece.Next
  else
    Result := nil;
end;

function PieceData(Piece: PPieceHeader): pointer;
begin
  if Piece <> nil then
    Result := PChar(Piece) + sizeof(TPieceHeader)
  else
    Result := nil;
end;

function PieceUsedPtr(Piece: PPieceHeader): pointer;
begin
  if Piece <> nil then
    Result := @Piece.Used
  else
    Result := nil;
end;

{ TBlob }

constructor TBlob.Create(IsUnicode: boolean = False);
begin
  inherited Create;

  FIsUnicode := IsUnicode;
  PieceSize := DefaultPieceSize;
  Test := btSign;                    // DEBUG
end;

destructor TBlob.Destroy;
begin
  CheckValid;   // DEBUG
  Test := 0;    // DEBUG
  
  FNeedRollback := False;
  Clear;

  if Rollback <> nil then
    Rollback.Free;

  inherited;
end;

procedure TBlob.CheckValid;
begin
  if Test <> btSign then                    // DEBUG
    raise Exception.Create(SInvalidBlob);
end;

procedure TBlob.Clear;
var
  Piece: PPieceHeader;
begin
  if FNeedRollback and (Rollback = nil) then
    SaveToRollback;

  while FFirstPiece <> nil do begin
    Piece := FFirstPiece;
    FFirstPiece := FFirstPiece.Next;
    FreeMem(Piece);
  end;
end;

{ Pieces }

procedure TBlob.AllocPiece(var Piece: PPieceHeader; Size: cardinal);
begin
  Assert(Size > 0);
  GetMem(Piece, sizeof(TPieceHeader) + Size);
  Piece.Blob := nil;
  Piece.Size := Size;
  Piece.Used := 0;
  Piece.Prev := nil;
  Piece.Next := nil;
end;

procedure TBlob.ReallocPiece(var Piece: PPieceHeader; Size: cardinal);
begin
  if Size = 0 then begin
    FreePiece(Piece);
    Piece := nil;
  end
  else begin
    ReallocMem(Piece, sizeof(TPieceHeader) + Size);
    Piece.Size := Size;
    if Piece.Used > Size then
      Piece.Used := Size;
    if Piece.Blob <> nil then begin
      if Piece.Prev <> nil then
        Piece.Prev.Next := Piece
      else
        FFirstPiece := Piece;

      if Piece.Next <> nil then
        Piece.Next.Prev := Piece;
    end;
  end;
end;

procedure TBlob.FreePiece(Piece: PPieceHeader);
begin
  if Piece.Blob <> nil then
    DeletePiece(Piece);

  FreeMem(Piece);
end;

procedure TBlob.AppendPiece(Piece: PPieceHeader);
var
  Last: PPieceHeader;
begin
  Piece.Blob := Self;
  Piece.Next := nil;
  if FFirstPiece = nil then begin
    Piece.Prev := nil;
    FFirstPiece := Piece;
  end
  else begin
    Last := FFirstPiece;
    while Last.Next <> nil do
      Last := Last.Next;
    Last.Next := Piece;
    Piece.Prev := Last;
  end;
end;

procedure TBlob.DeletePiece(Piece: PPieceHeader);
begin
  Assert(Piece.Blob = Self);

  if FFirstPiece = Piece then begin
    FFirstPiece := Piece.Next;
    if FFirstPiece <> nil then
      FFirstPiece.Prev := nil;
  end
  else
  begin
    Piece.Prev.Next := Piece.Next;
    if Piece.Next <> nil then
      Piece.Next.Prev := Piece.Prev;
  end;

  Piece.Blob := nil;
end;

procedure TBlob.CompressPiece(var Piece: PPieceHeader);
begin
  if Piece.Used < Piece.Size then
    ReallocPiece(Piece, Piece.Used);
end;

function TBlob.FirstPiece: PPieceHeader;
begin
  Result := FFirstPiece;
end;

procedure TBlob.CheckValue;
begin
end;

function TBlob.Read(Position: cardinal; Count: cardinal; Dest: pointer): cardinal;
var
  Piece: PPieceHeader;
  Pos, { hift from Blob begin }
  Shift, { for read, in Piece }
  ReadCount, { all }
  MoveSize: cardinal; { in Piece }
begin
  CheckValid;   // DEBUG

  CheckValue;

  Result := 0;

  if Count = 0 then
    Count := Size;

  if (FFirstPiece = nil) or (Position > Size) then
    Exit;

  Piece := FFirstPiece;
  ReadCount := 0;
  Pos := 0;
  while (Piece <> nil) and (Pos < (Position + Count)) do begin
    if Pos + Piece.Used > Position then begin
      if Position > Pos then
        Shift := Position - Pos
      else
        Shift := 0;

      if (Pos + Piece.Used) > (Position + Count) then
        MoveSize := (Position + Count) - (Pos + Shift)
      else
        MoveSize := Piece.Used - Shift;

      Move((PChar(Piece) + sizeof(TPieceHeader) + Shift)^,
          (PChar(Dest) + ReadCount)^ , MoveSize);
      Inc(ReadCount, MoveSize);
    end;
    Inc(Pos, Piece.Used);
    Piece := Piece.Next;
  end;
  Result := ReadCount;
end;

{ similar to Read }

procedure TBlob.Write(Position: cardinal; Count: cardinal; Source: pointer);
var
  Piece: PPieceHeader;
  Pos, { shift from Blob begin }
  Shift, { for write, in Piece }
  WriteCount, { all }
  MoveSize: cardinal; { in Piece }
begin
  CheckValid;   // DEBUG

  if FNeedRollback and (Rollback = nil) then
    SaveToRollback;

  if (Position > Size) then
    Position := Size;

  Piece := FFirstPiece;
  WriteCount := 0;
  Pos := 0;
  while (Pos < (Position + Count)) do begin
    if Piece = nil then begin
      if Count > PieceSize then
        AllocPiece(Piece, PieceSize)
      else
        AllocPiece(Piece, Count);
      AppendPiece(Piece);
    end;

    if Pos + Piece.Size > Position then begin
      if Position > Pos then
        Shift := Position - Pos
      else
        Shift := 0;

      if (Pos + Piece.Size) > (Position + Count) then
        MoveSize := (Position + Count) - (Pos + Shift)
      else
        MoveSize := Piece.Size - Shift;

      Move((PChar(Source) + WriteCount)^,
        (PChar(Piece) + sizeof(TPieceHeader) + Shift)^,
        MoveSize);
      Inc(WriteCount, MoveSize);

      Assert(Shift <= Piece.Used);
      if (Shift + MoveSize) > Piece.Used then
        Piece.Used := Shift + MoveSize;
    end;
    Inc(Pos, Piece.Used);
    Piece := Piece.Next;
  end;
end;

procedure TBlob.Truncate(NewSize: cardinal);
var
  Piece: PPieceHeader;
  Size: cardinal;
begin
  if FNeedRollback and (Rollback = nil) then
    SaveToRollback;

  if NewSize = 0 then
    Clear
  else begin
    Size := 0;
    Piece := FirstPiece;
    while Piece <> nil do begin
      if Size + Piece.Used > NewSize then
        Piece.Used := NewSize - Size;
      Inc(Size, Piece.Used);
      Piece := Piece.Next;
    end;
  end;
end;

procedure TBlob.Compress;
var
  Piece: PPieceHeader;
  NextPiece: PPieceHeader;
begin
  Piece := FirstPiece;
  while Piece <> nil do begin
    NextPiece := Piece.Next;
    CompressPiece(Piece);
    Piece := NextPiece;
  end;
end;

procedure TBlob.Defrag; // Move all data to first piece
var
  pc: PChar;
  Piece: PPieceHeader;
  NextPiece: PPieceHeader;
begin
  if FirstPiece = nil then
    Exit; // Is empty

  ReallocPiece(FFirstPiece, Size);
  pc := PChar(FFirstPiece) + sizeof(TPieceHeader) + FFirstPiece.Used;

  Piece := FFirstPiece.Next;
  while Piece <> nil do begin
    Move((PChar(Piece) + sizeof(TPieceHeader))^, pc^, Piece.Used);
    Inc(pc, Piece.Used);
    Inc(FFirstPiece.Used, Piece.Used);

    NextPiece := Piece.Next;
    FreePiece(Piece);
    Piece := NextPiece;
  end;
end;

{ Stream/File }

procedure TBlob.LoadFromStream(Stream:TStream);
var
  Piece: PPieceHeader;
  Remainder: cardinal;
  BufLen: cardinal;
begin
  Clear;

  Stream.Seek(0, soFromBeginning);

  Remainder := Stream.Size;
  while Remainder > 0 do begin
    if Remainder > PieceSize then
      BufLen := PieceSize
    else
      BufLen := Remainder;

    AllocPiece(Piece, BufLen);
    Stream.Read(Pointer(PChar(Piece) + Sizeof(TPieceHeader))^, BufLen);
    Piece.Used := BufLen;
    AppendPiece(Piece);

    Dec(Remainder, BufLen);
  end;
end;

procedure TBlob.SaveToStream(Stream:TStream);
var
  Piece: PPieceHeader;
  BufLen: cardinal;
begin
  Stream.Size := 0;

  Piece := FirstPiece;

  while Piece <> nil do begin
    BufLen := Piece.Used;

    Stream.Write(Pointer(PChar(Piece) + Sizeof(TPieceHeader))^, BufLen);

    Piece := Piece.Next;
  end;
end;

procedure TBlob.LoadFromFile(const FileName: string);
var
  Stream:TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBlob.SaveToFile(const FileName: string);
var
  Stream:TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TBlob.Assign(Source: TBlob);
const
  BufSize = 65536;
var
  Buf: array [0..BufSize - 1] of byte;
  Pos: cardinal;
  Size: cardinal;
begin
  Clear;

  Pos := 0;
  repeat
    Size := Source.Read(Pos, BufSize, @Buf);
    if Size > 0 then begin
      Write(Pos, Size, @Buf);
      Inc(Pos, Size);
    end;
  until Size = 0;
end;

{ Cached }

procedure TBlob.CheckCached;
begin
  if not FNeedRollback then
    raise Exception.Create(SBlobMustBeCached);
end;

procedure TBlob.SaveToRollback;
begin
  CheckCached;

  Rollback := TBlob.Create;
  Rollback.FIsUnicode := FIsUnicode;

  Rollback.FFirstPiece := FFirstPiece;

  FFirstPiece := nil;
end;

procedure TBlob.EnableRollback;
begin
  {if FNeedRollback then
    raise Exception.Create(SCachedAlreadyEnabled);}

  FNeedRollback := True;
end;

procedure TBlob.Commit;
begin
  //CheckCached;

  if Rollback <> nil then begin
    Rollback.Free;
    Rollback := nil;
  end;

  FNeedRollback := False;
end;

procedure TBlob.Cancel;
var
  Piece: PPieceHeader;
begin
  //CheckCached;

  if Rollback <> nil then begin
    Piece := Rollback.FFirstPiece;
    Rollback.FFirstPiece := FFirstPiece;
    FFirstPiece := Piece;

    Rollback.Free;
    Rollback := nil;
  end;

  FNeedRollback := False;
end;

function TBlob.CanRollback: boolean;
begin
  Result := Rollback <> nil;
end;

function TBlob.GetSize: cardinal;
var
  Piece: PPieceHeader;
begin
  Result := 0;
  Piece := FFirstPiece;
  while Piece <> nil do begin
    Inc(Result, Piece.Used);
    Piece := Piece.Next;
  end;
end;

procedure TBlob.SetIsUnicode(Value: boolean);
begin
  if Value = IsUnicode then
    Exit;

  if Size > 0 then
    DataError(SCannotChangeIsUnicode);

  FIsUnicode := Value;
end;

function TBlob.GetAsString: string;
var
  ws: WideString;
begin
  if not FIsUnicode then begin
    System.SetLength(Result, Size);
    Read(0, 0, PChar(Result));
  end
  else
  begin
    System.SetLength(ws, Size shr 1);
    Read(0, 0, PWideChar(ws));
    Result := ws;
  end;
end;

procedure TBlob.SetAsString(Value: string);
var
  ws: WideString;
begin
  Clear;

  if not FIsUnicode then
    Write(0, System.Length(Value), PChar(Value))
  else
  begin
    ws := Value;
    Write(0, System.Length(ws) shl 1, PWideChar(ws));
  end;
end;

function TBlob.GetAsWideString: WideString;
var
  s: string;
begin
  if not FIsUnicode then begin
    System.SetLength(s, Size);
    Read(0, 0, PChar(s));
    Result := s;
  end
  else
  begin
    Assert(Size mod 2 = 0); // WideString must have even Size
    System.SetLength(Result, Size shr 1);
    Read(0, 0, PWideChar(Result));
  end;
end;

procedure TBlob.SetAsWideString(Value: WideString);
var
  s: string;
begin
  Clear;
  if not FIsUnicode then begin
    s := Value;
    Write(0, System.Length(s), PChar(s));
  end
  else
    Write(0, System.Length(Value) shl 1, PWideChar(Value));
end;

procedure TBlob.AddCR;
begin
  if FIsUnicode then
    AddCR_Unicode
  else
    AddCR_String;
end;

procedure TBlob.RemoveCR;
begin
  if FIsUnicode then
    RemoveCR_Unicode
  else
    RemoveCR_String;
end;

procedure TBlob.AddCR_String;
var
  SourcePiece: PPieceHeader;
  DestPiece: PPieceHeader;
  LastPiece: PPieceHeader;
  FirstPiece: PPieceHeader;
  TempPiece: PPieceHeader;

  Source: PChar;
  SourceStart: PChar;
  Dest: PChar;
  DestEnd: PChar;
  SourceEnd: PChar;

  Shift: cardinal;
  Used: cardinal;
  w: word;
  b: byte;
  c: char;

  procedure AllocDestPiece;
  var
    AUsed: cardinal;
  begin
    AUsed := Used + SourceStart - Source;
    if Dest <> nil then
      DestPiece.Used := DestPiece.Size - 1 + Dest - DestEnd;
    if AUsed < PieceSize div 2 then
      AllocPiece(DestPiece, AUsed * 2)
    else
      AllocPiece(DestPiece, PieceSize);
    Dest := PChar(DestPiece) + SizeOf(TPieceHeader);
    DestEnd := Dest + DestPiece.Size - 1;
    DestPiece.Blob := Self;
    DestPiece.Prev := LastPiece;
    if LastPiece <> nil then
      LastPiece.Next := DestPiece;
    LastPiece := DestPiece;
    if FirstPiece = nil then
      FirstPiece := DestPiece;
  end;

begin
  CheckValid;   // DEBUG

  CheckValue;

  if (FFirstPiece = nil) then
    Exit;

  SourcePiece := FFirstPiece;
  FirstPiece := nil;
  LastPiece := nil;
  DestPiece := nil;
  Dest := nil;
  DestEnd := nil;
  Shift := 0;
  Used := Size;

  while (SourcePiece <> nil) do begin
    if SourcePiece.Used > Shift then begin
      SourceStart := PChar(SourcePiece) + SizeOf(TPieceHeader) + Shift;
      Source := SourceStart;
      SourceEnd := Source + SourcePiece.Used - 1 - Shift;

      while Source < SourceEnd do begin
        if Dest >= DestEnd then
          AllocDestPiece;
        w := Word(Pointer(Source)^);
        if w = CRLF then begin
          Word(Pointer(Dest)^) := w;
          Inc(Source, 2);
          Inc(Dest, 2);
        end
        else begin
          b := Lo(w);
          if b = LF then begin
            Word(Pointer(Dest)^) := CRLF;
            Inc(Source);
            Inc(Dest, 2);
          end
          else begin
            Dest^ := Char(b);
            Inc(Source);
            Inc(Dest);
          end;
        end;
      end;

      if Source = SourceEnd then begin
        c := Source^;
        if Dest >= DestEnd then
          AllocDestPiece;
        Shift := Ord((c = #13) and (SourcePiece.Next <> nil)
          and ((PChar(SourcePiece.Next) + SizeOf(TPieceHeader))^ = #10));
        if Shift = 1 then begin
          Word(Pointer(Dest)^) := CRLF;
          Inc(Dest, 2);
        end
        else begin
          Dest^ := c;
          Inc(Dest);
        end;
      end else
        Shift := 0;
    end;
    Dec(Used, SourcePiece.Used);
    TempPiece := SourcePiece;
    SourcePiece := SourcePiece.Next;
    FreeMem(TempPiece);
  end;
  if Dest <> nil then
    DestPiece.Used := DestPiece.Size - 1 + Dest - DestEnd;
  FFirstPiece := FirstPiece;
end;

procedure TBlob.RemoveCR_String;
var
  SourcePiece: PPieceHeader;
  DestPiece: PPieceHeader;
  LastPiece: PPieceHeader;
  FirstPiece: PPieceHeader;
  TempPiece: PPieceHeader;

  SourceStart: PChar;
  Source: PChar;
  Dest: PChar;
  DestEnd: PChar;
  SourceEnd: PChar;

  Shift: cardinal;
  Used: cardinal;
  w: word;
  c: char;

  procedure AllocDestPiece;
  var
    AUsed: cardinal;
  begin
    AUsed := Used + SourceStart - Source;
    if Dest <> nil then
      DestPiece.Used := DestPiece.Size + Dest - DestEnd;
    if AUsed < PieceSize then
      AllocPiece(DestPiece, AUsed)
    else
      AllocPiece(DestPiece, PieceSize);
    Dest := PChar(DestPiece) + SizeOf(TPieceHeader);
    DestEnd := Dest + DestPiece.Size;
    DestPiece.Blob := Self;
    DestPiece.Prev := LastPiece;
    if LastPiece <> nil then
      LastPiece.Next := DestPiece;
    LastPiece := DestPiece;
    if FirstPiece = nil then
      FirstPiece := DestPiece;
  end;

begin
  CheckValid;   // DEBUG

  CheckValue;

  if (FFirstPiece = nil) then
    Exit;

  SourcePiece := FFirstPiece;
  FirstPiece := nil;
  LastPiece := nil;
  DestPiece := nil;
  Dest := nil;
  DestEnd := nil;
  Shift := 0;
  Used := Size;

  while (SourcePiece <> nil) do begin
    if SourcePiece.Used > Shift then begin
      SourceStart := PChar(SourcePiece) + SizeOf(TPieceHeader) + Shift;
      Source := SourceStart;
      SourceEnd := Source + SourcePiece.Used - 1 - Shift;

      while Source < SourceEnd do begin
        if Dest >= DestEnd then
          AllocDestPiece;
        w := Word(Pointer(Source)^);
        if w = CRLF then begin
          Dest^ := Char(LF);
          Inc(Source, 2);
          Inc(Dest);
        end
        else
        begin
          Dest^ := Char(Lo(w));
          Inc(Source);
          Inc(Dest);
        end;
      end;

      if Source = SourceEnd then begin
        c := Source^;
        if Dest >= DestEnd then
          AllocDestPiece;
        Shift := Ord((c = #13) and (SourcePiece.Next <> nil)
          and ((PChar(SourcePiece.Next) + SizeOf(TPieceHeader))^ = #10));
        if Shift = 1 then
          c := #10;
        Dest^ := c;
        Inc(Dest);
      end else
        Shift := 0;
    end;
    Dec(Used, SourcePiece.Used);
    TempPiece := SourcePiece;
    SourcePiece := SourcePiece.Next;
    FreeMem(TempPiece);
  end;
  if Dest <> nil then
    DestPiece.Used := DestPiece.Size + Dest - DestEnd;
  FFirstPiece := FirstPiece;
end;

procedure TBlob.AddCR_Unicode;
var
  SourcePiece: PPieceHeader;
  DestPiece: PPieceHeader;
  LastPiece: PPieceHeader;
  FirstPiece: PPieceHeader;
  TempPiece: PPieceHeader;

  Source: PWideChar;
  SourceStart: PWideChar;
  Dest: PWideChar;
  DestEnd: PWideChar;
  SourceEnd: PWideChar;

  Shift: cardinal; //bytes
  Used: cardinal; //bytes
  w: LongWord;
  b: Word;
  c: WideChar;
  procedure AllocDestPiece;
  var
    AUsed: cardinal;
  begin
    AUsed := Used + PChar(SourceStart) - PChar(Source);
    if Dest <> nil then
      DestPiece.Used := DestPiece.Size - sizeof(WideChar) + PChar(Dest) - PChar(DestEnd);
    if AUsed < PieceSize div 2 then
      AllocPiece(DestPiece, AUsed * 2)
    else
      AllocPiece(DestPiece, PieceSize);
    PChar(Dest) := PChar(DestPiece) + SizeOf(TPieceHeader);
    PChar(DestEnd) := PChar(Dest) + DestPiece.Size - sizeof(WideChar);
    DestPiece.Blob := Self;
    DestPiece.Prev := LastPiece;
    if LastPiece <> nil then
      LastPiece.Next := DestPiece;
    LastPiece := DestPiece;
    if FirstPiece = nil then
      FirstPiece := DestPiece;
  end;

begin
  CheckValid;   // DEBUG

  CheckValue;

  if (FFirstPiece = nil) then
    Exit;

  SourcePiece := FFirstPiece;
  FirstPiece := nil;
  LastPiece := nil;
  DestPiece := nil;
  Dest := nil;
  DestEnd := nil;
  Shift := 0;
  Used := Size;

  while (SourcePiece <> nil) do begin
    if SourcePiece.Used > Shift then begin
      PChar(SourceStart) := PChar(SourcePiece) + SizeOf(TPieceHeader) + Shift;
      Source := SourceStart;
      PChar(SourceEnd) := PChar(Source) + SourcePiece.Used - sizeof(WideChar) - Shift;

      while Source < SourceEnd do begin
        if Dest >= DestEnd then
          AllocDestPiece;
        w := LongWord(Pointer(Source)^);
        if w = CRLF_UTF16 then begin
          LongWord(Pointer(Dest)^) := w;
          Inc(Source, 2);
          Inc(Dest, 2);
        end
        else begin
          b := Word(w);
          if b = LF_UTF16 then begin
            LongWord(Pointer(Dest)^) := CRLF_UTF16;
            Inc(Source);
            Inc(Dest, 2);
          end
          else begin
            Dest^ := WideChar(b);
            Inc(Source);
            Inc(Dest);
          end;
        end;
      end;

      if Source = SourceEnd then begin
        c := Source^;
        if Dest >= DestEnd then
          AllocDestPiece;
        Shift := Ord(
          (c = #13) and (SourcePiece.Next <> nil) and
          (PWideChar(PChar(SourcePiece.Next) + SizeOf(TPieceHeader))^ = #10)
        ) * sizeof(WideChar);
        if Shift = sizeof(WideChar) then begin
          LongWord(Pointer(Dest)^) := CRLF_UTF16;
          Inc(Dest, 2);
        end
        else begin
          Dest^ := c;
          Inc(Dest);
        end;
      end else
        Shift := 0;
    end;
    Dec(Used, SourcePiece.Used);
    TempPiece := SourcePiece;
    SourcePiece := SourcePiece.Next;
    FreeMem(TempPiece);
  end;
  if Dest <> nil then
    DestPiece.Used := DestPiece.Size - sizeof(WideChar) + PChar(Dest) - PChar(DestEnd);
  FFirstPiece := FirstPiece;
end;

procedure TBlob.RemoveCR_Unicode;
var
  SourcePiece: PPieceHeader;
  DestPiece: PPieceHeader;
  LastPiece: PPieceHeader;
  FirstPiece: PPieceHeader;
  TempPiece: PPieceHeader;

  SourceStart: PWideChar;
  Source: PWideChar;
  Dest: PWideChar;
  DestEnd: PWideChar;
  SourceEnd: PWideChar;

  Shift: cardinal; //bytes
  Used: cardinal;  //bytes
  w: LongWord;
  c: WideChar;

  procedure AllocDestPiece;
  var
    AUsed: cardinal;
  begin
    AUsed := Used + PChar(SourceStart) - PChar(Source);
    if Dest <> nil then
      DestPiece.Used := DestPiece.Size + PChar(Dest) - PChar(DestEnd);
    if AUsed < PieceSize then
      AllocPiece(DestPiece, AUsed)
    else
      AllocPiece(DestPiece, PieceSize);
    PChar(Dest) := PChar(DestPiece) + SizeOf(TPieceHeader);
    PChar(DestEnd) := PChar(Dest) + DestPiece.Size;
    DestPiece.Blob := Self;
    DestPiece.Prev := LastPiece;
    if LastPiece <> nil then
      LastPiece.Next := DestPiece;
    LastPiece := DestPiece;
    if FirstPiece = nil then
      FirstPiece := DestPiece;
  end;

begin
  CheckValid;   // DEBUG

  CheckValue;

  if (FFirstPiece = nil) then
    Exit;

  SourcePiece := FFirstPiece;
  FirstPiece := nil;
  LastPiece := nil;
  DestPiece := nil;
  Dest := nil;
  DestEnd := nil;
  Shift := 0;
  Used := Size;

  while (SourcePiece <> nil) do begin
    if SourcePiece.Used > Shift then begin
      PChar(SourceStart) := PChar(SourcePiece) + SizeOf(TPieceHeader) + Shift;
      Source := SourceStart;
      PChar(SourceEnd) := PChar(Source) + SourcePiece.Used - sizeof(WideChar) - Shift;

      while Source < SourceEnd do begin
        if Dest >= DestEnd then
          AllocDestPiece;
        w := LongWord(Pointer(Source)^);
        if w = CRLF_UTF16 then begin
          Dest^ := WideChar(LF_UTF16);
          Inc(Source, 2);
          Inc(Dest);
        end
        else
        begin
          Dest^ := WideChar(Word(w));
          Inc(Source);
          Inc(Dest);
        end;
      end;

      if Source = SourceEnd then begin
        c := Source^;
        if Dest >= DestEnd then
          AllocDestPiece;
        Shift := Ord(
          (c = #13) and (SourcePiece.Next <> nil) and
          (PWideChar(PChar(SourcePiece.Next) + SizeOf(TPieceHeader))^ = #10)
        ) * sizeof(WideChar);
        if Shift = sizeof(WideChar) then
          c := #10;
        Dest^ := c;
        Inc(Dest);
      end else
        Shift := 0;
    end;
    Dec(Used, SourcePiece.Used);
    TempPiece := SourcePiece;
    SourcePiece := SourcePiece.Next;
    FreeMem(TempPiece);
  end;
  if Dest <> nil then
    DestPiece.Used := DestPiece.Size + PChar(Dest) - PChar(DestEnd);
  FFirstPiece := FirstPiece;
end;

var
  CompareField: integer;
  CompareData: TMemData;

function CompareItems(Item1, Item2: Pointer): Integer;
var
  value1: variant;
  value2: variant;
begin
  with CompareData do begin
    GetFieldAsVariant(CompareField, BlockMan.GetRecordPtr(Item1), value1);
    GetFieldAsVariant(CompareField, BlockMan.GetRecordPtr(Item2), value2);
  end;
  if value1 = value2 then
    result := 0
  else if value1 < value2 then
    result := -1
  else
    result := 1;
end;

procedure TMemData.Sort(const FieldName: string);
var
  list: TList;
  item: PItemHeader;
  i: integer;
  FieldDesc: TFieldDesc;
begin
  item := FirstItem;
  list := TList.Create;
  FieldDesc := FindField(FieldName);
  CompareField := FieldDesc.FieldNo;
  CompareData := self;
  try
    while item <> nil do begin
      list.Add(item);
      item := item.Next;
    end;
    if list.Count > 1 then begin
      list.Sort(CompareItems);
      FirstItem := list[0];
      FirstItem.Prev := nil;
      FirstItem.Next := list[1];
      for i := 1 to list.Count - 2 do begin
        item := list[i];
        item.Prev := list[i-1];
        item.Next := list[i+1];
      end;
      LastItem := list[list.Count - 1];
      LastItem.Prev := list[list.Count - 2];
      LastItem.Next := nil;
      CurrentItem := FirstItem;
      ReorderItems(CurrentItem, roFull);
    end;
  finally
    list.Free;
  end;
end;

const
{$IFNDEF VER130}
  DataTypeMap: array [TFieldType] of word = (
    // ftUnknown, ftString, ftSmallint, ftInteger, ftWord
    dtUnknown, dtString, dtInteger, dtInteger, dtInteger,
    // ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
    dtBoolean, dtFloat, dtCurrency, dtUnknown, dtDate, dtTime, dtDateTime,
    // ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    dtBytes, dtVarBytes, dtInteger, dtBlob, dtMemo, dtGraphic, dtFmtMemo,
    // ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    0, 0, 0, 0, dtString, dtWideString,
    // ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    dtInt64, dtObject, dtArray, dtReference, dtTable, 0, 0,
    // ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd
    dtVariant, 0, 0, 0, 0, 0 {$IFDEF VER180}, 0, 0, 0, 0{$ENDIF});
{$ELSE}
  DataTypeMap: array [TFieldType] of word = (
    // ftUnknown, ftString, ftSmallint, ftInteger, ftWord
    dtUnknown, dtString, dtInteger, dtInteger, dtInteger,
    // ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime,
    dtBoolean, dtFloat, dtCurrency, dtUnknown, dtDate, dtTime, dtDateTime,
    // ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    dtBytes, dtVarBytes, dtInteger, dtBlob, dtMemo, dtGraphic, dtFmtMemo,
    // ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    0, 0, 0, 0, dtString, dtWideString,
    // ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    dtInt64, dtObject, dtArray, dtReference, dtTable, 0, 0,
    // ftVariant, ftInterface, ftIDispatch, ftGuid
    dtVariant, 0, 0, 0);
{$ENDIF}

function GetFieldType(DataType: word): TFieldType;
begin
  case DataType of
    dtUnknown:
      Result := ftUnknown;
    dtString:
      Result := ftString;
    dtWideString:
      Result := ftWideString;
    dtInt16:
      Result := ftSmallint;
    dtInteger:
      Result := ftInteger;
    dtUInt32:
      Result := ftLargeInt;
    dtFloat:
      Result := ftFloat;
    dtDate:
      Result := ftDate;
    dtTime:
      Result := ftTime;
    dtDateTime:
      Result := ftDateTime;
    dtMemo:
      Result := ftMemo;
    dtBlob:
      Result := ftBlob;
    dtObject:
      Result := ftADT;
    dtReference:
      Result := ftReference;
    dtArray:
      Result := ftArray;
    dtTable:
      Result := ftDataSet;
    dtBoolean:
      Result := ftBoolean;
    dtVariant:
      Result := ftVariant;
    dtExtString:
      Result := ftString;
    dtExtWideString:
      Result := ftWideString;
    dtBytes:
      Result := ftBytes;
    dtVarBytes:
      Result := ftVarBytes;
    dtExtVarBytes:
      Result := ftVarBytes;
    dtGraphic:
      Result := ftGraphic;
    dtFmtMemo:
      Result := ftFmtMemo;
  else
    Assert(False, SUnknownDataType);
    Result := ftUnknown;
  end;
end;

function GetDataType(FieldType: TFieldType): word;
begin
  Result := DataTypeMap[FieldType];
end;

function AnsiStrLCompW(S1, S2: PWideChar; MaxLen: Cardinal): Integer;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0,
    S1, MaxLen, S2, MaxLen) - 2;
end;

function AnsiStrLICompW(S1, S2: PWideChar; MaxLen: Cardinal): Integer;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
    S1, MaxLen, S2, MaxLen) - 2;
end;

function AnsiStrCompW(S1, S2: PWideChar): Integer;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0, S1, -1, S2, -1) - 2;
end;

function AnsiStrICompW(S1, S2: PWideChar): Integer;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, -1,
    S2, -1) - 2;
end;

{ TDbMemDataSetBase }

constructor TDbMemDataSetBase.Create(aOwner:TComponent);
begin
  inherited;

  FLocalConstraints := True;

  CreateIRecordSet;
end;

destructor TDbMemDataSetBase.Destroy;
begin
  inherited;

  UnPrepare;

  FreeIRecordSet;
end;

procedure TDbMemDataSetBase.CreateIRecordSet;
begin
  SetIRecordSet(TMemData.Create);
end;

procedure TDbMemDataSetBase.FreeIRecordSet;
begin
  Data.Free;
end;

procedure TDbMemDataSetBase.SetIRecordSet(Value: TData);
begin
  Data := Value;

  if Data <> nil then begin
    Data.CachedUpdates := FCachedUpdates;
    Data.OnAppend := DoPerformAppend;
    Data.OnDelete := DoPerformDelete;
    Data.OnUpdate := DoPerformUpdate;
    Data.OnApplyRecord := DoApplyRecord;
  end;
end;

{ Open/Close DataSet }

procedure TDbMemDataSetBase.Prepare;
begin
  if not Prepared then begin
    Data.Prepare;
    CreateFieldDefs;
  end;
end;

procedure TDbMemDataSetBase.UnPrepare;
begin
  if Active then
    Close;

  Data.UnPrepare;

  if not (csDestroying in ComponentState) then // This line may be called after destroing FieldDefs. For details see TDbMemDataSetBase.Destroy
    FieldDefs.Updated := False;
end;

procedure TDbMemDataSetBase.CheckPrepared;
begin
  if not Prepared then
    DatabaseError(SDataSetIsNotPrepared);
end;

procedure TDbMemDataSetBase.InternalOpen;
var
  Field: TField;
  i: integer;
begin
  Data.Open;

  CreateFieldDefs;

  // Update FieldDefs once to avoid multiple Update calls when working with FieldDefsList
  // (Perfomance optimization)
//  FieldDefs.Updated := False;
//  FieldDefs.Update;

  if DefaultFields then
    CreateFields;

  // Setting actual size
  if not DefaultFields then
    for i := 0 to FieldDefs.Count - 1 do
      if FieldDefs[i].DataType = ftString then begin
        Field := FindField(FieldDefs[i].Name);
        if Field <> nil then begin
          CheckFieldCompatibility(Field, FieldDefs[i]);
          Field.Size := FieldDefs[i].Size;
        end;
      end;

  // Set number specific
  if FNumberRange then
    for i := 0 to FieldDefs.Count - 1 do
      SetNumberRange(FieldDefs[i]);

  BindFields(True);

  if ObjectView then
    GetObjectTypeNames(Fields);

  BookmarkSize := SizeOf(TRecBookmark);

  //FBlobCacheOfs := Data.RecordSize + CalcFieldsSize;
  FRecInfoOfs := Data.RecordSize + CalcFieldsSize; //FBlobCacheOfs + BlobFieldCount * SizeOf(Pointer);
  FBookmarkOfs := FRecInfoOfs + SizeOf(TRecInfo);
  FRecBufSize := FBookmarkOfs + BookmarkSize;

  FInCacheProcessing := False;

  if Filtered then
    ActivateFilters;
end;

function TDbMemDataSetBase.IsCursorOpen: boolean;
begin
  Result := Data.Active;
end;

procedure TDbMemDataSetBase.InternalClose;
begin
  BindFields(False);
  if DefaultFields then
    DestroyFields;

  Data.Close;
end;

procedure TDbMemDataSetBase.OpenCursor(InfoQuery: boolean);
begin
  inherited;

  if not InfoQuery then begin
    if FOldRecBuf <> nil then begin
      FreeRecordBuffer(FOldRecBuf);
      FOldRecBuf := nil;
    end;

    FOldRecBuf := AllocRecordBuffer;
  end;
  //DataEvent(deDataSetChange, 0); // Notify nested datasets // DEBUG
end;

procedure TDbMemDataSetBase.CloseCursor;
begin
// free complex fields if call Close in dsInsert or dsEdit mode
// TDataSet.Close doesn't call Cancel
  if Data.IsComplexFields then begin
    if FInInserting then
      Data.FreeComplexFields(ActiveBuffer, True);
    if FInEditing then
      Data.FreeComplexFields(ActiveBuffer, False); // Blobs isn't created

    FInInserting := False;
    FInEditing := False;
  end;

  if FOldRecBuf <> nil then begin
    FreeRecordBuffer(FOldRecBuf);
    FOldRecBuf := nil;
  end;

  try
    inherited;
  finally
    FParentDataSet := nil;
  end;
end;

procedure TDbMemDataSetBase.InternalRefresh;
begin
  FreeRefBuffers;
  Data.Reopen;
end;

procedure TDbMemDataSetBase.FreeRefBuffers;
var
  i: integer;
begin
  if FNeedAddRef then
    for i := 0 to BufferCount do
      FreeRefComplexFields(Buffers[i]);
  FreeRefComplexFields(TempBuffer);
end;

procedure TDbMemDataSetBase.FreeRefComplexFields(Buffer: PChar);
begin
  with PRecInfo(Buffer + FRecInfoOfs)^ do begin
    if RefComplexFields then begin
      Data.FreeComplexFields(Buffer, True);
      RefComplexFields := False;
    end;
  end;
end;

{ Field Management }

procedure TDbMemDataSetBase.InternalInitFieldDefs;
begin
  // can't CreateFieldDefs if FieldDefs.Update(InitFieldDefs)
  if not Data.Active then begin
    Data.InitFields;
    CreateFieldDefs;
  end;
end;

function TDbMemDataSetBase.GetFieldType(DataType: word): TFieldType;
begin
  Result := DbMemDS.GetFieldType(DataType);
end;

function TDbMemDataSetBase.GetFieldType(FieldDesc: TFieldDesc): TFieldType;
begin
  Result := GetFieldType(FieldDesc.DataType);
end;

procedure TDbMemDataSetBase.CreateFieldDefs;

  procedure CreateObjectFields(ObjType: TObjectType; Parent: TFieldDef);
  var
    i: integer;
    FieldDef: TFieldDef;
    FieldType: TFieldType;
    aSize: word;
    Item,CountItem: integer;
  begin
    for i := 0 to ObjType.AttributeCount - 1 do begin
      if ObjType.DataType = dtObject then
        CountItem := 1
      else begin
        CountItem := ObjType.Size;
        if CountItem > MaxArrayItem then // Restriction of array length
          CountItem := MaxArrayItem;
      end;

      for Item := 0 to CountItem - 1 do begin
        with ObjType.Attributes[i] do begin
          FieldType := GetFieldType(DataType);

          aSize := 0;
          case FieldType of
            ftString:
              aSize := Length;
          end;

          FieldDef := TFieldDef.Create(Parent.ChildDefs);
          if ObjType.DataType = dtObject then
            FieldDef.Name := Name
          else
            FieldDef.Name := IntToStr(Item);
          FieldDef.DataType := FieldType;
          FieldDef.Size := aSize;
          FieldDef.Required := False;

          if FieldType in [ftADT,ftArray] then
            CreateObjectFields(ObjectType, FieldDef);
        end;
      end;
    end;
  end;

var
  FieldType: TFieldType;
  aSize: word;
  i: integer;
  FieldDef: TFieldDef;
begin
  if FieldDefs.Updated then Exit;
  FieldDefs.BeginUpdate;
  try
    FieldDefs.Clear;
    for i := 0 to Data.FieldCount - 1 do
      with Data.Fields[i] do
        if not HiddenObject and (not HasParent or ParentField.HiddenObject) then begin
        // FieldNo 1..
          FieldType := GetFieldType(Data.Fields[i]);
          aSize := 0;
          case FieldType of
            ftString, ftWideString{$IFDEF VER5P},ftGuid{$ENDIF}: begin
              aSize := Length;
              if aSize = 0 then
                aSize := 1;  // For SELECT NULL FROM ...
            end;
            ftBytes,ftVarBytes:
              aSize := Length;
          end;

          if FieldType <> ftUnknown then begin
            FieldDef := TFieldDef.Create(FieldDefs, Name, FieldType, aSize,
              Required and FLocalConstraints, i + 1);

            if FieldType in [ftFloat, ftInteger] then
              FieldDef.Precision := Length
            else
            if FieldType in [ftBCD{$IFDEF VER6P}, ftFMTBCD{$ENDIF}] then begin
              FieldDef.Precision := Length;
              FieldDef.Size := Scale;
            end
            else
              if FieldType in [ftADT,ftArray] then
                CreateObjectFields(ObjectType, FieldDef);

            if ReadOnly then
              FieldDef.Attributes := FieldDef.Attributes + [DB.faReadonly];
            if Hidden then
              FieldDef.Attributes := FieldDef.Attributes + [DB.faHiddenCol];
          {$IFDEF VER5P}
            if Fixed then
              FieldDef.Attributes := FieldDef.Attributes + [DB.faFixed];
          {$ENDIF}
          end;
        end;
  finally
    FieldDefs.EndUpdate;
  end;
  FieldDefList.Update;
  FieldDefs.Updated := True;
end;

procedure TDbMemDataSetBase.GetObjectTypeNames(Fields: TFields);
var
  i: integer;
  ObjectField: TObjectField;
begin
  for i := 0 to Fields.Count - 1 do
    if Fields[i] is TObjectField then begin
      ObjectField := TObjectField(Fields[i]);

      ObjectField.ObjectType := Data.Fields[Fields[i].FieldNo - 1].ObjectType.Name;

      with ObjectField do
        if DataType in [ftADT, ftArray] then begin
          if (DataType = ftArray) and SparseArrays and
           (Fields[0].DataType = ftADT)
          then
            GetObjectTypeNames(TObjectField(Fields[0]).Fields)
          else
            GetObjectTypeNames(Fields);
        end;
    end
end;

function TDbMemDataSetBase.GetFieldData(Field: TField; Buffer: pointer): boolean;
var
  IsBlank: boolean;
  RecBuf: PChar;
begin
  Result := False;            
  if not GetActiveRecBuf(RecBuf) then
    Exit;

  with Field do
    if FieldNo > 0 then begin
      Data.GetField(FieldNo, RecBuf, Buffer, IsBlank);
      Result := not IsBlank;
    end
    else
      if State in [dsBrowse, dsEdit, dsInsert, dsCalcFields, dsBlockRead] then begin
        Inc(RecBuf, RecordSize + Offset);
        Result := Boolean(RecBuf[0]);
        if Result and (Buffer <> nil) then
          Move(RecBuf[1], Buffer^, DataSize);
      end;
end;

function TDbMemDataSetBase.GetFieldData(FieldNo: integer; Buffer: pointer): boolean;
var
  IsBlank: boolean;
  RecBuf: PChar;
begin
//if BlockReadSize > 0 then

  Result := GetActiveRecBuf(RecBuf);
  if Result then begin
    Data.GetField(FieldNo, RecBuf, Buffer, IsBlank);
    Result := not IsBlank;
  end
end;

procedure TDbMemDataSetBase.SetFieldData(Field: TField; Buffer: pointer);
var
  RecBuf: PChar;
begin
  with Field do begin
    if not (State in dsWriteModes) then
      DatabaseError(SNotEditing);
    GetActiveRecBuf(RecBuf);
    if FieldNo > 0 then begin
      if State = dsCalcFields then
        DatabaseError(SNotEditing);
      if ReadOnly and not (State in [dsSetKey, dsFilter]) then
        DatabaseErrorFmt(SFieldReadOnly, [DisplayName]);
      Validate(Buffer);
      if FieldKind <> fkInternalCalc then begin
        Data.PutField(FieldNo, RecBuf, Buffer);
      end;
    end
    else begin {fkCalculated, fkLookup}
      Inc(RecBuf, RecordSize + Offset);
      Boolean(RecBuf[0]) := LongBool(Buffer);
      if Boolean(RecBuf[0]) then
        Move(Buffer^, RecBuf[1], DataSize);
    end;
    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Longint(Field));
  end;
end;

function TDbMemDataSetBase.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
var
  Temp: PWideChar;
begin
  if (Field.DataType = ftWideString) and (Buffer <> nil) then begin
    { Cannot copy direct - may be conflict with Delphi string manager
    SetLength(WideString(Buffer^), Field.Size * sizeof(WideChar));
    Result := inherited GetFieldData(Field, PWideChar(WideString(Buffer^)), True);}

    GetMem(Temp, (Field.Size + 1 {#0} + 8) * sizeof(WideChar));//+ 8 for numbers
    try
      Result := inherited GetFieldData(Field, Temp, True);
      if Result then
        WideString(Buffer^) := Temp
      else
        WideString(Buffer^) := '';
    finally
      FreeMem(Temp);
    end;
  end
  else
    Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;

procedure TDbMemDataSetBase.SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean);
begin
  if Field is TWideStringField then begin
    //Assert(Buffer <> nil);
    inherited SetFieldData(Field, Buffer, True);
  end
  else
    inherited SetFieldData(Field, Buffer, NativeFormat);
end;

procedure TDbMemDataSetBase.SetNumberRange(FieldDef: TFieldDef);
var
  Field: TField;
  FieldDesc: TFieldDesc;
begin
  if FieldDef.DataType in [ftInteger, ftFloat] then begin
    Field := FindField(FieldDef.Name);
    if Field <> nil then begin
      CheckFieldCompatibility(Field, FieldDef);
      FieldDesc := Data.FindField(FieldDef.Name);
      if FieldDef.DataType = ftInteger then begin
        Assert(Field is TIntegerField);
        TIntegerField(Field).MaxValue := Round(IntPower(10, FieldDesc.Length)) - 1;
        TIntegerField(Field).MinValue := -TIntegerField(Field).MaxValue;
      end
      else
        if (FieldDesc.Length > 0) and (FieldDesc.Length <= 15) then begin
          Assert(Field is TFloatField);
          TFloatField(Field).Precision := FieldDesc.Length;
          TFloatField(Field).MaxValue :=
            IntPower(10, FieldDesc.Length - FieldDesc.Scale) -
            IntPower(10, - FieldDesc.Scale);
          TFloatField(Field).MinValue := - TFloatField(Field).MaxValue;
        end;
    end;
  end;
end;

function TDbMemDataSetBase.Translate(Src,Dest: PChar; ToOem: boolean): integer;
begin
  inherited Translate(Src, Dest, ToOem);
  Result := StrLen(Src);
{  if ToOem then
    AnsiToNativeBuf(Locale, Src, Dest, Result)
  else
    NativeToAnsiBuf(Locale, Src, Dest, Result);
  if Src <> Dest then
    Dest[Result] := #0;}
end;

{ Buffer/Record Management }

function TDbMemDataSetBase.AllocRecordBuffer: PChar;
begin
  GetMem(Result, FRecBufSize);
  with PRecInfo(Result + FRecInfoOfs)^ do
    RefComplexFields := False;
end;

procedure TDbMemDataSetBase.FreeRecordBuffer(var Buffer: PChar);
begin
  FreeRefComplexFields(Buffer);
  FreeMem(Buffer);
end;

function TDbMemDataSetBase.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  FreeRefComplexFields(Buffer);
  case GetMode of
    gmCurrent:
      Data.GetRecord(Buffer);
    gmNext:
      Data.GetNextRecord(Buffer);
    gmPrior:
      Data.GetPriorRecord(Buffer);
  end;
  if Data.BOF then
    Result := grBOF
  else
    if Data.EOF then
      Result := grEOF
    else begin
      with PRecInfo(Buffer + FRecInfoOfs)^ do begin
        RecordNumber := Data.RecordNo;
        UpdateStatus := TUpdateStatus(Data.GetUpdateStatus);
        BookmarkFlag := bfCurrent;
        if FNeedAddRef then begin
          RefComplexFields := True;
          Data.AddRefComplexFields(Buffer);
        end;
      end;

      //ClearBlobCache(Buffer);
      GetCalcFields(Buffer);
      //SetBookmarkFlag(Buffer, bfCurrent);
      Data.GetBookmark(PRecBookmark(Buffer + FBookmarkOfs));

      Result := grOK;
    end;
end;

procedure TDbMemDataSetBase.InternalInitRecord(Buffer: PChar);
begin
  //FInDeferredPost := False; moved to InternalInsert
  //FInInserting := True;
  FreeRefComplexFields(Buffer);
  Data.InitRecord(Buffer);
  if Data.IsComplexFields then begin
    Data.CreateComplexFields(Buffer, True);
  end;
end;

procedure TDbMemDataSetBase.InitRecord(Buffer: PChar);
begin
  inherited InitRecord(Buffer);
  //ClearBlobCache(Buffer);
  with PRecInfo(Buffer + FRecInfoOfs)^ do begin
    RecordNumber := 0;
    UpdateStatus := TUpdateStatus(usInserted);
    BookMarkFlag := bfInserted;
  end;
  Data.InitRecord(FOldRecBuf);  // clear OldRecBuf
end;

function TDbMemDataSetBase.GetActiveRecBuf(var RecBuf: PChar): boolean;
begin
  case State of
    dsBlockRead:
      if IsEmpty then
        RecBuf := nil
      else
        RecBuf := ActiveBuffer;
    dsBrowse:
      if FInCacheProcessing then
        RecBuf := NewCacheRecBuf
      else
        if IsEmpty then
          RecBuf := nil
        else
          RecBuf := ActiveBuffer;
    dsEdit,dsInsert:
      RecBuf := ActiveBuffer;
    dsCalcFields:
      RecBuf := CalcBuffer;
    dsFilter:
      RecBuf := FFilterBuffer;
    dsNewValue:
      if FInCacheProcessing then
        RecBuf := NewCacheRecBuf
      else
        RecBuf := ActiveBuffer;
    dsOldValue:
      if FInCacheProcessing then
        RecBuf := OldCacheRecBuf
      else
        RecBuf := GetOldRecord;
//    dsSetKey: RecBuf := PChar(FKeyBuffer) + SizeOf(TKeyBuffer);
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

function TDbMemDataSetBase.GetOldRecord: PChar;
begin
  UpdateCursorPos;
  Data.GetOldRecord(FOldRecBuf);
  Result := FOldRecBuf;
end;

procedure TDbMemDataSetBase.ClearCalcFields(Buffer: PChar);
begin
  FillChar(Buffer[RecordSize], CalcFieldsSize, 0);
end;

// WAR don't support BlockRead

procedure TDbMemDataSetBase.SetBlockReadSize(Value: Integer);
begin
  if Value <> BlockReadSize then
    if (Value > 0) or (Value < -1) then begin
      UpdateCursorPos;
      inherited;
    {$IFDEF VER4}
      BlockReadNext;
    {$ENDIF}
    end
    else
      inherited;
end;

procedure TDbMemDataSetBase.BlockReadNext;
begin
  inherited;
end;

{ Bookmarks }

function TDbMemDataSetBase.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag;
end;

procedure TDbMemDataSetBase.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag := Value
end;

// Data - pointer to bookmark
procedure TDbMemDataSetBase.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Move(Buffer[FBookmarkOfs], Data^, BookmarkSize);
end;

procedure TDbMemDataSetBase.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  Move(Data^, Buffer[FBookmarkOfs], BookmarkSize);
end;

procedure TDbMemDataSetBase.InternalGotoBookmark(Bookmark: Pointer);
begin
  Data.SetToBookMark(Bookmark);
end;

function TDbMemDataSetBase.GetBookmarkStr: TBookmarkStr;
begin
  if State in [dsFilter] then begin
    SetLength(Result, BookmarkSize);
    Data.GetBookmark(Pointer(Result));
  end
  else
    Result := inherited GetBookmarkStr;
end;

function TDbMemDataSetBase.BookmarkValid(Bookmark:DB.TBookmark): boolean;
begin
  Result := Data.BookmarkValid(Bookmark);
end;

function TDbMemDataSetBase.CompareBookmarks(Bookmark1, Bookmark2:DB.TBookmark): integer;
begin
  Result := Data.CompareBookmarks(Bookmark1, Bookmark2);
end;

{ Navigation }

procedure TDbMemDataSetBase.InternalSetToRecord(Buffer: PChar);
begin
  InternalGotoBookmark(Buffer + FBookmarkOfs);
end;

procedure TDbMemDataSetBase.InternalFirst;
begin
  Data.SetToBegin;
end;

procedure TDbMemDataSetBase.InternalLast;
begin
  Data.SetToEnd;
end;

{ Editing }

procedure TDbMemDataSetBase.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  if Append then
    Data.AppendRecord(Buffer)
  else
    Data.InsertRecord(Buffer);
end;

procedure TDbMemDataSetBase.InternalInsert;
begin
  FInDeferredPost := False;
  FInInserting := True;
end;

procedure TDbMemDataSetBase.InternalEdit;
begin
  FInDeferredPost := False;
  FInEditing := True;

  FreeRefComplexFields(ActiveBuffer);
  Data.EditRecord(ActiveBuffer);
end;

procedure TDbMemDataSetBase.InternalDelete;
begin
  if not CanModify then DatabaseError(SDataSetReadOnly, Self);
  Data.DeleteRecord;
end;

procedure TDbMemDataSetBase.InternalPost;
begin
{$IFDEF VER6P}
  inherited;
{$ENDIF}

  if State = dsEdit then begin
    Data.PostRecord(ActiveBuffer);
  end
  else
    Data.InsertRecord(ActiveBuffer);

  FInInserting := False;
  FInEditing := False;
end;

procedure TDbMemDataSetBase.Cancel;
begin
  if State = dsInsert then  // for D3, before Ok
    if Data.IsComplexFields then
      Data.FreeComplexFields(ActiveBuffer, True);

  inherited;
end;

procedure TDbMemDataSetBase.InternalCancel;
begin
  FInInserting := False;
  FInEditing := False;

  if State <> dsInsert then
    Data.CancelRecord(ActiveBuffer);
end;

procedure TDbMemDataSetBase.InternalDeferredPost;
begin
  if State = dsEdit then
    DoPerformUpdate
  else
    DoPerformAppend;
end;

procedure TDbMemDataSetBase.DeferredPost;
  procedure CheckRequiredFields;
  var
    I: Integer;
  begin
    for I := 0 to Fields.Count - 1 do
      with Fields[I] do
        if Required and not ReadOnly and (FieldKind = fkData) and IsNull then
        begin
          FocusControl;
          DatabaseErrorFmt(SFieldRequired, [DisplayName]);
        end;
  end;
begin
  if not CachedUpdates then begin
    UpdateRecord;
    case State of
      dsEdit, dsInsert:
        begin
          DataEvent(deCheckBrowseMode, 0);
          CheckRequiredFields;
          UpdateCursorPos;

          InternalDeferredPost;

          FInDeferredPost := True;
        end;
    end;
  end;
end;

procedure TDbMemDataSetBase.DoOnNewRecord;
var
  i: integer;
begin
  for i := 0 to FieldCount - 1 do
    if Fields[i].DefaultExpression <> '' then
      Fields[i].AsString := Fields[i].DefaultExpression;
  inherited;
end;

function TDbMemDataSetBase.PerformAppend: boolean;
begin
  Result := False;
end;

function TDbMemDataSetBase.PerformDelete: boolean;
begin
  Result := False;
end;

function TDbMemDataSetBase.PerformUpdate: boolean;
begin
  Result := False;
end;

procedure TDbMemDataSetBase.DoPerformAppend;
var
  OldModified: boolean;
begin
  OldModified := Modified;
  try
    if not FLocalUpdate then
      if not FInDeferredPost then   // WAR supports defer posting
        PerformAppend
      else
        DoPerformUpdate;
  finally
    SetModified(OldModified);
  end;
end;

procedure TDbMemDataSetBase.DoPerformDelete;
var
  OldModified: boolean;
begin
  OldModified := Modified;
  try
    if not FLocalUpdate then
      PerformDelete
  finally
    SetModified(OldModified);
  end;
end;

procedure TDbMemDataSetBase.DoPerformUpdate;
var
  OldModified: boolean;
begin
  OldModified := Modified;
  try
    if not FLocalUpdate then
      PerformUpdate
  finally
    SetModified(OldModified);
  end;
end;

{ Filter / Locate / Find }

procedure TDbMemDataSetBase.ActivateFilters;
begin
  DeactivateFilters;

  if Assigned(OnFilterRecord) then
    Data.FilterFunc := RecordFilter;
  if Trim(Filter) <> '' then
    Data.FilterText := Filter;
end;

procedure TDbMemDataSetBase.DeactivateFilters;
begin
  Data.FilterFunc := nil;
  Data.FilterText := '';
end;

function TDbMemDataSetBase.RecordFilter(RecBuf: pointer): boolean;
var
  Accept: boolean;
  SaveState: TDataSetState;
begin
  SaveState := SetTempState(dsFilter);
  FFilterBuffer := RecBuf;
  try
    Accept := True;
    OnFilterRecord(Self, Accept);
  except
    InternalHandleException;
  end;
  RestoreState(SaveState);
  Result := Accept;
end;

procedure TDbMemDataSetBase.SetFiltered(Value: boolean);
begin
  if Active then begin
    CheckBrowseMode;
    UpdateCursorPos;
  end;

  if Value <> Filtered then begin
    if Value then
      ActivateFilters
    else
      DeactivateFilters;

    inherited SetFiltered(Value);

    if Active then begin
      Data.FilterUpdated;
      Resync([]);
      First;
      //DoAfterScroll;
    end;
  end;
end;

procedure TDbMemDataSetBase.SetFilterData(const Text: string; Options: TFilterOptions);
begin
  if Active then begin
    CheckBrowseMode;
    UpdateCursorPos;
  end;

  if (Text <> Filter) or (Options <> FilterOptions) then begin
    Data.FilterCaseInsensitive := foCaseInsensitive in Options;
    Data.FilterNoPartialCompare := foNoPartialCompare in Options;

    if Filtered and (Trim(Text) <> '') then
      Data.FilterText := Text
    else
      Data.FilterText := '';

    inherited SetFilterText(Text);
    inherited SetFilterOptions(Options);

    if Active and Filtered then begin
      Data.FilterUpdated;
      Resync([]);
      First;
      //DoAfterScroll;
    end;
  end;
end;

procedure TDbMemDataSetBase.SetFilterOptions(Value: TFilterOptions);
begin
  SetFilterData(Filter, Value);
end;

procedure TDbMemDataSetBase.SetFilterText(const Value: string);
begin
  SetFilterData(Value, FilterOptions);
end;

procedure TDbMemDataSetBase.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  if Active then begin
    CheckBrowseMode;
    UpdateCursorPos;
  end;

  inherited SetOnFilterRecord(Value);

  if Filtered and Assigned(OnFilterRecord) then
    Data.FilterFunc := RecordFilter
  else
    Data.FilterFunc := nil;

  if Active then begin
    Data.FilterUpdated;
    Resync([]);
    First;
  end;
end;

function TDbMemDataSetBase.FindRecord(Restart, GoForward: boolean): boolean;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  SetFound(False);
  UpdateCursorPos;
  CursorPosChanged;
  if not Filtered then
    ActivateFilters;
  try
    if GoForward then begin
      if Restart then
        Data.SetToBegin;
      Data.GetNextRecord(nil);
    end
    else begin
      if Restart then
        Data.SetToEnd;
      Data.GetPriorRecord(nil);
    end;
  finally
    if not Filtered then
      DeactivateFilters;
  end;

  if not Data.BOF and not Data.EOF then begin
    Resync([rmExact, rmCenter]);
    SetFound(True);
  end;
  Result := Found;
  if Result then
    DoAfterScroll;
end;

// Allocate memory for Value (ValuePtr - must call FreeMem!) and copy Value to ValuePtr
procedure TDbMemDataSetBase.CopyFieldValue(const Value: variant; out ValuePtr: pointer; out ValueType: integer; FieldDesc: TFieldDesc); // Allocate memory for Value (ValuePtr - must call FreeMem!) and copy Value to ValuePtr
var
  DateValue: TDateTime;
  BoolValue: boolean;
  s: string;
  ws: WideString;
  l: integer;
begin
  case VarType(Value) of
    varEmpty,varNull:
      ValuePtr := nil;
    varString, varOleStr:
      case FieldDesc.DataType of
        dtBoolean: begin
          BoolValue := Value;
          GetMem(ValuePtr, SizeOf(Boolean));
          Boolean(ValuePtr^) := BoolValue;
          ValueType := dtBoolean;
        end;
        dtBytes, dtVarBytes, dtExtVarBytes: begin
          s := Value;
          l := Length(s);
          GetMem(ValuePtr, l + SizeOf(Word));
          Move(s[1], (PChar(ValuePtr) + SizeOf(Word))^, l);
          Word(ValuePtr^) := l;
          ValueType := FieldDesc.DataType;
        end;
        dtWideString:
        begin
          ws := Value;
          l := (Length(ws) + 1) * sizeof(WideChar);
          GetMem(ValuePtr, l);
          StrCopyW(ValuePtr, PWideChar(ws));
          ValueType := dtWideString;
        end;
      else
        begin
          s := VarToStr(Value);
          GetMem(ValuePtr, Length(s) + 1);
          StrCopy(ValuePtr, PChar(s));
          ValueType := dtString;
        end;
      end;
  else
    ValueType := FieldDesc.DataType;
    case ValueType of
      dtInt32, dtInt8, dtInt16, dtUInt16: begin
        GetMem(ValuePtr, SizeOf(Integer));
        Integer(ValuePtr^) := Value;
      end;
      dtUInt32: begin
        GetMem(ValuePtr, SizeOf(Longword));
        Longword(ValuePtr^) := Value;
      end;
      dtBoolean: begin
        GetMem(ValuePtr, SizeOf(Boolean));
        Boolean(ValuePtr^) := Value;
      end;
      dtInt64: begin
        GetMem(ValuePtr, SizeOf(Int64));
      {$IFDEF VER6P}
        Int64(ValuePtr^) := Value;
      {$ELSE}
        if TVarData(Value).VType = varDecimal then
          Int64(ValuePtr^) := TVarDataD6(Value).VInt64
        else
          Int64(ValuePtr^) := StrToInt64(Value);
      {$ENDIF}
      end;
      dtFloat, dtCurrency: begin
        GetMem(ValuePtr, SizeOf(Double));
        Double(ValuePtr^) := Value;
      end;
      dtDateTime, dtDate, dtTime: begin
        DateValue := VarToDateTime(Value);
        GetMem(ValuePtr, FieldDesc.Size);
        Data.PutDateToBuf(ValuePtr, @DateValue, dfDateTime);
      end;
      dtBytes, dtVarBytes, dtExtVarBytes: begin
        Assert(VarType(Value) = varArray + varByte);
        GetMem(ValuePtr, TVarData(Value).VArray.Bounds[0].ElementCount + SizeOf(Word));
        Move(TVarData(Value).VArray.Data^, (PChar(ValuePtr) + SizeOf(Word))^, TVarData(Value).VArray.Bounds[0].ElementCount);
        Word(ValuePtr^) := TVarData(Value).VArray.Bounds[0].ElementCount;
      end;
    else
      raise EConvertError.Create(SCannotConvertType);
    end;
  end;
end;

// Used to compare field value and string KeyValue with matching options
function TDbMemDataSetBase.CompareStrValues(const Value: PChar; const FieldValue: PChar; const Options: TLocateExOptions): boolean;
var
  Res: integer;
  ValueLen: cardinal;
begin
  if lxPartialKey in Options then begin
    ValueLen := StrLen(Value);
    Result := StrLen(FieldValue) >= ValueLen;
    if not Result then
      Exit // To avoid AV in case Len(Value) > Len(St)
    else
      if lxCaseInsensitive in Options then
        Res := AnsiStrLIComp(Value, FieldValue, ValueLen)
      else
        Res := AnsiStrLComp(Value, FieldValue, ValueLen)
  end
  else
    if lxCaseInsensitive in Options then
      Res := AnsiStrIComp(Value, FieldValue)
    else
      Res := AnsiStrComp(Value, FieldValue);

  Result := (Res = 0) or (Res = -1) and (lxNearest in Options);
end;

function TDbMemDataSetBase.CompareWideStrValues(const Value: PWideChar; const FieldValue: PWideChar; const Options: TLocateExOptions): boolean;
var
  Res: integer;
  ValueLen: cardinal;
begin
  if lxPartialKey in Options then begin
    ValueLen := StrLenW(Value);
    Result := StrLenW(FieldValue) >= ValueLen;
    if not Result then
      Exit // To avoid AV in case Len(Value) > Len(St)
    else
      if lxCaseInsensitive in Options then
        Res := AnsiStrLICompW(Value, FieldValue, ValueLen)
      else
        Res := AnsiStrLCompW(Value, FieldValue, ValueLen)
  end
  else
    if lxCaseInsensitive in Options then
      Res := AnsiStrICompW(Value, FieldValue)
    else
      Res := AnsiStrCompW(Value, FieldValue);

  Result := (Res = 0) or (Res = -1) and (lxNearest in Options);
end;

// Used to compare binary field value and binary KeyValue with matching options
function TDbMemDataSetBase.CompareBinValues(const Value: PChar; const ValueLen: integer; const FieldValue: PChar; const FieldValueLen: integer; const Options: TLocateExOptions): boolean; // Used to compare binary field value and binary KeyValue with matching options
begin
  if lxPartialKey in Options then begin
    Result := FieldValueLen >= ValueLen;
    if not Result then
      Exit // Field value is shorter when Value
    else
      Result := CompareMem(FieldValue, Value, ValueLen)
  end
  else
  begin
    Result := FieldValueLen = ValueLen;
    if not Result then
      Exit
    else
      Result := CompareMem(FieldValue, Value, FieldValueLen);
  end;
end;

// Used to compare field value and KeyValue from MemDataSet.LocateRecord
function TDbMemDataSetBase.CompareFieldValue(
  ValuePtr: pointer; const ValueType: integer;
  FieldDesc: TFieldDesc; RecBuf: PChar;
  const Options: TLocateExOptions): boolean; // Used to compare field value and KeyValue from MemDataSet.LocateRecord

var
  St: string;
  WSt: WideString;
  pSt: PChar;
  pWSt: PWideChar;
  BlobValue: string;
  BlobValueW: WideString;
  DateValue: TDateTime;
  FieldBuf: pointer;
  FieldBufStatic: array [0..4000] of byte;
  IsBlank: boolean;
begin
  Result := False;

  if FieldDesc.ParentField = nil then
    FieldBuf := RecBuf + FieldDesc.Offset
  else begin
  // support objects
    FieldBuf := @FieldBufStatic;
    Data.GetField(FieldDesc.FieldNo, RecBuf, FieldBuf, IsBlank);  // GetChildField
  end;

  case ValueType of
    dtString: begin
      case FieldDesc.DataType of
        dtString: begin
          pSt := FieldBuf;
          if FieldDesc.Fixed and Data.TrimFixedChar then
            StrTrim(pSt)
        end;
        dtWideString: begin
          St := PWideChar(FieldBuf);
          pSt := PChar(St);
          if FieldDesc.Fixed and Data.TrimFixedChar then
            StrTrim(pSt)
        end;
        dtExtString:
          pSt := PChar(FieldBuf^);
        dtExtWideString: begin
          St := PWideChar(FieldBuf^);
          pSt := PChar(St);
        end;
      {$IFDEF VER5P}
        dtVariant: begin
          St := TVariantObject(FieldBuf^).Value;
          pSt := PChar(St);
        end;
      {$ENDIF}
        dtInt8: begin
          St := IntToStr(ShortInt(FieldBuf^));
          pSt := PChar(St);
        end;
        dtInt16: begin
          St := IntToStr(SmallInt(FieldBuf^));
          pSt := PChar(St);
        end;
        dtUInt16: begin
          St := IntToStr(Word(FieldBuf^));
          pSt := PChar(St);
        end;
        dtInt32: begin
          St := IntToStr(Integer(FieldBuf^));
          pSt := PChar(St);
        end;
        dtUInt32: begin
          St := IntToStr(Longword(FieldBuf^));
          pSt := PChar(St);
        end;
        dtInt64: begin
          St := IntToStr(Int64(FieldBuf^));
          pSt := PChar(St);
        end;
        dtFloat, dtCurrency: begin
          St := FloatToStr(Double(FieldBuf^));
          pSt := PChar(St);
        end;
        dtDate: begin
          Data.GetDateFromBuf(FieldBuf, @DateValue, dfDateTime);
          St := DateToStr(DateValue);
          pSt := PChar(St);
        end;
        dtTime: begin
          Data.GetDateFromBuf(FieldBuf, @DateValue, dfDateTime);
          St := TimeToStr(DateValue);
          pSt := PChar(St);
        end;
        dtDateTime: begin
          Data.GetDateFromBuf(FieldBuf, @DateValue, dfDateTime);
          St := DateTimeToStr(DateValue);
          pSt := PChar(St);
        end;
      else
        if FieldDesc.DataType in Data.BlobFieldTypes then begin
          SetLength(BlobValue, Data.GetBlobSize(FieldDesc.FieldNo, RecBuf));
          Data.ReadBlob(FieldDesc.FieldNo, RecBuf, 0, 0, PChar(BlobValue));
          pSt := PChar(BlobValue);
        end
        else
          raise EConvertError.Create(SCannotConvertType);
      end;

      Result := CompareStrValues(PChar(ValuePtr), pSt, Options);
    end;
    dtWideString: begin
      case FieldDesc.DataType of
        dtWideString: begin
          pWSt := FieldBuf;
          if FieldDesc.Fixed and Data.TrimFixedChar then
            StrTrimW(pWSt)
        end;
        dtString: begin
          WSt := PChar(FieldBuf);
          pWSt := PWideChar(WSt);
          if FieldDesc.Fixed and Data.TrimFixedChar then
            StrTrimW(pWSt)
        end;
        dtExtString: begin
          WSt := PChar(FieldBuf^);
          pWSt := PWideChar(WSt);
        end;
        dtExtWideString:
          pWSt := PWideChar(FieldBuf^);
      {$IFDEF VER5P}
        dtVariant: begin
          WSt := TVariantObject(FieldBuf^).Value;
          pWSt := PWideChar(WSt);
        end;
      {$ENDIF}
        dtInt8: begin
          WSt := IntToStr(ShortInt(FieldBuf^));
          pWSt := PWideChar(WSt);
        end;
        dtInt16: begin
          WSt := IntToStr(SmallInt(FieldBuf^));
          pWSt := PWideChar(WSt);
        end;
        dtUInt16: begin
          WSt := IntToStr(Word(FieldBuf^));
          pWSt := PWideChar(WSt);
        end;
        dtInt32: begin
          WSt := IntToStr(Integer(FieldBuf^));
          pWSt := PWideChar(WSt);
        end;
        dtUInt32: begin
          WSt := IntToStr(Longword(FieldBuf^));
          pWSt := PWideChar(WSt);
        end;
        dtInt64: begin
          WSt := IntToStr(Int64(FieldBuf^));
          pWSt := PWideChar(WSt);
        end;
        dtFloat, dtCurrency: begin
          WSt := FloatToStr(Double(FieldBuf^));
          pWSt := PWideChar(WSt);
        end;
        dtDate: begin
          Data.GetDateFromBuf(FieldBuf, @DateValue, dfDateTime);
          WSt := DateToStr(DateValue);
          pWSt := PWideChar(WSt);
        end;
        dtTime: begin
          Data.GetDateFromBuf(FieldBuf, @DateValue, dfDateTime);
          WSt := TimeToStr(DateValue);
          pWSt := PWideChar(WSt);
        end;
        dtDateTime: begin
          Data.GetDateFromBuf(FieldBuf, @DateValue, dfDateTime);
          WSt := DateTimeToStr(DateValue);
          pWSt := PWideChar(WSt);
        end;
      else
        if FieldDesc.DataType in Data.BlobFieldTypes then begin
          /// May be (length div 2)?
          SetLength(BlobValueW, Data.GetBlobSize(FieldDesc.FieldNo, RecBuf));
          Data.ReadBlob(FieldDesc.FieldNo, RecBuf, 0, 0, PWideChar(BlobValueW));
          pWSt := PWideChar(BlobValueW);
        end
        else
          raise EConvertError.Create(SCannotConvertType);
      end;

      Result := CompareWideStrValues(PWideChar(ValuePtr), pWSt, Options);
    end;
    dtInt8:
      if lxNearest in Options then
        Result := ShortInt(FieldBuf^) >= ShortInt(ValuePtr^)
      else
        Result := ShortInt(FieldBuf^) = ShortInt(ValuePtr^);
    dtInt16:
      if lxNearest in Options then
        Result := SmallInt(FieldBuf^) >= SmallInt(ValuePtr^)
      else
        Result := SmallInt(FieldBuf^) = SmallInt(ValuePtr^);
    dtUInt16:
      if lxNearest in Options then
        Result := Word(FieldBuf^) >= Word(ValuePtr^)
      else
        Result := Word(FieldBuf^) = Word(ValuePtr^);
    dtInt32:
      if lxNearest in Options then
        Result := Integer(FieldBuf^) >= Integer(ValuePtr^)
      else
        Result := Integer(FieldBuf^) = Integer(ValuePtr^);
    dtUInt32:
      if lxNearest in Options then
        Result := Longword(FieldBuf^) >= Longword(ValuePtr^)
      else
        Result := Longword(FieldBuf^) = Longword(ValuePtr^);
    dtInt64:
      if lxNearest in Options then
        Result := Int64(FieldBuf^) >= Int64(ValuePtr^)
      else
        Result := Int64(FieldBuf^) = Int64(ValuePtr^);
    dtBoolean:
      Result := (boolean(FieldBuf^) = False) = (boolean(ValuePtr^) = False); // Cannot use 'boolean(FieldBuf^) = boolean(ValuePtr^)' because 'True' may have any value without 0
    dtFloat, dtCurrency:
      if lxNearest in Options then
        Result := Double(FieldBuf^) >= Double(ValuePtr^)
      else
        Result := Double(FieldBuf^) = Double(ValuePtr^);
    dtDateTime, dtDate, dtTime:
      Result := CompareMem(FieldBuf, ValuePtr, FieldDesc.Size);
    dtBytes:
      Result := CompareBinValues(PChar(ValuePtr) + SizeOf(Word), Word(ValuePtr^), FieldBuf, FieldDesc.Length, Options);
    dtVarBytes:
      Result := CompareBinValues(PChar(ValuePtr) + SizeOf(Word), Word(ValuePtr^), PChar(FieldBuf) + SizeOf(Word), Word(FieldBuf^), Options);
    dtExtVarBytes:
      Result := CompareBinValues(PChar(ValuePtr) + SizeOf(Word), Word(ValuePtr^), PChar(Pointer(FieldBuf^)) + SizeOf(Word), Word(Pointer(FieldBuf^)^), Options)
  else
    Assert(False);
  end;
end;

function TDbMemDataSetBase.LocateRecord(const KeyFields: string;
  const KeyValues: variant; Options: TLocateExOptions; SavePos: boolean): boolean;
var
  St: string;
  Fields: TList;
  FieldDesc: TFieldDesc;
  RecBuf: PChar;
  Value: variant;
  Values: array of pointer;
  Types: array of integer;
  i, FieldCount: integer;
  Bookmark: TRecBookmark;
begin
  CheckBrowseMode;
  CursorPosChanged;
  UpdateCursorPos;

  Result := False;
  FieldCount := 0;

  RecBuf := TempBuffer;
  FreeRefComplexFields(RecBuf);

  Fields := TList.Create;
  Values := nil;
  try
    i := 1;
    while True do begin
      St := ExtractFieldName(KeyFields, i);
      if St <> '' then begin
        FieldDesc := Data.FieldByName(St);
        Fields.Add(FieldDesc);
        Inc(FieldCount);
      end
      else
        break;
    end;

    SetLength(Values, FieldCount);
    for i := 0 to FieldCount - 1 do
      Values[i] := nil; // Clear Values array to prevent AV in 'finally' section after Exception

    SetLength(Types, FieldCount);
    for i := 0 to FieldCount - 1 do begin
      if VarIsArray(KeyValues) then
        if i <= VarArrayHighBound(KeyValues, 1) then
          Value := KeyValues[i]
        else
          Value := Null
      else
        if i = 0 then
          Value := KeyValues
        else
          Value := Null;

        CopyFieldValue(Value, Values[i], Types[i], Fields[i]);
    end;

    Data.GetBookmark(@Bookmark);
    if not((lxNext in Options) or (lxUp in Options)) then
      Data.SetToBegin;

    while True do begin
      if lxUp in Options then
        Data.GetPriorRecord(RecBuf)
      else
        Data.GetNextRecord(RecBuf);

      if not (Data.EOF or Data.BOF) then begin
        Result := True;
        i := 0;
        while Result and (i < FieldCount) do begin
          FieldDesc := Fields[i];
          if (Values[i] = nil) or Data.GetNull(FieldDesc.FieldNo, RecBuf) then
            Result := (Values[i] = nil) and Data.GetNull(FieldDesc.FieldNo, RecBuf)
          else
            Result := CompareFieldValue(Values[i], Types[i], FieldDesc, RecBuf, Options);
          Inc(i);
        end;

        if Result then
          break;
      end
      else begin
        Result := False;
        break;
      end;
    end;

    if SavePos or not Result then
      Data.SetToBookmark(@Bookmark);
  finally
    for i := 0 to FieldCount - 1 do
      FreeMem(Values[i]);

    Fields.Free;
  end;
end;

function TDbMemDataSetBase.Locate(const KeyFields: string;
  const KeyValues: variant; Options: TLocateOptions): boolean;
var
  LocOptions: TLocateExOptions;
begin
  DoBeforeScroll;

  LocOptions := [];
  if loCaseInsensitive in Options then
    LocOptions := LocOptions + [lxCaseInsensitive];

  if loPartialKey in Options then
    LocOptions := LocOptions + [lxPartialKey];

  Result := LocateRecord(KeyFields, KeyValues, LocOptions, False);

  if Result then begin
    Resync([rmExact{, rmCenter}]);
    DoAfterScroll;
  end;
end;

function TDbMemDataSetBase.LocateEx(const KeyFields: string;
  const KeyValues: variant; Options: TLocateExOptions): boolean;
begin
  DoBeforeScroll;

  Result := LocateRecord(KeyFields, KeyValues, Options, False);

  if Result then begin
    Resync([{rmExact, rmCenter}]);
    DoAfterScroll;
  end;
end;

function TDbMemDataSetBase.Lookup(const KeyFields: string; const KeyValues: variant;
  const ResultFields: string): variant;
begin
  Result := Null;
  if LocateRecord(KeyFields, KeyValues, [], True) then begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TempBuffer);
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

{ CachedUpdates }

procedure TDbMemDataSetBase.CheckCachedUpdateMode;
begin
  if not CachedUpdates then
    DatabaseError(SNotCachedUpdate);
end;

function TDbMemDataSetBase.UpdateStatus: TUpdateStatus;
var
  RecBuf: PChar;
begin
  if CachedUpdates and not IsEmpty then begin
    if State = dsCalcFields then
      RecBuf := CalcBuffer
    else
      RecBuf := ActiveBuffer;

    Result := PRecInfo(RecBuf + FRecInfoOfs).UpdateStatus;
  end
  else
    Result := usUnModified;
end;

function TDbMemDataSetBase.UpdateResult: TUpdateAction;
begin
  UpdateCursorPos;

  if Data.GetUpdateResult = urNone then
    Result := uaApplied
  else
    Result := TUpdateAction(Data.GetUpdateResult);
end;

procedure TDbMemDataSetBase.ApplyUpdates;
begin
  FreeRefBuffers;
  CheckActive;
  if State <> dsBrowse then
    Post;
  CheckCachedUpdateMode;
  UpdateCursorPos;

  NewCacheRecBuf := AllocRecordBuffer;
  OldCacheRecBuf := AllocRecordBuffer;
  FInCacheProcessing := True;
  try
    Data.SetCacheRecBuf(NewCacheRecBuf, OldCacheRecBuf);
    Data.ApplyUpdates;
  finally
    FInCacheProcessing := False;
    FreeRecordBuffer(NewCacheRecBuf);
    FreeRecordBuffer(OldCacheRecBuf);
    Resync([]);
  end;
end;

procedure TDbMemDataSetBase.CommitUpdates;
begin
  FreeRefBuffers;
  CheckActive;
  CheckCachedUpdateMode;
  UpdateCursorPos;

  NewCacheRecBuf := AllocRecordBuffer;
  OldCacheRecBuf := AllocRecordBuffer;
  FInCacheProcessing := True;
  try
    Data.SetCacheRecBuf(NewCacheRecBuf, OldCacheRecBuf);
    Data.CommitUpdates;
  finally
    FInCacheProcessing := False;
    FreeRecordBuffer(NewCacheRecBuf);
    FreeRecordBuffer(OldCacheRecBuf);
  end;
  Resync([]);
end;

procedure TDbMemDataSetBase.CancelUpdates;
begin
  FreeRefBuffers;
  CheckActive;
  Cancel;
  CheckCachedUpdateMode;
  UpdateCursorPos;
  Data.CancelUpdates;
  Resync([]);
end;

procedure TDbMemDataSetBase.RestoreUpdates;
begin
  FreeRefBuffers;
  CheckActive;
  Cancel;
  CheckCachedUpdateMode;
  UpdateCursorPos;
  Data.RestoreUpdates;
  Resync([]);
end;

procedure TDbMemDataSetBase.RevertRecord;
begin
  FreeRefComplexFields(ActiveBuffer);
  CheckActive;
  if State in dsEditModes then
    Cancel;
  CheckCachedUpdateMode;
  UpdateCursorPos;
  Data.RevertRecord;
  Resync([]);
end;

procedure TDbMemDataSetBase.DoApplyRecord(UpdateKind: TUpdateRecKind; var Action: TUpdateRecAction);
var
  OldModified: boolean;
  UpdateAction: TUpdateAction;
begin
  OldModified := Modified;  // NewValue change Modified ??? or MemDS
  try
    UpdateAction := uaFail;
    try
      if Assigned(OnUpdateRecord) then begin
        OnUpdateRecord(Self, TUpdateKind(UpdateKind), UpdateAction);
        if UpdateAction in [uaAbort] then
          Abort;
      end;
      //else begin
      if not Assigned(OnUpdateRecord) or (UpdateAction = TUpdateAction(uaDefault)) then begin
        case UpdateKind of
          ukUpdate:
            PerformUpdate;
          ukInsert:
            PerformAppend;
          ukDelete:
            PerformDelete;
        end;

        UpdateAction := uaApplied;
      end;
    except
      on E:Exception do
        if (E is EDatabaseError) and Assigned(OnUpdateError) then begin
          OnUpdateError(Self, EDatabaseError(E), TUpdateKind(UpdateKind), UpdateAction);
          case UpdateAction of
            uaFail:
              raise;
            uaAbort:
              Abort;
          end;
        end
        else
          raise;
    end;
  finally
    SetModified(OldModified);
  end;
  Action := TUpdateRecAction(UpdateAction);
end;

{ BLOB Support }

{function TDbMemDataSetBase.GetBlobData(Field:TField; Buffer: PChar):TBlobData;
begin
  Result := PBlobDataArray(Buffer + FBlobCacheOfs)[Field.Offset];
end;

procedure TDbMemDataSetBase.SetBlobData(Field:TField; Buffer: PChar; Value:TBlobData);
begin
  if Buffer = ActiveBuffer then
    PBlobDataArray(Buffer + FBlobCacheOfs)[Field.Offset] := Value;
end;

procedure TDbMemDataSetBase.ClearBlobCache(Buffer: PChar);
var
  i: integer;
begin
  for i := 0 to BlobFieldCount - 1 do
    PBlobDataArray(Buffer + FBlobCacheOfs)[i] := '';
end;}

procedure TDbMemDataSetBase.CloseBlob(Field: TField);
begin
end;

function TDbMemDataSetBase.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TBlobStream.Create(Field as TBlobField, Mode);
end;

{ Informational }

function TDbMemDataSetBase.IsSequenced: boolean;
begin
  Result := True;
end;

function TDbMemDataSetBase.GetRecordSize: word;
begin
  Result := word(Data.RecordSize);
end;

function TDbMemDataSetBase.GetRecordCount: integer;
begin
  if Active then
    Result := Data.RecordCount
  else
    Result := 0;
end;

function TDbMemDataSetBase.GetRecNo: integer;
var
  RecBuf: PChar;
begin
  if GetActiveRecBuf(RecBuf) then
    Result := PRecInfo(RecBuf + FRecInfoOfs)^.RecordNumber
  else
    Result := 0;
end;

procedure TDbMemDataSetBase.SetRecNo(Value: integer);
begin
  CheckBrowseMode;
  DoBeforeScroll;
  Data.RecordNo := Value;
  Resync([{rmCenter}]);
  DoAfterScroll;
end;

{ More }

procedure TDbMemDataSetBase.InternalHandleException;
begin
end;

procedure TDbMemDataSetBase.DataEvent(Event: TDataEvent; Info: longint);
  procedure CheckIfParentScrolled;
  var
    CSum, i: integer;
    ParentPosition: pointer;
  begin
    with FParentDataSet do
      if IsEmpty then
        ParentPosition := nil
      else begin
        CSum := 0;
        for i := 0 to BookmarkSize - 1 do
          CSum := CSum + Byte(ActiveBuffer[FBookmarkOfs + i]);
        ParentPosition := Pointer(CSum);
      end;
    if (Reserved = nil) or (ParentPosition <> Reserved) then begin
      First;
      Reserved := ParentPosition;
    end
    else begin
      UpdateCursorPos;
      Resync([]);
    end;
  end;
begin
  inherited DataEvent(Event, Info);
end;

procedure TDbMemDataSetBase.AssignTo(Dest: TPersistent);
begin
  if Dest is TDbMemDataSetBase then begin
    TDbMemDataSetBase(Dest).CachedUpdates := CachedUpdates;
    TDbMemDataSetBase(Dest).LocalConstraints := LocalConstraints;
    TDbMemDataSetBase(Dest).LocalUpdate := LocalUpdate;
  end
  else
    inherited;
end;

procedure TDbMemDataSetBase.SetCachedUpdates(Value: boolean);
begin
  if FCachedUpdates <> Value then begin
    CheckInactive;
    FCachedUpdates := Value;
    Data.CachedUpdates := FCachedUpdates;
  end;
end;

function TDbMemDataSetBase.GetUpdatesPending: boolean;
begin
  Result := Data.UpdatesPending;
end;

function TDbMemDataSetBase.GetPrepared: boolean;
begin
  Result := Data.Prepared;
end;

procedure TDbMemDataSetBase.SetPrepared(Value: boolean);
begin
  if Value then
    Prepare
  else
    UnPrepare;
end;

function TDbMemDataSetBase.GetUpdateRecordSet: TUpdateRecordTypes;
var
  ItemTypes: TItemTypes;
begin
//  if Active then begin
    CheckCachedUpdateMode;
    ItemTypes := Data.FilterItemTypes;

    Result := [];
    if isUnmodified in ItemTypes then
      Result := Result + [rtUnmodified];
    if isUpdated in ItemTypes then
      Result := Result + [rtModified];
    if isAppended in ItemTypes then
      Result := Result + [rtInserted];
    if isDeleted in ItemTypes then
      Result := Result + [rtDeleted];
{  end
  else
    Result := [];}
end;

procedure TDbMemDataSetBase.SetUpdateRecordSet(Value: TUpdateRecordTypes);
var
  ItemTypes: TItemTypes;
begin
  CheckCachedUpdateMode;

  //CheckBrowseMode;
  if Active then
    UpdateCursorPos;

  ItemTypes := [];
  if rtUnmodified in Value then
    ItemTypes := ItemTypes + [isUnmodified];
  if rtModified in Value then
    ItemTypes := ItemTypes + [isUpdated];
  if rtInserted in Value then
    ItemTypes := ItemTypes + [isAppended];
  if rtDeleted in Value then
    ItemTypes := ItemTypes + [isDeleted];

  Data.FilterItemTypes := ItemTypes;

 if Active then
   Resync([]);
end;

procedure TDbMemDataSetBase.SetIndexFieldNames(const Value: string);
begin
  FIndexFieldNames := Value;
  Data.Sort(Value);
  First;
end;

{ TBlobStream }

constructor TBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FMode := Mode;
  FField := Field;
  FDataSet := FField.DataSet as TDbMemDataSetBase;
  FFieldNo := FField.FieldNo;
  if not FDataSet.GetActiveRecBuf(FBuffer) then
    Exit;
  if FDataSet.State = dsFilter then
    DatabaseErrorFmt('SNoFieldAccess', [FField.DisplayName]);
  if not FField.Modified then begin
    if Mode = bmRead then begin
{      FCached := FDataSet.FCacheBlobs and (FBuffer = FDataSet.ActiveBuffer) and
        (FField.IsNull or (FDataSet.GetBlobData(FField, FBuffer) <> ''));}
    end
    else begin
//      FDataSet.SetBlobData(FField, FBuffer, '');
      if FField.ReadOnly then
        DatabaseErrorFmt(SFieldReadOnly, [FField.DisplayName]);
      if not (FDataSet.State in [dsNewValue, dsEdit, dsInsert]) then
        DatabaseError(SNotEditing);
    end;
  end;
  FOpened := True;
  if Mode = bmWrite then
    Truncate;
end;

destructor TBlobStream.Destroy;
begin
  if FOpened then begin
    if FModified then
      FField.Modified := True;
  end;
  if FModified then
    //try
      FDataSet.DataEvent(deFieldChange, Longint(FField));
    {except
      Application.HandleException(Self);
    end;}
end;

function TBlobStream.Read(var Buffer; Count: longint): longint;
begin
  Result := 0;
  if FBlobData <> '' then begin
    Result := Length(FBlobData) - FPosition;
    if Result > Count then Result := Count;
    Move(PChar(@FBlobData[FPosition + 1])^, Buffer, Result);
    Inc(FPosition, Result);
  end
  else
  if FOpened then begin
    if Count > Size - FPosition then
      Result := Size - FPosition
    else
      Result := Count;
    if Result > 0 then begin
      Result := FDataSet.Data.ReadBlob(FFieldNo, FBuffer, FPosition, Count, @Buffer,
        (FDataSet.State = dsOldValue));
      Inc(FPosition, Result);
    end;
  end;
end;

function TBlobStream.Write(const Buffer; Count: Longint): Longint;
var
  Temp: Pointer;
begin
  Result := 0;
  if FBlobData <> '' then begin
    Result := Count;
    SetLength(FBlobData, (FPosition + Result));
    Move(Buffer, PChar(@FBlobData[FPosition + 1])^, Result);
    Inc(FPosition, Result);
  end
  else
  if FOpened then begin
    if FField.Transliterate then begin
      GetMem(Temp, Count);
      try
        // WAR AnsiToNativeBuf(FDataSet.Locale, @Buffer, Temp, Count);
        FDataSet.Data.WriteBlob(FFieldNo, FBuffer, FPosition, Count, @Buffer);
      finally
        FreeMem(Temp, Count);
      end;
    end
    else
      FDataSet.Data.WriteBlob(FFieldNo, FBuffer, FPosition, Count, @Buffer);

    Inc(FPosition, Count);
    Result := Count;
    FModified := True;
{    FDataSet.SetBlobData(FField, FBuffer, '');}
  end;
end;

function TBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning:
      FPosition := Offset;
    soFromCurrent:
      Inc(FPosition, Offset);
    soFromEnd:
     if FBlobData <> '' then
       FPosition := Length(FBlobData) - Offset
     else
       FPosition := GetBlobSize + Offset;
  end;
  Result := FPosition;
end;

procedure TBlobStream.Truncate;
begin
  if FOpened then begin
    FDataSet.Data.TruncateBlob(FFieldNo, FBuffer, FPosition);
    FModified := True;
//    FDataSet.SetBlobData(FField, FBuffer, '');
  end;
end;

function TBlobStream.GetBlobSize: longint;
begin
  Result := 0;
  if FOpened then
    Result := FDataSet.Data.GetBlobSize(FFieldNo, FBuffer, (FDataSet.State = dsOldValue));
end;

const
  SupportFieldTypes = [ftString, ftWideString, ftSmallint, ftInteger, ftAutoInc,
   ftWord, ftBoolean, ftLargeint, ftFloat, ftCurrency, ftDate, ftTime,
   ftDateTime, ftBlob, ftMemo];

  SNotSupportFieldType = 'Field type is not supported by TDbMemDataSet. '#13 +
    'Valid types is String, WideString, Smallint, Integer, Word, Boolean, Largeint, Float, Currency, Date, Time, DateTime, Blob, Memo';

type
  TVirtualData = class (TMemData)
  protected
    Owner: TDataSet;

    procedure InternalOpen; override;
    procedure InternalInitFields; override;

  public
    constructor Create;
  end;

{ TVirtualData }

constructor TVirtualData.Create;
begin
  inherited;

  Owner := nil;
end;

procedure TVirtualData.InternalInitFields;

  procedure AddFieldDesc(const FieldName: string; const FieldType: TFieldType; const FieldSize: integer);
  var
    Field: TFieldDesc;
  begin
    Field := TFieldDesc.Create;
    try
      Field.FieldNo := FFields.Count + 1;
      Field.Name := FieldName;
      Field.DataType := GetDataType(FieldType);
      case FieldType of
        ftString: begin
          Field.Size := FieldSize + 1;
          Field.Length := FieldSize;
        end;
        ftWideString: begin
          Field.Size := (FieldSize + 1) * sizeof(WideChar);
          Field.Length := FieldSize;
        end;
        ftSmallint:
          Field.Size := sizeof(smallint);
        ftInteger, ftAutoInc:
          Field.Size := sizeof(Integer);
        ftWord:
          Field.Size := sizeof(word);
        ftBoolean:
          Field.Size := sizeof(boolean);
        ftLargeint:
          Field.Size := sizeof(Largeint);
        ftFloat:
          Field.Size := sizeof(Double);
        ftCurrency:
          Field.Size := sizeof(Double);
        ftDate, ftTime, ftDateTime:
          Field.Size := sizeof(TDateTime);
        ftGraphic, ftFmtMemo, ftBlob, ftMemo:
          Field.Size := sizeof(Pointer);
{
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString,
    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd
}
          else
        DatabaseError(SNotSupportFieldType);
      end;
      Field.Required := False;
      FFields.Add(Field);
    except
      Field.Free;
    end;
  end;


var
  i: integer;
  OwnerField: TField;
  OwnerFieldDef: TFieldDef;
  DataFieldCount: integer;
begin
  inherited;

  DataFieldCount := 0;
  for i := 0 to Owner.FieldCount - 1 do
    if Owner.Fields[i].FieldKind = fkData then
      Inc(DataFieldCount);

  if not Owner.DefaultFields and (DataFieldCount > Owner.FieldDefs.Count) then
  // From fields
    for i := 0 to Owner.FieldCount - 1 do begin
      if Owner.Fields[i].FieldKind = fkData then begin
        OwnerField := Owner.Fields[i];
        AddFieldDesc(OwnerField.FieldName, OwnerField.DataType, OwnerField.Size);
      end
    end
  else
  // From FieldDefs
    for i := 0 to Owner.FieldDefs.Count - 1 do begin
      OwnerFieldDef := Owner.FieldDefs[i];
      AddFieldDesc(OwnerFieldDef.Name, OwnerFieldDef.DataType, OwnerFieldDef.Size);
    end
end;

procedure TVirtualData.InternalOpen;
begin
  InitFields;

  inherited;
end;

{ TDbMemDataSet }

constructor TDbMemDataSet.Create(Owner: TComponent);
begin
  inherited;

  Data.EnableEmptyStrings := True;
  FOptions := [voPersistentData,voStored];
  FStreamedActive := False;
end;

destructor TDbMemDataSet.Destroy;
begin
  Data.Close; // Clear data

  inherited;
end;

procedure TDbMemDataSet.Loaded;
begin
  inherited;

  try
    if FStreamedActive then
      Active := True;
  except
    if csDesigning in ComponentState then
      InternalHandleException
    else
      raise;
  end;
end;

procedure TDbMemDataSet.CreateIRecordSet;
begin
  SetIRecordSet(TVirtualData.Create);
  TVirtualData(Data).Owner := Self;
end;

procedure TDbMemDataSet.InternalClose;
begin
  BindFields(False);
  if DefaultFields then
    DestroyFields;

  if not (voPersistentData in FOptions) then
    Data.Close
  else
    Data.SetToBegin;
end;

function TDbMemDataSet.IsCursorOpen: boolean;
begin
  Result := inherited IsCursorOpen;
end;

procedure TDbMemDataSet.CreateFieldDefs;
var
  DataFieldCount: integer;
  i: integer;
begin
  DataFieldCount := 0;
  for i := 0 to FieldCount - 1 do
    if Fields[i].FieldKind = fkData then
      Inc(DataFieldCount);

  if not DefaultFields and (DataFieldCount > FieldDefs.Count) then
    try
      // Used to prevent save/load table DefChanged
      FAvoidReload := True;
      inherited;
    finally
      FAvoidReload := False;
    end;
end;

procedure TDbMemDataSet.DefChanged(Sender: TObject);
var
  OldActive: boolean;
  Stream: TMemoryStream;
  FieldDef: TFieldDef;
  i: integer;
begin
  if not FAvoidRefreshData then begin
    for i := 0 to TFieldDefs(Sender).Count - 1 do begin
      FieldDef := TFieldDefs(Sender)[i];
      if (FieldDef.DataType = ftGUID) and (FieldDef.Size = 0) then
        FieldDef.Size := 38 else
      if FieldDef.DataType = ftUnknown then begin
        FAvoidRefreshData := True;
        FieldDef.DataType := ftString;
        FieldDef.Size := 20;
        FAvoidRefreshData := False;
      end;
    end;

    if not FAvoidReload then begin
      OldActive := Active;
      Stream := TMemoryStream.Create;
      DisableControls;
      try
        SaveToStream(Stream, False);
        Close;
        Clear;
      finally
        LoadFromStream(Stream, False);
        Active := OldActive;
        Stream.Free;
        EnableControls;
      end;
    end;
  end;
end;

procedure TDbMemDataSet.Assign(Source: TPersistent);
var
  Stream: TMemoryStream;
begin
  if Source is TDbMemDataSet then begin
    Stream := TMemoryStream.Create;
    try
      TDbMemDataSet(Source).SaveToStream(Stream);
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end
  else
  if Source is TDataSet then
    AssignDataSet(TDataSet(Source))
  else
    inherited;
end;

procedure TDbMemDataSet.AssignDataSet(Source: TDataSet);

  procedure CreateFieldDefs(Fields: TFields; FieldDefs: TFieldDefs);
  var
    I: Integer;
    F: TField;
    FieldDef: TFieldDef;
  begin
    FieldDefs.BeginUpdate;
    try
      for I := 0 to Fields.Count - 1 do
      begin
        F := Fields[I];
        with F do
          if (FieldKind = fkData) and (DataType in SupportFieldTypes)
          then begin
            FieldDef := FieldDefs.AddFieldDef;
            FieldDef.Name := FieldName;
            FieldDef.DataType := DataType;
            FieldDef.Size := Size;
            if Required then
              FieldDef.Attributes := [faRequired];
            if ReadOnly then
              FieldDef.Attributes := FieldDef.Attributes + [faReadonly];
            if (DataType = ftBCD) and (F is TBCDField) then
              FieldDef.Precision := TBCDField(F).Precision;
            if F is TObjectField then
              CreateFieldDefs(TObjectField(F).Fields, FieldDef.ChildDefs);
          end;
      end;

    finally
      FieldDefs.EndUpdate;
    end;
  end;

var
  OldActive: boolean;
  Bookmark: string;
  i: integer;

  FieldsRO: array of boolean;
begin
  OldActive := Active;
  Close;
  Clear;
  DeleteFields;

  CreateFieldDefs(Source.Fields, FieldDefs);

  if Source.Active then begin
    DisableControls;
    Source.DisableControls;
    Bookmark := Source.Bookmark;
    Source.First;

    Open;

    // Temporary clear Field.ReadOnly flag
    SetLength(FieldsRO, Fields.Count);
    for i := 0 to Fields.Count - 1 do begin
      FieldsRO[i] := Fields[i].ReadOnly;
      Fields[i].ReadOnly := False;
    end;

    try
      while not Source.EOF do begin
        Append;
        for i := 0 to Fields.Count - 1 do
          if Fields[i] is TLargeIntField then
            TLargeIntField(Fields[i]).AsLargeInt := TLargeIntField(Source.FieldByName(Fields[i].FieldName)).AsLargeInt
          else
            Fields[i].Value := Source.FieldByName(Fields[i].FieldName).Value;
        Post;
        Source.Next;
      end;
    finally
      First;

      // Restore Field.ReadOnly flag
      for i := 0 to Fields.Count - 1 do
        Fields[i].ReadOnly := FieldsRO[i];

      Source.Bookmark := Bookmark;
      Source.EnableControls;
      EnableControls
    end;
  end;
  Active := OldActive;
end;

procedure TDbMemDataSet.DefineProperties(Filer: TFiler);
  function WriteData: boolean;
  begin
    Result := True;
  end;
begin
  inherited DefineProperties(Filer);

  Filer.DefineBinaryProperty('Data', ReadBinaryData, WriteBinaryData,
    WriteData);
end;

procedure TDbMemDataSet.ReadBinaryData(Stream: TStream);
begin
  if voStored in FOptions then
    LoadFromStream(Stream);
end;

procedure TDbMemDataSet.WriteBinaryData(Stream: TStream);
begin
  if voStored in FOptions then
    SaveToStream(Stream, False);
end;

function TDbMemDataSet.IsSequenced: boolean;
begin
  Result := True;
end;

procedure TDbMemDataSet.AddField(Name: string; FieldType: TFieldType; Size: integer; Required: boolean);
begin
  if not (FieldType in SupportFieldTypes) then
    DatabaseError(SNotSupportFieldType);

  FieldDefs.Add(Name, FieldType, Size, Required);
end;

procedure TDbMemDataSet.DeleteField(Name: string);
var
  Stream: TMemoryStream;
  OldActive: boolean;
  FieldDef: TFieldDef;
begin
  FieldDef := FieldDefs.Find(Name);

  OldActive := Active;
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream, False);
    Close;
    Clear;

    FieldDef.Free;
    //FieldDefs.Delete(FieldDef.Index);
  finally
    LoadFromStream(Stream, False);
    Active := OldActive;
    Stream.Free;
  end;
end;

procedure TDbMemDataSet.DeleteFields;
begin
  Clear;
  FieldDefs.Clear;
{$IFDEF VER3}
  while FieldCount > 0 do
    Fields[0].Free;
{$ELSE}
  Fields.Clear;
{$ENDIF}
end;

procedure TDbMemDataSet.Clear;
begin
  if State in [dsInsert,dsEdit] then
    Cancel;
  Data.Close;
  if Active then begin
    Data.Open;
    Resync([]);
  end;
end;

{ Stream/File }

{ Storage format:
  Version        2 // 0 = 2.00, 1 = 2.10
-- FieldDefs
  FieldCount     2
    NameLength   2
    Name         Length(Name)
    DataType     2
    Size         2

-- Fields
  FieldCount     2              -|
    NameLength   2               |
    Name         Length(Name)    | for 1
    Kind         2               |
    DataType     2               |
    Size         4              -|

  RecordCount    4
    Size         4
    Value        Size
}

procedure TDbMemDataSet.LoadFromStream(Stream: TStream; LoadFields: boolean);
var
  D2: Word;
  D4: Cardinal;
  Version: word;
  FieldCount: word;
  FieldName: string;
  FieldType: word;
  FieldSize: word;
  FieldKind: word;
  RecordCount: integer;
  i,j: integer;
  OldActive: boolean;
  St: string;
  WSt: WideString;
  FieldClass: TFieldClass;
  Field: TField;
  LocFieldDefs: TFieldDefs;
begin
  OldActive := Active;
  Close;
  Clear;
  if LoadFields then begin
    LocFieldDefs := FieldDefs;
    DeleteFields;
  end
  else
    LocFieldDefs := TFieldDefs.Create(Self);

  Stream.Seek(0, soFromBeginning);
  with Stream do begin
    Read(Version, 2);  // Version

  // FieldDefs
    FAvoidRefreshData := True;
    Read(FieldCount, 2);
    for i := 0 to FieldCount - 1 do begin
      Read(D2, 2);
      SetLength(FieldName, D2);
      Read(PChar(FieldName)^, D2);
      Read(FieldType, 2);
      Read(FieldSize, 2);

      LocFieldDefs.Add(FieldName, TFieldType(FieldType), FieldSize, False);
      //AddField(FieldName, TFieldType(FieldType), FieldSize);
    end;
    FAvoidRefreshData := False;

    if Version >= 1 then begin
    // Fields
      Read(D2, 2);
      for i := 0 to D2 - 1 do begin
        Read(D2, 2);
        SetLength(FieldName, D2);
        Read(PChar(FieldName)^, D2);
        Read(FieldKind, 2);
        Read(FieldType, 2);
        Read(FieldSize, 2);

        FieldClass := GetFieldClass(TFieldType(FieldType));

        Field := FieldClass.Create(Self.Owner);//  Self);
        try
          Field.FieldName := FieldName;
          Field.FieldKind := TFieldKind(FieldKind);
          case TFieldType(FieldType) of
            ftString:
              Field.Size := FieldSize;
            ftWideString:
              Field.Size := FieldSize * sizeof(WideChar);
          end;
          Field.DataSet := Self;
        except
          Field.Free;
          raise;
        end;
      end;
    end;
    Read(RecordCount, 4);
    if RecordCount > 0 then begin
      DisableControls;

      if not (csReading in ComponentState) then
        Open
      else begin
        DoBeforeOpen;
        try
          OpenCursor(False);
          SetState(dsBrowse)
        except
          SetState(dsInactive);
          CloseCursor;
          raise;
        end;
        DoAfterOpen;
        DoAfterScroll;
      end;

      try
        for j := 0 to RecordCount - 1 do begin
          Append;
          try
            for i := 0 to LocFieldDefs.Count - 1 do begin
              Field := FindField(LocFieldDefs[i].Name);

              Read(D4, 4);
              if D4 > 0 then begin
                if (Field <> nil) and (Field.DataType = ftWideString) then begin
                  SetLength(WSt, D4 div 2);
                  Read(PWideChar(WSt)^, D4);
                  TWideStringField(Field).Value := WSt
                end
                else
                begin
                  SetLength(St, D4);
                  Read(PChar(St)^, D4);

                  if Field <> nil then
                    if Field.DataType in [ftString, ftBlob, ftMemo] then
                      Field.AsString := St
                    else
                      Field.SetData(PChar(St));
                end;
              end;
            end;
          finally
            Post;
          end;
        end;
      finally
        First;
        EnableControls;
      end;

      if not OldActive and (voPersistentData in FOptions) then
        if csReading in ComponentState then begin
          SetState(dsInactive);
          CloseCursor;
        end
        else
          Close;
    end
    else
      Active := OldActive;
  end;

  if LocFieldDefs <> FieldDefs then
    LocFieldDefs.Free;
end;

procedure TDbMemDataSet.SaveToStream(Stream: TStream; StoreFields: boolean);
var
  D2: Word;
  D4: Cardinal;
  St: string;
  i: integer;
  Bookmark: DB.TBookmark;
  OldActive: boolean;
  TempFields: TFields;
  Field: TField;
  FieldDesc: TFieldDesc;
  Buf: array [0..4000] of byte;
  RecBuf: PChar;
  IsNull: boolean;
  Piece: PPieceHeader;
  BufLen: cardinal;

  procedure AssignFields(Dest: TFields; Source: TFields);
  var
    Field:TField;
  begin
    Dest.Clear;
    while Source.Count > 0 do begin
      Field := Source[0];
      Source.Remove(Field);
      Dest.Add(Field);
    end;
  end;
begin
  OldActive := Active;
  with Stream do begin
    D2 := 1;
    Write(D2, 2);  // Version 0 - 2.00 1 - 2.10

  // FieldDefs
    D2 := FieldDefs.Count;
    Write(D2, 2);
    for i := 0 to FieldDefs.Count - 1 do begin
      D2 := Length(FieldDefs[i].Name);
      Write(D2, 2);
      St := FieldDefs[i].Name;
      Write(PChar(St)^, Length(St));
      D2 := Word(FieldDefs[i].DataType);
      Write(D2, 2);
      D2 := FieldDefs[i].Size;
      Write(D2, 2);
    end;

  // Fields
    if DefaultFields or not StoreFields then begin
      D2 := 0;
      Write(D2, 2);
    end
    else begin
      D2 := FieldCount;
      Write(D2, 2);
      for i := 0 to FieldCount - 1 do begin
        D2 := Length(Fields[i].FieldName);
        Write(D2, 2);
        St := Fields[i].FieldName;
        Write(PChar(St)^, Length(St));
        D2 := Word(Fields[i].FieldKind);  // for ver 1
        Write(D2, 2);
        D2 := Word(Fields[i].DataType);
        Write(D2, 2);
        D2 := Fields[i].Size;
        Write(D2, 2);
      end;
    end;

    if FieldDefs.Count = 0 then begin
      D4 := 0;
      Write(D4, 4);
    end
    else begin
      DisableControls;
      Bookmark := GetBookmark;

      if not DefaultFields then begin
        Close;
        TempFields := TFields.Create(nil);
        AssignFields(TempFields, Fields);
        Fields.Clear;
      end
      else
        TempFields := nil;

      Open;
      First;
      try
        D4 := RecordCount;
        Write(D4, 4);

        while not EOF do begin
          for i := 0 to FieldDefs.Count - 1 do begin
            Field := FindField(FieldDefs[i].Name);

            // get field desc and data from record buffer
            FieldDesc := Data.FindField(FieldDefs[i].Name);
            if FieldDesc <> nil then begin
              GetActiveRecBuf(RecBuf);
              Data.GetField(FieldDesc.FieldNo, RecBuf, @Buf, IsNull);
            end
            else
              IsNull := True;

            D4 := 0;
            if not ((Field = nil) or (FieldDesc = nil) or IsNull) then
            begin
              // to write field data there must be Field and FieldDesc
              case FieldDesc.DataType of
                dtString:
                  D4 := Length(PChar(@Buf));
                dtWideString:
                  D4 := Length(PWideChar(@Buf)) * sizeof(WideChar);
                dtInt8, dtInt16, dtInt32, dtInt64, dtUInt16, dtUInt32,
                dtBoolean, dtCurrency, dtFloat:
                  D4 := FieldDesc.Size;
                dtDateTime, dtDate, dtTime:
                  D4 := sizeof(TDateTime);
                dtBlob, dtMemo, dtGraphic, dtFmtMemo:
                  D4 := TBlob(PInteger(@Buf)^).Size;
              else
                Assert(False, SUnknownDataType);
              end;
            end;
            Write(D4, 4);
            if D4 > 0 then begin
              if FieldDesc.DataType in [dtBlob, dtMemo, dtGraphic, dtFmtMemo] then begin
                // save blob to stream
                Piece := TBlob(PInteger(@Buf)^).FirstPiece;

                while Piece <> nil do begin
                  BufLen := Piece.Used;

                  Write(Pointer(PChar(Piece) + Sizeof(TPieceHeader))^, BufLen);

                  Piece := Piece.Next;
                end;
              end
              else
                Write(PChar(@Buf)^, D4);
            end;
          end;
          Next;
        end;
      finally
        if TempFields <> nil then begin
          Close;
          AssignFields(Fields, TempFields);
          TempFields.Free;
        end;

        Active := OldActive;
        if OldActive then
          GotoBookmark(Bookmark);
        FreeBookmark(Bookmark);
        EnableControls;
      end;
    end;
  end;
end;

function TDbMemDataSet.IsFieldDefsStored: boolean;
begin
  Result := FieldDefs.Count > 0;
end;

procedure TDbMemDataSet.LoadFromFile(const FileName: string);
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

procedure TDbMemDataSet.SaveToFile(const FileName: string);
var
  Stream:TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TDbMemDataSet.SetActive(Value: boolean);
begin
  if (csReading in ComponentState) then begin
    if not FStreamedActive then
      FStreamedActive := Value
  end
  else
    inherited;
end;

function TDbMemDataSet.GetFieldDefs: TFieldDefs;
begin
  Result := inherited FieldDefs;
end;

procedure TDbMemDataSet.SetFieldDefs(Value: TFieldDefs);
begin
  inherited FieldDefs := Value;
end;

procedure TDbMemDataSet.LoadFromVariant(Data: Variant; LoadFields: Boolean = True);
var
  FieldCount: word;
  FieldName: string;
  FieldType: word;
  FieldSize: word;
  RecordCount: integer;
  I, J: Integer;
  WasActive: boolean;
  Field: TField;
  LocFieldDefs: TFieldDefs;
begin
  LocFieldDefs := nil;
  WasActive := Active;
  try
    Close;
    Clear;
    if LoadFields then
    begin
      LocFieldDefs := FieldDefs;
      DeleteFields;
    end else
      LocFieldDefs := TFieldDefs.Create(Self);

    FAvoidRefreshData := True;
    FieldCount := VarArrayHighBound(Data, 1);
    for I := 0 to FieldCount - 1 do
    begin
      FieldName := Data[I, 0];
      FieldType := Data[I, 1];
      FieldSize := Data[I, 2];
      LocFieldDefs.Add(FieldName, TFieldType(FieldType), FieldSize, False);
    end;
    FAvoidRefreshData := False;

    RecordCount := VarArrayHighBound(Data, 2) + 1 - 3;
    if RecordCount > 0 then
    begin
      DisableControls;
      try
        Open;
        for J := 0 to RecordCount - 1 do
        begin
          Append;
          try
            for I := 0 to LocFieldDefs.Count - 1 do
            begin
              if not LoadFields then
                Field := FindField(LocFieldDefs[I].Name)
              else Field := Fields[I];
              Field.AsVariant := Data[I, J + 3];
            end;
            Post;
          except
            Cancel;
            raise;
          end;
        end;
      finally
        First;
        EnableControls;
      end;
    end;
  finally
    if Assigned(LocFieldDefs) and (LocFieldDefs <> FieldDefs) then
      LocFieldDefs.Free;
    Active := WasActive;
  end;
end;

end.
