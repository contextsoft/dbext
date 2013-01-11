(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  TCtxDataSet - TDataSet descendant, which can store rows in memory or
(*                access rows from TCtxDataTable object residing in TCtxDataContainer
(*                component.
(*
(*  Copyright (c) 2005-2011, Context Software LLC
(*
(******************************************************************************)
unit CtxDataSet;

{.$DEFINE _NODATASETFILTER}

{$I CtxVer.inc}

interface

uses SysUtils, Classes, DB, CtxDataTypes, CtxData;

{$IFDEF D2009_ORLATER}
type
  TDataSetBookmark = TBytes;
const
  NilBookmark = nil;
{$ELSE}
type
  TDataSetBookmark = String;
  TRecordBuffer = PChar;
const
  NilBookmark = '';
{$ENDIF}

type
  {:$ TCtxDataSet - TDataSet descendant, which can store rows in memory or }
  {:$ access rows from TCtxDataTable object residing in TCtxDataContainer }
  {:$ component. }
  TCtxDataSet = class(TDataSet)
  private
    FInDelete: boolean;
  protected
    { FieldMap }
    FRecBufSize: integer;
    FFlagOffset: integer;
    FDataOffset: integer;
    FOffsets: array of integer;
    FColumns: array of TCtxDataColumn;

    FCurRec: Integer;
    FLastBookmark: Integer;
    FSaveChanges: Boolean;
    FCursorOpen: Boolean;
    FInsertedRow: TCtxDataRow;

    FIsInternal: boolean;
    FDataContainer: TCtxDataContainer;
    FCursor: TCtxRowSet;
    FDataTable: TCtxDataTable;
    FDataTableName: String;

    // Master - Detail
    FMasterLink: TMasterDataLink;
    FDetailFields: string;

    FAutoDisplayLabels: Boolean;
    FAutoFillDataTable: Boolean;
    FOrderBy: String;

    FFilterEvaluator: TObject;
    FIgnoreFilteringForLookup: Boolean;

    procedure OnFilterDataRow(ARow: TCtxDataRow; var Accept: boolean);
    procedure OnNotifyDataEvent(Context: TObject; DataEvent: TCtxDataEventType);
    function GetDataTable: TCtxDataTable;
    function  GetDataContainer: TCtxDataContainer;
    procedure SetDataContainer(const Value: TCtxDataContainer);
    procedure SetDataTableName(const Value: String); // TCtxTable

    // procedure DataBufferToObject(ABuf: PChar; AObj: TCtxDataRow);
    // function ObjectToDataBuffer(AObj: TCtxDataRow; ABuf: PChar): boolean;
    function CheckRowBuffer(ARow: TCtxDataRow; ABuf: TRecordBuffer): Boolean;
    function CheckTableRowBuffer(ARow: TCtxDataRow; ABuf: TRecordBuffer): Boolean;
    function ValidRow(ARow: TCtxDataRow): Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { Data buffer access }
    procedure CalcBufSizes;
    { Overriden abstract methods (required) }
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;

    function  _GetFieldSize(AField: TField): integer;
    procedure _FreeRecordPointers(AData: TRecordBuffer);

    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalClose; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalEdit; override;
    procedure InternalInsert; override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalCancel; override;
    procedure InternalRefresh; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen: Boolean; override;
    procedure OpenCursor(InfoQuery: Boolean = False); override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure ClearCalcFields(Buffer: TRecordBuffer); override;
    procedure DoOnNewRecord; override;
    procedure CreateInternalDataColumns;
    procedure SetOrderBy(const Value: String);
    function GetDataRow: TCtxDataRow;

    procedure DoOnDataEvent(Sender, Context: TObject; DataEvent: TCtxDataEventType);
  protected
    { Additional overrides (optional) }
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    procedure CheckActive; override;
    function FindRecord(Restart, GoForward: Boolean): boolean; override;
    function LocateObject(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): TCtxDataRow;
    function GetActiveRecBuf(var RecBuf: TRecordBuffer): boolean;
    //Master-Detail
    procedure SetMasterFields(const Value: string);
    function GetMasterFields: string;
    procedure SetDetailFields(const Value: string);
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    procedure SetDataSource(Value: TDataSource);
    function GetDataSource: TDataSource; override;
    procedure InternalUpdateFilter;
    procedure UpdateOrderBy;
    //OldValue Support
    function GetStateFieldValue(State: TDataSetState; Field: TField): Variant; override;
  public
    {:$ Creates an instance of TCtxDataSet component. }
    constructor Create(AOwner: TComponent); override;
    {:$ Closes and destroys the instance of TCtxDataSet component and frees all allocated }
    {:$ resource. The data will NOT be destroyed if it resides in an external container. }
    destructor Destroy; override;

    {:$ Retrieves the current value of a field into a buffer. }
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    {:$ Retrieves the current value of a field into a buffer. }
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean); override;

    {:$ Retrieves the row referenced by value in the specified Column. }
    function GetReferencedRow(Column: TCtxDataColumn): TCtxDataRow;

    {:$ Force sync with master even if master link is disabled . }
    procedure MasterSync;

    {:$ Saves data to a disk file. This method will write all data from a data  }
    {:$ table (TCtxDataTable) in proprietary binary format. It will NOT respect filters or }
    {:$ persistent fields defined for this data set. }
    procedure SaveToFile(const FileName: String);
    {:$ Loads data from a disk file previously saved using SaveToFile method. }
    procedure LoadFromFile(const FileName: String);
    {:$ Saves data to stream object. This method will write all data from a data  }
    {:$ table (TCtxDataTable) in proprietary binary format. It will NOT respect filters or }
    {:$ persistent fields defined for this data set. }
    procedure SaveToStream(Stream: TStream);
    {:$ Loads data from stream previously writted by SaveToStream method. }
    procedure LoadFromStream(Stream: TStream);
    {:$ Loads data from 2-dimensional variant array. If LoadFields parameter is True }
    {:$ Then first 3 rows of variant array must contain field names, data types and sizes. }
    procedure LoadFromVariant(Data: Variant; LoadFields: Boolean = True);

    {:$ Retrieve field values from a record that matches specified search values. }
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    {:$ Retrieve field values from a record that matches specified search values. }
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    {:$ Lists field components that link this dataset as a detail of a master dataset.}
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList); override;
    {:$ Creates a blob stream for a Binary large object (BLOB) field in the dataset. }
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    {:$ Indicates whether the underlying database table uses record numbers to indicate the order of records.}
    function IsSequenced: boolean; override;
    {:$ Updates display label for peristent fields from meta-data associated with TCtxDataTable object }
    {:$ referenced by DataTable (or DataTableName) property.}
    procedure UpdateDisplayLabels;

    {:$ Returns True if records has been inserted deleted or updated after the last AcceptChanges.}
    function IsDataModified: Boolean;
    {:$ Returns True if current record has been inserted or updated after the last AcceptChanges.}
    function IsRecordModified: Boolean;

    {:$ Contains reference to TCtxDataTable object, which stores data. }
    property DataTable: TCtxDataTable read GetDataTable;
    property DataRow: TCtxDataRow read GetDataRow;
  published
    {:$ References data container component, which contains actual data in data tables (TCtxDataTable object). }
    {:$ If this property is left unassigned, then when activating this dataset }
    {:$ an internal data container with one data table will be created based on }
    {:$ specified FieldDefs or peristent fields. The data in internal data container will }
    {:$ only live while the DataSet is open and will be cleared once Active property is set to False. }
    property DataContainer: TCtxDataContainer read GetDataContainer write SetDataContainer;
    {:$ Specifies data table name in the data container referenced by DataContainer property. }
    property DataTableName: String read FDataTableName write SetDataTableName;
    {:$ Specifies one or more fields in a master table to link with corresponding }
    {:$ fields in this table in order to establish a master-detail relationship between the tables. }
    property MasterFields: string read GetMasterFields write SetMasterFields;
    {:$ Specifies one or more fields in this table to link with corresponding }
    {:$ fields in master data source (see MasterSource property) in order to establish }
    {:$ a master-detail relationship between the tables. }
    property DetailFields: string read FDetailFields write SetDetailFields;
    {:$ Specifies the name of the data source for a dataset to use as a master table in establishing }
    {:$ a detail-master relationship between this table and another one. }
    property MasterSource: TDataSource read GetDataSource write SetDataSource;
    {:$ If True, then display labels for TField(s) will be updated automatically }
    {:$ from meta-data associated with TCtxDataTable object. }
    property AutoDisplayLabels: Boolean read FAutoDisplayLabels write FAutoDisplayLabels default True;
    {:$ Determines the order in which records will appear in this DataSet. }
    property OrderBy: String read FOrderBy write SetOrderBy;
    {:$ Determines whether the dataset will attempt to automatically fill data table upon opening. }
    property AutoFillDataTable: Boolean read FAutoFillDataTable write FAutoFillDataTable default False;
    {:$ Determines if filter will be used when searching for lookup value using Lookup method. }
    {:$ If this property is False (by default) then only values which fall within filtering and ranging criteria will }
    {:$ be returned by Lookup. Otherwise, Lookup will search the whole table. }
    property IgnoreFilteringForLookup: Boolean read FIgnoreFilteringForLookup write FIgnoreFilteringForLookup default False;

    property FieldDefs;
    property Active;
    property Filter;
    property Filtered;
    property FilterOptions;
    property AutoCalcFields;
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
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  TCtxBlobStream = class(TMemoryStream)
  protected
    FField: TBlobField;
    FDataSet: TCtxDataSet;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    FOpened: Boolean;
    procedure LoadBlobData;
    procedure SaveBlobData;
    function Realloc(var NewCapacity: Longint): Pointer; override;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;


const
  DEFAULT_TABLE_NAME = 'Table';

  {:$ Converts TFieldType enumeration value to TCtxDataType enumeration value. }
  function GetCtxDataType(AType: TFieldType): TCtxDataType;
  {:$ Converts TCtxDataType enumeration value to TFieldType enumeration value. }
  function GetFieldType(CtxDataType: TCtxDataType): TFieldType;

resourcestring
  SUnableToWriteIntoReadOnlyBlobStream = 'Unable to write into a read-only blob stream';

implementation

uses DBConsts, Variants  {$IFnDEF _NODATASETFILTER}, dbExtParser {$ENDIF};

const
  ARefTypes = [ftString, ftWideString, ftBlob, ftMemo, ftGraphic, ftFmtMemo];

{$IFnDEF _NODATASETFILTER}
type
  TDataSetContext = class (TExpressionContext)
    class function GetItem(context: pointer; const name: string): variant; override;
  end;

{ TDataSetContext }

class function TDataSetContext.GetItem(context: pointer;
  const name: string): variant;
begin
  Result := TDataSet(context).FieldByName(name).Value;
end;
{$ENDIF}

type
  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    Idx: Integer;
    Obj: TCtxDataRow;
    Flag: TBookmarkFlag;
  end;

function GetFieldType(CtxDataType: TCtxDataType): TFieldType;
begin
  case CtxDataType of
    cdtString:
      Result := ftString;
    cdtWideString:
      Result := ftWideString;
    cdtSmallInt:
      Result := ftSmallInt;
    cdtLargeInt:
      Result := ftLargeInt;
    cdtInteger:
      Result := ftInteger;
    cdtFloat:
      Result := ftFloat;
    cdtDateTime:
      Result := ftDateTime;
    cdtDate:
      Result := ftDate;
    cdtTime:
      Result := ftTime;  
    cdtBoolean:
      Result := ftBoolean;
    cdtGuid:
      Result := ftGuid;
    cdtBlob:
      Result := ftBlob;
    cdtMemo:
      Result := ftMemo;
  else
    Result := ftUnknown;
  end;
end;

function GetCtxDataType(AType: TFieldType): TCtxDataType;
begin
  case AType of
    ftUnknown:
      Result := cdtUnknown;
    ftString:
      Result := cdtString;
    ftInteger:
      Result := cdtInteger;
    ftBoolean:
      Result := cdtBoolean;
    ftFloat:
      Result := cdtFloat;
    ftDateTime, ftTimeStamp:
      Result := cdtDateTime;
    ftDate:
      Result := cdtDate;
    ftTime:
      Result := cdtTime;
    ftSmallInt:
      Result := cdtSmallInt;
    ftLargeInt:
      Result := cdtLargeInt;
    ftBlob, ftGraphic:
      Result := cdtBlob;
    ftMemo, ftFmtMemo:
      Result := cdtMemo;
    ftCurrency:
      Result := cdtFloat;
    ftBCD:
      Result := cdtFloat;
    ftGuid:
      Result := cdtGuid;
  else
    Result := cdtUnknown;
  end;
end;

{ TCtxDataSet }

constructor TCtxDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMasterLink := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange := MasterChanged;
  FMasterLink.OnMasterDisable := MasterDisabled;
  FCursorOpen := False;
  FCursor := TCtxRowSet.Create(nil);
  FAutoDisplayLabels := True;
  SetLength(FOffsets, 0);
  SetLength(FColumns, 0);
  FInsertedRow := nil;
  FFilterEvaluator := nil;
  FIgnoreFilteringForLookup := False;
end;

destructor TCtxDataSet.Destroy;
begin
  inherited Destroy;
  // Active := False;
  FCursor.Free;
  FCursor := nil;
  FMasterLink.Free;
  FMasterLink := nil;
  FFilterEvaluator.Free;
  FFilterEvaluator := nil;
end;

procedure TCtxDataSet.OnNotifyDataEvent(Context: TObject; DataEvent: TCtxDataEventType);
var
  T: Integer;
begin
  if Context = nil then
    Exit;

  case DataEvent of
    cdeContainerDeactivated:
      Active := False;
    cdeContainerDataChanged,
    cdeTableDataChanged:
      if Active then
      begin
        if State in dsEditModes then
          Cancel;
        Refresh;
      end;
    cdeCursorClosed:
      if Context = FCursor then
        Active := False;
    cdeRowDeleted:
      if not FInDelete then
      begin
        if State in dsEditModes then
          Cancel;

        T := FCursor.IndexOfRow(DataRow);
        if T >= 0 then
          FCurRec := T;

        Resync([]);
      end;
  end;
end;


function TCtxDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Result, FRecBufSize);
  FillChar(Result^, FRecBufSize, 0);
end;

procedure TCtxDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  _FreeRecordPointers(Buffer);
  FreeMem(Buffer, FRecBufSize);
end;

procedure TCtxDataSet._FreeRecordPointers(AData: TRecordBuffer);
var
  I: integer;
  P: pointer;
begin
  for I := 0 to FieldCount - 1 do
    if (FColumns[I] = nil) and (Fields[I].DataType in ARefTypes) then
    begin
      P := AData + FOffsets[I];
      if Fields[I].DataType = ftWideString then
        PWideString(P)^ := ''
      else PAnsiString(P)^ := '';
    end;
end;

function TCtxDataSet._GetFieldSize(AField: TField): Integer;
begin
  case AField.DataType of
    ftBlob, ftMemo, ftGraphic, ftString, ftWideString:
      Result := SizeOf(Pointer);
    ftSmallInt, ftWord:
      Result := SizeOf(SmallInt);
    ftAutoInc, ftInteger:
      Result := SizeOf(Integer);
    ftFloat, ftCurrency, ftDate, ftTime, ftDateTime, ftTimeStamp:
      Result := SizeOf(Double);
    ftLargeint:
      Result := SizeOf(Int64);
    ftGUID:
      Result := SizeOf(TGuid);
    ftBoolean:
      Result := SizeOf(WordBool);
    else
      Result := 0;
  end;
end;

{
Buffer structure:
  RecInfo
  NullFlags
  Data
}

procedure TCtxDataSet.CalcBufSizes;
var
  I: integer;
  F: integer;
  C: TCtxDataColumn;
begin
  FFlagOffset := SizeOf(TRecInfo);
  F := Fields.Count;
  if F mod 8 <> 0 then
    F := (F div 8) + 1 else
    F := F div 8;
  FDataOffset := FFlagOffset + F;
  SetLength(FOffsets, Fields.Count);
  SetLength(FColumns, Fields.Count);
  FRecBufSize := FDataOffset;
  for I := 0 to Fields.Count-1 do
  begin
    if Fields[I].FieldKind = fkData then
      C := DataTable.Columns.Find(Fields[I].FieldName)
    else C := nil;
    FColumns[I] := C;
    if C <> nil then
      FOffsets[I] := -1 // assign invalid offset
    else
    begin
      FOffsets[I] := FRecBufSize;
      Inc(FRecBufSize, _GetFieldSize(Fields[I]));
    end;
  end;
end;

(*

  MB: 11/16/2007 - unused. we store data directly in the row now so we
  never copy it, but access via GetFieldData and SetFieldData
  Same true for ObjectToDataBuffer method. Validation function is
  replaced by CheckRowBuffer function.

procedure TCtxDataSet.DataBufferToObject(ABuf: PChar; AObj: TCtxDataRow);
var
  I, FldIdx: integer;
  F: TField;
  B: PChar;
begin
  // Data to Object +++
  AObj.BeginEdit;
  try
    for I := 0 to Fields.Count-1 do
    begin
      F := Fields[I];
      FldIdx := F.Index;
      if FColumns[FldIdx] <> nil then
      begin
        if GetBit(ABuf+FFlagOffset, FldIdx) then
          B := ABuf + FOffsets[FldIdx] else
          B := nil;
        AObj.SetColumnData(FColumns[FldIdx], B);
      end;
    end;
    AObj.EndEdit;
  except
    AObj.CancelEdit;
    raise;
  end;
end;

function TCtxDataSet.ObjectToDataBuffer(AObj: TCtxDataRow; ABuf: PChar): boolean;
var
  I: integer;
  F: TField;
  C: TCtxDataColumn;
begin
  I := FCursor.IndexOfRow(AObj);
  if (ABuf <> TempBuffer) and (I < 0) or AObj.Deleted then
  begin
    Result := False;
    exit;
  end;
  _FreeRecordPointers(ABuf);
  PRecInfo(ABuf).Obj := AObj;
  PRecInfo(ABuf).Idx := I;
// Object to Data+++
  for I := 0 to Fields.Count-1 do
  begin
    F := Fields[I];
    C := FColumns[F.Index];
    if C <> nil then
    begin
      AObj.GetColumnData(C, ABuf+FOffsets[F.Index]);
      SetBit(ABuf+FFlagOffset, F.Index, AObj.AssignedValue[C]);
    end;
  end;
  Result := True;
end;
*)

function TCtxDataSet.ValidRow(ARow: TCtxDataRow): Boolean;
begin
  Result := FCursor.IndexOfRow(ARow) >= 0;
end;

function TCtxDataSet.CheckRowBuffer(ARow: TCtxDataRow; ABuf: TRecordBuffer): Boolean;
var
  RowIdx: Integer;
begin
  // MB: 11/16/2007
  // this function returns true if Row is valid and the buffer is
  // initialized successfully.
  Result := False;
  RowIdx := FCursor.IndexOfRow(ARow);
  if (RowIdx < 0) or ARow.Deleted then exit;
  if (ABuf <> TempBuffer) and (RowIdx < 0) then exit;
  _FreeRecordPointers(ABuf);
  // Initialize record info for buffer, so that we have access to fields
  PRecInfo(ABuf).Obj := ARow;
  PRecInfo(ABuf).Idx := RowIdx;
  Result := True;
end;

function TCtxDataSet.CheckTableRowBuffer(ARow: TCtxDataRow; ABuf: TRecordBuffer): Boolean;
var
  RowIdx: Integer;
begin
  // MB: 11/16/2007
  // this function returns true if Row is valid and the buffer is
  // initialized successfully.
  Result := False;
  RowIdx := FDataTable.IndexOfRow(ARow); // MB: 101309 was ARow.Index; which may cause A/V if row is invalid
  if (RowIdx < 0) or ARow.Deleted then exit;
  if (ABuf <> TempBuffer) and (RowIdx < 0) then exit;
  _FreeRecordPointers(ABuf);
  // Initialize record info for buffer, so that we have access to fields
  PRecInfo(ABuf).Obj := ARow;
  PRecInfo(ABuf).Idx := RowIdx;
  Result := True;
end;


function TCtxDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TCtxBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TCtxDataSet.CheckActive;
begin
  Active := True;
  inherited CheckActive;
end;

procedure TCtxDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PRecInfo(Data)^ := PRecInfo(Buffer)^;
end;

procedure TCtxDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PRecInfo(Buffer)^ := PRecInfo(Data)^;
end;

function TCtxDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer).Flag;
end;

procedure TCtxDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer).Flag := Value;
end;

function TCtxDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  I, S: integer;
  B, P: TRecordBuffer;
  StrValue: AnsiString;
  {$IFDEF D2009_ORLATER}
  WStrValue: WideString;
  {$ENDIF}
begin
  Result := False;
  if not GetActiveRecBuf(B) then
    Exit;

  I := Field.Index;
  if FColumns[I] <> nil then
  begin
    Result := PRecInfo(B).Obj.AssignedValue[FColumns[I]];
    if Buffer = nil then exit;
    if Field.DataType = ftString then
    begin
      // String is apparently the only data type stored in buffer
      PRecInfo(B).Obj.GetColumnData(FColumns[I], @StrValue);
      StrPLCopy(Buffer, StrValue, Field.DataSize);
    end else if Field.DataType = ftWideString then
    begin
      {$IFDEF D2009_ORLATER}
      PRecInfo(B).Obj.GetColumnData(FColumns[I], @WStrValue);
      StrPLCopy(Buffer, WStrValue, Field.DataSize);
      {$ELSE}
      PRecInfo(B).Obj.GetColumnData(FColumns[I], Buffer);
      {$ENDIF}
    end else
      PRecInfo(B).Obj.GetColumnData(FColumns[I], Buffer);
  end else
  begin
    S := _GetFieldSize(Field);
    if S = 0 then exit;
    Result := GetBit(B+FFlagOffset, I);
    if Buffer = nil then exit;

    if not Result then
      FillChar(Buffer^, Field.DataSize, 0)
    else begin
      P := B + FOffsets[I];
      // We don't handle blob type fields here, because
      // VCL does not allow for caluclated blob fields
      if Field.DataType = ftString then
        StrPLCopy(Buffer, PAnsiString(P)^, Field.DataSize)
      else if Field.DataType = ftWideString then
        {$IFDEF D2009_ORLATER}
        StrPLCopy(Buffer, PWideString(P)^, Field.DataSize)
        {$ELSE}
        PWideString(Buffer)^ := PWideString(P)^
        {$ENDIF}
      else if Field.DataType in ARefTypes then
        PAnsiString(Buffer)^ := PAnsiString(P)^
      else
        Move(P^, Buffer^, S);
    end;
  end;
end;

procedure TCtxDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  I, S: integer;
  P: Pointer;
  B: TRecordBuffer;
  StrValue: AnsiString;
  {$IFDEF D2009_ORLATER}
  WStrValue: WideString;
  {$ENDIF}
begin
  if State = dsBrowse then
    Edit;
  if not (State in dsWriteModes) then
    DatabaseError(SNotEditing);
  if not GetActiveRecBuf(B) then
    exit;

  I := Field.Index;
  if FColumns[I] <> nil then
  begin
    if (Buffer <> nil) and (Field.DataType = ftString) then
    begin
      StrValue := PAnsiChar(Buffer);
      PRecInfo(B).Obj.SetColumnData(FColumns[I], @StrValue);
    end else if Field.DataType = ftWideString then
    begin
      {$IFDEF D2009_ORLATER}
      WStrValue := PWideChar(Buffer);
      PRecInfo(B).Obj.SetColumnData(FColumns[I], @WStrValue);
      {$ELSE}
      PRecInfo(B).Obj.SetColumnData(FColumns[I], Buffer);
      {$ENDIF}
    end else
      PRecInfo(B).Obj.SetColumnData(FColumns[I], Buffer);
  end else
  begin
    S := _GetFieldSize(Field);
    if S = 0 then
      exit;
    SetBit(B+FFlagOffset, I, Buffer <> nil);
    if Buffer <> nil then
    begin
      P := B + FOffsets[I];
      // We don't handle blob type fields here, because
      // VCL does not allow for caluclated blob fields
      if Field.DataType = ftString then
        PAnsiString(P)^ := PAnsiChar(Buffer)
      else if Field.DataType = ftWideString then
        {$IFDEF D2009_ORLATER}
        PWideString(P)^ := PWideChar(Buffer)
        {$ELSE}
        PWideString(P)^ := PWideString(Buffer)^
        {$ENDIF}
      else if Field.DataType in ARefTypes then
        PAnsiString(P)^ := PAnsiString(Buffer)^
      else
        Move(Buffer^, P^, S);
    end;
  end;
  DataEvent(deFieldChange, Longint(Field));
end;

function TCtxDataSet.GetStateFieldValue(State: TDataSetState; Field: TField): Variant;
var
  B: TRecordBuffer;
  Obj: TCtxDataRow;
  I: integer;
begin
  if (State = dsOldValue) and GetActiveRecBuf(B) then
  begin
    Obj := PRecInfo(B)^.Obj;
    I := Field.Index;
    if (Obj <> nil) and (FColumns[I] <> nil) and (not Obj.Inserted) then
      Result := Obj.OriginalValue[FColumns[I]]
    else
      Result := Null;
  end else
    Result := inherited GetStateFieldValue(State, Field);
end;

procedure TCtxDataSet.ClearCalcFields(Buffer: TRecordBuffer);
begin
  // ?
end;

function TCtxDataSet.GetRecNo: Integer;
var
  B: TRecordBuffer;
begin
  if GetActiveRecBuf(B) then
    Result := PRecInfo(B)^.Idx+1 else
    Result := 0;
end;

procedure TCtxDataSet.SetRecNo(Value: Integer);
begin
  if (Value > 0) and (Value <= RecordCount) then
  begin
    FCurRec := Value-1;
    Resync([]);
  end;
end;

procedure TCtxDataSet.SetFiltered(Value: Boolean);
begin
  if Active then
  begin
    CheckBrowseMode;
    if Filtered <> Value then
    begin
      InternalUpdateFilter;
      inherited SetFiltered(Value);
      FCursor.Filtered := Filtered;
    end;
    First;
  end
  else
    inherited SetFiltered(Value);
end;

function TCtxDataSet.GetActiveRecBuf(var RecBuf: TRecordBuffer): boolean;
begin
  RecBuf := nil;
  case State of
    dsBlockRead, dsBrowse:
      if IsEmpty then RecBuf := nil else RecBuf := ActiveBuffer;
    dsEdit, dsInsert:
      RecBuf := ActiveBuffer;
    dsCalcFields:
      RecBuf := CalcBuffer;
    dsFilter:
      RecBuf := TempBuffer;
    dsNewValue:
      RecBuf := ActiveBuffer;
    dsOldValue:;
    dsSetKey:;
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

procedure TCtxDataSet.OnFilterDataRow(ARow: TCtxDataRow; var Accept: boolean);
var
  SaveState: TDataSetState;
  SaveModified: Boolean;
  B: TRecordBuffer;
begin
  Accept := True;
  if Assigned(OnFilterRecord) or (FFilterEvaluator <> nil) then
  begin
    SaveState := SetTempState(dsFilter);
    SaveModified := Modified;
    try
      B := TempBuffer;
      with PRecInfo(B)^ do
      begin
        Flag := bfCurrent;
        Idx := 0;
        Obj := ARow;
      end;
      if CheckTableRowBuffer(ARow, B) then
      begin
        GetCalcFields(B);
        try
          {$IFnDEF _NODATASETFILTER}
          if Assigned(OnFilterRecord) then
            OnFilterRecord(Self, Accept)
          else
            Accept := TEvaluator(FFilterEvaluator).Evaluate;
          {$ELSE}
          OnFilterRecord(Self, Accept);
          {$ENDIF}
        except
          InternalHandleException();
        end;
      end;
    finally
      RestoreState(SaveState);
      SetModified(SaveModified);
    end;
  end;
end;

function TCtxDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  RecCount:integer;
begin
  RecCount := FCursor.RowCount;
  if RecCount < 1 then
    Result := grEOF else
  begin
    Result := grOK;
    case GetMode of
      gmNext:
        if FCurRec < RecCount-1 then
          inc(FCurRec) else
          Result := grEOF;
      gmPrior:
        if FCurRec > 0 then
          dec(FCurRec) else
          Result := grBOF;
      gmCurrent:
        if (FCurRec < 0) or (FCurRec >= RecordCount) then
          Result := grError;
    end;
    if Result = grOK then
    begin
      with PRecInfo(Buffer)^ do
      begin
        Flag := bfCurrent;
        Idx := FCurRec;
        Obj := FCursor.Rows[Idx];
      end;
      if not CheckRowBuffer(PRecInfo(Buffer)^.Obj, Buffer) then
      begin
        Result := grError;
        Exit;
      end;
      GetCalcFields(Buffer);
    end else
    {
    if (Result = grError) and DoCheck then
      DatabaseError('No Records');
    }
    if Result = grError then
      if DoCheck then
        DatabaseError(SRecordNotFound)
      else FCurRec := -1;
  end;
end;

function TCtxDataSet.GetRecordCount: Integer;
begin
  CheckActive;
  Result := FCursor.RowCount;
end;

function TCtxDataSet.GetRecordSize: Word;
begin
  Result := FRecBufSize;
end;

procedure TCtxDataSet.InternalFirst;
begin
  FCurRec := -1;
end;

procedure TCtxDataSet.InternalLast;
begin
  FCurRec := FCursor.RowCount;
end;

procedure TCtxDataSet.OpenCursor(InfoQuery: Boolean = False);
begin
  FIsInternal := FDataContainer = nil;
  if FIsInternal then
  begin
    FDataContainer := TCtxDataContainer.Create(nil);
    FDataTable := FDataContainer.Tables.Add;
    FDataTableName := DEFAULT_TABLE_NAME;
    FDataTable.Name := FDataTableName;

    CreateInternalDataColumns;
  end;
  FDataTable := FDataContainer.Tables.Get(FDataTableName);

  FCursor.DataTable := FDataTable;
  FCursor.Relation := nil;
  FCursor.OnNotifyDataEvent := OnNotifyDataEvent;
  FCursor.OnFilterDataRow := OnFilterDataRow;
  FCursor.Filtered := Filtered;
  InternalUpdateFilter;

  if FDetailFields <> '' then
    FCursor.SetMasterKey(FDetailFields, NULL);

  // MB: Added initialization in case MasterLink is already assigned and Active +++
  if (FDetailFields <> '') and FMasterLink.Active and (FMasterLink.Fields.Count > 0) then
    FCursor.SetMasterKey(FDetailFields, DataSource.DataSet.FieldValues[FMasterLink.FieldNames]);

  UpdateOrderBy;

  FieldDefs.Updated := False;
  inherited OpenCursor(InfoQuery);

  FCursor.Active := True;
end;

procedure TCtxDataSet.InternalOpen;
begin
  if DataTable = nil then
    DatabaseError(SDataTableNotFound);

  // MB: Container must be active +++
  FDataTable.DataContainer.Active := True;

  // MB: Fill data table if it is empty
  if AutoFillDataTable and (FDataTable.DataContainer.DataAdapter <> nil) and (FDataTable.RowCount = 0) then
    FDataTable.Fill();

  FRecBufSize := SizeOf(TRecInfo);
  BookmarkSize := SizeOf(TRecInfo);
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;

  if FAutoDisplayLabels then
    UpdateDisplayLabels;

  BindFields(True);

  CalcBufSizes;
  FCurRec := -1;

  FCursorOpen := True;
end;

procedure TCtxDataSet.InternalClose;
begin
  FreeAndNil(FInsertedRow);
  if FIsInternal then
    FreeAndNil(FDataContainer);
  FIsInternal := False;
  FCursor.OnNotifyDataEvent := nil;
  FDataTable := nil;
  FCursor.Active := False;
  BindFields(False);
  if DefaultFields then DestroyFields;
  FCursorOpen := False;
end;

procedure TCtxDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  if Buffer <> nil then
  begin
    _FreeRecordPointers(Buffer);
    FillChar(Buffer^, RecordSize, 0);
    PRecInfo(Buffer)^.Idx := -1;
  end;
end;

procedure TCtxDataSet.InternalEdit;
begin
  // Edit Row
  PRecInfo(ActiveBuffer)^.Obj.BeginEdit;
end;

procedure TCtxDataSet.InternalInsert;
var
  I: Integer;
begin
  FreeAndNil(FInsertedRow);
  FInsertedRow := FDataTable.New;
  PRecInfo(ActiveBuffer).Obj := FInsertedRow;
  for I := 0 to Fields.Count - 1 do
  with Fields[I] do
  if Required and (FieldKind = fkData) then
  begin
    if DataType in [ftString, ftWideString] then
       AsString := DefaultExpression
    else if DefaultExpression <> '' then
       AsString := DefaultExpression;
  end;
end;

procedure TCtxDataSet.InternalDelete;
var
  I: TRecInfo;
begin
  // Delete Object
  FInDelete := True;
  try
    I := PRecInfo(ActiveBuffer)^;
    FDataTable.Delete(I.Obj);
  finally
    FInDelete := False;
  end;
end;

procedure TCtxDataSet.InternalPost;
var
  I: PRecInfo;
  T: Integer;
begin
  if not (State in dsEditModes) then
    DatabaseError(SNotEditing);
  // Insert or Update Object +++
  I := PRecInfo(ActiveBuffer);
  if State = dsInsert then
  begin
    ASSERT(I.Obj = FInsertedRow);
    FDataTable.Insert(I.Obj);
    FInsertedRow := nil;
    (*
    I.Obj := FDataTable.New;
    try
      DataBufferToObject(ActiveBuffer, I.Obj);
      FDataTable.Insert(I.Obj);
    except
      FreeAndNil(I.Obj);
      raise;
    end;
    *)
  end else
  begin
    if not PRecInfo(ActiveBuffer)^.Obj.Editing then
      DatabaseError(SNotEditing);
    // DataBufferToObject(ActiveBuffer, I.Obj);
    // End editing row
    PRecInfo(ActiveBuffer)^.Obj.EndEdit;
  end;
  I.Idx := FCursor.IndexOfRow(I.Obj);
  T := I.Idx;
  Resync([]);
  FCurRec := T;
end;

procedure TCtxDataSet.InternalCancel;
var
  I: PRecInfo;
begin
  I := PRecInfo(ActiveBuffer);
  if not (State in dsEditModes) then
    DatabaseError(SNotEditing);
  if ValidRow(I.Obj) and I.Obj.Editing then
    I.Obj.CancelEdit;
end;

procedure TCtxDataSet.InternalRefresh;
begin
  FCursor.Refresh;
  ClearBuffers;
end;

procedure TCtxDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  FCurRec := PRecInfo(Bookmark)^.Idx;
end;

procedure TCtxDataSet.InternalHandleException;
begin
  inherited;
end;

procedure TCtxDataSet.InternalInitFieldDefs;
var
  I: integer;
  aSize: word;
  FieldType: TFieldType;
  FieldDef: TFieldDef;
begin
  // Initialize FieldDefs from DataTable
  if FIsInternal or (FDataTable = nil) or FieldDefs.Updated then Exit;

  FDataTable.DataContainer.Active := True;

  FieldDefs.BeginUpdate;
  try
    FieldDefs.Clear;
    for I := 0 to FDataTable.Columns.Count - 1 do
    begin
      with FDataTable.Columns[I] do
      begin
        FieldType := GetFieldType(FDataTable.Columns[I].DataType);
        aSize := 0;
        case FieldType of
          ftString, ftWideString {$IFDEF VER5P},ftGuid{$ENDIF}:
          begin
            aSize := DataLength;
            if aSize = 0 then
              aSize := 1;
          end;
          ftBytes, ftVarBytes:
            aSize := DataLength;
          ftBlob, ftMemo, ftFmtMemo, ftGraphic:
            aSize := SizeOf(Pointer);
        end;
        if FieldType <> ftUnknown then
        begin
          FieldDef := TFieldDef.Create(FieldDefs, Name, FieldType, aSize, Required, I+1);
          if ReadOnly then
            FieldDef.Attributes := FieldDef.Attributes + [DB.faReadonly];
        end;
      end;
    end;
  finally
    FieldDefs.EndUpdate;
  end;
  FieldDefList.Update;
  FieldDefs.Updated := True;
end;

procedure TCtxDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  InternalGotoBookmark(Buffer);
end;

function TCtxDataSet.IsCursorOpen: Boolean;
begin
  // Check if Open+++
  Result := FCursorOpen and (FDataTable <> nil) and FDataTable.Prepared;
end;

// Master - Detail
procedure TCtxDataSet.MasterChanged(Sender: TObject);
begin
  CheckBrowseMode;
  if FMasterLink.Active then
    MasterSync;
end;

procedure TCtxDataSet.MasterSync;
begin
  if (FMasterLink.Fields.Count > 0)
    and (FDetailFields <> '')
    and FCursor.SetMasterKey(FDetailFields, DataSource.DataSet.FieldValues[FMasterLink.FieldNames])
  then
    First;
end;

procedure TCtxDataSet.MasterDisabled(Sender: TObject);
begin
  if FCursor.SetMasterKey('', Null) then
    First;
end;

function TCtxDataSet.GetDataSource: TDataSource;
begin
  Result := FMasterLink.DataSource;
end;

procedure TCtxDataSet.SetDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then DatabaseError('Circular DataLink', Self);
  FMasterLink.DataSource := Value;
end;

function TCtxDataSet.GetMasterFields: string;
begin
  Result := FMasterLink.FieldNames;
end;

procedure TCtxDataSet.SetMasterFields(const Value: string);
begin
  FMasterLink.FieldNames := Value;
end;

procedure TCtxDataSet.SetDetailFields(const Value: string);
begin
  if FDetailFields <> Trim(Value) then
  begin
    FDetailFields := Trim(Value);
    if Active then
      MasterChanged(nil);
  end;
end;

procedure TCtxDataSet.DoOnNewRecord;
begin
  if FMasterLink.Active and (FMasterLink.Fields.Count > 0) then
    FieldValues[FDetailFields] := DataSource.DataSet.FieldValues[FMasterLink.FieldNames];
  inherited DoOnNewRecord;
end;

procedure TCtxDataSet.GetDetailLinkFields(MasterFields, DetailFields: TList);
var
  I: integer;
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
  MasterFields.Clear;
  for I := 0 to DataSource.DataSet.Fields.Count-1 do
    MasterFields.Add(DataSource.DataSet.Fields[I]);
  DetailFields.Clear;
  for I := 0 to Fields.Count-1 do
    DetailFields.Add(Fields[I]);
end;


function TCtxDataSet.LocateObject(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): TCtxDataRow;
var
  Opt: TCtxCompareOptions;
begin
  CheckActive;
  Opt := [];
  if loCaseInsensitive in Options then
    Include(Opt, coCaseInsensitive);
  if loPartialKey in Options then
    Include(Opt, coPartialKey);
  Result := FCursor.Locate(KeyFields, KeyValues, Opt);
end;

function TCtxDataSet.FindRecord(Restart, GoForward: Boolean): Boolean;
var
  Obj: TCtxDataRow;
  I: integer;
begin
  Result := True;
  if not Assigned(OnFilterRecord) then
    Exit;
  CheckActive;
  I := PRecInfo(ActiveBuffer)^.Idx;
  Obj := FCursor.FindNext(Restart, GoForward, I);
  Result := Obj <> nil;
  if Result then
  begin
    I := FCursor.IndexOfRow(Obj);
    SetRecNo(I+1);
  end;
end;

function TCtxDataSet.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  Obj: TCtxDataRow;
  I: integer;
begin
  CheckBrowseMode; // 2009-10-29 MB: Must check borowse mode before doing locate
                   // otherwise edit and then locate sequence will result in not being able to post record
  Obj := LocateObject(KeyFields, KeyValues, Options);
  Result := Obj <> nil;
  if Result then
  begin
    I := FCursor.IndexOfRow(Obj);
    SetRecNo(I+1);
  end;
end;

function TCtxDataSet.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
var
  Obj: TCtxDataRow;
  B: TRecordBuffer;
  SaveState: TDataSetState;
  SaveModified: Boolean;
begin
  Result := null;

  if IgnoreFilteringForLookup then
    Obj := FCursor.DataTable.Locate(KeyFields, KeyValues, [])
  else Obj := LocateObject(KeyFields, KeyValues, []);

  if Obj <> nil then
  begin
    B := TempBuffer;
    // if not CheckRowBuffer(Obj, B) then exit;

    if Obj.Deleted then exit;
    _FreeRecordPointers(B);
    PRecInfo(B).Obj := Obj;
    PRecInfo(B).Idx := FCursor.IndexOfRow(Obj);

    SaveModified := Modified;
    SaveState := SetTempState(dsCalcFields);
    try
      CalculateFields(B);
      Result := FieldValues[ResultFields];
    finally
      RestoreState(SaveState); // dsBrowse);
      SetModified(SaveModified);
    end;
  end;
end;

function TCtxDataSet.GetDataTable: TCtxDataTable;
begin
//  if not Active then
//    raise Exception.Create(SDataSetClosed);

  Result := FDataTable;
end;

procedure TCtxDataSet.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FDataContainer) then
    DataContainer := nil;
  inherited;
end;

function TCtxDataSet.GetDataContainer: TCtxDataContainer;
begin
  Result := FDataContainer;
end;

procedure TCtxDataSet.SetDataContainer(const Value: TCtxDataContainer);
begin
  if FDataContainer <> Value then
  begin
    Active := False;
    if FDataContainer <> nil then
      FDataContainer.RemoveFreeNotification(Self);
    FDataContainer := Value;
    if FDataContainer <> nil then
      FDataContainer.FreeNotification(Self);
  end;
end;

procedure TCtxDataSet.SetDataTableName(const Value: String);
begin
  if FDataTableName <> Value then
  begin
    Active := False;
    FDataTableName := Value;
  end;
end;

procedure TCtxDataSet.CreateInternalDataColumns;
var
  I: Integer;
  Column: TCtxDataColumn;
begin
  if FieldCount > 0 then
  begin
    FDataTable.Columns.Clear;
    for I := 0 to Fields.Count - 1 do
    if Fields[I].FieldKind = fkData then
    begin
      Column := FDataTable.Columns.Add;
      Column.Name := Fields[I].FieldName;
      Column.DataType := GetCtxDataType(Fields[I].DataType);
      Column.DataLength := Fields[I].DataSize;
    end;
  end else
  begin
    for I := 0 to FieldDefs.Count - 1 do
    begin
      Column := FDataTable.Columns.Add;
      Column.Name := FieldDefs[I].Name;
      Column.DataType := GetCtxDataType(FieldDefs[I].DataType);
      Column.DataLength := FieldDefs[I].Size;
    end;
  end;
end;

procedure TCtxDataSet.DoOnDataEvent(Sender, Context: TObject;
  DataEvent: TCtxDataEventType);
begin
  if (Sender = nil) or (FDataContainer = nil) then exit;
end;

function TCtxDataSet.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
begin
  Result := GetFieldData(Field, Buffer);
end;

procedure TCtxDataSet.SetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean);
begin
  SetFieldData(Field, Buffer);
end;

procedure TCtxDataSet.UpdateDisplayLabels;
var
  I: Integer;
  C: TCtxDataColumn;
begin
  for I := 0 to FieldCount -1 do
  begin
    C := FDataTable.Columns.Find(Fields[I].FieldName);
    if (C <> nil) and (C.DisplayLabel <> '') and (Fields[I].DisplayLabel = Fields[I].FieldName) then
      Fields[I].DisplayLabel := C.DisplayLabel;
  end;
end;

function TCtxDataSet.GetReferencedRow(Column: TCtxDataColumn): TCtxDataRow;
var
  B: TRecordBuffer;
begin
  if GetActiveRecBuf(B) then
    Result := PRecInfo(B)^.Obj.ReferencedRow[Column]
  else Result := DataTable.NullRow.ReferencedRow[Column];
end;

procedure TCtxDataSet.LoadFromFile(const FileName: String);
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

procedure TCtxDataSet.SaveToFile(const FileName: String);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCtxDataSet.LoadFromStream(Stream: TStream);
var
  Reader: TReader;
begin
  Reader := TReader.Create(Stream, 4096);
  try
    Active := True;
    DataTable.BeginUpdate;
    try
      DataTable.Clear;
      DataTable.ReadData(Reader);
    finally
      DataTable.EndUpdate;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TCtxDataSet.SaveToStream(Stream: TStream);
var
  Writer: TWriter;
begin
  Writer := TWriter.Create(Stream, 4096);
  try
    Active := True;
    DataTable.WriteData(Writer);
  finally
    Writer.Free;
  end;
end;

procedure TCtxDataSet.InternalUpdateFilter;
begin
  FreeAndNil(FFilterEvaluator);
{$IFnDEF _NODATASETFILTER}
  if not Assigned(OnFilterRecord) and (Filter <> '') then
    FFilterEvaluator := TEvaluator.Create(Filter, Self, TDataSetContext);
{$ENDIF}
end;

function TCtxDataSet.GetDataRow: TCtxDataRow;
var
  B: TRecordBuffer;
begin
  if GetActiveRecBuf(B) then
    Result := PRecInfo(B).Obj
  else Result := nil;
end;

function TCtxDataSet.IsDataModified: Boolean;
var
  I: Integer;
begin
  Result := False;
  if not Active then exit;

  Result := (DataTable.PhysicalRowCount > DataTable.RowCount) or IsRecordModified;

  if not Result then
  for I := 0 to DataTable.RowCount - 1 do
    with DataTable.Rows[I] do
    if Inserted or Updated then
    begin
      Result := True;
      exit;
    end;
end;

function TCtxDataSet.IsRecordModified: Boolean;
var
  I: Integer;
begin
  Result := True;
  if Active and not EOF and not BOF then
  begin
    for I := 0 to FieldCount - 1 do
      if (Fields[I].FieldKind = fkData) and (Fields[I].OldValue <> Fields[I].Value) then
        exit;
  end;
  Result := False;
end;

{ TCtxBlobStream }

constructor TCtxBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  inherited Create;
  FField := Field;
  FMode := Mode;
  FDataSet := FField.DataSet as TCtxDataSet;
  if Mode = bmWrite then
    FModified := True
  else LoadBlobData;
end;

destructor TCtxBlobStream.Destroy;
begin
  if (FMode in [bmWrite, bmReadWrite]) and FModified then
    SaveBlobData;
  inherited Destroy;
end;

function TCtxBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  if not (FMode in [bmWrite, bmReadWrite]) then
    raise Exception.Create(SUnableToWriteIntoReadOnlyBlobStream);
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

function TCtxBlobStream.Realloc(var NewCapacity: Integer): Pointer;
begin
  Result := inherited Realloc(NewCapacity);
  FModified := True;
end;

procedure TCtxBlobStream.LoadBlobData;
var
  // B: PChar;
  // P: Pointer;
  S: AnsiString;
begin
  SetSize(0);
  FDataset.GetFieldData(FField, @S);
  inherited Write(S[1], Length(S));
  (*
  B := nil;
  FDataset.GetActiveRecBuf(B);
  if (B <> nil) and GetBit(B+FDataSet.FFlagOffset, FField.Index) then
  begin
    P := B + FDataSet.FOffsets[FField.Index];
    S := PString(P)^;
    inherited Write(S[1], Length(S));
  end;
  *)
  Position := 0;
  FModified := False;
end;

procedure TCtxBlobStream.SaveBlobData;
var
//  B: PChar;
//  P: Pointer;
  S: AnsiString;
begin
  SetLength(S, Size);
  Position := 0;
  inherited Read(S[1], Size);
  FDataset.SetFieldData(FField, @S);
  (*
  B := nil;
  FDataset.GetActiveRecBuf(B);
  if B <> nil then
  begin
    SetLength(S, Size);
    Position := 0;
    inherited Read(S[1], Size);
    P := B + FDataSet.FOffsets[FField.Index];
    SetBit(B + FDataSet.FFlagOffset, FField.Index, Size > 0);
    PString(P)^ := S;
  end;
  *)
  FModified := False;
end;

procedure TCtxDataSet.SetOrderBy(const Value: String);
begin
  if FOrderBy <> Value then
  begin
    FOrderBy := Value;
    if Active then
      UpdateOrderBy;
  end;
end;

procedure TCtxDataSet.UpdateOrderBy;
var
  ItemList: TCtxOrderByColumns;
begin
  ItemList := DataTable.Columns.GetOrderByColumns(FOrderBy);
  DisableControls;
  try
    FCursor.SetOrderByColumns(ItemList);
    if Active then
      Resync([]);
  finally
    EnableControls;
  end;
end;

function TCtxDataSet.IsSequenced: boolean;
begin
  Result := True;
end;

procedure TCtxDataSet.LoadFromVariant(Data: Variant; LoadFields: Boolean = True);
var
  FieldCount: Integer;
  FieldName: String;
  FieldType: Integer;
  FieldSize: Integer;
  RecordCount: integer;
  I, J: Integer;
  Field: TField;
  LocFieldDefs: TFieldDefs;
begin
  if LoadFields then
  begin
    Active := False;
    LocFieldDefs := FieldDefs;
    FieldDefs.Clear;
  end else
    LocFieldDefs := TFieldDefs.Create(Self);

  FieldCount := VarArrayHighBound(Data, 1);
  for I := 0 to FieldCount - 1 do
  begin
    FieldName := Data[I, 0];
    FieldType := Data[I, 1];
    FieldSize := Data[I, 2];
    LocFieldDefs.Add(FieldName, TFieldType(FieldType), FieldSize, False);
  end;

  Active := True;

  DataTable.BeginUpdate;
  try
    DataTable.Clear;
    RecordCount := VarArrayHighBound(Data, 2) + 1 - 3;
    if RecordCount > 0 then
    begin
      DisableControls;
      try
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
    DataTable.EndUpdate;
    if Assigned(LocFieldDefs) and (LocFieldDefs <> FieldDefs) then
      LocFieldDefs.Free;
  end;
end;

end.


