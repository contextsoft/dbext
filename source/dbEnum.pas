(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Classes that implement generic enumerations derived from.
(*  datasets, that could be used for lookup fields viewing/editing.
(*  Contains:
(*              TDBCustomEnumeration = class(TDataSet)
(*              TDBEnumeration = class(TDBCustomEnumeration)
(*
(*  Copyright (c) 2005-2011, Context Software LLC
(*
(*  ------------------------------------------------------------
(*  FILE        : dbEnum.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.36
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009, 2010, XE
(*
(******************************************************************************)
unit dbEnum;

{$I CtxVer.inc}

interface

uses
{$IFnDEF VER130}
  Variants,
{$ENDIF}
  DB, SysUtils, Classes;

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
  {:$ TDBCustomEnumeration is a TDataSet descendant, representing a simple }
  {:$ dataset with Key, Value & Description fields. }
  {:: This component could be used to provide data for enumeration type lookup fields. }
  {:: Do not use TDBCustomEnumeration directly. Use TDBEnumeration instead. }
  TDBCustomEnumeration = class(TDataSet)
  protected
    FItems: TStrings;
    FDescriptions: TStrings;
    FShortDescriptions: TStrings;
    FRecBufSize: Integer;
    FRecInfoOfs: Integer;
    FCurRec: Integer;
    FLastBookmark: Integer;
    FSaveChanges: Boolean;
    FCursorOpen: Boolean;
    procedure SetItems(const Value: TStrings);
    procedure SetDescriptions(const Value: TStrings);
    procedure SetShortDescriptions(const Value: TStrings);
    function GetValues(const Key: String): String;
  protected
    { Overriden abstract methods (required) }
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
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
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
  protected
    { Additional overrides (optional) }
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetCanModify: Boolean; override;
    procedure CheckActive; override;
  public
    {:$ Creates an instance of TDBCustomEnumeration object. }
    constructor Create(AOwner: TComponent); override;
    {:$ Destroys the instance of TDBCustomEnumeration object. }
    destructor Destroy; override;

    {:$ Retrieves the current value of a field into a buffer. }
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    {:$ Implements a virtual method to retrieve field values from a record that }
    {:$ matches specified search values. }
    {:: Retrieves field values from a record that matches specified search values. }
    {:: Call Lookup to retrieve values for specified fields from a record that matches }
    {:: search criteria. KeyFields is a string containing a semicolon-delimited list }
    {:: of field names on which to search. }
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;

    {:: Searches the dataset for a specified record and makes that record the }
    {:: current record. Call Locate to search a dataset for a specific record }
    {:: and position the cursor on it.}
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;

    {:: Determines whether the data set is active. }
    property Active;
    {:: Contains Key=Value pairs, declaring an enumeration. }
    property Items: TStrings read FItems write SetItems;
    {:: Contains Key=Description pairs, describing enumeration's items. }
    property Descriptions: TStrings read FDescriptions write SetDescriptions;
    {:: Contains Key=ShortDescription pairs, describing enumeration's items. }
    property ShortDescriptions: TStrings read FShortDescriptions write SetShortDescriptions;
    {:: Returns values corresponding to keys in Items string list. }
    {:: Equivalent of Items.Values[Key]. }
    property Values[const Key: String]: String read GetValues; default;
  end;

  {:$ TDBEnumeration is a TDataSet descendant, representing a simple }
  {:$ dataset with Key, Value & Description fields. }
  {:: This component could be used to provide data for enumeration type lookup fields. }
  TDBEnumeration = class(TDBCustomEnumeration)
  published
    {:: Determines whether the data set is active. }
    property Active;
    {:: Contains Key=Value pairs, declaring an enumeration. }
    property Items;
    {:: Contains Key=Description pairs, describing enumeration's items. }
    property Descriptions;
    {:: Contains Key=ShortDescription pairs, describing enumeration's items. }
    property ShortDescriptions;
  end;

  {:: Returns trimmed of spaces left part of string (Str) before the deliter (Delim). }
  {:: Example: LeftPart('Smith, Y, John', ',') = 'Smith' }
  function LeftPart(const Str, Delim: String): String;
  {:: Returns trimmed of spaces right part of string (Str) after the deliter (Delim). }
  {:: Example: RightPart('Smith, Y, John', ',') = 'John' }
  function RightPart(const Str, Delim: String): String;

resourcestring
  SBookmarkNotFound = 'Bookmark not found';
  SUnknown = 'Unknown';
  SInvalidLocateFields = 'Invalid locate fields';
  SInvalidLocateResultFields = 'Invalid locate result fields';

implementation

uses Windows, Forms;

type
  { TRecInfo }
  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    Bookmark: Integer;
    BookmarkFlag: TBookmarkFlag;
  end;

{ TDBEnumeration }
const
  strKey = 'Key';
  strValue = 'Value';
  strDescription = 'Description';
  strShortDescription = 'ShortDescription';
  MaxStrLen = 240;

constructor TDBCustomEnumeration.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TStringList.Create;
  FDescriptions := TStringList.Create;
  FShortDescriptions := TStringList.Create;
  FCursorOpen := False;
end;

destructor TDBCustomEnumeration.Destroy;
begin
  inherited Destroy;
  FItems.Free;
  FDescriptions.Free;
  FShortDescriptions.Free;
end;

procedure TDBCustomEnumeration.InternalOpen;
var
  I: Integer;
begin
  { Fabricate integral bookmark values }
  for I := 1 to FItems.Count do
    FItems.Objects[I-1] := Pointer(I);
  FLastBookmark := FItems.Count;

  { Initialize our internal position.
    We use -1 to indicate the "crack" before the first record. }
  FCurRec := -1;

  { Initialize an offset value to find the TRecInfo in each buffer }
  FRecInfoOfs := 4 * MaxStrLen;

  { Calculate the size of the record buffers.
    Note: This is NOT the same as the RecordSize property which
    only gets the size of the data in the record buffer }
  FRecBufSize := FRecInfoOfs + SizeOf(TRecInfo);

  { Tell TDataSet how big our Bookmarks are (REQUIRED) }
  BookmarkSize := SizeOf(Integer);

  { Initialize the FieldDefs }
  InternalInitFieldDefs;

  { Create TField components when no persistent fields have been created }
  if DefaultFields then CreateFields;

  { Bind the TField components to the physical fields }
  BindFields(True);
  FCursorOpen := True;
end;

procedure TDBCustomEnumeration.InternalClose;
begin
  { Destroy the TField components if no persistent fields }
  if DefaultFields then DestroyFields;

  { Reset these internal flags }
  FLastBookmark := 0;
  FCurRec := -1;
  FCursorOpen := False;
end;

function TDBCustomEnumeration.IsCursorOpen: Boolean;
begin
  Result := FCursorOpen; 
end;

procedure TDBCustomEnumeration.InternalInitFieldDefs;
begin
  FieldDefs.Clear;
  FieldDefs.Add(strKey, ftString, MaxStrLen, False);
  FieldDefs.Add(strValue, ftString, MaxStrLen, False);
  FieldDefs.Add(strDescription, ftString, MaxStrLen, False);
  FieldDefs.Add(strShortDescription, ftString, MaxStrLen, False);
end;

{ This is the exception handler which is called if an exception is raised
  while the component is being stream in or streamed out.  In most cases this
  should be implemented useing the application exception handler as follows. }

procedure TDBCustomEnumeration.InternalHandleException;
begin
  Application.HandleException(Self);
end;

{ Bookmarks }

procedure TDBCustomEnumeration.InternalGotoBookmark(Bookmark: Pointer);
var
  Index: Integer;
begin
  Index := FItems.IndexOfObject(TObject(PInteger(Bookmark)^));
  if Index <> -1 then
    FCurRec := Index
  else DatabaseError(SBookmarkNotFound);
end;

procedure TDBCustomEnumeration.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  InternalGotoBookmark(@PRecInfo(Buffer + FRecInfoOfs).Bookmark);
end;

function TDBCustomEnumeration.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag;
end;

procedure TDBCustomEnumeration.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer + FRecInfoOfs).BookmarkFlag := Value;
end;

procedure TDBCustomEnumeration.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PInteger(Data)^ := PRecInfo(Buffer + FRecInfoOfs).Bookmark;
end;

procedure TDBCustomEnumeration.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PRecInfo(Buffer + FRecInfoOfs).Bookmark := PInteger(Data)^;
end;

{ Record / Field Access }

function TDBCustomEnumeration.GetRecordSize: Word;
begin
  Result := 4 * MaxStrLen;
end;

function TDBCustomEnumeration.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Result, FRecBufSize);
end;

procedure TDBCustomEnumeration.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer, FRecBufSize);
end;

{ This multi-purpose function does 3 jobs.  It retrieves data for either
  the current, the prior, or the next record.  It must return the status
  (TGetResult), and raise an exception if DoCheck is True. }

function LeftPart(const Str, Delim: String): String;
var
  P: Integer;
begin
  P := Pos(Delim, Str);
  if P > 0 then
    Result := Trim(copy(Str, 1, P - 1))
  else Result := Str;
end;

function RightPart(const Str, Delim: String): String;
var
  P: Integer;
begin
  P := Pos(Delim, Str);
  Result := Trim(copy(Str, P + Length(Delim), Length(Str)));
end;

function TDBCustomEnumeration.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  KeyValue: String;
  S: AnsiString;
begin
  if FItems.Count < 1 then
    Result := grEOF else
  begin
    Result := grOK;
    case GetMode of
      gmNext:
        if FCurRec >= RecordCount - 1  then
          Result := grEOF else
          Inc(FCurRec);
      gmPrior:
        if FCurRec <= 0 then
          Result := grBOF else
          Dec(FCurRec);
      gmCurrent:
        if (FCurRec < 0) or (FCurRec >= RecordCount) then
          Result := grError;
    end;
    if Result = grOK then
    begin
      (*
      KeyValue := AnsiString(LeftPart(FItems[FCurRec], '='));
      StrLCopy(PAnsiChar(Buffer), PAnsiChar(KeyValue), MaxStrLen);
      StrLCopy(PAnsiChar(Buffer) + MaxStrLen, PAnsiChar(AnsiString(RightPart(FItems[FCurRec], '=')), MaxStrLen);
      StrLCopy(PAnsiChar(Buffer) + MaxStrLen * 2, PAnsiChar(FDescriptions.Values[KeyValue]), MaxStrLen);
      StrLCopy(PAnsiChar(Buffer) + MaxStrLen * 3, PAnsiChar(FShortDescriptions.Values[KeyValue]), MaxStrLen);
      *)

      KeyValue := LeftPart(FItems[FCurRec], '=');
      S := AnsiString(KeyValue);
      StrLCopy(PAnsiChar(Buffer), PAnsiChar(S), MaxStrLen);
      S := AnsiString(RightPart(FItems[FCurRec], '='));
      StrLCopy(PAnsiChar(Buffer) + MaxStrLen, PAnsiChar(S), MaxStrLen);
      S := PAnsiChar(AnsiString(FDescriptions.Values[KeyValue]));
      StrLCopy(PAnsiChar(Buffer) + MaxStrLen * 2, PAnsiChar(S), MaxStrLen);
      S := PAnsiChar(AnsiString(FShortDescriptions.Values[KeyValue]));
      StrLCopy(PAnsiChar(Buffer) + MaxStrLen * 3, PAnsiChar(S), MaxStrLen);

      with PRecInfo(Buffer + FRecInfoOfs)^ do
      begin
        BookmarkFlag := bfCurrent;
        Bookmark := Integer(FItems.Objects[FCurRec]);
      end;
    end else
      if (Result = grError) and DoCheck then DatabaseError('No Records');
  end;
end;

procedure TDBCustomEnumeration.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillChar(Buffer^, RecordSize, 0);
end;

function TDBCustomEnumeration.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
  if (PByte(ActiveBuffer) = nil) or (Field.FieldNo < 1) or (Field.FieldNo > 4) then
  begin
    Result := False;
  end
  else begin
    if Buffer <> nil then begin
      StrLCopy(PAnsiChar(Buffer), PAnsiChar(ActiveBuffer) + (Field.FieldNo - 1) * MaxStrLen, Field.Size);
      Result := PChar(Buffer)^ <> #0;
    end
    else begin
      Result := True;
    end;
  end;
end;

{ Record Navigation / Editing }

procedure TDBCustomEnumeration.InternalFirst;
begin
  FCurRec := -1;
end;

procedure TDBCustomEnumeration.InternalLast;
begin
  FCurRec := FItems.Count;
end;

function TDBCustomEnumeration.GetRecordCount: Longint;
begin
  Result := FItems.Count;
end;

function TDBCustomEnumeration.GetRecNo: Longint;
begin
  UpdateCursorPos;
  if (FCurRec = -1) and (RecordCount > 0) then
    Result := 1 else
    Result := FCurRec + 1;
end;

procedure TDBCustomEnumeration.SetRecNo(Value: Integer);
begin
  if (Value >= 0) and (Value < FItems.Count) then
  begin
    FCurRec := Value - 1;
    Resync([]);
  end;
end;

function TDBCustomEnumeration.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
var
  KeyValuesStr: String;
  Fields: TList;
  I: Integer;

  function DoLookupFieldValue(const FieldName: String): String;
  begin
    if CompareText(FieldName, strValue) = 0 then
      Result := FItems.Values[KeyValuesStr]
    else if CompareText(FieldName, strDescription) = 0 then
      Result := FDescriptions.Values[KeyValuesStr]
    else if CompareText(FieldName, strShortDescription) = 0 then
      Result := FShortDescriptions.Values[KeyValuesStr]
    else begin
      DatabaseError(SInvalidLocateFields);
      Result := '';
    end;
  end;

begin
  CheckBrowseMode;
  Result := SUnknown;
  if (KeyFields <> strKey) then
    DatabaseError(SInvalidLocateFields);

  Fields := TList.Create;
  try
    GetFieldList(Fields, ResultFields);
    KeyValuesStr := VarToStr(KeyValues);

    if Fields.Count <= 0 then
      DatabaseError(SInvalidLocateResultFields)
    else if Fields.Count = 1 then
      Result := DoLookupFieldValue(ResultFields)
    else begin
      Result := VarArrayCreate([0, Fields.Count - 1], varVariant);
      for I := 0 to Fields.Count - 1 do
        Result[I] := DoLookupFieldValue(TField(Fields[I]).FieldName);
    end;
  finally
    Fields.Free;
  end;
end;

function TDBCustomEnumeration.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  I: Integer;
begin
  Result := False;
  if KeyFields <> strKey then exit;

  for I := 0 to FItems.Count - 1 do
    if AnsiCompareText(LeftPart(FItems[I], '='), KeyValues) = 0 then
    begin
      DoBeforeScroll;
      try
        Result := True;
        FCurRec := I;
        Resync([]);
        break;
      finally
        DoAfterScroll;
      end;
    end;
end;

procedure TDBCustomEnumeration.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
  if Active then
    Resync([]);
end;

procedure TDBCustomEnumeration.SetDescriptions(const Value: TStrings);
begin
  FDescriptions.Assign(Value);
  if Active then
    Resync([]);
end;

procedure TDBCustomEnumeration.SetShortDescriptions(const Value: TStrings);
begin
  FShortDescriptions.Assign(Value);
  if Active then
    Resync([]);
end;

function TDBCustomEnumeration.GetCanModify: Boolean;
begin
  Result := False;
end;

procedure TDBCustomEnumeration.CheckActive;
begin
  Active := True;
  inherited CheckActive;
end;

function TDBCustomEnumeration.GetValues(const Key: String): String;
begin
  CheckActive;
  Result := FItems.Values[Key];
end;

end.
