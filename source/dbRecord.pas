(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Class that implements a generic one record dataset that
(*  could be used to store form data.
(*  Contains:
(*              TDBRecord = class(TDataSet)
(*
(*  Copyright (c) 2004-2009 Michael Baytalsky
(*
(*  ------------------------------------------------------------
(*  FILE        : dbRecord.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.10
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009, 2010
(*
(******************************************************************************)
unit dbRecord;

{$I CtxVer.inc}

interface

uses SysUtils, Classes, Forms, DB;

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
  BufArray = array [0..1000] of Byte;
  PBufArray = ^BufArray;

  {:: TDBRecord is a simple TDataSet descendant, that represents }
  {:: a dataset (defined at design-time, by creating persistent fields) }
  {:: containing only one record of data. This component is usefull }
  {:: when implementing forms, using data-aware controls. It may contain }
  {:: additional fields, that are not part of any actual database }
  {:: dataset. TDBRecord may contain any field types except Blobs. }
  {:: This implementation also demonstrates how to implement dataset }
  {:: descendants. }
  TDBRecord = class (TDataSet)
  protected
    FRecordSize: Integer;
    FCursorOpen: Boolean;
    FFieldOffs: TList;
    FRecord: TRecordBuffer;
    FOldRecord: TRecordBuffer;

    { Overriden abstract methods (required) }
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalOpen; override;

    procedure InternalInsert; override;
    procedure InternalEdit; override;
    procedure InternalPost; override;
    function IsCursorOpen: Boolean; override;

    { Bookmarks - not supported }
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
  protected
    { Additional overrides (optional) }
    function GetRecordCount: Integer; override;
    function GetCanModify: Boolean; override;
    procedure CheckActive; override;
    procedure DefChanged(Sender: TObject); override;
    function GetActiveRecBuf(var RecBuf: TRecordBuffer): boolean;
  public
    {:$ Creates an instance of TDBRecord component. }
    constructor Create(AOwner: TComponent); override;
    {:$ Destroys the instance of TDBRecord component. }
    destructor Destroy; override;

    {:$ Retrieves the current value of a field into a buffer. }
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    {:$ Saves current field value into a record buffer. }
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
  published
    {:: Determines whether the record is active. }
    property Active;
    {:: Contains a collection of TFieldDef items. }
    property FieldDefs;
  end;

resourcestring
  SBookmarksNotSupported = 'Bookmarks not supported';
  SCapabilityNotSupported = 'Capability not supported';  

implementation

{ TDBRecord }

constructor TDBRecord.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecordSize := 0;
  FCursorOpen := False;
  FFieldOffs := TList.Create;
  FRecord := nil;
  FOldRecord := nil;
end;

destructor TDBRecord.Destroy;
begin
  FreeAndNil(FFieldOffs);
  inherited Destroy;
end;

procedure TDBRecord.CheckActive;
begin
  Active := True;
  inherited CheckActive;
end;

function TDBRecord.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Result, FRecordSize);
end;

procedure TDBRecord.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer, FRecordSize);
end;

procedure TDBRecord.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillChar(Buffer^, RecordSize, 0);
end;

function TDBRecord.GetCanModify: Boolean;
begin
  Result := True;
end;

function TDBRecord.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  FieldOffset: Integer;
  Ptr: TRecordBuffer;
begin
  Result := False;
  if not IsEmpty and (Field.Index >= 0) and GetActiveRecBuf(Ptr) then
  begin
    Result := PBufArray(Ptr)^[Field.Index] <> 0; // is null
    if Result and Assigned(Buffer) then
    begin
      FieldOffset := Integer(FFieldOffs[Field.Index]);
      Move(PBufArray(Ptr)^[FieldOffset], Buffer^, Field.DataSize);
    end;
  end;
end;

procedure TDBRecord.SetFieldData(Field: TField; Buffer: Pointer);
var
  FieldOffset: Integer;
  Ptr: TRecordBuffer;
begin
  if (Field.Index >= 0) and GetActiveRecBuf(Ptr) then
  begin
    if Assigned (Buffer) then
    begin
      FieldOffset := Integer(FFieldOffs[Field.Index]);
      Move(Buffer^, PBufArray(Ptr)^[FieldOffset], Field.DataSize);
      PBufArray(Ptr)^[Field.Index] := 1; // set to not null
    end else
      PBufArray(Ptr)^[Field.Index] := 0; // set to null
    DataEvent(deFieldChange, Longint(Field));
  end;
end;

function TDBRecord.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  Result := grOK;
  Move(FRecord^, Buffer^, FRecordSize);
  ClearCalcFields(Buffer);
  GetCalcFields(Buffer);

  if GetMode = gmNext then Result := grEOF
  else if GetMode = gmPrior then Result := grBOF
end;

function TDBRecord.GetRecordCount: Integer;
begin
  Result := 1;
end;

function TDBRecord.GetRecordSize: Word;
begin
  Result := FRecordSize; //
end;

procedure TDBRecord.InternalInitFieldDefs;
var
  I: Integer;
begin
  // Create FieldDefs from persistent fields
  for I := 0 to FieldCount - 1 do
  begin
    with Fields[I] do
      if (FieldKind = fkData) and (FieldDefs.IndexOf(FieldName) < 0) then
        FieldDefs.Add(FieldName, DataType, Size, Required);
  end;
end;

procedure TDBRecord.InternalOpen;
var
  I: Integer;
begin
  { Initialize field defs }
  InternalInitFieldDefs;
  { Calculate the size of the record buffers.
    Note: This is NOT the same as the RecordSize property which
    only gets the size of the data in the record buffer }
  FFieldOffs.Clear;
  FRecordSize := 0;
  Inc(FRecordSize, FieldCount); // First goes NULL flags
  for I := 0 to FieldCount - 1 do
  begin
    FFieldOffs.Add(Pointer(FRecordSize));
    Inc(FRecordSize, Fields[I].DataSize);
  end;
  FRecord := AllocRecordBuffer;
  FOldRecord := AllocRecordBuffer;
  InternalInitRecord(FRecord);
  InternalInitRecord(FOldRecord);
  { Bind the TField components to the physical fields }
  BindFields(True);
  FCursorOpen := True;
end;

procedure TDBRecord.InternalClose;
begin
  { Destroy the TField components if no persistent fields }
  if DefaultFields then DestroyFields;
  FFieldOffs.Clear;
  FCursorOpen := False;
  FreeRecordBuffer(FRecord);
  FreeRecordBuffer(FOldRecord);
end;

procedure TDBRecord.InternalHandleException;
begin
  Application.HandleException(Self);
end;

function TDBRecord.IsCursorOpen: Boolean;
begin
  Result := FCursorOpen;
end;

procedure TDBRecord.InternalPost;
begin
  CheckActive;
  if State in dsEditModes then
  begin
    // replace data with new data
    Move(ActiveBuffer^, FRecord^, FRecordSize);
    Move(FRecord^, FOldRecord^, FRecordSize);
  end;
end;

procedure TDBRecord.DefChanged(Sender: TObject);
var
  I: Integer;
  FieldDef: TFieldDef;
begin
  for I := 0 to TFieldDefs(Sender).Count - 1 do
  begin
    FieldDef := TFieldDefs(Sender)[I];
    if (FieldDef.DataType = ftGUID) and (FieldDef.Size = 0) then
      FieldDef.Size := 38;
  end;
  inherited;
end;

procedure TDBRecord.InternalEdit;
begin
  Move(FRecord^, FOldRecord^, FRecordSize);
  inherited;
end;

procedure TDBRecord.InternalInsert;
begin
  InternalInitRecord(FOldRecord);
  inherited;
end;

function TDBRecord.GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean;
begin
  case State of
    dsBlockRead,
    dsBrowse,
    dsEdit,
    dsInsert,
    dsNewValue:
      RecBuf := ActiveBuffer;
    dsCalcFields:
      RecBuf := CalcBuffer;
    dsOldValue:
      RecBuf := FOldRecord;
    else
      RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

procedure TDBRecord.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  DatabaseError(SCapabilityNotSupported, Self);
end;

procedure TDBRecord.InternalDelete;
begin
  DatabaseError(SCapabilityNotSupported, Self);
end;

procedure TDBRecord.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
end;

function TDBRecord.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := bfCurrent;
end;

procedure TDBRecord.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
end;

procedure TDBRecord.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
end;

procedure TDBRecord.InternalGotoBookmark(Bookmark: Pointer);
begin
end;

procedure TDBRecord.InternalFirst;
begin
end;

procedure TDBRecord.InternalLast;
begin
end;

procedure TDBRecord.InternalSetToRecord(Buffer: TRecordBuffer);
begin
end;


end.
