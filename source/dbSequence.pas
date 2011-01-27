(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Contains: TDBSequences component implementing generic sequences, that
(*            could be used as unque key generators (similar to Oracle's
(*            sequences). TDBSequence is database independant. 
(*
(*  Copyright (c) 2005-2010, Context Software LLC
(*
(*  ------------------------------------------------------------
(*  FILE        : dbSequence.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.19
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009, 2010
(*
(******************************************************************************)
unit dbSequence;

interface

uses SysUtils, Classes, DB;

type
  TSequence = class;

  {:: TDBSequence component implements generic sequences, that could be }
  {:: used as unque key generators (similar to Oracle's sequences). }
  {:: TDBSequence is database independant. It uses published SequenceTable }
  {:: property of type TDataSet to access a table, where each sequence }
  {:: must have a corresponding field. Sequence table is assumed to always }
  {:: have only one record. If it is empty, a new record (with null values)  }
  {:: will be added when you first try to access any sequence. }
  TDBSequences = class (TComponent)
  protected
    FSequences: TStringList;
    FSequenceTable: TDataSet;
    FBaseValue: Integer;
    procedure SetSequenceTable(Value: TDataSet);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetSequence(const Sequence: String): TSequence;
    function GetAllocBy(const Sequence: String): Integer;
    procedure SetAllocBy(const Sequence: String; const Value: Integer);
    {:: Assigns passed value as the next value to the associated sequence. }
    {:: All allocated values are released. }
    procedure SetCurValue(const Sequence: String; Value: Integer);
    {:: Returns current value for a given sequence. }
    function GetCurValue(const Sequence: String): Integer;
  public
    {:: Creates an instance of TSequences component. }
    constructor Create(AOwner: TComponent); override;
    {:: Destroys the instance of TSequences component. }
    destructor Destroy; override;

    {:: Returns next unique value and allocates more values if AllocBy property }
    {:: of the corresponding sequence is more then 1. See also AllocBy[] array property.}
    function NextValue(const Sequence: String): Integer; overload;
    {:: Assigned next unique value to the IDField and allocates more values if AllocBy property }
    {:: of the corresponding sequence (IDField.FieldName) is more then 1. See also AllocBy[] array property.}
    procedure NextValue(IDField: TField); overload;

    {:: Releases ans stores unused value, so it can be reused later by calling NextValue. }
    procedure Release(const Sequence: String; Value: Integer); overload;
    {:: Releases ans stores unused value, so it can be reused later by calling NextValue. }
    procedure Release(IDField: TField); overload;

    {:: Clears all allocated values for a given sequence. To clear all sequences, use }
    {:: ClearAll method. }
    procedure Clear(const Sequence: String);
    {:: Clears all released values. The counters in the physical table are }
    {:: not reset with this operation, so the allocated values are lost. }
    procedure ClearAll;

    {:: Set AllocBy parameter for the corresponding sequence. AllocBy value determines }
    {:: how the values are allocated. If AllocaBy is less or equal to zero 1 is asigned instead. }
    {:: If AllocBy is more then 1, then when incrementing a counter, the component }
    {:: will allocate AllocBy values and }
    property AllocBy[const Sequence: String]: Integer read GetAllocBy write SetAllocBy;

    {:: Returns/assignes the current value to a given sequence. This method does not }
    {:: increment current value. Assigning current value will clear allocated values for the sequence. }
    property CurValue[const Sequence: String]: Integer read GetCurValue write SetCurValue;
  published
    {:: SequenceTable property points to Sequence table component containing }
    {:: the record with integer fields used as counters for the corresponding Sequences. }
    {:: Field names must be the same as sequences names. }
    {:: The example of table could look like follows:<br> }
    {:: CREATE TABLE Sequences (<br> }
    {::   INT DocumentID,<br> }
    {::   INT RecordID,<br> }
    {::   INT ReceiptID,<br> }
    {::   etc.<br> }
    {:: ) }
    property SequenceTable: TDataSet read FSequenceTable write SetSequenceTable;
    {:: BaseValue (0 by default) is added to each generated number. }
    {:: This is convenient to generate numbers starting from a certain large }
    {:: base. This can be used as a way to generate unqie identifiers for }
    {:: different databases each of which has its own base number. Thus the }
    {:: first database will generate numbers from 10000000 through 19999999 (BaseValue = 10000000), }
    {:: second - from 20000000 through 29999999 (BaseValue = 20000000) and so forth. }
    property BaseValue: Integer read FBaseValue write FBaseValue default 0;
  end;

  TSequence = class
  protected
    FField: TField;
    FValues: TList;
    FAllocBy: Integer;
  public
    constructor Create(AField: TField);
    destructor Destroy; override;

    procedure SetCurValue(Value: Integer);
    function GetCurValue: Integer;
    function NextValue: Integer;
    procedure Release(Value: Integer);
    procedure Clear;

    property Field: TField read FField;
    property Values: TList read FValues;
    property AllocBy: Integer read FAllocBy write FAllocBy;
    property CurValue: Integer read GetCurValue write SetCurValue;
  end;

implementation

resourcestring
  SSequenceTableNotAssigned = 'Sequence table is not assigned';

{ TDBSequences }

constructor TDBSequences.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSequences := TStringList.Create;
  FSequences.Sorted := True;
  FSequences.Duplicates := dupError;
  FSequenceTable := nil;
  FBaseValue := 0;
end;

destructor TDBSequences.Destroy;
begin
  ClearAll;
  inherited Destroy;
end;

procedure TDBSequences.ClearAll;
begin
  with FSequences do
  while Count > 0 do
  begin
    Objects[Count - 1].Free;
    Delete(Count - 1);
  end;
end;

function TDBSequences.NextValue(const Sequence: String): Integer;
begin
  Result := GetSequence(Sequence).NextValue + BaseValue;
end;

procedure TDBSequences.NextValue(IDField: TField);
begin
  IDField.DataSet.Edit;
  IDField.AsInteger := NextValue(IDField.FieldName);
end;

procedure TDBSequences.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FSequenceTable) and (Operation = opRemove) then
    SequenceTable := nil
  else inherited;
end;

procedure TDBSequences.Release(const Sequence: String; Value: Integer);
begin
  if Value > FBaseValue then
    GetSequence(Sequence).Release(Value - FBaseValue)
  else GetSequence(Sequence).Release(Value)
end;

procedure TDBSequences.Release(IDField: TField);
begin
  Release(IDField.FieldName, IDField.AsInteger);
end;

procedure TDBSequences.SetSequenceTable(Value: TDataSet);
begin
  if Value <> FSequenceTable then
  begin
    ClearAll;
    FSequenceTable := Value;
  end;
end;

function TDBSequences.GetSequence(const Sequence: String): TSequence;
var
  Idx: Integer;
begin
  Idx := FSequences.IndexOf(Sequence);
  if Idx < 0 then
  begin
    if not Assigned(FSequenceTable) then
      DatabaseError(SSequenceTableNotAssigned);
    // Make sure sequence table has at least one record
    Result := TSequence.Create(FSequenceTable.FieldByName(Sequence));
    FSequences.AddObject(Sequence, Result);
  end else
    Result := TSequence(FSequences.Objects[Idx]);
end;

function TDBSequences.GetAllocBy(const Sequence: String): Integer;
begin
  Result := GetSequence(Sequence).AllocBy;
end;

procedure TDBSequences.SetAllocBy(const Sequence: String;
  const Value: Integer);
begin
  if Value > 0 then
    GetSequence(Sequence).AllocBy := Value
  else GetSequence(Sequence).AllocBy := 1;
end;

procedure TDBSequences.SetCurValue(const Sequence: String; Value: Integer);
begin
  GetSequence(Sequence).CurValue := Value;
end;

function TDBSequences.GetCurValue(const Sequence: String): Integer;
begin
  Result := GetSequence(Sequence).CurValue;
end;

procedure TDBSequences.Clear(const Sequence: String);
begin
  GetSequence(Sequence).Clear;
end;

{ TSequence }

constructor TSequence.Create(AField: TField);
begin
  inherited Create;
  FField := AField;
  FValues := TList.Create;
  FAllocBy := 1;
end;

destructor TSequence.Destroy;
begin
  FValues.Free;
  inherited;
end;

function TSequence.NextValue: Integer;
var
  I: Integer;
begin
  if FValues.Count > 0 then
  begin
    Result := Integer(FValues[0]);
    FValues.Delete(0);
  end else begin
    with FField.DataSet do
    begin
      if Active then
        Refresh
      else Open;
      Edit;
      try
        Result := FField.AsInteger;
        // allocate more if necessary
        for I := 1 to FAllocBy - 1 do
          Release(Result + I);
        FField.AsInteger := Result + FAllocBy;
        Post;
      except
        Cancel;
        raise;
      end;
    end;
  end;
end;

function TSequence.GetCurValue: Integer;
begin
  if FValues.Count > 0 then
    Result := Integer(FValues[0])
  else
    with FField.DataSet do
    begin
      if Active then
        Refresh
      else Open;
      Result := FField.AsInteger;
    end;
end;

procedure TSequence.SetCurValue(Value: Integer);
begin
  with FField.DataSet do
  begin
    if Active then
      Refresh
    else Open;
    Edit;
    try
      FField.AsInteger := Value;
      FValues.Clear;
      Post;
    except
      Cancel;
      raise;
    end;
  end;
end;

procedure TSequence.Release(Value: Integer);
begin
  FValues.Add(Pointer(Value));
end;

procedure TSequence.Clear;
begin
  FValues.Clear;
end;

end.
