(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  ICtxDatabase - common interface implemented by all database adapters
(*
(*  Copyright (c) 2005-2007 Michael Baytalsky
(*
(******************************************************************************)
unit CtxDBIntf;

interface

uses SysUtils, Classes, DB, CtxDataTypes;

type
  ICtxDatabase = interface (ICtxDataProvider)
    ['{E6B82E14-AC0F-41DF-B055-24548FE11EBE}']

    { Creates inactive table component. Assigns table name property. }
    function CreateTable(const TableName: AnsiString): TDataSet;
    { Creates inactive table component. Assigns SQL property. }
    function CreateQuery(const Statement: AnsiString): TDataSet;

    { Returns assignable SQL property. }
    function GetQuerySQL(Query: TDataSet): AnsiString;
    procedure SetQuerySQL(Query: TDataSet; const Statement: AnsiString);
    { Returns assignable Params property. }
    procedure GetQueryParams(Query: TDataSet; Params: TParams);
    procedure SetQueryParams(Query: TDataSet; Params: TParams);
    { Executes query that does not return result set. }
    procedure ExecSQL(Query: TDataSet);
    { Executes statement that may return result set. }
    function ExecuteStatement(SQL: AnsiString; ResultSet: Pointer = nil): Integer;

    function FindKey(Table: TDataSet; const KeyValues: array of const): Boolean;

    { Schema information }
    procedure GetTableNames(List: TStrings; SystemTables: Boolean = False);
    function GetIndexDefs(DataSet: TDataSet): TIndexDefs;
  end;

  function FindCtxDatabase(const DatabaseName: AnsiString): TComponent;
  function GetCtxDataType(AType: TFieldType): TCtxDataType;

const
  VCLFieldTypes: array [TCtxDataType] of TFieldType = (
    ftUnknown, ftSmallInt, ftLargeInt, ftBoolean, ftInteger,
    ftFloat, ftDateTime, ftDate, ftTime, ftString, ftWideString, ftGuid,
    ftMemo, ftBlob, ftReference
  );

implementation

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
    ftDateTime:
      Result := cdtDateTime;
    {$IFnDEF VER130}
    ftTimeStamp:
      Result := cdtDateTime;
    {$ENDIF}
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

function FindCtxDatabase(const DatabaseName: AnsiString): TComponent;
var
  I: Integer;
  DB: ICtxDatabase;
begin
  Result := nil;
  for I := 0 to DBDatabases.Count - 1 do
  if TObject(DBDatabases[I]).GetInterface(ICtxDatabase, DB) then
  begin
    if AnsiCompareText(DB.DatabaseName, DatabaseName) = 0
    then begin
      Result := TComponent(DBDatabases[I]);
      exit;
    end;
  end;
end;

end.
