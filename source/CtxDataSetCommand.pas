(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  TCtxDataSetCommand - implementation of data command, which executes based
(*                       on VCL.DB DataSet based databases, implementing ICtxDatabase
(*                       interface (defined in CtxDBIntf unit).
(*
(*  Copyright (c) 2005-2007 Michael Baytalsky
(*
(******************************************************************************)
unit CtxDataSetCommand;

interface

uses SysUtils, Classes, CtxDataTypes, {$IFnDEF VER130}Variants,{$ENDIF}
  CtxDBIntf, DB;

type
  {:$ TCtxDataSetCommand - implementation of data command, which executes based }
  {:$ on VCL.DB DataSet based databases, implementing ICtxDatabase }
  {:$ interface (defined in CtxDBIntf unit). }
  TCtxDataSetCommand = class (TCtxDataCommand)
  protected
    FPrepared: Boolean;
    FDataSet: TDataSet;
    FDatabase: ICtxDatabase;
    FTempParams: TParams;
    FEOF: Boolean;
    FOwnDataSet: Boolean;
    FFetchAll: Boolean;
    FUseBookmarks: Boolean;
    FStoredPos: {$IFDEF VER200}TBookmark{$ELSE}String{$ENDIF};


    function GetPrepared: Boolean; override;
    procedure SetPrepared(Value: Boolean); override;
    procedure InternalPrepare; virtual;
    procedure InternalUnPrepare; virtual;
    procedure GetFieldValues;
  public
    {:$ Creates an instance of TCtxDataCommand object. Takes a reference to the object, }
    {:$ implementing data provider interface (ICtxDatabase) }
    constructor Create(ADataProviderObject: TObject); overload;
    {:$ Creates an instance of TCtxDataCommand object. Takes a reference to the object, }
    {:$ implementing data provider interface (ICtxDatabase) or the data set itself. }
    constructor Create(ADataProviderObject: TObject; ADataSet: TDataSet); overload;
    {:$ Destroys the instance of TCtxDataCommand object. }
    destructor Destroy; override;

    procedure BeginIteration; override;
    procedure EndIteration; override;

    { Navigational methods}
    {:$ Executes command. The results are returned in Fields and Params collections. }
    {:$ Use Next and EOF methods for forward-only navigation through the returned record set. }
    {:$ The values for fields are contained in the Fields collection. }
    procedure Execute; override;
    {:$ Moves to the next record in returned record set.  }
    {:$ The values for fields are contained in the Fields collection. }
    procedure Next; override;
    {:$ Returns True if the cursor riches past the end of the recordset. }
    function EOF: Boolean; override;

    property FetchAll: Boolean read FFetchAll;
    property UseBookmarks: Boolean read FUseBookmarks write FUseBookmarks;
  end;


resourcestring
  SDataSetNotAssigned = 'Data Set component is not assigned to provider %s';

implementation

{ TCtxDataSetCommand }

constructor TCtxDataSetCommand.Create(ADataProviderObject: TObject);
begin
  inherited Create(ADataProviderObject);
  if not Supports(ADataProviderObject, ICtxDatabase, FDatabase) then
    raise Exception.Create(SInvalidDataProviderObject);
  FDataSet := nil;
  FTempParams := TParams.Create;
  FOwnDataSet := False;
  FFetchAll := True;
end;

constructor TCtxDataSetCommand.Create(ADataProviderObject: TObject; ADataSet: TDataSet);
begin
  inherited Create(ADataProviderObject);
  FDatabase := nil;
  FDataSet := ADataSet;
  FTempParams := TParams.Create;
  FEOF := True;
  FOwnDataSet := False;
  FFetchAll := True;
end;

destructor TCtxDataSetCommand.Destroy;
begin
  Prepared := False;
  FDatabase := nil;
  FTempParams.Free;
  inherited;
end;

function TCtxDataSetCommand.EOF: Boolean;
begin
  CheckPrepared;
  Result := not FDataSet.Active or FEOF; // FDataSet.EOF;
end;

procedure TCtxDataSetCommand.Execute;
var
  I: Integer;
  Param: TParam;
begin
  CheckProviderConnected;
  Prepared := True;
  FEOF := False;
  FFetchAll := True;
  {$IFDEF VER200}
  FStoredPos := nil;
  {$ELSE}
  FStoredPos := '';
  {$ENDIF}
  if FDatabase <> nil then
  begin
    if CommandType = ctSQLUpdate then
    begin
      FDatabase.ExecSQL(FDataSet);
      FTempParams.Clear;
      FDatabase.GetQueryParams(FDataSet, FTempParams);
      // Assign output parameters from executed query
      for I := 0 to FTempParams.Count - 1 do
      with FTempParams[I] do
        if ParamType in [ptOutput, ptInputOutput, ptResult] then
          Params.AssignByName(Name, Value);
    end else
    begin
      FDataSet.Active := False;
      if CommandType = ctSQLSelect then
      begin
        // Assign parameters to query
        FTempParams.Clear;
        for I := 0 to FParams.Count - 1 do
        begin
          Param := TParam.Create(FTempParams);
          Param.Name := FParams[I].Name;
          Param.ParamType := TParamType(FParams[I].ParamType);
          Param.DataType := VCLFieldTypes[FParams[I].DataType];
          Param.Value := FParams[I].Value;
        end;
        FDatabase.SetQueryParams(FDataSet, FTempParams);
      end;
      FDataSet.Active := True; // Execute select
    end;
  end else
  begin
    if AnsiSameText(Trim(CommandText), 'CURRENT') then
      FFetchAll := False;
    if FDataSet.Active then
    begin
      if FUseBookmarks then
        FStoredPos := FDataSet.Bookmark;
      if FFetchAll then
      begin
        // FDataSet.Refresh;
        FDataSet.First;
      end;
    end else
      FDataSet.Active := True; // Activate data set
  end;
  // Next;
  GetFieldValues; // Fetch first row
end;

procedure TCtxDataSetCommand.Next;
begin
  // Return EOF immediate on Next if not FetchAll
  if not FFetchAll then
  begin
    FEOF := True;
    FFields.ClearValues;
    exit;
  end;
  CheckPrepared;
  FDataSet.Next;
  GetFieldValues;
end;

function TCtxDataSetCommand.GetPrepared: Boolean;
begin
  Result := FPrepared;
end;

procedure TCtxDataSetCommand.SetPrepared(Value: Boolean);
begin
  if Value <> Prepared then
  begin
    if Value then
      InternalPrepare
    else InternalUnPrepare;
  end;
end;

procedure TCtxDataSetCommand.InternalPrepare;
var
  I: Integer;
  Param: TCtxParameter;
  ParamsChanged: Boolean;
begin
  FPrepared := False;
  try
    CheckProviderConnected;
    if FDatabase <> nil then
    begin
      FOwnDataSet := True;
      if CommandType = ctTableDirect then
      begin
        FDataSet := FDatabase.CreateTable(CommandText);
      end else
      begin
        FTempParams.Clear;
        FDataSet := FDatabase.CreateQuery(CommandText);
        FDatabase.GetQueryParams(FDataSet, FTempParams);
        ParamsChanged := False;
        for I := 0 to FTempParams.Count - 1 do
        with FTempParams[I] do
        begin
          Param := Params.Find(Name);
          if Param = nil then
            Params.AddParameter(Name, TCtxParamType(ParamType)).Value := Value
          else begin
            DataType := VCLFieldTypes[Param.DataType];
            ParamType := TParamType(Param.ParamType);
            ParamsChanged := True;
          end;
        end;
        // Update parameter types
        if ParamsChanged then
          FDatabase.SetQueryParams(FDataSet, FTempParams);
      end;
    end else
      FOwnDataSet := False;

    if FDataSet = nil then
      raise Exception.CreateFmt(SDataSetNotAssigned, [DataProvider.DatabaseName]);

    // Initialize fields collection from field defs or fields
    FFields.Clear;
    if FDataSet.FieldCount > 0 then
    begin
      for I := 0 to FDataSet.FieldCount - 1 do
        with FDataSet.Fields[I] do
          FFields.AddParameter(FieldName, cptOutput, GetCtxDataType(DataType), Size);
    end else begin
      FDataSet.FieldDefs.Update;
      // FDataSet.FieldDefs.Updated := True;
      for I := 0 to FDataSet.FieldDefs.Count - 1 do
        with FDataSet.FieldDefs[I] do
          FFields.AddParameter(Name, cptOutput, GetCtxDataType(DataType), Size);
    end;

    FPrepared := True;
  except
    InternalUnPrepare;
    raise;
  end;
end;

procedure TCtxDataSetCommand.InternalUnPrepare;
begin
  if FOwnDataSet then
    FreeAndNil(FDataSet);
  FPrepared := False;
end;

procedure TCtxDataSetCommand.BeginIteration;
begin
  if FDataSet <> nil then
    FDataSet.DisableControls;
end;

procedure TCtxDataSetCommand.EndIteration;
begin
  if FDataSet <> nil then
  begin
    FDataSet.EnableControls;
    {$IFDEF VER200}
    if FStoredPos <> nil then
    {$ELSE}
    if FStoredPos <> '' then
    {$ENDIF}
      FDataSet.Bookmark := FStoredPos

    else FDataSet.First; // Restore position +++
  end;
end;

procedure TCtxDataSetCommand.GetFieldValues;
var
  I: Integer;
begin
  FFields.ClearValues;
  FEOF := FDataSet.EOF;
  if not FEOF then
  begin
    // Assign fields from current record
    for I := 0 to FDataSet.FieldCount - 1 do
      // FFields[I].Value := FDataSet.Fields[I].Value;
      with FDataSet.Fields[I] do
        FFields.AssignByName(FieldName, Value);
  end
end;

end.
