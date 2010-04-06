(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  TCtxDataSetProvider - implementation of data provider based on a standard
(*                        VCL DataSet descendant.
(*
(*  Copyright (c) 2005-2010, Context Software LLC
(*
(******************************************************************************)
unit CtxDataSetProvider;

interface

uses SysUtils, Classes, DB, CtxDataTypes, CtxData, CtxDataSetCommand;

type
  {:$ TCtxDataSetProvider is an implementation of data provider based on a standard }
  {:$ VCL DataSet descendant. This data provider can only select all records of data }
  {:$ from a given data set. }
  TCtxDataSetProvider = class (TComponent, IUnknown, ICtxDataProvider)
  protected
    FDatabaseName: String;
    FDataSet: TDataSet;
    FActive: Boolean;
    FUseBookmarks: Boolean;

    procedure SetActive(const Value: Boolean);
    procedure SetDataSet(const Value: TDataSet);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    {:$ Destroys the instance of TCtxDataSetProvider component. }
    destructor Destroy; override;

    { Property handlers }
    procedure SetConnected(Value: Boolean);
    function GetConnected: Boolean;
    function GetDatabaseName: String;
    procedure SetDatabaseName(const Value: String);
    function GetDriverName: String;
    function GetDatabaseURL: String;
    procedure SetDatabaseURL(const Value: String);

    { Transactions }
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;
    function GetInTransaction: Boolean;

    { Disconnected mode methods }
    {:$ Creates a command of type TCtxDataSetCommand to be used to server data set's }
    {:$ records to data adapters. }
    function CreateCommand: TCtxDataCommand;
  published
    {:$ Specifies the name of this data provider. }
    property DatabaseName: String read GetDatabaseName write SetDatabaseName;
    {:$ Specifies the data set, this data provider will serve to data adapters. }
    property DataSet: TDataSet read FDataSet write SetDataSet;
    {:$ Specifies whether the provider is active, i.e. registered and visible to other components. }
    property Active: Boolean read FActive write SetActive default False;
    property UseBookmarks: Boolean read FUseBookmarks write FUseBookmarks default False;
  end;

implementation

{ TCtxDataSetProvider }

destructor TCtxDataSetProvider.Destroy;
begin
  Active := False;
  DataSet := nil;
  inherited;
end;

function TCtxDataSetProvider.CreateCommand: TCtxDataCommand;
begin
  if FDataSet = nil then
    raise Exception.CreateFmt(SDataSetNotAssigned, [DatabaseName]);
  Result := TCtxDataSetCommand.Create(Self, FDataSet);
  TCtxDataSetCommand(Result).UseBookmarks := UseBookmarks;
end;

function TCtxDataSetProvider.GetConnected: Boolean;
begin
  Result := (FDataSet <> nil) and DataSet.Active;
end;

function TCtxDataSetProvider.GetDatabaseName: String;
begin
  Result := FDatabaseName;
end;

function TCtxDataSetProvider.GetDatabaseURL: String;
begin
  Result := '';
end;

function TCtxDataSetProvider.GetDriverName: String;
begin
  Result := 'DataSet';
end;

function TCtxDataSetProvider.GetInTransaction: Boolean;
begin
  Result := False;
end;

procedure TCtxDataSetProvider.StartTransaction;
begin
  // Nothing here
end;

procedure TCtxDataSetProvider.Commit;
begin
  // Nothing here
end;

procedure TCtxDataSetProvider.Rollback;
begin
  // Nothing here
end;

procedure TCtxDataSetProvider.SetDatabaseURL(const Value: String);
begin
  // Nothing here
end;

procedure TCtxDataSetProvider.SetConnected(Value: Boolean);
begin
  if FDataSet <> nil then
    FDataSet.Active := Value;
end;

procedure TCtxDataSetProvider.SetDatabaseName(const Value: String);
begin
  FDatabaseName := Value;
  if FActive then
    RegisterCtxDataProvider(Self);
end;

procedure TCtxDataSetProvider.SetDataSet(const Value: TDataSet);
begin
  if FDataSet <> Value then
  begin
    if FDataSet <> nil then
      FDataSet.RemoveFreeNotification(Self);
    FDataSet := Value;
    if FDataSet <> nil then
      FDataSet.FreeNotification(Self);
  end;
end;

procedure TCtxDataSetProvider.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FDataSet) then
    DataSet := nil;
  inherited;
end;

procedure TCtxDataSetProvider.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
      RegisterCtxDataProvider(Self)
    else UnRegisterCtxDataProvider(Self);
  end;
end;

end.
 
