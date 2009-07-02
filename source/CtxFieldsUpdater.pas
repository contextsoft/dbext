(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  TCtxFieldsUpdater - a simple design-time component which helps updating
(*              field names (capitalization) and displya labels from schema.
(*
(*  Copyright (c) 2009, Context Software LLC
(*
(******************************************************************************)
unit CtxFieldsUpdater;

interface

uses SysUtils, Classes, DB, dbSchema;

type
  TCtxFieldsUpdater = class (TComponent)
  private
    FSchema: TDatabaseSchema;
    function GetUpdateFields: Boolean;
    procedure SetSchema(const Value: TDatabaseSchema);
    procedure SetUpdateFields(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    destructor Destroy; override;
  published
    property Schema: TDatabaseSchema read FSchema write SetSchema;
    property UpdateFields: Boolean read GetUpdateFields write SetUpdateFields stored False;
  end;

  procedure UpdateFieldNamesFromSchema(AOwner: TComponent; Schema: TDatabaseSchema);

implementation

{ Routines }

procedure UpdateFieldNamesFromSchema(AOwner: TComponent; Schema: TDatabaseSchema);
var
  I, J: Integer;
  FieldNames: TStringList;
begin
  // Load field names and descriptions from schema and then update fields
  // in the tables accordingly
  FieldNames := TStringList.Create;
  try
    with Schema do
      for I := 0 to TableDefs.Count - 1 do
        for J := 0 to TableDefs[I].FieldDefs.Count - 1 do
          FieldNames.Add(TableDefs[I].FieldDefs[J].Name + '=' + TableDefs[I].FieldDefs[J].Description);

    with AOwner do
    for I := 0 to ComponentCount - 1 do
    if Components[I].InheritsFrom(TField) then
    begin
      J := FieldNames.IndexOfName(TField(Components[I]).FieldName);
      if J < 0 then continue; 
      TField(Components[I]).FieldName := FieldNames.Names[J];
      if (TField(Components[I]).DisplayLabel = '') or (TField(Components[I]).DisplayLabel = TField(Components[I]).FieldName) then
        TField(Components[I]).DisplayLabel := FieldNames.ValueFromIndex[J];
    end;
  finally
    FieldNames.Free;
  end;
end;

{ TCtxFieldsUpdater }

destructor TCtxFieldsUpdater.Destroy;
begin
  Schema := nil;
  inherited;
end;

function TCtxFieldsUpdater.GetUpdateFields: Boolean;
begin
  Result := False;
end;

procedure TCtxFieldsUpdater.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FSchema) and (Operation = opRemove) then
    Schema := nil;
  inherited;
end;

procedure TCtxFieldsUpdater.SetSchema(const Value: TDatabaseSchema);
begin
  if FSchema <> Value then
  begin
    if FSchema <> nil then
      FSchema.RemoveFreeNotification(Self);
    FSchema := Value;
    if FSchema <> nil then
      FSchema.FreeNotification(Self);
  end;
end;

procedure TCtxFieldsUpdater.SetUpdateFields(const Value: Boolean);
begin
  if Value and (Schema <> nil) and not (csLoading in ComponentState) then
    UpdateFieldNamesFromSchema(Owner, Schema);
end;

end.
 
