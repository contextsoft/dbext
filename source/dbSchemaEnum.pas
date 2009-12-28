(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Class that implements enumeration connected to the ones
(*  declared in linked TDatabaseSchema component.
(*  Could be used for lookup fields viewing/editing.
(*  Contains:
(*                TDBSchemaEnum = class(TDBCustomEnumeration)
(*
(*  Copyright (c) 2005-2009, Context Software LLC
(*
(*  ------------------------------------------------------------
(*  FILE        : dbSchemaEnum.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.08
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009, 2010
(*
(******************************************************************************)
unit dbSchemaEnum;

interface

uses
  DB, Classes, dbSchema, dbEnum;

type
  {:$ TDBSchemaEnum class exposes an enumeration declared in DatabaseSchema as a TDataSet.}
  TDBSchemaEnum = class(TDBCustomEnumeration)
  private
    FEnumeration: String;
    FSchema: TDatabaseSchema;
    procedure SetEnumeration(const Value: String);
    procedure SetSchema(const Value: TDatabaseSchema);
  protected
    procedure InternalOpen; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    {:$ Creates an instance of TDBSchemaEnum object. }
    constructor Create(AOwner: TComponent); override;
    {:$ Loads items and descriptions from associated enumeration, declared in }
    {:$ database schema (Schema property). }
    procedure UpdateFromSchema;
  published
    {:$ Whether the dataset is active. }
    property Active;
    {:$ Specifies the name of enumeration from the associated DatabaseSchema. }
    property Enumeration: String read FEnumeration write SetEnumeration;
    {:$ DatabaseSchema associated with this component where the enumeration is declared.}
    property Schema: TDatabaseSchema read FSchema write SetSchema;
  end;

implementation

{ TDBSchemaEnum }

constructor TDBSchemaEnum.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnumeration := '';
  FSchema := nil;
end;

procedure TDBSchemaEnum.InternalOpen;
begin
  UpdateFromSchema;
  inherited;
end;

procedure TDBSchemaEnum.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (AComponent = FSchema) and (Operation = opRemove) then
    Schema := nil;
  inherited;
end;

procedure TDBSchemaEnum.SetEnumeration(const Value: String);
begin
  FEnumeration := Value;
  UpdateFromSchema;
  if Active then Resync([]);
end;

procedure TDBSchemaEnum.SetSchema(const Value: TDatabaseSchema);
begin
  FSchema := Value;
  UpdateFromSchema;
  if Active then Resync([]);
end;

procedure TDBSchemaEnum.UpdateFromSchema;
var
  Enum: TEnumeration;
begin
  if Assigned(FSchema) then
    Enum := FSchema.Enumerations.Find(FEnumeration)
  else Enum := nil;

  if Enum = nil then
  begin
    FItems.Clear;
    FDescriptions.Clear;
    FShortDescriptions.Clear;
  end else begin
    FItems.Assign(Enum.Items);
    FDescriptions.Assign(Enum.Descriptions);
    FShortDescriptions.Assign(Enum.ShortDescriptions);
  end;
end;

end.
 
