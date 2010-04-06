(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  ICtxDBDesignerAdapter - interface implemented by database designer adapters
(*
(*  Copyright (c) 2005-2010, Context Software LLC
(*
(******************************************************************************)
unit CtxDBDesignerAdapter;

interface

type
  ICtxDBDesignerAdapter = interface
    ['{34350FC2-922B-42AE-BF16-DAD7BD1F1446}']

    function GetDriverName: OleVariant; stdcall;
    function SelectDatabase(var ConnectionStr: OleVariant): Boolean; stdcall;
    function OpenDatabase(const DatabaseName, DriverName, ConnectionStr, Params: OleVariant): Integer; stdcall;
    procedure CloseDatabase(DatabaseID: Integer); stdcall;
    function ExecuteVerb(DatabaseID, Verb: Integer; var Data: OleVariant): Integer; stdcall;
    function GetParamNames(var Params: OleVariant): Integer; stdcall;
  end;

const
  { Database Verbs }
  dvCreateDatabase = 1;
  dvReverseEngineer = 2;
  dvGetVersion = 3;
  dvSetVersion = 4;
  dvExecuteSQL = 5;
  dvGetSystemTableName = 6;
  dvSetSystemTableName = 7;

  
implementation

end.










