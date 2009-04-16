unit CtxDBDesigner_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 04/29/06 15:28:40 from Type Library described below.

// ************************************************************************  //
// Type Lib: E:\D7\SDK\Context\DBDesign\bin\CtxDBDesigner.exe (1)
// LIBID: {674BDCDC-749D-48F0-98DC-4972E2106F40}
// LCID: 0
// Helpfile: 
// HelpString: CtxDBDesigner Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WRITEABLECONST ON}
{$IFnDEF VER130}
  {$WARN SYMBOL_PLATFORM OFF}
  {$IFnDEF VER140}
    {$VARPROPSETTER ON}
  {$ENDIF}
{$ENDIF}

interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL
  {$IFnDEF VER130}, Variants {$ENDIF};
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  CtxDBDesignerMajorVersion = 1;
  CtxDBDesignerMinorVersion = 0;

  LIBID_CtxDBDesigner: TGUID = '{674BDCDC-749D-48F0-98DC-4972E2106F40}';

  IID_ICtxDiagramEditor: TGUID = '{10748151-F2A0-45C7-B6F7-81B449DCF008}';
  DIID_ICtxDiagramEditorEvents: TGUID = '{83CB95B0-B5BE-41F9-BDA0-1E24C5C195C2}';
  CLASS_CtxDiagramEditor: TGUID = '{EE8E920D-7165-45D8-A769-116E74F5FE47}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ICtxDiagramEditor = interface;
  ICtxDiagramEditorDisp = dispinterface;
  ICtxDiagramEditorEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CtxDiagramEditor = ICtxDiagramEditor;


// *********************************************************************//
// Interface: ICtxDiagramEditor
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {10748151-F2A0-45C7-B6F7-81B449DCF008}
// *********************************************************************//
  ICtxDiagramEditor = interface(IDispatch)
    ['{10748151-F2A0-45C7-B6F7-81B449DCF008}']
    procedure SetSchema(Data: OleVariant; const ModulePath: WideString); safecall;
    function GetSchema: OleVariant; safecall;
    procedure AddDatabase(DatabaseID: Integer; const DatabaseName: WideString; 
                          const DriverName: WideString; const EngineName: WideString; 
                          const ConnectionStr: WideString); safecall;
    procedure ShowDesigner; safecall;
    procedure SetResult(Data: OleVariant; ResCode: Integer); safecall;
    function GetLibVersion: Integer; safecall;
  end;

// *********************************************************************//
// DispIntf:  ICtxDiagramEditorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {10748151-F2A0-45C7-B6F7-81B449DCF008}
// *********************************************************************//
  ICtxDiagramEditorDisp = dispinterface
    ['{10748151-F2A0-45C7-B6F7-81B449DCF008}']
    procedure SetSchema(Data: OleVariant; const ModulePath: WideString); dispid 201;
    function GetSchema: OleVariant; dispid 202;
    procedure AddDatabase(DatabaseID: Integer; const DatabaseName: WideString; 
                          const DriverName: WideString; const EngineName: WideString; 
                          const ConnectionStr: WideString); dispid 203;
    procedure ShowDesigner; dispid 204;
    procedure SetResult(Data: OleVariant; ResCode: Integer); dispid 205;
    function GetLibVersion: Integer; dispid 206;
  end;

// *********************************************************************//
// DispIntf:  ICtxDiagramEditorEvents
// Flags:     (4096) Dispatchable
// GUID:      {83CB95B0-B5BE-41F9-BDA0-1E24C5C195C2}
// *********************************************************************//
  ICtxDiagramEditorEvents = dispinterface
    ['{83CB95B0-B5BE-41F9-BDA0-1E24C5C195C2}']
    procedure Save; dispid 201;
    procedure Close; dispid 202;
    procedure EnumDatabases; dispid 203;
    procedure DatabaseVerb(DatabaseID: Integer; Verb: Integer; Data: OleVariant); dispid 204;
    procedure Reload; dispid 205;
  end;

// *********************************************************************//
// The Class CoCtxDiagramEditor provides a Create and CreateRemote method to          
// create instances of the default interface ICtxDiagramEditor exposed by              
// the CoClass CtxDiagramEditor. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCtxDiagramEditor = class
    class function Create: ICtxDiagramEditor;
    class function CreateRemote(const MachineName: string): ICtxDiagramEditor;
  end;

  TCtxDiagramEditorDatabaseVerb = procedure(ASender: TObject; DatabaseID: Integer; Verb: Integer; 
                                                              Data: OleVariant) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCtxDiagramEditor
// Help String      : CtxDiagramEditor Object
// Default Interface: ICtxDiagramEditor
// Def. Intf. DISP? : No
// Event   Interface: ICtxDiagramEditorEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCtxDiagramEditorProperties= class;
{$ENDIF}
  TCtxDiagramEditor = class(TOleServer)
  private
    FOnSave: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnEnumDatabases: TNotifyEvent;
    FOnDatabaseVerb: TCtxDiagramEditorDatabaseVerb;
    FOnReload: TNotifyEvent;
    FIntf:        ICtxDiagramEditor;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TCtxDiagramEditorProperties;
    function      GetServerProperties: TCtxDiagramEditorProperties;
{$ENDIF}
    function      GetDefaultInterface: ICtxDiagramEditor;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ICtxDiagramEditor);
    procedure Disconnect; override;
    procedure SetSchema(Data: OleVariant; const ModulePath: WideString);
    function GetSchema: OleVariant;
    procedure AddDatabase(DatabaseID: Integer; const DatabaseName: WideString; 
                          const DriverName: WideString; const EngineName: WideString; 
                          const ConnectionStr: WideString);
    procedure ShowDesigner;
    procedure SetResult(Data: OleVariant; ResCode: Integer);
    function GetLibVersion: Integer;
    property DefaultInterface: ICtxDiagramEditor read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCtxDiagramEditorProperties read GetServerProperties;
{$ENDIF}
    property OnSave: TNotifyEvent read FOnSave write FOnSave;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnEnumDatabases: TNotifyEvent read FOnEnumDatabases write FOnEnumDatabases;
    property OnDatabaseVerb: TCtxDiagramEditorDatabaseVerb read FOnDatabaseVerb write FOnDatabaseVerb;
    property OnReload: TNotifyEvent read FOnReload write FOnReload;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCtxDiagramEditor
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCtxDiagramEditorProperties = class(TPersistent)
  private
    FServer:    TCtxDiagramEditor;
    function    GetDefaultInterface: ICtxDiagramEditor;
    constructor Create(AServer: TCtxDiagramEditor);
  protected
  public
    property DefaultInterface: ICtxDiagramEditor read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = 'Database Extensions';

  dtlOcxPage = 'Database Extensions';

implementation

uses ComObj;

class function CoCtxDiagramEditor.Create: ICtxDiagramEditor;
begin
  Result := CreateComObject(CLASS_CtxDiagramEditor) as ICtxDiagramEditor;
end;

class function CoCtxDiagramEditor.CreateRemote(const MachineName: string): ICtxDiagramEditor;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CtxDiagramEditor) as ICtxDiagramEditor;
end;

procedure TCtxDiagramEditor.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{EE8E920D-7165-45D8-A769-116E74F5FE47}';
    IntfIID:   '{10748151-F2A0-45C7-B6F7-81B449DCF008}';
    EventIID:  '{83CB95B0-B5BE-41F9-BDA0-1E24C5C195C2}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCtxDiagramEditor.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as ICtxDiagramEditor;
  end;
end;

procedure TCtxDiagramEditor.ConnectTo(svrIntf: ICtxDiagramEditor);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TCtxDiagramEditor.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TCtxDiagramEditor.GetDefaultInterface: ICtxDiagramEditor;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TCtxDiagramEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCtxDiagramEditorProperties.Create(Self);
{$ENDIF}
end;

destructor TCtxDiagramEditor.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCtxDiagramEditor.GetServerProperties: TCtxDiagramEditorProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TCtxDiagramEditor.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
    201: if Assigned(FOnSave) then
         FOnSave(Self);
    202: if Assigned(FOnClose) then
         FOnClose(Self);
    203: if Assigned(FOnEnumDatabases) then
         FOnEnumDatabases(Self);
    204: if Assigned(FOnDatabaseVerb) then
         FOnDatabaseVerb(Self,
                         Params[0] {Integer},
                         Params[1] {Integer},
                         Params[2] {OleVariant});
    205: if Assigned(FOnReload) then
         FOnReload(Self);
  end; {case DispID}
end;

procedure TCtxDiagramEditor.SetSchema(Data: OleVariant; const ModulePath: WideString);
begin
  DefaultInterface.SetSchema(Data, ModulePath);
end;

function TCtxDiagramEditor.GetSchema: OleVariant;
begin
  Result := DefaultInterface.GetSchema;
end;

procedure TCtxDiagramEditor.AddDatabase(DatabaseID: Integer; const DatabaseName: WideString; 
                                        const DriverName: WideString; const EngineName: WideString; 
                                        const ConnectionStr: WideString);
begin
  DefaultInterface.AddDatabase(DatabaseID, DatabaseName, DriverName, EngineName, ConnectionStr);
end;

procedure TCtxDiagramEditor.ShowDesigner;
begin
  DefaultInterface.ShowDesigner;
end;

procedure TCtxDiagramEditor.SetResult(Data: OleVariant; ResCode: Integer);
begin
  DefaultInterface.SetResult(Data, ResCode);
end;

function TCtxDiagramEditor.GetLibVersion: Integer;
begin
  Result := DefaultInterface.GetLibVersion;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCtxDiagramEditorProperties.Create(AServer: TCtxDiagramEditor);
begin
  inherited Create;
  FServer := AServer;
end;

function TCtxDiagramEditorProperties.GetDefaultInterface: ICtxDiagramEditor;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

procedure Register;
begin
  // RegisterComponents(dtlServerPage, [TCtxDiagramEditor]);
end;

end.
