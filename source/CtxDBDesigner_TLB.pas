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

// $Rev: 17252 $
// File generated on 6/20/2009 8:07:48 PM from Type Library described below.

// ************************************************************************  //
// Type Lib: E:\projects\Context\DBDesign\source\CtxDBDesigner.tlb (1)
// LIBID: {C7A076DA-57D3-4341-A850-5A5D23DE1A71}
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
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  CtxDBDesignerMajorVersion = 3;
  CtxDBDesignerMinorVersion = 0;

  LIBID_CtxDBDesigner: TGUID = '{C7A076DA-57D3-4341-A850-5A5D23DE1A71}';

  IID_ICtxDiagramEditor3: TGUID = '{9CE058FF-6A6D-4883-AD8F-89576C04F232}';
  DIID_ICtxDiagramEditorEvents3: TGUID = '{1337D159-EFA3-4164-962F-756EDD488CD3}';
  CLASS_CtxDiagramEditor3: TGUID = '{86F3FBE0-17BC-4FA7-9235-F01972DAB48F}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ICtxDiagramEditor3 = interface;
  ICtxDiagramEditor3Disp = dispinterface;
  ICtxDiagramEditorEvents3 = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  CtxDiagramEditor3 = ICtxDiagramEditor3;


// *********************************************************************//
// Interface: ICtxDiagramEditor3
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9CE058FF-6A6D-4883-AD8F-89576C04F232}
// *********************************************************************//
  ICtxDiagramEditor3 = interface(IDispatch)
    ['{9CE058FF-6A6D-4883-AD8F-89576C04F232}']
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
// DispIntf:  ICtxDiagramEditor3Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9CE058FF-6A6D-4883-AD8F-89576C04F232}
// *********************************************************************//
  ICtxDiagramEditor3Disp = dispinterface
    ['{9CE058FF-6A6D-4883-AD8F-89576C04F232}']
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
// DispIntf:  ICtxDiagramEditorEvents3
// Flags:     (4096) Dispatchable
// GUID:      {1337D159-EFA3-4164-962F-756EDD488CD3}
// *********************************************************************//
  ICtxDiagramEditorEvents3 = dispinterface
    ['{1337D159-EFA3-4164-962F-756EDD488CD3}']
    procedure Save; dispid 201;
    procedure Close; dispid 202;
    procedure EnumDatabases; dispid 203;
    procedure DatabaseVerb(DatabaseID: Integer; Verb: Integer; Data: OleVariant); dispid 204;
    procedure Reload; dispid 205;
  end;

// *********************************************************************//
// The Class CoCtxDiagramEditor3 provides a Create and CreateRemote method to          
// create instances of the default interface ICtxDiagramEditor3 exposed by              
// the CoClass CtxDiagramEditor3. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoCtxDiagramEditor3 = class
    class function Create: ICtxDiagramEditor3;
    class function CreateRemote(const MachineName: string): ICtxDiagramEditor3;
  end;

  TCtxDiagramEditor3DatabaseVerb = procedure(ASender: TObject; DatabaseID: Integer; Verb: Integer; 
                                                               Data: OleVariant) of object;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TCtxDiagramEditor3
// Help String      : CtxDiagramEditor Object
// Default Interface: ICtxDiagramEditor3
// Def. Intf. DISP? : No
// Event   Interface: ICtxDiagramEditorEvents3
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TCtxDiagramEditor3Properties= class;
{$ENDIF}
  TCtxDiagramEditor3 = class(TOleServer)
  private
    FOnSave: TNotifyEvent;
    FOnClose: TNotifyEvent;
    FOnEnumDatabases: TNotifyEvent;
    FOnDatabaseVerb: TCtxDiagramEditor3DatabaseVerb;
    FOnReload: TNotifyEvent;
    FIntf: ICtxDiagramEditor3;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TCtxDiagramEditor3Properties;
    function GetServerProperties: TCtxDiagramEditor3Properties;
{$ENDIF}
    function GetDefaultInterface: ICtxDiagramEditor3;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: ICtxDiagramEditor3);
    procedure Disconnect; override;
    procedure SetSchema(Data: OleVariant; const ModulePath: WideString);
    function GetSchema: OleVariant;
    procedure AddDatabase(DatabaseID: Integer; const DatabaseName: WideString; 
                          const DriverName: WideString; const EngineName: WideString; 
                          const ConnectionStr: WideString);
    procedure ShowDesigner;
    procedure SetResult(Data: OleVariant; ResCode: Integer);
    function GetLibVersion: Integer;
    property DefaultInterface: ICtxDiagramEditor3 read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TCtxDiagramEditor3Properties read GetServerProperties;
{$ENDIF}
    property OnSave: TNotifyEvent read FOnSave write FOnSave;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnEnumDatabases: TNotifyEvent read FOnEnumDatabases write FOnEnumDatabases;
    property OnDatabaseVerb: TCtxDiagramEditor3DatabaseVerb read FOnDatabaseVerb write FOnDatabaseVerb;
    property OnReload: TNotifyEvent read FOnReload write FOnReload;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TCtxDiagramEditor3
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TCtxDiagramEditor3Properties = class(TPersistent)
  private
    FServer:    TCtxDiagramEditor3;
    function    GetDefaultInterface: ICtxDiagramEditor3;
    constructor Create(AServer: TCtxDiagramEditor3);
  protected
  public
    property DefaultInterface: ICtxDiagramEditor3 read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = 'Context Components';

  dtlOcxPage = 'Context Components';

implementation

uses ComObj;

class function CoCtxDiagramEditor3.Create: ICtxDiagramEditor3;
begin
  Result := CreateComObject(CLASS_CtxDiagramEditor3) as ICtxDiagramEditor3;
end;

class function CoCtxDiagramEditor3.CreateRemote(const MachineName: string): ICtxDiagramEditor3;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_CtxDiagramEditor3) as ICtxDiagramEditor3;
end;

procedure TCtxDiagramEditor3.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{86F3FBE0-17BC-4FA7-9235-F01972DAB48F}';
    IntfIID:   '{9CE058FF-6A6D-4883-AD8F-89576C04F232}';
    EventIID:  '{1337D159-EFA3-4164-962F-756EDD488CD3}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TCtxDiagramEditor3.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as ICtxDiagramEditor3;
  end;
end;

procedure TCtxDiagramEditor3.ConnectTo(svrIntf: ICtxDiagramEditor3);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TCtxDiagramEditor3.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TCtxDiagramEditor3.GetDefaultInterface: ICtxDiagramEditor3;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TCtxDiagramEditor3.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TCtxDiagramEditor3Properties.Create(Self);
{$ENDIF}
end;

destructor TCtxDiagramEditor3.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TCtxDiagramEditor3.GetServerProperties: TCtxDiagramEditor3Properties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TCtxDiagramEditor3.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
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

procedure TCtxDiagramEditor3.SetSchema(Data: OleVariant; const ModulePath: WideString);
begin
  DefaultInterface.SetSchema(Data, ModulePath);
end;

function TCtxDiagramEditor3.GetSchema: OleVariant;
begin
  Result := DefaultInterface.GetSchema;
end;

procedure TCtxDiagramEditor3.AddDatabase(DatabaseID: Integer; const DatabaseName: WideString; 
                                         const DriverName: WideString; 
                                         const EngineName: WideString; 
                                         const ConnectionStr: WideString);
begin
  DefaultInterface.AddDatabase(DatabaseID, DatabaseName, DriverName, EngineName, ConnectionStr);
end;

procedure TCtxDiagramEditor3.ShowDesigner;
begin
  DefaultInterface.ShowDesigner;
end;

procedure TCtxDiagramEditor3.SetResult(Data: OleVariant; ResCode: Integer);
begin
  DefaultInterface.SetResult(Data, ResCode);
end;

function TCtxDiagramEditor3.GetLibVersion: Integer;
begin
  Result := DefaultInterface.GetLibVersion;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TCtxDiagramEditor3Properties.Create(AServer: TCtxDiagramEditor3);
begin
  inherited Create;
  FServer := AServer;
end;

function TCtxDiagramEditor3Properties.GetDefaultInterface: ICtxDiagramEditor3;
begin
  Result := FServer.DefaultInterface;
end;

{$ENDIF}

procedure Register;
begin
//  RegisterComponents(dtlServerPage, [TCtxDiagramEditor3]);
end;

end.
