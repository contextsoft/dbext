(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Registration unit.
(*
(*  Copyright (c) 2005-2017, Context Software LLC
(*
(******************************************************************************)
unit dbExtReg;

interface

procedure Register;

implementation

uses SysUtils, Classes, DB,
{$IFDEF VER130}
DsgnIntf, ExptIntf,
{$ELSE}
Variants, DesignEditors, ToolsAPI, {ExptIntf,} VCLEditors, DesignIntf, StringsEdit,
{$ENDIF}
TypInfo, Controls, Dialogs, Forms, ColnEdit, StrEdit, Contnrs,
  dbSchema, dbEngProfile, dbSequence, dbRecord, dbEnum, dbSchemaEnum,
  CtxDataTypes, CtxDBIntf,
  {$IFnDEF VER130}
  CtxData, CtxDataSet, CtxDBAdapter, CtxDataSetProvider, CtxDataSetCommandBuilder,
  fDataContainerEditor, fDBAdapterEditor, fDataExplorer,
  {$ENDIF}
  CtxDBDesigner_TLB,
  dbDocument, CtxFieldsUpdater,
  fSelFields;

resourcestring
  SVersionMismatch =
    'Context Database Designer has different version then TDatabaseSchema component loaded in the IDE.'#13#10 +
    'Context Database Designer version: %d'#13#10 +
    'Context Database Extensions package version: %d'#13#10 +
    'Please, make sure that version of Database Designer matches the version of Database Extensions package!';

  SConfirmSchemaReload = 'Would you like to reload database schema from the selected DSD file?';

type
  TSchemaTableNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TTableDefTableNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TEnumerationProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDatabaseSchemaEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TDBEngineProfileEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TDocumentTypeEditor = class(TComponentEditor)
  protected
    FFileName: String;
    FFormName: String;
  public
    procedure GetFormFile(const FileName, UnitName,
      FormName, DesignClass: string; CoClasses: TStrings);
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TSQLScriptProperty = class(TCaptionProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TObjectTypeProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDocumentTypeProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TFormTypeProperty = class(TStringProperty)
  protected
    FProc: TGetStrProc;
    procedure GetModuleProc(const FileName, UnitName, FormName, DesignClass: string; CoClasses: TStrings);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TDatabaseNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TFieldNamesProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TTableCategoryProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TForeignKeyFieldsProperty = class(TFieldNamesProperty)
  public
    procedure Edit; override;
  end;

  TSchemaFileNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TSchemaItemsProperty = class (TCollectionProperty)
  public
    function GetColOptions: TColOptions; override;
  end;

{$IFnDEF VER130}
  TCtxDataTableNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TCtxDatabaseNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TCtxDataContainerEditor = class (TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TCtxDBAdapterEditor = class (TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TCtxIDEDataContainerDesigner = class (TCtxCustomDataContainerDesigner)
  private
    FIDEDesigner: IDesigner;
    FList: TStringList;
    procedure AddComponentName(const S: string);
  public
    procedure GetDataObjects(List: TStringList); override;
    // function GetComponent(const AName: String): TComponent; override;
    procedure NotifyEvent(Context: TObject; DataEvent: TCtxDataEventType); override;
  end;
{$ENDIF}

procedure Register;
begin
{$IFnDEF VER130}
  // Class aliases
  RegisterClassAlias(TCtxDataSet, 'TDbMemDataset');
{$ENDIF}

  // Components
  RegisterComponents('Database Extensions', [TDatabaseSchema,
    TDBSequences, TDBRecord, TDBEnumeration, TDBSchemaEnum, TDBEngineProfile]);

{$IFnDEF VER130}
  RegisterComponents('Database Extensions', [TCtxDataContainer, TCtxDataSet]);
  RegisterComponents('Database Extensions', [TCtxDBAdapter, TCtxDataSetProvider]);
  RegisterComponents('Database Extensions', [TCtxFieldsUpdater, TCtxDataSetCommandBuilder]);

  TCtxDataSetCommandBuilder.Create(nil);
{$ENDIF}

  // Deprecated components

  RegisterComponents('Database Ext. (Deprecated)', [TDBDocument,
    TDBDocumentType, TDBJournal]);

  // Property editors

{$IFnDEF VER130}
  RegisterPropertyEditor(TypeInfo(string), TCtxDataSet, 'DataTableName', TCtxDataTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TCtxDBAdapter, 'DataProviderName', TCtxDatabaseNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TCtxDBCommandItem, 'DataProviderName', TCtxDatabaseNameProperty);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(string), TTableDefinition, 'ObjectType', TObjectTypeProperty);
  RegisterPropertyEditor(TypeInfo(string), TTableDefinition, 'Category', TTableCategoryProperty);

  RegisterPropertyEditor(TypeInfo(string), TFieldDefinition, 'Enumeration', TEnumerationProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBSchemaEnum, 'Enumeration', TEnumerationProperty);

  RegisterPropertyEditor(TypeInfo(string), TDatabaseUpdate, 'TableName', TSchemaTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDatabaseUpdate, 'SQLScript', TSQLScriptProperty);

  RegisterPropertyEditor(TypeInfo(string), TRelation, 'DeleteErrorMessage', TSQLScriptProperty);
  RegisterPropertyEditor(TypeInfo(string), TRelation, 'UpdateErrorMessage', TSQLScriptProperty);
  RegisterPropertyEditor(TypeInfo(string), TRelation, 'RequireRecordErrorMessage', TSQLScriptProperty);

  RegisterPropertyEditor(TypeInfo(string), TTriggerDefinition, 'SQLScript', TSQLScriptProperty);
  RegisterPropertyEditor(TypeInfo(string), TTriggerDefinition, 'TableName', TSchemaTableNameProperty);

  RegisterPropertyEditor(TypeInfo(string), TIndexDefinition, 'DescFields', TFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(string), TIndexDefinition, 'Fields', TFieldNamesProperty);

  RegisterPropertyEditor(TypeInfo(string), TRelation, 'KeyFields', TFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(string), TRelation, 'ForeignTable', TTableDefTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TRelation, 'ForeignKeyFields', TForeignKeyFieldsProperty);
  RegisterPropertyEditor(TypeInfo(string), TRelation, 'SumField', TFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(string), TRelation, 'ForeignSumField', TForeignKeyFieldsProperty);
  RegisterPropertyEditor(TypeInfo(string), TRelation, 'ConditionField', TFieldNamesProperty);

  RegisterPropertyEditor(TypeInfo(string), TDatabaseSchema, 'DSDFileName', TSchemaFileNameProperty);
  RegisterPropertyEditor(TypeInfo(TSchemaItemsCollection), TDatabaseSchema, '', TSchemaItemsProperty);

  // Deprecated property editors

  RegisterPropertyEditor(TypeInfo(string), TDBDocumentType, 'MasterTable', TSchemaTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBJournal, 'JournalTable', TSchemaTableNameProperty);

  RegisterPropertyEditor(TypeInfo(string), TDataSetItem, 'TableName', TSchemaTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDataSetItem, 'DocumentIDField', TFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(string), TDataSetItem, 'ItemIDField', TFieldNamesProperty);

  RegisterPropertyEditor(TypeInfo(string), TDBDocumentType, 'DocumentIDField', TFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBDocumentType, 'EditFormName', TFormTypeProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBDocumentType, 'LookupFormName', TFormTypeProperty);

  RegisterPropertyEditor(TypeInfo(string), TDBJournalRule, 'DocumentType', TDocumentTypeProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBDocument, 'DocumentType', TDocumentTypeProperty);

  RegisterPropertyEditor(TypeInfo(string), TDBDocument, 'DatabaseName', TDatabaseNameProperty);

  // Component editors

  RegisterComponentEditor(TDatabaseSchema, TDatabaseSchemaEditor);
  RegisterComponentEditor(TDBDocumentType, TDocumentTypeEditor);
  RegisterComponentEditor(TDBEngineProfile, TDBEngineProfileEditor);

{$IFnDEF VER130}
  RegisterComponentEditor(TCtxDataContainer, TCtxDataContainerEditor);
  RegisterComponentEditor(TCtxDBAdapter, TCtxDBAdapterEditor);
{$ENDIF}
end;

{ TSchemaTableNameProperty }

function TSchemaTableNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TSchemaTableNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Schema: TDatabaseSchema;
begin
  Schema := nil;
  if GetComponent(0) is TDataSetItem then
    Schema := TDataSetItem(GetComponent(0)).GetSchema
  else if GetComponent(0) is TDBDocumentType then
    Schema := TDBDocumentType(GetComponent(0)).Schema
  else if GetComponent(0) is TDBJournal then
    Schema := TDBJournal(GetComponent(0)).Schema
  else if GetComponent(0) is TCollectionItem then
    Schema := TSchemaItemsCollection((GetComponent(0) as TCollectionItem).Collection).GetSchema;

  if Schema <> nil then
  with Schema do
    for I := 0 to TableDefs.Count - 1 do
      Proc(TableDefs[I].TableName);
end;

{ TTableDefTableNameProperty }

function TTableDefTableNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TTableDefTableNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  with TTableDefItemsCollection((GetComponent(0) as TCollectionItem).Collection).GetSchema do
    for I := 0 to TableDefs.Count - 1 do
      Proc(TableDefs[I].TableName);
end;

{ TSchemaEnumerationProperty }

function TEnumerationProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TEnumerationProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Schema: TDatabaseSchema;
begin
  Schema := nil;
  if GetComponent(0) is TCollectionItem then
    Schema := TTableDefItemsCollection((GetComponent(0) as TCollectionItem).Collection).GetSchema
  else if GetComponent(0) is TDBSchemaEnum then
    Schema := TDBSchemaEnum(GetComponent(0)).Schema;

  if Schema <> nil then
  with Schema do
    for I := 0 to Enumerations.Count - 1 do
      Proc(Enumerations[I].Name);
end;

type
  TDiagram = class (TComponent)
  public
    procedure DoOnReadError(Reader: TReader; const Message: String; var Handled: Boolean);
    procedure ReadFromResFile(const FileName: String);
  end;

procedure TDiagram.DoOnReadError(Reader: TReader; const Message: String;
  var Handled: Boolean);
begin
  Handled := True;
end;

procedure TDiagram.ReadFromResFile(const FileName: String);
var
  Stream: TFileStream;
  Reader: TReader;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Stream.ReadResHeader;
    Reader := TReader.Create(Stream, 4096);
    try
      Reader.OnError := DoOnReadError;
      Reader.ReadRootComponent(Self);
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure LoadFromDSDFile(Schema: TDatabaseSchema; const FileName: String);
const
  STORED_DOCUMENT_NAME = 'StoredDocument';
var
  Diagram: TDiagram;
  StoredSchema: TComponent;
begin
  Diagram := TDiagram.Create(nil);
  try
    Diagram.ReadFromResFile(FileName);
    StoredSchema := Diagram.FindComponent(STORED_DOCUMENT_NAME);
    if (StoredSchema = nil) or not StoredSchema.InheritsFrom(TDatabaseSchema) then
      raise Exception.Create(FileName + ' does contain schema information.');
    Schema.Assign(StoredSchema);
  finally
    Diagram.Free;
  end;
end;

type
  TCtxDiagramEditor = TCtxDiagramEditor3;

  TDBSchemaDesigner = class (TDatabaseSchemaDesigner)
  private
{$IFDEF VER130}
    FFormDesigner: IFormDesigner;
{$ELSE}
    FFormDesigner: IDesigner;
{$ENDIF}
    FDiagramEditor: TCtxDiagramEditor;
  public
    constructor Create(Owner: TDatabaseSchema);
    destructor Destroy; override;
    procedure Modified; override;

    procedure ShowDesigner;

    procedure DoOnSave(Sender: TObject);
    procedure DoOnClose(Sender: TObject);
    procedure DoOnEnumDatabases(Sender: TObject);
    procedure DoOnReload(Sender: TObject);
    procedure DoOnDatabaseVerb(Sender: TObject; DatabaseID: Integer;
      Verb: Integer; Data: OleVariant); // const
    property DiagramEditor: TCtxDiagramEditor read FDiagramEditor;
  end;

const
  { Database Verbs }
  dvCreateDatabase = 1;
  dvReverseEngineer = 2;
  dvGetVersion = 3;
  dvSetVersion = 4;
  dvExecuteSQL = 5;

{ TDBSchemaDesigner }

constructor TDBSchemaDesigner.Create(Owner: TDatabaseSchema);
begin
  inherited Create(Owner);
  FDiagramEditor := TCtxDiagramEditor.Create(nil);
  FDiagramEditor.Connect;
  FDiagramEditor.OnSave := DoOnSave;
  FDiagramEditor.OnClose := DoOnClose;
  FDiagramEditor.OnReload := DoOnReload;
  FDiagramEditor.OnEnumDatabases := DoOnEnumDatabases;
  FDiagramEditor.OnDatabaseVerb := DoOnDatabaseVerb;
end;

destructor TDBSchemaDesigner.Destroy;
begin
  inherited Destroy;
  FDiagramEditor.Free;
end;

procedure TDBSchemaDesigner.ShowDesigner;
begin
  FDiagramEditor.ShowDesigner;
end;

procedure TDBSchemaDesigner.DoOnClose(Sender: TObject);
begin
  FDiagramEditor.Disconnect;
  Free;
end;

procedure TDBSchemaDesigner.DoOnDatabaseVerb(Sender: TObject; DatabaseID: Integer;
  Verb: Integer; Data: OleVariant);
var
  DB: ISchemaDatabase; // ISchemaDatabase const
  SaveDatabaseSchema: TDatabaseSchema;
  ResultSet: TDataSet;
  ResData: OleVariant;
  ResCode: Integer;
  Database: TComponent;
begin
  ResData := NULL;
  ResCode := 0;
  try
    Database := TComponent(DatabaseID);
    if (DBDatabases.IndexOf(Database) < 0) or (not Database.GetInterface(ISchemaDatabase, DB)) then
      raise Exception.Create('Invalid database');
    SaveDatabaseSchema := DB.Schema;
    try
      DB.Schema := FSchema;
      DB.Connected := True;
      case Verb of
        dvCreateDatabase:
          ; // ++DB.CreateNewDatabase(False);
        dvReverseEngineer: begin
          DB.Schema := TDatabaseSchema.Create(nil);
          try
            DB.ReverseEngineer;
            ResData := StrToVarArray(DB.Schema.SaveToStr);
          finally
            DB.Schema.Free;
            DB.Schema := nil;
          end;
          // DoOnReload(nil);
        end;
        dvGetVersion: begin
          ResData := WideString(VersionToStr(DB.GetVersion));
        end;
        dvSetVersion: DB.SetVersion(StrToVersion(Data));
        dvExecuteSQL: begin
          ResultSet := nil;
          try
            DB.ExecuteStatement(VarToStr(Data), @ResultSet);
            // IProviderSupport(Query).PSExecuteStatement(VarToStr(Data), AParams, @ResultSet);
            if (ResultSet <> nil) and (ResultSet.FieldCount > 0) then
              ResData := DataSetToVariant(ResultSet);
          finally
            FreeAndNil(ResultSet);
          end;
        end
        else
          raise Exception.Create('Invalid command');
      end;
      //ShowMessage('Output: ' + Data);
    finally
      DB.Schema := SaveDatabaseSchema;
    end;
  except
    on E: Exception do
    begin
      ResData := E.Message;
      ResCode := -1;
    end;
  end;
  FDiagramEditor.SetResult(ResData, ResCode);
end;

procedure TDBSchemaDesigner.DoOnEnumDatabases(Sender: TObject);
var
  I: Integer;
  DB: ISchemaDatabase;
  TargetDB: String;
begin
  for I := 0 to DBDatabases.Count - 1 do
    if TObject(DBDatabases[I]).GetInterface(ISchemaDatabase, DB) then
    begin
      TargetDB := '';
      if DB.Schema <> nil then
        TargetDB := DB.Schema.TargetDB;
      FDiagramEditor.AddDatabase(Integer(DBDatabases[I]), DB.DatabaseName, 'IDE\'+DB.DriverName, TargetDB, DB.DatabaseURL);
    end;
end;

procedure TDBSchemaDesigner.DoOnReload(Sender: TObject);
begin
  FDiagramEditor.SetSchema(StrToVarArray(FSchema.SaveToStr), ModulePath);
end;

procedure TDBSchemaDesigner.DoOnSave(Sender: TObject);
begin
  FSchema.LoadFromStr(VarArrayToStr(FDiagramEditor.GetSchema));
  Modified;
end;

procedure TDBSchemaDesigner.Modified;
begin
  FFormDesigner.Modified;
end;


{ TDatabaseSchemaEditor }

function TDatabaseSchemaEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit...';
    1: Result := 'Reload From DSD File...';
    2: Result := 'Export To DBS File...';
    3: Result := 'Import From DBS File...';
  end;
end;

function TDatabaseSchemaEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

procedure TDatabaseSchemaEditor.ExecuteVerb(Index: Integer);
var
  TempPath, ImplFileName, IntfFileName, FormFileName: String;
  SchemaDesigner: TDBSchemaDesigner;
  Schema: TDatabaseSchema;
begin
  Designer.ModuleFileNames(ImplFileName, IntfFileName, FormFileName);
  Schema := Component as TDatabaseSchema;
  if Schema.DSDFileName = '' then
  begin
    Schema.DSDFileName := Schema.SchemaName + '.dsd';
    Designer.Modified;
  end;
  TempPath := ExtractFilePath(Schema.DSDFileName);
  if TempPath = '' then
    TempPath := IncludeTrailingPathDelimiter(ExtractFilePath(FormFileName)) + Schema.DSDFileName
  else TempPath := Schema.DSDFileName;

  case Index of
    0: begin // Result := 'Open...'; dbSchema
      SchemaDesigner := Schema.Designer as TDBSchemaDesigner;
      if SchemaDesigner = nil then
      begin
        SchemaDesigner := TDBSchemaDesigner.Create(Schema);
        try
          if SchemaDesigner.DiagramEditor.GetLibVersion <> Schema.LibVersion then
            raise Exception.CreateFmt(SVersionMismatch, [SchemaDesigner.DiagramEditor.GetLibVersion, Schema.LibVersion]);

          SchemaDesigner.FFormDesigner := Designer;
          SchemaDesigner.ModulePath := TempPath;
          SchemaDesigner.DoOnReload(nil);
        except
          FreeAndNil(SchemaDesigner);
          raise;
        end;
      end else
      begin
        SchemaDesigner.FFormDesigner := Designer;
        SchemaDesigner.ShowDesigner;
      end;
    end;
    1: begin
      // reload form DSD file
      LoadFromDSDFile(Schema, TempPath);
      MessageDlg('Schema has been reloaded successfully.', mtInformation, [mbOk], 0);
    end;
    2: begin // Result := 'Save To File...';
      with TSaveDialog.Create(nil) do
      try
        DefaultExt := '.dbs';
        Filter := 'Schema Files (*.dbs)|*.dbs|All Files (*.*)|*.*';
        if Execute then
          Schema.SaveToFile(FileName);
      finally
        Free;
      end;
    end;
    3: begin // Result := 'Load From File...';
      with TOpenDialog.Create(nil) do
      try
        DefaultExt := '.dbs';
        Filter := 'Schema Files (*.dbs)|*.dbs|All Files (*.*)|*.*';
        if Execute then
          Schema.LoadFromFile(FileName);
      finally
        Free;
      end;
    end;
  end;
end;

{ TSQLScriptProperty }

procedure TSQLScriptProperty.Edit;
var
  Temp: String;
  Comp: TPersistent;
begin
{$IFDEF VER130}
  with TStrEditDlg.Create(Application) do
{$ELSE}
  with TStringsEditDlg.Create(Application) do
{$ENDIF}
  try
    Comp := GetComponent(0);
    if Comp is TComponent then
      Caption := TComponent(Comp).Name + '.' + GetName
    else Caption := GetName;

    Temp := GetStrValue;

{$IFDEF VER130}
    Memo.Text := Temp;
    UpdateStatus(nil);
{$ELSE}
    Lines.Text := Temp;
{$ENDIF}

    ActiveControl := Memo;
    CodeEditorItem.Enabled := False;
    CodeWndBtn.Enabled := False;
    CodeEditorItem.Visible := False;
    CodeWndBtn.Visible := False;

    if ShowModal = mrOk then
    begin
  {$IFDEF VER130}
      Temp := Memo.Text;
  {$ELSE}
      Temp := Lines.Text;
  {$ENDIF}
      while (Length(Temp) > 0) and (Temp[Length(Temp)] < ' ') do
        System.Delete(Temp, Length(Temp), 1);
      SetStrValue(Temp);
    end;
  finally
    Free;
  end;
end;

function TSQLScriptProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TObjectTypeProperty }

function TObjectTypeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TObjectTypeProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  List: TStringList;
  Schema: TDatabaseSchema;
begin
  List := TStringList.Create;
  List.Duplicates := dupIgnore;
  List.Sorted := True;
  try
    Schema := nil;
    if GetComponent(0) is TTableDefinition then
      List.Add(TTableDefinition(GetComponent(0)).TableName);

    if GetComponent(0) is TCollectionItem then
      Schema := TSchemaItemsCollection((GetComponent(0) as TCollectionItem).Collection).GetSchema;

    if Schema <> nil then
    with Schema do
      for I := 0 to TableDefs.Count - 1 do
        if TableDefs[I].ObjectType <> '' then
          List.Add(TableDefs[I].ObjectType);

    for I := 0 to List.Count - 1 do
      Proc(List[I]);
  finally
    List.Free;
  end;
end;

{ TFieldNamesProperty }

procedure TFieldNamesProperty.Edit;
var
  Schema: TDatabaseSchema;
begin
  with TfrmSelectFields.Create(Application) do
  try
    TableDef := nil;

    if GetComponent(0) is TDBDocumentType then begin
      TDBDocumentType(GetComponent(0)).CheckSchema;
      TableDef := TDBDocumentType(GetComponent(0)).Schema.GetTableDef(nil,
          TDBDocumentType(GetComponent(0)).MasterTable);
    end else if GetComponent(0) is TTableDefinition then begin
      TableDef := TTableDefinition(GetComponent(0))
    end else if GetComponent(0).InheritsFrom(TDataSetItem) then begin
      Schema := TDataSetItem(GetComponent(0)).GetSchema;
      if Schema <> nil then
        TableDef := Schema.GetTableDef(nil, TDataSetItem(GetComponent(0)).TableName);
    end else if GetComponent(0).InheritsFrom(TCollectionItem) then
      TableDef := TTableDefItemsCollection(TCollectionItem(GetComponent(0)).Collection).GetTableDef;

    if TableDef = nil then exit;

    SelectedFields[DstList.Items] := GetStrValue;
    if ShowModal = mrOK then
      SetStrValue(SelectedFields[DstList.Items]);
  finally
    Free;
  end;
end;

function TFieldNamesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

{ TSchemaFileNameProperty }

function TSchemaFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog]; // inherited GetAttributes +
end;

procedure TSchemaFileNameProperty.Edit;
var
  ImplFileName, IntfFileName, FormFileName: String;
  Schema: TDatabaseSchema;
begin
  Schema := GetComponent(0) as TDatabaseSchema;
  if Schema.Designer <> nil then
    raise Exception.Create(SUnableToAssignDSDFileName);

  Designer.ModuleFileNames(ImplFileName, IntfFileName, FormFileName);
  FormFileName := ExtractFilePath(FormFileName);
  with TOpenDialog.Create(nil) do
  try
    DefaultExt := '.dsd';
    Filter := 'Database Structure Design (*.dsd)|*.dsd|All Files (*.*)|*.*';
    if Schema.DSDFileName <> '' then
      FileName := Schema.DSDFileName
    else InitialDir := FormFileName;
    if Execute then
    begin
      if AnsiSameText(ExtractFilePath(FileName), FormFileName) then
        Schema.DSDFileName := ExtractFileName(FileName)
      else Schema.DSDFileName := FileName;
      Designer.Modified;

      if FileExists(FileName) then
        if MessageDlg(SConfirmSchemaReload, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          // reload from dsd file
          LoadFromDSDFile(Schema, FileName);
        end;
    end;
  finally
    Free;
  end;
end;

{ TSchemaItemsProperty }

function TSchemaItemsProperty.GetColOptions: TColOptions;
begin
  Result := [];
end;

{ TForeignKeyFieldsProperty }

procedure TForeignKeyFieldsProperty.Edit;
var
  Relation: TRelation;
begin
  with TfrmSelectFields.Create(Application) do
  try
    Relation := GetComponent(0) as TRelation;
    TableDef := TTableDefItemsCollection(Relation.Collection).GetSchema.GetTableDef(nil, Relation.ForeignTable);
    if TableDef = nil then
      raise Exception.Create('Please select a valid Foreign Table'); 
    SelectedFields[DstList.Items] := GetStrValue;
    if ShowModal = mrOK then
      SetStrValue(SelectedFields[DstList.Items]);
  finally
    Free;
  end;
end;

{ TDocumentTypeProperty }

function TDocumentTypeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDocumentTypeProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  List: TStringList;
begin
  if DBDocumentTypes = nil then exit;
  List := TStringList.Create;
  List.Duplicates := dupIgnore;
  List.Sorted := True;
  try
    for I := 0 to DBDocumentTypes.Count - 1 do
      List.Add(TDBDocumentType(DBDocumentTypes[I]).DocumentType);
    for I := 0 to List.Count - 1 do
      Proc(List[I]);
  finally
    List.Free;
  end;
end;

{ TDocumentStorageProperty }

function TDatabaseNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDatabaseNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  List: TStringList;
  DB: ISchemaDatabase; 
begin
  if DBDatabases = nil then exit;
  List := TStringList.Create;
  List.Duplicates := dupIgnore;
  List.Sorted := True;
  try
    for I := 0 to DBDatabases.Count - 1 do
    if TObject(DBDatabases[I]).GetInterface(ISchemaDatabase, DB) then
      List.Add(DB.DatabaseName);

    for I := 0 to List.Count - 1 do
      Proc(List[I]);
  finally
    List.Free;
  end;
end;


{ TSchemaDataSetEditor }
(*
function TSchemaDataSetEditor.GetDSDesignerClass: TSchemaDSDesignerClass;
begin
  Result := TSchemaDSDesigner;
end;

procedure TSchemaDataSetEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    ShowFieldsEditor(Designer, TDataSet(Component), GetDSDesignerClass);
end;

function TSchemaDataSetEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Schema Dataset Desginer'; // SDatasetDesigner;
end;

function TSchemaDataSetEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;
*)
{ TTableCategoryProperty }

function TTableCategoryProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TTableCategoryProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  List: TStringList;
  Schema: TDatabaseSchema;
begin
  List := TStringList.Create;
  List.Duplicates := dupIgnore;
  List.Sorted := True;
  try
    Schema := nil;

    if GetComponent(0) is TCollectionItem then
      Schema := TSchemaItemsCollection((GetComponent(0) as TCollectionItem).Collection).GetSchema;

    if Schema <> nil then
      Schema.GetTableCategories(List);

    for I := 0 to List.Count - 1 do
      Proc(List[I]);
  finally
    List.Free;
  end;
end;

{ TFormTypeProperty }

function TFormTypeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TFormTypeProperty.GetModuleProc(const FileName, UnitName,
  FormName, DesignClass: string; CoClasses: TStrings);
begin
  if (DesignClass = '') and (FormName <> '') then
    FProc(FormName);
end;

procedure TFormTypeProperty.GetValues(Proc: TGetStrProc);
begin
  FProc := Proc;
  Designer.GetProjectModules(GetModuleProc);
end;

{ TDocumentTypeEditor }

procedure TDocumentTypeEditor.GetFormFile(const FileName, UnitName,
  FormName, DesignClass: string; CoClasses: TStrings);
begin
  if AnsiSameText(FormName, FFormName) then
    FFileName := FileName;
end;

{$IFDEF VER130}
procedure TDocumentTypeEditor.ExecuteVerb(Index: Integer);
begin
  FFormName := (Component as TDBDocumentType).EditFormName;
  if (ToolServices <> nil) and (FFormName <> '') then
  begin
    FFileName := '';
    Designer.GetProjectModules(GetFormFile);
    if FFileName <> '' then
      ToolServices.OpenFile(FFileName)
    else ShowMessageFmt('Form not found: %s', [FFormName]);
  end;
end;
{$ELSE}
procedure TDocumentTypeEditor.ExecuteVerb(Index: Integer);
var
  ToolServices: IOTAActionServices;
begin
  if BorlandIDEServices = nil then exit;
  if not Supports(BorlandIDEServices, IOTAActionServices, ToolServices) then exit;

  FFormName := (Component as TDBDocumentType).EditFormName;
  if (ToolServices <> nil) and (FFormName <> '') then
  begin
    FFileName := '';
    Designer.GetProjectModules(GetFormFile);
    if FFileName <> '' then
      ToolServices.OpenFile(FFileName)
    else ShowMessageFmt('Form not found: %s', [FFormName]);
  end;
end;
{$ENDIF}

function TDocumentTypeEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Open Edit Form';
end;

function TDocumentTypeEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TDBEngineProfileEditor }

procedure TDBEngineProfileEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: begin // load from file
      with TOpenDialog.Create(nil) do
      try
        DefaultExt := '.dbp';
        Filter := 'Database Profiles (*.dbp)|*.dbp|All Files (*.*)|*.*';
        if Execute then
        begin
          TDBEngineProfile(Component).LoadFromFile(FileName);
          Designer.Modified;
        end;
      finally
        Free;
      end;
    end;
    1: begin // save to file
      with TSaveDialog.Create(nil) do
      try
        DefaultExt := '.dbp';
        Filter := 'Database Profiles (*.dbp)|*.dbp|All Files (*.*)|*.*';
        if Execute then
          TDBEngineProfile(Component).SaveToFile(FileName);
      finally
        Free;
      end;
    end;
  end;
end;

function TDBEngineProfileEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then Result := 'Load from file...'
  else Result := 'Save to file...';
end;

function TDBEngineProfileEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{$IFnDEF VER130}

{ TCtxDataTableNameProperty }

function TCtxDataTableNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TCtxDataTableNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  DS: TCtxDataContainer;
begin
  DS := (GetComponent(0) as TCtxDataSet).DataContainer;
  if DS <> nil then
    for I := 0 to DS.Tables.Count - 1 do
      Proc(DS.Tables[I].Name);
end;

{ TCtxDatabaseNameProperty }

function TCtxDatabaseNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TCtxDatabaseNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  DB: ICtxDatabase;
begin
  for I := 0 to DBDatabases.Count - 1 do
    if TObject(DBDatabases[I]).GetInterface(ICtxDataProvider, DB) then
       Proc(DB.DatabaseName);
end;

{ TCtxIDEDataContainerDesigner }

procedure TCtxIDEDataContainerDesigner.AddComponentName(const S: string);
begin
  if (FList <> nil) and (S <> '') then
    FList.AddObject(S, FIDEDesigner.GetComponent(S));
end;

procedure TCtxIDEDataContainerDesigner.GetDataObjects(List: TStringList);
begin
  inherited GetDataObjects(List);
  FList := List;
  if FList <> nil then
  begin
    FIDEDesigner.GetComponentNames(GetTypeData(PTypeInfo(TDatabaseSchema.ClassInfo)), AddComponentName);
    FIDEDesigner.GetComponentNames(GetTypeData(PTypeInfo(TDataSet.ClassInfo)), AddComponentName);
    // FIDEDesigner.GetComponentNames(GetTypeData(PTypeInfo(TCtxDBConnection.ClassInfo)), AddComponentName);
    // FIDEDesigner.GetComponentNames(GetTypeData(PTypeInfo(TCtxDataContainer.ClassInfo)), AddComponentName);
    FIDEDesigner.GetComponentNames(GetTypeData(PTypeInfo(TCtxDataAdapter.ClassInfo)), AddComponentName);
  end;
end;

procedure TCtxIDEDataContainerDesigner.NotifyEvent(Context: TObject;
  DataEvent: TCtxDataEventType);
begin
  inherited;
  if FIDEDesigner <> nil then
    FIDEDesigner.Modified;
end;

{ TCtxDataContainerEditor }

procedure TCtxDataContainerEditor.ExecuteVerb(Index: Integer);
var
  DC: TCtxDataContainer;
  AdapterDesigner: IDesignerNotify;
begin
  DC := Component as TCtxDataContainer;
  case Index of
    0:
      with TCtxIDEDataContainerDesigner.Create(DC) do
      begin
        FIDEDesigner := Designer;
        Show;
      end;
    1:
      begin
        ExploreContainer(DC);
        Designer.Modified;
      end;
    2:
      if (DC.DataAdapter <> nil) and DC.DataAdapter.InheritsFrom(TCtxDBAdapter) then
      begin
        EditDBAdapter(TCtxDBAdapter(DC.DataAdapter));
        AdapterDesigner := FindRootDesigner(DC.DataAdapter);
        if AdapterDesigner <> nil then
          AdapterDesigner.Modified;
      end;
  end;
end;

function TCtxDataContainerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Design Data Container';
    1: Result := 'Explore Data Container';
    2: Result := 'Edit Data Adapter';
  end;
end;

function TCtxDataContainerEditor.GetVerbCount: Integer;
var
  DC: TCtxDataContainer;
begin
  DC := Component as TCtxDataContainer;
  if (DC.DataAdapter <> nil) and DC.DataAdapter.InheritsFrom(TCtxDBAdapter) then
    Result := 3
  else Result := 2;
end;

{ TCtxDBAdapterEditor }

procedure TCtxDBAdapterEditor.ExecuteVerb(Index: Integer);
begin
  EditDBAdapter(Component as TCtxDBAdapter);
  Designer.Modified;
end;

function TCtxDBAdapterEditor.GetVerb(Index: Integer): string;
begin
  Result := 'Edit Database Adapter...';
end;

function TCtxDBAdapterEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{$ENDIF}

end.

