(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Classes that implement generic database engine profile
(*  Contains:
(*                TDBEngineProfile
(*
(*  Copyright (c) 2005-2010, Context Software LLC
(*
(*  ------------------------------------------------------------
(*  FILE        : dbEngProfile.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.14
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009, 2010
(*
(******************************************************************************)
unit dbEngProfile;

{$I CtxVer.inc}

interface

uses Classes, Contnrs, SysUtils, DB, dbSchema;

type
  TFmtExpression = class;

  TGetPropMethod = function (Item: TCompareItem; const PropName: String): String of object;

  TGenerateStatement = (gsCreate, gsAlter, gsDrop, gsAdd, gsNone);

  TExecSQLProc = procedure (const SQL: String; ResultSet: Pointer = nil) of object;
  TOnReverseEvent = procedure (Tag: integer; const LogStr: string = '') of object;

  TDBEngineProfile = class (TComponent)
  protected
    FEngineName: String;
    FDisplayName: String;
    FEngineProps: TStrings;
    FFieldTypeMap: TStrings;
    FParentStatements: TStrings;
    FStatements: TStrings;
    FProperties: TStrings;
    FInfoSchemaSQL: TStrings;
    FInfoSchemaValueMap: TStrings;
    FSynonyms: TStrings;
    FKeywords: TStrings;
    FFileName: String;
    FLastAlterOperations: TList;
    FDropCreateTables: TStringList;
    FExpressions: TObjectList;
    FSupportsDomains: Boolean;
    FSupportsTriggers: Boolean;
    FIndexNamesUnique: Boolean;
    FNamedPrimaryKeys: Boolean;
    FEncloseNames: String;
    FFileDateTime: TDateTime;
    FIdentProps: TStrings;
    FSetTermCommand: String;
    FDefaultTerm: String;
    FOutputTerm: String;
    FCommentChars: String;
    FCommentFormat: String;
    FSupportsScripts: Boolean;
    FTopLevelObjects: TStrings;
    FIgnoreSubExpressionsChanged: Boolean;
    FQuoteChar: Char;
    FTestChangeCount: Integer;
    FNamesCaseSensitive: Boolean;
    FEncloseIdentifiersInQuotes: Boolean;
    FCustomObjectTypes: TStrings;
    FNamePatterns: TStrings;
    FMaxIdLength: Integer;
    FViewObjects: string;
    procedure SetInfoSchemaValueMap(const Value: TStrings);
    procedure SetSynonyms(const Value: TStrings);
    procedure SetInfoSchemaSQL(const Value: TStrings);
    function GetSupportsInfoSchema: Boolean;
    procedure SetEngineProps(const Value: TStrings);
    procedure SetProperties(const Value: TStrings);
    function GetExpressionCount: Integer;
    function GetExpressions(Idx: Integer): TFmtExpression;
    procedure SetEngineName(const Value: String);
    procedure SetFieldTypeMap(const Value: TStrings);
    procedure SetStatements(const Value: TStrings);
    function GetKeywords: TStrings;

    function OnObjectField(const FieldText: String; Data: Pointer): String;
    function OnItemField(Item: TCompareItem; const FieldText: String): String;
    function InternalGenerateSQL(const Statement: String; Item: TCompareItem): String;

    function RecreateTable(Item: TCompareSchemaItem): String;
    function RecreateObject(Item: TCompareSchemaItem): String;
    procedure AddHeader(HeaderExpr: TFmtExpression; var Res: String; Item: TCompareItem);

    function InsertIdentitySwitch(TableDef: TTableDefinition; Value: Boolean): String;
    procedure StatementChanged(Sender: TObject);

    function LocateObject(Schema: TDatabaseSchema; const ObjPath: String; ResultSet: TDataSet;
      var Obj: TSchemaCollectionItem; var Col: TSchemaItemsCollection): Boolean;
    procedure AssignObject(Obj: TSchemaCollectionItem; ResultSet: TDataSet; const ObjPath: String);
    procedure ParseSQL(ASchema: TDatabaseSchema; const ASQL: string);

    function FormatIBDataType(const FieldType, SubType, Scale, Precision,
      Size, SegLen, CharSet: String): String;

    function GetHRName(const AToken: string): string;
    function GetDisplayName: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(FileName: String = '');
    procedure SaveToFile(FileName: String = '');

    function CreateTempTableSQL(TableDef: TTableDefinition; var TempName: String): String;
    function CreateDatabaseSQL(Schema: TDatabaseSchema): String;
    function CreateObjectSQL(Item: TSchemaCollectionItem): String;
    function AlterObjectSQL(SrcItem, DestItem: TSchemaCollectionItem): String;
    function AlterDatabaseSQL(SrcSchema, DestSchema: TDatabaseSchema): String;
    function DropDatabaseSQL(Schema: TDatabaseSchema): String;
    function DropObjectSQL(Item: TSchemaCollectionItem): String;

    procedure ReverseEngineer(Schema: TDatabaseSchema; ExecSQLProc: TExecSQLProc);

    function HasInvalidSQLFieldTypes(Schema: TDatabaseSchema): Boolean;
    procedure ConvertSQLFieldTypes(Schema: TDatabaseSchema);

    function GetObjectTemplate(AnItem: TSchemaCollectionItem): String;

    procedure GetProperties(const ObjType: String; Props: TStrings); virtual;
    function GetPropInfo(const ObjType, PropName: String): String; virtual;

    procedure GetEngineFieldTypes(List: TStrings; const OnlyValidTypes: Boolean = False);

    function GetVCLFieldType(const SQLFieldType: String): String; { ft string value }
    function GetEngineFieldType(const VCLFieldType: String): String;
    function IsValidFieldType(const SQLFieldType: String): Boolean;
    function IsValidNamePattern(const AName: String): Boolean;
    procedure UpdateSchemaSQLFieldTypes(Schema: TDatabaseSchema);
    function GetValidIdentifier(const Ident: String): String;

    function IsIdentProp(const PropName: String): Boolean;

    procedure Refresh;

    function FormatName(const Name: String): String;
    function FormatNameList(const NameList: String): String;
    function FormatSQLString(const Str: String): String;

    function IsTopLevelObject(const ObjClassName: String): Boolean;
    function GetExpression(const Name: String): TFmtExpression;
    procedure RebuildExpressions;
    function GetStatementDelimiter: String;
    function GetSQLHeader: String;
    function ViewSupport(AClass: TClass): boolean;

    property FileName: String read FFileName;
    property Expressions[Idx: Integer]: TFmtExpression read GetExpressions;
    property ExpressionCount: Integer read GetExpressionCount;
    property Keywords: TStrings read GetKeywords;
    property SupportsDomains: Boolean read FSupportsDomains;
    property SupportsTriggers: Boolean read FSupportsTriggers;
    property IndexNamesUnique: Boolean read FIndexNamesUnique;
    property ParentStatements: TStrings read FParentStatements;
    property EncloseNames: String read FEncloseNames;
    property FileDateTime: TDateTime read FFileDateTime;
    property IdentProps: TStrings read FIdentProps;
    property CommentChars: String read FCommentChars;
    property DefaultTerm: String read FDefaultTerm write FDefaultTerm;
    property OutputTerm: String read FOutputTerm;
    property SetTermCommand: String read FSetTermCommand;
    property SupportsScripts: Boolean read FSupportsScripts;
    property CommentFormat: String read FCommentFormat;
    property NamedPrimaryKeys: Boolean read FNamedPrimaryKeys;
    property NamesCaseSensitive: Boolean read FNamesCaseSensitive;
    property TopLevelObjects: TStrings read FTopLevelObjects;
    property QuoteChar: Char read FQuoteChar;
    property SupportsInfoSchema: Boolean read GetSupportsInfoSchema;
    property EncloseIdentifiersInQuotes: Boolean read FEncloseIdentifiersInQuotes write FEncloseIdentifiersInQuotes;
    property CustomObjectTypes: TStrings read FCustomObjectTypes;
    property NamePatterns: TStrings read FNamePatterns;
    property MaxIdLength: Integer read FMaxIdLength;
    property DisplayName: string read GetDisplayName;
  published
    property EngineName: String read FEngineName write SetEngineName;
    property EngineProps: TStrings read FEngineProps write SetEngineProps;
    property PropDefs: TStrings read FProperties write SetProperties;
    property Statements: TStrings read FStatements write SetStatements;
    property FieldTypeMap: TStrings read FFieldTypeMap write SetFieldTypeMap;
    property Synonyms: TStrings read FSynonyms write SetSynonyms;
    property InfoSchemaSQL: TStrings read FInfoSchemaSQL write SetInfoSchemaSQL;
    property InfoSchemaValueMap: TStrings read FInfoSchemaValueMap write SetInfoSchemaValueMap;
  end;

  { Format Expression Parsing }
  TFmtOperandType = (otText, otValue, otExpression, otCase);

  TFmtExpression = class (TObjectList)
  protected
    FName: String;
  public
    IgnoreChanged: Boolean;
    constructor Parse(const FmtStr: String; var CurPos: Integer);
    constructor Create(const FmtStr: String);
    function Evaluate(Item: TCompareItem; OnGetProp: TGetPropMethod): String;
    procedure AddOperand(OpType: TFmtOperandType; Value: String; Expression: TFmtExpression = nil);
    property Name: String read FName;
  end;

  TFmtOperand = class
    OpType: TFmtOperandType;
    Value: String;
    Expression: TFmtExpression;
    destructor Destroy; override;
  end;

  procedure LoadProfiles(AOwner: TComponent; const Path: String; const FileExt: String = '.dbp');

  procedure GetDBEngineProfiles(List: TStrings; ADisplayName: boolean);
  function  GetDBEngineProfile(const EngineName: String): TDBEngineProfile;
  function  FindDBEngineProfile(const EngineName: String): TDBEngineProfile;
  procedure RegisterDBEngineProfile(Engine: TDBEngineProfile);
  procedure UnRegisterDBEngineProfile(Engine: TDBEngineProfile);

  procedure ReadSection(const SectionName: String; ConfigFile, Section: TStrings);
  procedure ReplaceSection(const SectionName: String; ConfigFile, Section: TStrings);
  procedure WriteSection(const SectionName: String; ConfigFile, Section: TStrings);

const
  SchemaOperations: array [TItemOperation] of TGenerateStatement = (gsCreate, gsAlter, gsDrop, gsNone);
  StatementNames: array [TGenerateStatement] of String = ('create', 'alter', 'drop', 'add', 'none');

const
  SECTION_ENGINE          = 'Engine';
  SECTION_FIELDTYPES      = 'FieldTypes';
  SECTION_SYNONYMS        = 'Synonyms';
  SECTION_STATEMENTS      = 'Statements';
  SECTION_PROPERTIES      = 'Properties';
  SECTION_INFOSCHEMASQL   = 'InfoSchemaSQL';
  SECTION_INFOSCHEMAVALUEMAP = 'InfoSchemaValueMap';

  ENTRY_ENGINENAME        = 'EngineName';
  ENTRY_DISPLAYNAME       = 'DisplayName';
  ENTRY_DOMAINS           = 'Domains';
  ENTRY_TRIGGERS          = 'Triggers';
  ENTRY_INDEXNAMESUNIQUE  = 'IndexNamesUnique';
  ENTRY_COMMENTCHARS      = 'CommentChars';
  ENTRY_COMMENTFORMAT     = 'CommentFormat';
  ENTRY_DEFAULTTERM       = 'DefaultTerm';
  ENTRY_OUTPUTTERM        = 'OutputTerm';
  ENTRY_SUPPORTSSCRIPTS    = 'SupportsScripts';
  ENTRY_SETTERM           = 'SetTerm';
  ENTRY_LITERALPREFIXES   = 'LiteralPrefixes';
  ENTRY_EXPRESSIONFELDS   = 'ExpressionFields';
  ENTRY_STATEMENTFIELDS   = 'StatementFields';
  ENTRY_USES              = 'Uses';
  ENTRY_ENCLOSENAMES      = 'EncloseNames';
  ENTRY_IDENTPROPS        = 'Identifiers';
  ENTRY_IMPORTOBJECTS     = 'ImportObjects';
  ENTRY_NAMEDPRIMARYKEYS  = 'NamedPrimaryKeys';
  ENTRY_NAMESCASESENSITIVE= 'NamesCaseSensitive';
  ENTRY_TOPLEVELOBJECTS   = 'TopLevelObjects';
  ENTRY_CUSTOMOBJECTS     = 'CustomObjects';
  ENTRY_QUOTECHAR         = 'QuoteChar';
  ENTRY_NAMEPATTERNS      = 'NamePatterns';
  ENTRY_MAXIDLENGTH       = 'MaxIdLength';
  ENTRY_VIEWOBJECTS       = 'ViewObjects';

const
  DefaultIdentProps = 'Name,TableName,ForeignTable,ForeignKeyFields,KeyFields,AddKeyField,AddForeignKeyField,RelationshipName';
  DefaultCommentChars = '--;//;/*,*/';
  DefaultMaxIDLength = 1024;

var
  DBEngineProfiles: TList;
  OnReverse: TOnReverseEvent;

resourcestring
  SCapabilityNotSupported = 'Capability not supported';
  SInfoSchemaNotSupported = 'Reverse Engineering from Info Schema views is not supported';
  SIdentifierTooLong = 'Identifier is too long: "%s"';

implementation

uses Forms,{$IFnDEF VER130}dbSQLParser,{$ENDIF} TypInfo, Masks;

{$I CtxD2009.inc}

{$IFDEF VER130}
function IncludeTrailingPathDelimiter(const Path: String): String;
begin
  Result := IncludeTrailingBackslash(Path);
end;
{$ENDIF}

procedure LoadProfiles(AOwner: TComponent; const Path: String; const FileExt: String = '.dbp');
var
  SR: TSearchRec;
  Found: Integer;
  I: Integer;
begin
  I := DBEngineProfiles.Count - 1;
  while I >= 0 do
  begin
    if TDBEngineProfile(DBEngineProfiles[I]).InheritsFrom(TDBEngineProfile)
      and (TDBEngineProfile(DBEngineProfiles[I]).Owner = AOwner)
    then
      TDBEngineProfile(DBEngineProfiles[I]).Free;
    Dec(I);
  end;

  Found := FindFirst(IncludeTrailingPathDelimiter(Path)+'*'+FileExt, faAnyFile, SR);
  try
    while Found = 0 do
    begin
      if SR.Attr and faDirectory = 0 then
      begin
        with TDBEngineProfile.Create(AOwner) do
        try
          FFileName := IncludeTrailingPathDelimiter(Path) + ExtractFileName(SR.Name);
          LoadFromFile(FFileName);
        except
          Free; // ignore exceptions
        end;
      end;
      Found := FindNext(SR);
    end;
  finally
    FindClose(SR);
  end;
end;

function FindDBEngineProfile(const EngineName: String): TDBEngineProfile;
var
  I: Integer;
begin
  for I := 0 to DBEngineProfiles.Count - 1 do
  begin
    Result := DBEngineProfiles[I];
    if AnsiSameText(Result.EngineName, EngineName) then exit;
  end;
  Result := nil;
end;

function GetDBEngineProfile(const EngineName: String): TDBEngineProfile;
begin
  Result := FindDBEngineProfile(EngineName);
  if Result = nil then
    raise Exception.CreateFmt('Database engine profile not found: %s', [EngineName]);
  Result.Refresh;
end;

procedure GetDBEngineProfiles(List: TStrings; ADisplayName: boolean);
var
  I: Integer;
  Eng: TDBEngineProfile;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := 0 to DBEngineProfiles.Count - 1 do
    begin
      Eng := TDBEngineProfile(DBEngineProfiles[I]);
      if ADisplayName then
        List.AddObject(Eng.DisplayName, DBEngineProfiles[I]) else
        List.AddObject(Eng.EngineName, DBEngineProfiles[I]);
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure RegisterDBEngineProfile(Engine: TDBEngineProfile);
begin
  if DBEngineProfiles.IndexOf(Engine) < 0 then
    DBEngineProfiles.Add(Engine);
end;

procedure UnRegisterDBEngineProfile(Engine: TDBEngineProfile);
begin
  DBEngineProfiles.Remove(Engine);
end;

{ Profile Utils (Ini-like parsing) }

procedure ReadSection(const SectionName: String; ConfigFile, Section: TStrings);
var
  I: Integer;
  Temp: String;
  SectionFound: Boolean;
begin
  Section.BeginUpdate;
  try
    Section.Clear;
    SectionFound := False;
    for I := 0 to ConfigFile.Count - 1 do
    begin
      Temp := Trim(ConfigFile[I]);
      if (Temp <> '') and (Temp[1] = '[') then
        if not SectionFound then
        begin
          Delete(Temp, 1, 1);
          if Temp = '' then continue;
          if Temp[Length(Temp)] = ']' then
            Delete(Temp, Length(Temp), 1);
          SectionFound := AnsiSameText(Temp, SectionName);
          continue;
        end else
          exit;

      if SectionFound then
        Section.Add(Temp);
    end;
  finally
    Section.EndUpdate;
  end;
end;

procedure ReplaceSection(const SectionName: String; ConfigFile, Section: TStrings);
var
  I: Integer;
  SectionBegin, SectionEnd: Integer;
  Temp: String;
  SectionFound: Boolean;
begin
  ConfigFile.BeginUpdate;
  try
    SectionBegin := 0;
    SectionEnd := -1;
    SectionFound := False;
    for I := 0 to ConfigFile.Count - 1 do
    begin
      Temp := Trim(ConfigFile[I]);
      if (Temp <> '') and (Temp[1] = '[') then
        if not SectionFound then
        begin
          Delete(Temp, 1, 1);
          if Temp = '' then continue;
          if Temp[Length(Temp)] = ']' then
            Delete(Temp, Length(Temp), 1);
          SectionFound := AnsiSameText(Temp, SectionName);
          SectionBegin := I + 1;
        end else begin
          SectionEnd := I;
          break;
        end;
    end;
    if SectionFound then
    begin
      // Erase section if it is found
      if SectionEnd < 0 then
        I := ConfigFile.Count
      else I := SectionEnd;
      while SectionBegin < I do
      begin
        if SectionBegin < ConfigFile.Count then
          ConfigFile.Delete(SectionBegin);
        Dec(I);
      end;
    end else
      SectionBegin := ConfigFile.Count;
    // Insert section into its place
    for I := 0 to Section.Count - 1 do
      ConfigFile.Insert(SectionBegin + I, Section[I]);
  finally
    ConfigFile.EndUpdate;
  end;
end;

procedure WriteSection(const SectionName: String; ConfigFile, Section: TStrings);
begin
  ConfigFile.Add('['+SectionName+']');
  ConfigFile.AddStrings(Section);
  if not ((Section.Count > 0) and (Trim(Section[Section.Count - 1]) = '')) then
    ConfigFile.Add('');
end;

{ TFmtOperand }

destructor TFmtOperand.Destroy;
begin
  Expression.Free;
  inherited;
end;

{ TFmtExpression }

constructor TFmtExpression.Create(const FmtStr: String);
var
  P: Integer;
begin
  P := 1;
  Parse(Trim(FmtStr), P);
end;

constructor TFmtExpression.Parse(const FmtStr: String; var CurPos: Integer);
var
  NextChar: Char;
  NextLiteral: Char;
  CaseExpr: TFmtExpression;
  Token: String;

  procedure GetNextChar(Literal: Boolean = False);
  begin
    if CurPos <= Length(FmtStr) then
    begin
      NextLiteral := FmtStr[CurPos];
      Inc(CurPos);
      if Literal then
        NextChar := '\'
      else if NextLiteral = '\' then
        GetNextChar(True)
      else NextChar := NextLiteral;
    end else begin
      NextLiteral := #0;
      NextChar := #0;
    end;
  end;

begin
  inherited Create(True);
  // Parse FmtStr starting from CurPos, stop at line end or at ']'
  Token := '';
  GetNextChar;
  IgnoreChanged := NextChar = '~';
  if IgnoreChanged then
    GetNextChar;
  while True do
  case NextChar of
    '<': begin
      GetNextChar;
      while not CharInSet(NextChar, [#0, '>']) do
      begin
        Token := Token + NextLiteral;
        GetNextChar;
      end;
      if NextChar = '>' then
        GetNextChar;
      // <!?^~*&$/'name'>
      AddOperand(otValue, Token);
      Token := '';
    end;
    '[': begin
      AddOperand(otExpression, '', TFmtExpression.Parse(FmtStr, CurPos));
      GetNextChar;
      Token := '';
    end;
    '{': begin
      CaseExpr := TFmtExpression.Create('');
      AddOperand(otCase, '', CaseExpr);
      repeat
        CaseExpr.AddOperand(otExpression, '', TFmtExpression.Parse(FmtStr, CurPos));
      until (CurPos > Length(FmtStr)) or (FmtStr[CurPos-1] = '}');
      GetNextChar;
      Token := '';
    end;
    #0, '}', '|', ']': exit;
    else begin
      while not CharInSet(NextChar, [#0, ']', '[', '{', '}', '|', '<']) do
      begin
        Token := Token + NextLiteral;
        GetNextChar;
      end;
      AddOperand(otText, Token);
      Token := '';
    end;
  end;
end;

procedure TFmtExpression.AddOperand(OpType: TFmtOperandType; Value: String; Expression: TFmtExpression = nil);
var
  Operand: TFmtOperand;
begin
  Operand := TFmtOperand.Create;
  Operand.OpType := OpType;
  Operand.Value := Value;
  Operand.Expression := Expression;
  Add(Operand);
end;

function TFmtExpression.Evaluate(Item: TCompareItem; OnGetProp: TGetPropMethod): String;
var
  J, I: Integer;
  SavePropsEqual: Boolean;
begin
  Result := '';
  // Attention! Evaluate sets Item.PropsEqual field to true if it contains changed properties
  // or false otheriwse. Caller must save previous state in order to restore it.
  Item.PropsEqual := True;
  for I := 0 to Count - 1 do
  with TFmtOperand(Items[I]) do
    case OpType of
      otText: Result := Result + Value;
      otValue: Result := Result + OnGetProp(Item, Value);
      otExpression: begin
        SavePropsEqual := Item.PropsEqual;
        Result := Result + Expression.Evaluate(Item, OnGetProp);
        Item.PropsEqual := Item.PropsEqual and SavePropsEqual;
      end;
      otCase: begin
        for J := 0 to Expression.Count - 1 do
        begin
          SavePropsEqual := Item.PropsEqual;
          Result := Result + TFmtOperand(Expression[J]).Expression.Evaluate(Item, OnGetProp);
          if not Item.PropsEqual then break;
          Item.PropsEqual := Item.PropsEqual and SavePropsEqual;
        end;
      end;
    end;
  if Item.PropsEqual then
    Result := '';
  if IgnoreChanged then
    Item.PropsEqual := True;
end;

{ TDBEngineProfile }

constructor TDBEngineProfile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExpressions := nil;
  FIgnoreSubExpressionsChanged := False;
  FEngineProps := TStringList.Create;
  FProperties := TStringList.Create;
  FInfoSchemaSQL := TStringList.Create;
  FInfoSchemaValueMap := TStringList.Create;
  FStatements := TStringList.Create;
  FParentStatements := TStringList.Create;
  FFieldTypeMap := TStringList.Create;
  FSynonyms := TStringList.Create;
  FKeywords := TStringList.Create;
  TStringList(FKeywords).Sorted := True;
  TStringList(FKeywords).Duplicates := dupIgnore;
  TStringList(FStatements).OnChange := StatementChanged;
  TStringList(FEngineProps).OnChange := StatementChanged;
  TStringList(FParentStatements).OnChange := StatementChanged;
  TStringList(FFieldTypeMap).OnChange := StatementChanged;
  TStringList(FSynonyms).OnChange := StatementChanged;
  TStringList(FInfoSchemaValueMap).OnChange := StatementChanged;
  TStringList(FInfoSchemaSQL).OnChange := StatementChanged;

  FCustomObjectTypes := TStringList.Create;
  FNamePatterns := TStringList.Create;
  FMaxIdLength := DefaultMaxIDLength;

  FIdentProps := TStringList.Create;
  TStringList(FIdentProps).Duplicates := dupIgnore;
  TStringList(FIdentProps).Sorted := True;
  FTopLevelObjects := TStringList.Create;
  FQuoteChar := '''';
  FLastAlterOperations := nil;
  FTestChangeCount := 0;
  FEncloseIdentifiersInQuotes := True;
end;

destructor TDBEngineProfile.Destroy;
begin
  UnRegisterDBEngineProfile(Self);
  FFieldTypeMap.Free;
  FSynonyms.Free;
  FStatements.Free;
  FProperties.Free;
  FInfoSchemaSQL.Free;
  FInfoSchemaValueMap.Free;
  FEngineProps.Free;
  FTopLevelObjects.Free;
  FParentStatements.Free;
  FLastAlterOperations.Free;
  FDropCreateTables.Free;
  FreeAndNil(FExpressions);
  FKeywords.Free;
  FIdentProps.Free;
  FCustomObjectTypes.Free;
  FNamePatterns.Free;
  inherited Destroy;
end;

procedure TDBEngineProfile.SetEngineName(const Value: String);
begin
  if FEngineName <> Value then
  begin
    FEngineName := Value;
    FEngineProps.Values[ENTRY_ENGINENAME] := FEngineName;
    if FEngineName <> '' then
      RegisterDBEngineProfile(Self)
    else UnRegisterDBEngineProfile(Self);
  end;
end;

procedure TDBEngineProfile.SetFieldTypeMap(const Value: TStrings);
begin
  FFieldTypeMap.Assign(Value);
end;

procedure TDBEngineProfile.SetStatements(const Value: TStrings);
begin
  FStatements.Assign(Value);
end;

function TDBEngineProfile.GetStatementDelimiter: String;
begin
  Result := OutputTerm + #13#10;
  // if (Result <> '') and (Result[1] in ['a'..'z', 'A'..'Z', '_']) then
  if OutputTerm <> ';' then
    Result := #13#10 + Result;
  // Result := Result; // '-- GO --'#13#10;
  // Result := Format(#13#10+EngineProps.Values['CommentFormat']+#13#10, ['GO']);
end;

function TDBEngineProfile.GetSQLHeader: String;
begin
  // -- TargetDB: DBISAM4; Delimiter: ";"; Comments: "--;/*,*/;//";
  Result := '-- ## TargetDB: ' + EngineName + ';';
  if not SupportsScripts and (OutputTerm <> '') then
    Result := Result + ' Delimiter: "' + OutputTerm + '";';
  if (CommentChars <> DefaultCommentChars) and (CommentChars <> '') then
    Result := Result + ' Comments: "' + CommentChars + '";';
end;

function TDBEngineProfile.ViewSupport(AClass: TClass): boolean;
begin
  if AClass = TFieldDefinitions then
    Result := AnsiPos('FIELD', FViewObjects) > 0 else
    if AClass = TIndexDefinitions then
      Result := AnsiPos('INDEX', FViewObjects) > 0 else
      if AClass = TRelations then
        Result := AnsiPos('RELATION', FViewObjects) > 0 else
        if AClass = TTriggerDefinitions then
          Result := AnsiPos('TRIGGER', FViewObjects) > 0 else
          if AClass = TTableConstraints then
            Result := AnsiPos('CHECK', FViewObjects) > 0 else
            Result := False;
end;

function TDBEngineProfile.CreateDatabaseSQL(Schema: TDatabaseSchema): String;
begin
  Result := AlterDatabaseSQL(nil, Schema);
end;

function TDBEngineProfile.AlterDatabaseSQL(SrcSchema, DestSchema: TDatabaseSchema): String;
var
  Item: TCompareSchema;
begin
  FreeAndNil(FDropCreateTables);
  Item := TCompareSchema.Create(SrcSchema, DestSchema);
  try
    // Preprocess TCompareItem to explore dependencies and add dependant objects
    // We also need to replace all alters with drop/create and separate those
    Result := InternalGenerateSQL('schema.'+StatementNames[SchemaOperations[Item.Operation]], Item);
    Result := Result + InternalGenerateSQL('schema.after.'+StatementNames[SchemaOperations[Item.Operation]], Item);
  finally
    Item.Free;
  end;
end;

function TDBEngineProfile.AlterObjectSQL(SrcItem, DestItem: TSchemaCollectionItem): String;
var
  Item: TCompareSchemaItem;
begin
  Item := TCompareSchemaItem.Create(SrcItem, DestItem);
  try
    Result := InternalGenerateSQL(
        Item.GetItem.GetSchemaClassName +'.'+StatementNames[SchemaOperations[Item.Operation]], Item);
    Result := Result + InternalGenerateSQL(
        Item.GetItem.GetSchemaClassName +'.after.'+StatementNames[SchemaOperations[Item.Operation]], Item);
  finally
    Item.Free;
  end;
end;

function TDBEngineProfile.CreateObjectSQL(Item: TSchemaCollectionItem): String;
begin
  Result := AlterObjectSQL(nil, Item);
end;

function TDBEngineProfile.DropDatabaseSQL(Schema: TDatabaseSchema): String;
begin
  Result := AlterDatabaseSQL(Schema, nil);
end;

function TDBEngineProfile.DropObjectSQL(Item: TSchemaCollectionItem): String;
begin
  Result := AlterObjectSQL(Item, nil);
end;

function TDBEngineProfile.CreateTempTableSQL(TableDef: TTableDefinition; var TempName: String): String;
var
  TempTableDef: TTableDefinition;
begin
  TempTableDef := TableDef.Schema.TableDefs.Add;
  try
    TempTableDef.Assign(TableDef);
    if TempName = '' then
      TempName := TempTableDef.GetAutoName('temp_'+TableDef.Name);
    TempTableDef.Name := TempName;
    TempTableDef.IndexDefs.Clear;
    TempTableDef.Relations.Clear;
    TempTableDef.Triggers.Clear;
    CreateObjectSQL(TempTableDef);
  finally
    TempTableDef.Free;
  end;
end;

function TDBEngineProfile.GetObjectTemplate(AnItem: TSchemaCollectionItem): String;
var
  Item: TCompareSchemaItem;
begin
  Item := TCompareSchemaItem.Create(nil, AnItem);
  try
    Result := InternalGenerateSQL(AnItem.GetSchemaClassName +'.template', Item);
  finally
    Item.Free;
  end;
end;

function TDBEngineProfile.OnObjectField(const FieldText: String; Data: Pointer): String;
begin
  Result := OnItemField(TCompareItem(Data), FieldText);
end;

function CalcPropChanged(Item: TCompareItem; SrcObj, DestObj: TObject; const APropName: String): Boolean;
var
  SrcValue, Prop: String;
begin
  Result := False;
  if (APropName = '') then exit;
  Result := True;
  if not Assigned(DestObj) then exit;
  Prop := APropName;

  // in create changed means not equal to DefaultValue
  // in alter changed means not equal to same of SrcObject
  if Assigned(SrcObj) then
    SrcValue := Item.GetPropValue(SrcObj, Prop)
  else SrcValue := Item.GetDefaultPropValue(DestObj, Prop);

  Result := not AnsiSameText(SrcValue, Item.GetPropValue(DestObj, Prop));
end;

function TDBEngineProfile.OnItemField(Item: TCompareItem; const FieldText: String): String;
var
  PropName, PropValue, EngineFldType, TempRes: String;
  Generate, TestPropChanged, TestOnly, TestNotEqual: Boolean;
  QuoteFieldValue, UseOldValue, OldPropsEqual: Boolean;
  SrcObject: TObject;
  ObjClasses, ObjClass: String;
  Delimiter, LineBreak: String;
  P: Integer;
  IdxDef: TIndexDefinition;

  function EffectiveEngineFieldType(const AFieldType: String; AItem: TSchemaCollectionItem): String;
  begin
    Result := AFieldType;
    if not AnsiSameText(EngineName, AItem.Schema.TargetDB) and
      (FieldTypeMap.Values[AFieldType] = '')
    then
    begin
      Result := Synonyms.Values[AFieldType];
      if Result = '' then
        Result := GetEngineFieldType(AItem.GetPropValue('DataType'));
    end;
  end;

  procedure AddDelimiter(SubExpression: String);
  begin
    if SubExpression <> '' then
    begin
      if TempRes <> '' then TempRes := TempRes + Delimiter;
      TempRes := TempRes + LineBreak + SubExpression;
    end;
  end;

  procedure ForEach(Op: TItemOperation; ObjStmt: TGenerateStatement);
  var
    I: Integer;
    SubItem: TCompareSchemaItem;
    TempSubItem: TCompareSchemaItem;
    SavePropsEqual, UseRecreate: Boolean;
    HeaderExpr, HeaderExprDrop, FmtExpr, FmtExprDrop: TFmtExpression;
    TempSubExpression, SubExpression: String;
  begin
    FmtExpr := GetExpression(PropName+'.'+StatementNames[ObjStmt]);
    FmtExprDrop := nil;
    UseRecreate := False;


    if (FmtExpr = nil) and (ObjStmt = gsAlter) then
    begin
      { DB 06112009 Changed in alter order. First step - drop, second step - add}
      FmtExprDrop := GetExpression(PropName+'.'+StatementNames[gsDrop]);
      FmtExpr := GetExpression(PropName+'.'+StatementNames[gsAdd]);
      HeaderExprDrop := GetExpression(PropName+'.'+StatementNames[gsDrop]+'.header');
      HeaderExpr := GetExpression(PropName+'.'+StatementNames[gsAdd]+'.header');
      {
      FmtExpr := GetExpression(PropName+'.'+StatementNames[gsAdd]);
      FmtExprDrop := GetExpression(PropName+'.'+StatementNames[gsDrop]);
      HeaderExpr := GetExpression(PropName+'.'+StatementNames[gsAdd]+'.header');
      HeaderExprDrop := GetExpression(PropName+'.'+StatementNames[gsDrop]+'.header');
      }
      UseRecreate := True;
    end else
    begin
      HeaderExpr := GetExpression(PropName+'.'+StatementNames[ObjStmt]+'.header');
      HeaderExprDrop := nil;
    end;

    if FmtExpr = nil then exit;

    with Item do
    if SubItems <> nil then
    for I := 0 to SubItems.Count - 1 do
    begin
      SubItem := SubItems[I] as TCompareSchemaItem;
      if AnsiSameText(ObjClass, SubItem.GetItem.GetSchemaClassName) and (SubItem.Operation = Op) then
      begin
        SavePropsEqual := SubItem.PropsEqual;

        UseRecreate := UseRecreate or SubItem.GetObj.InheritsFrom(TIndexDefinition);
        if UseRecreate then
        begin
          // Use Drop & Add statements instead of Alter
          if FmtExprDrop <> nil then
          begin
            TempSubItem := TCompareSchemaItem.Create(SubItem.SrcItem, nil);
            try
              TempSubExpression := FmtExprDrop.Evaluate(TempSubItem, OnItemField);
              AddHeader(HeaderExprDrop, TempSubExpression, TempSubItem);
            finally
              FreeAndNil(TempSubItem);
            end;
          end else
            TempSubExpression := '';
          TempSubItem := TCompareSchemaItem.Create(nil, SubItem.DestItem);
          try
            // Problem: even if subitems hasn't changed, it will still generate
            // the statement, because we're changing them effectively by recomparing
            // to nil items. We must be able to optionally ignore (~) subitems changes
            // and still generate them, which effectively means we need to affect the
            // expression, which is not possible.
            // We need to invent flag to ignore changes in subexpressions (*)
            TempSubItem.SrcObj := SubItem.SrcItem;
            FIgnoreSubExpressionsChanged := not (Assigned(SubItem.SubItems) and (SubItem.SubItems.Count > 0));
            try
              SubExpression := FmtExpr.Evaluate(TempSubItem, OnItemField);
              AddHeader(HeaderExpr, SubExpression, TempSubItem);
            finally
              FIgnoreSubExpressionsChanged := False;
            end;
            if SubExpression <> '' then
              AddDelimiter(TempSubExpression);
            AddDelimiter(SubExpression);
            SubItem.PropsEqual := TempSubItem.PropsEqual;
          finally
            FreeAndNil(TempSubItem);
          end;
        end else
        begin
          SubExpression := FmtExpr.Evaluate(SubItem, OnItemField);
          AddHeader(HeaderExpr, SubExpression, SubItem);
          AddDelimiter(SubExpression);
        end;

        if (Op <> ioAlter) and SubItem.GetObj.InheritsFrom(TTableDefinition) then
        begin
          if FDropCreateTables = nil then
            FDropCreateTables := TStringList.Create;
          FDropCreateTables.Add(AnsiUpperCase(TTableDefinition(SubItem.GetObj).Name));
        end;

        Item.PropsEqual := Item.PropsEqual and SubItem.PropsEqual;
        SubItem.PropsEqual := SubItem.PropsEqual and SavePropsEqual;
      end;
    end;
  end;

  procedure ForEach2(Op: TItemOperation; ObjStmt: TGenerateStatement);
  var
    _P: integer;
  begin
    _P := 1;
    while True do
    begin
      PropName := NextToken(ObjClasses, ';', _P);
      if PropName = '' then break;
      ObjClass := ChangeFileExt(PropName, '');
      ForEach(Op, ObjStmt);
    end;
  end;


begin
  TempRes := '';
  Result := '';
  if FieldText = '' then exit;

  P := AnsiPos('=', FieldText);
  TestOnly := P > 0;
  TestNotEqual := False;
  if TestOnly then
  begin
    PropName := copy(FieldText, 1, P-1);
    PropValue := copy(FieldText, P+1, MaxInt);
    if PropName = '' then exit;
    TestNotEqual := PropName[Length(PropName)] = '!';
    if TestNotEqual then
      Delete(PropName, Length(PropName), 1);
  end else begin
    PropName := FieldText;
    PropValue := '';
  end;

  // ?~{!|$|&|*|}/^
  Generate := PropName[1] <> '?';
  if not Generate then
    Delete(PropName, 1, 1);
  if PropName = '' then exit;

  TestPropChanged := PropName[1] <> '~';
  if not TestPropChanged then
    Delete(PropName, 1, 1);
  if PropName = '' then exit;

  if FTestChangeCount > 0 then
  begin
    if not TestPropChanged then exit;
    if TestOnly then exit;
    if CharInSet(AnsiChar(PropName[1]), ['^', '!']) then exit;
  end;

  UseOldValue := PropName[1] = '^';
  if UseOldValue then
    Delete(PropName, 1, 1);

  QuoteFieldValue := PropName[1] = '/';
  if QuoteFieldValue then
    Delete(PropName, 1, 1);

  if (PropName = '') or (PropName[1] = '+') then
    exit;

  if PropName = '!' then
    Item.PropsEqual := False
  else if PropName[1] = '$' then
    TempRes := InternalGenerateSQL(copy(PropName, 2, MaxInt), Item)
  else if PropName[1] = '&' then
  begin
    P := 2;
    ObjClasses := NextToken(PropName, '|', P);
    Delimiter := NextToken(PropName, '|', P);
    if Item.GetObj is TTableDefinition then
      LineBreak := #13#10
    else LineBreak := '';
    P := 1;
    while P <= Length(ObjClasses) do
    begin
      ObjClass := NextToken(ObjClasses, ';', P);
      if ObjClass = '' then continue;
      AddDelimiter(InternalGenerateSQL(ObjClass, Item));
    end;
  end else if PropName[1] = '*' then
  begin
    P := 2;
    ObjClasses := NextToken(PropName, '|', P);
    Delimiter := NextToken(PropName, '|', P);
    if (Delimiter = '') or (Item.GetObj is TTableDefinition) then
      LineBreak := #13#10
    else LineBreak := '';
    OldPropsEqual := Item.PropsEqual;

    case Item.Operation of
      ioCreate: begin
        if Item.SrcObj <> nil then
        begin
          Item.SrcObj := nil;
          if Item.SubItems <> nil then
            Item.SubItems.Clear;
          Item.SwapSrcDest;
          Item.CompareObjects;
          Item.SwapSrcDest;
        end;
        ForEach2(ioCreate, gsCreate);
      end;
      ioAlter: begin
        ForEach2(ioDrop, gsDrop);
        ForEach2(ioCreate, gsAdd);
        ForEach2(ioAlter, gsAlter);
      end;
      ioDrop: begin
        if Item.DestObj <> nil then
        begin
          Item.DestObj := nil;
          if Item.SubItems <> nil then
            Item.SubItems.Clear;
          Item.CompareObjects;
        end;
        ForEach2(ioDrop, gsDrop);
      end;     
    end;
    if OldPropsEqual and FIgnoreSubExpressionsChanged then
      Item.PropsEqual := True;
  end else if AnsiSameText(PropName, 'go') then
  begin
    TempRes := GetStatementDelimiter;
  end else if AnsiSameText(PropName, 'before_alter') then
  begin
    if FLastAlterOperations <> nil then
      FLastAlterOperations.Clear;
  end else if AnsiSameText(PropName, 'after_alter') then
  begin
    if (FLastAlterOperations <> nil) and (FLastAlterOperations.Count > 0) then
    begin
      if AnsiSameText((Item as TCompareSchemaItem).GetItem.GetSchemaClassName, 'table') then
        TempRes := RecreateTable(Item as TCompareSchemaItem)
      else
        TempRes := RecreateObject(Item as TCompareSchemaItem);
      FLastAlterOperations.Clear;
      Item.PropsEqual := False;
    end;
  end else if AnsiSameText(PropName, 'alter_failed') then
  begin
    if not Item.PropsEqual then
    begin
      if FLastAlterOperations = nil then
        FLastAlterOperations := TList.Create;
      if FLastAlterOperations.IndexOf(Item) < 0 then
        FLastAlterOperations.Add(Item);
    end;
  end else if AnsiSameText(PropName, 'alter_succeed') then
  begin
    if (FLastAlterOperations <> nil) and (FLastAlterOperations.Count > 0) then
      Item.PropsEqual := True;
  end else if AnsiSameText(PropName, 'sqldatatype') then
  begin
    OldPropsEqual := Item.PropsEqual;
    if Item.DestObj.InheritsFrom(TFieldDefinition) and FSupportsDomains
      and (TFieldDefinition(Item.DestObj).Domain <> '')
    then
      TempRes := FormatName(OnItemField(Item, 'domain'))
    else begin
      TempRes := OnItemField(Item, 'sqlfieldtype');
      // Calculate engine field type
      EngineFldType := EffectiveEngineFieldType(TempRes, TSchemaCollectionItem(Item.GetObj));
      if EngineFldType = '' then
        TempRes := '{undefined}'
      else begin
        if Generate then
        begin
          TempRes := InternalGenerateSQL('datatype.'+EngineFldType, Item);
          if TempRes = '' then
            TempRes := EngineFldType;
        end;
        Item.PropsEqual := OldPropsEqual;
        if TestPropChanged then
        begin
          OnItemField(Item, 'sqlfieldtype');
          // By incrementing this lock counter we eliminate all =, != old values, etc. instructions
          Inc(FTestChangeCount);
          try
            InternalGenerateSQL('datatype.'+EngineFldType, Item);
          finally
            Dec(FTestChangeCount);
          end;
        end;
      end;
    end;
    // if ignore changed status (~) then restore previous value 
    if not TestPropChanged then
      Item.PropsEqual := OldPropsEqual;

  end else if AnsiSameText(PropName, 'indexname') then
  begin
    if Item.GetObj.InheritsFrom(TIndexDefinition) then
    begin
      if UseOldValue then
        IdxDef := TIndexDefinition(Item.SrcObj)
      else IdxDef := TIndexDefinition(Item.DestObj);
      if IdxDef = nil then
        IdxDef := TIndexDefinition(Item.GetObj);

      if IndexNamesUnique then
        TempRes := FormatName(IdxDef.Name)
      else
        TempRes := FormatName(IdxDef.TableDef.Name) +'.'+ FormatName(IdxDef.Name);
    end;
  end else if AnsiSameText(PropName, 'br') then
  begin
    TempRes := #13#10;
  end else if AnsiSameText(PropName, 'term') then
  begin
    TempRes := OutputTerm;
  end else if AnsiSameText(PropName, 'tab') then
  begin
    TempRes := #32#32;
  end else if AnsiSameText(PropName, '__create') then
  begin
    if Item.Operation = ioCreate then
      Item.PropsEqual := False;
  end else if AnsiSameText(PropName, '__alter') then
  begin
    if Item.Operation = ioAlter then
      Item.PropsEqual := False;
  end else if AnsiSameText(PropName, '__drop') then
  begin
    if Item.Operation = ioDrop then
      Item.PropsEqual := False;
  end else with Item do
  begin
    if UseOldValue then
    begin
      if SrcObj <> nil then
        TempRes := GetPropValue(SrcObj, PropName);
    end else begin
      if DestObj <> nil then
        TempRes := GetPropValue(DestObj, PropName)
      else TempRes := GetPropValue(SrcObj, PropName)
    end;

    if not TestOnly then
      if QuoteFieldValue then
        TempRes := FormatSQLString(TempRes)
      else begin
        if AnsiSameText(PropName, 'ForeignKeyFields') or AnsiSameText(PropName, 'KeyFields') then
          TempRes := FormatNameList(TempRes)
        else if IsIdentProp(PropName) then
          TempRes := FormatName(TempRes);
      end;

    if TestPropChanged and not TestOnly then
    begin
      SrcObject := nil;
      if FSupportsDomains and (Item.DestObj <> nil)
        and Item.DestObj.InheritsFrom(TFieldDefinition)
        and TFieldDefinition(Item.DestObj).IsInheritedProp(PropName)
      then
        with TFieldDefinition(Item.DestObj) do
          SrcObject := Schema.Domains.Find(Domain);

      if SrcObject = nil then
        SrcObject := Item.SrcObj;

      Item.PropsEqual := Item.PropsEqual and not CalcPropChanged(Item, SrcObject, Item.DestObj, PropName);
    end;
  end;

  if TestOnly then
  begin
    // ~ will defeat the test, This can be used for input
    if not TestPropChanged then exit;

    TestOnly := not AnsiSameText(TempRes, PropValue);
    if TestNotEqual then
      TestOnly := not TestOnly;
    Item.PropsEqual := Item.PropsEqual and TestOnly;
  end else if Generate then
    Result := TempRes;
end;

function TDBEngineProfile.InsertIdentitySwitch(TableDef: TTableDefinition; Value: Boolean): String;
const
  BoolSwitch: array [Boolean] of String = ('off', 'on');
var
  TempItem: TCompareSchemaItem;
begin
  Result := '';
  // Generates SQL statement to allow insert identities if value is True
  if TableDef.HasIdentityFields then
  begin
    TempItem := TCompareSchemaItem.Create(TableDef, nil);
    try
      Result := InternalGenerateSQL('identity.'+BoolSwitch[Value], TempItem);
    finally
      TempItem.Free;
    end;
  end;
end;

function TDBEngineProfile.RecreateObject(Item: TCompareSchemaItem): String;
begin
  Result := DropObjectSQL(Item.SrcItem) + #13#10 + CreateObjectSQL(Item.DestItem);
end;

function TDBEngineProfile.RecreateTable(Item: TCompareSchemaItem): String;
var
  TempTable, SrcTable, DestTable: TTableDefinition;
  TempSchema: TDatabaseSchema;
  FKItem: TCompareSchemaItem;
  I: Integer;

  function GenerateHeader(const EntryName: String): String;
  var
    SavePropsEqual: Boolean;
  begin
    SavePropsEqual := Item.PropsEqual;
    Result := InternalGenerateSQL(EntryName, Item);
    Item.PropsEqual := SavePropsEqual;
    if (Result = '') or (Result[Length(Result)] <> #10) then
      Result := Result + #13#10;
  end;

  function CreateDestTableSQL(ADestTable: TTableDefinition): String;
  begin
    Result := CreateObjectSQL(ADestTable);
    (*
    The problem here is that we will alter some of the foreign keys two times.
    First, when we create table in table.after.create and then when this
    alter statement finishes in table.after.alter.
    There's no way to avoid this currently.

  var
    TableItem: TCompareSchemaItem;

    TableItem := TCompareSchemaItem.Create(nil, ADestTable);
    try
      Result := InternalGenerateSQL(
        TableItem.GetItem.GetSchemaClassName +'.'+StatementNames[SchemaOperations[TableItem.Operation]], TableItem);
    finally
      TableItem.Free;
    end;
    *)
  end;

  procedure CopyData(const SrcTableName: String);
  var
    I: Integer;
    ByName: Boolean;
    SrcFields, DestFields: String;
    OldFieldDef: TSchemaCollectionItem;
  begin
    SrcFields := '';
    DestFields := '';
    ByName := SrcTable.Schema.SchemaID <> DestTable.Schema.SchemaID;
    for I := 0 to DestTable.FieldDefs.Count - 1 do
    begin
      if ByName then
        OldFieldDef := SrcTable.FieldDefs.Find(DestTable.FieldDefs[I].Name)
      else OldFieldDef := SrcTable.FieldDefs.FindByItemID(DestTable.FieldDefs[I].ItemID);
      if OldFieldDef = nil then continue;

      if SrcFields <> '' then SrcFields := SrcFields + ', ';
      if DestFields <> '' then DestFields := DestFields + ', ';
      SrcFields := SrcFields + FormatName(OldFieldDef.Name);
      DestFields := DestFields + FormatName(DestTable.FieldDefs[I].Name);
    end;
    if (SrcFields <> '') and (DestFields <> '') then
    begin
      Result := Result
        + InsertIdentitySwitch(DestTable, True)
        + GenerateHeader('copydata.header')
        + Format('INSERT INTO %s('#13#10'  %s'#13#10') SELECT'#13#10'  %s'#13#10'FROM %s',
          [FormatName(DestTable.Name), DestFields, SrcFields, FormatName(SrcTableName)])
        + GetStatementDelimiter
        + InsertIdentitySwitch(DestTable, False);
    end;
  end;

begin
  Result := '';
  SrcTable := Item.SrcItem as TTableDefinition;
  DestTable := Item.DestItem as TTableDefinition;

  // <srctable> <srcfields> <desttable> <destfields>
  TempSchema := TDatabaseSchema.Create(nil);
  try
    TempSchema.TargetDB := EngineName;
    TempTable := TempSchema.TableDefs.Add;
    if not AnsiSameText(SrcTable.Name, DestTable.Name) then // Rename ONLY!!!
    begin
      TempTable.Name := SrcTable.Name;
      TempTable.FieldDefs := SrcTable.FieldDefs;
      // Drop everything except for fields
      Result := Result + AlterObjectSQL(SrcTable, TempTable);
      // Create destination table
      Result := Result + CreateDestTableSQL(DestTable);
      if (Item.SubItems = nil) or (Item.SubItems.Count = 0) then
      begin
      // if (FLastAlterOperations.Count = 1) and (FLastAlterOperations[0] = Item) then
        Result := Result
          + InsertIdentitySwitch(DestTable, True)
          + GenerateHeader('insertdata.header')
          + Format('INSERT INTO %s SELECT * FROM %s', [FormatName(DestTable.Name), FormatName(SrcTable.Name)])
          + GetStatementDelimiter
          + InsertIdentitySwitch(DestTable, False)
      end else CopyData(SrcTable.Name);
      Result := Result + DropObjectSQL(SrcTable);
    end else begin
      TempTable.Name := TempTable.GetAutoName('temp_' + SrcTable.Name);
      // DB++ 09032010
      for I := 0 to SrcTable.FieldDefs.Count - 1 do
        with TempTable.FieldDefs.Add do
        begin
          Name := SrcTable.FieldDefs[I].Name;
          SQLFieldType := SrcTable.FieldDefs[I].SQLFieldType;
          Size := SrcTable.FieldDefs[I].Size;
        end;
      { DB-- 09032010
      TempTable.FieldDefs := SrcTable.FieldDefs;
      // Turn off identity for temporary table. This will not guarantee, that the identity will be actually turned off.
      for I := 0 to TempTable.FieldDefs.Count - 1 do
        TempTable.FieldDefs[I].Identity := False;
      }
      Result := Result + CreateObjectSQL(TempTable);
      Result := Result
        + InsertIdentitySwitch(TempTable, True)
        + GenerateHeader('backupdata.header')
        + Format('INSERT INTO %s SELECT * FROM %s', [FormatName(TempTable.Name), FormatName(SrcTable.Name)])
        + GetStatementDelimiter
        + InsertIdentitySwitch(TempTable, False);
      Result := Result + #13#10 + DropObjectSQL(SrcTable);
      Result := Result + #13#10 + CreateDestTableSQL(DestTable);
      CopyData(TempTable.Name);
      Result := Result + #13#10 + DropObjectSQL(TempTable);
    end;
  finally
    TempSchema.Free;
  end;
  // Recreate Master Relations Relations[I].GetPeerRelation +++
  // Result := Result + #13#10 + InternalGenerateSQL('table.recreate', Item); dbSchema
  for I := 0 to DestTable.Schema.Relationships.Count - 1 do
  with DestTable.Schema.Relationships[I] do
    if EnforceForeignKey and (MasterTableDef = DestTable)
      and ((FDropCreateTables = nil) or (FDropCreateTables.IndexOf(AnsiUpperCase(DetailTableName)) < 0)) then
    begin
      FKItem := TCompareSchemaItem.Create(nil, DetailRelation);
      try
        Result := Result + #13#10 + InternalGenerateSQL('foreignkey.recreate', FKItem);
      finally
        FKItem.Free;
      end;
    end;

  FreeAndNil(Item.SubItems);
end;

procedure TDBEngineProfile.AddHeader(HeaderExpr: TFmtExpression; var Res: String; Item: TCompareItem);
var
  Header: String;
  SavePropsEqual: Boolean;
begin
  if (HeaderExpr <> nil) and (Res <> '') then
  begin
    SavePropsEqual := Item.PropsEqual;
    Header := HeaderExpr.Evaluate(Item, OnItemField);
    if Header <> '' then
      Res := Header + Res; //#13#10 + 
    Item.PropsEqual := SavePropsEqual;
  end;
end;

function TDBEngineProfile.InternalGenerateSQL(const Statement: String; Item: TCompareItem): String;
var
  HeaderExpr, FmtExpr: TFmtExpression;
  SavePropsEqual: Boolean;
begin
  Result := '';
  with Item do
  begin
    FmtExpr := GetExpression(Statement);
    if FmtExpr = nil then exit;
    HeaderExpr := GetExpression(Statement + '.header');

    SavePropsEqual := Item.PropsEqual;
    Result := Result + FmtExpr.Evaluate(Item, OnItemField);
    if Item.PropsEqual then
      Result := '';
    Item.PropsEqual := Item.PropsEqual and SavePropsEqual;

    AddHeader(HeaderExpr, Result, Item);
  end;
end;

procedure TDBEngineProfile.GetEngineFieldTypes(List: TStrings; const OnlyValidTypes: Boolean = False);
var
  I: Integer; // TFieldDataType;
  Temp: String;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := 0 to FieldTypeMap.Count - 1 do
    begin
      Temp := Trim(FieldTypeMap.Names[I]);
      if IsValidFieldType(Temp) or not OnlyValidTypes then
        List.Add(Temp);
    (* +++
    for I := Low(FieldDataTypes) to High(FieldDataTypes) do
    begin
      Temp := GetEngineFieldType(FieldDataTypes[I]);
      if IsValidFieldType(Temp) or not OnlyValidTypes then
        List.Add(Temp);
    *)
    end;
  finally
    List.EndUpdate;
  end;

end;

function TDBEngineProfile.GetEngineFieldType(const VCLFieldType: String): String;
var
  I: Integer;
begin
  // Convert engine field type to schema field type
  // Result := FSynonyms.Values[EngineFieldType];
  // if Result <> '' then exit;
  for I := 0 to FieldTypeMap.Count - 1 do
    if AnsiSameText(ValueFromIndex(FieldTypeMap, I), VCLFieldType) then
    begin
      Result := FieldTypeMap.Names[I];
      exit;
    end;
  Result := '';
end;

function TDBEngineProfile.GetVCLFieldType(
  const SQLFieldType: String): String;
begin
  Result := FieldTypeMap.Values[SQLFieldType];
  if Result = '' then
    Result := FieldTypeMap.Values[FSynonyms.Values[SQLFieldType]];
  if Result = '' then
    Result := 'ftUnknown';
end;

function TDBEngineProfile.IsValidFieldType(const SQLFieldType: String): Boolean;
var
  Temp: String;
begin
  Temp := Trim(FieldTypeMap.Values[SQLFieldType]);
  Result := (Temp <> '') and (Temp <> 'ftUnknown');
end;

function TDBEngineProfile.IsValidNamePattern(const AName: String): Boolean;
var
  I: Integer;
begin
  for I := 0 to FNamePatterns.Count - 1 do
    if MatchesMask(AName, FNamePatterns[I]) then
    begin
      Result := False;
      exit;
    end;
  Result := True;
end;

function TDBEngineProfile.GetValidIdentifier(const Ident: String): String;
begin
  Result := Ident;
  if Length(Result) > FMaxIdLength then
    raise Exception.CreateFmt(SIdentifierTooLong, [Result]);
end;

procedure TDBEngineProfile.UpdateSchemaSQLFieldTypes(Schema: TDatabaseSchema);
var
  I, J: Integer;
begin
  for I := 0 to Schema.TableDefs.Count - 1 do
    for J := 0 to Schema.TableDefs[I].FieldDefs.Count - 1 do
    with Schema.TableDefs[I].FieldDefs[J] do
      if (ItemID >= 0) and (SQLFieldType = '') then
        SQLFieldType := GetEngineFieldType(GetPropValue('DataType'));
  for I := 0 to Schema.Domains.Count - 1 do
    with Schema.Domains[I] do
      if (ItemID >= 0) and (SQLFieldType = '') then
        SQLFieldType := GetEngineFieldType(GetPropValue('DataType'));
end;

procedure TDBEngineProfile.LoadFromFile(FileName: String);
var
  ParentFileName: String;
  D: TDateTime;
  TempList: TStringList;
begin
  if FileName = '' then
    FileName := FFileName;
  if FileExists(FileName) then
    FFileDateTime := FileDateToDateTime(FileAge(FileName));
  // Load from ini file
  TempList := TStringList.Create;
  try
    TempList.LoadFromFile(FileName);
    ReadSection(SECTION_ENGINE, TempList, FEngineProps);
    ReadSection(SECTION_PROPERTIES, TempList, FProperties);
    ReadSection(SECTION_STATEMENTS, TempList, FStatements);
    ReadSection(SECTION_FIELDTYPES, TempList, FFieldTypeMap);
    ReadSection(SECTION_SYNONYMS, TempList, FSynonyms);
    ReadSection(SECTION_INFOSCHEMASQL, TempList, FInfoSchemaSQL);
    ReadSection(SECTION_INFOSCHEMAVALUEMAP, TempList, FInfoSchemaValueMap);
    FParentStatements.Clear;
  finally
    TempList.Free;
  end;
  FFileName := FileName;
  // Load parent profile and override statements
  ParentFileName := Trim(FEngineProps.Values[ENTRY_USES]);
  if ParentFileName = '' then exit;
  ParentFileName := IncludeTrailingPathDelimiter(ExtractFilePath(FileName)) + ParentFileName;

  if FileExists(ParentFileName) then
  begin
    D := FileDateToDateTime(FileAge(FileName));
    if D > FFileDateTime then
      FFileDateTime := D;
  end;

  if FileExists(ParentFileName) then
  begin
    TempList := TStringList.Create;
    try
      TempList.LoadFromFile(ParentFileName);
      ReadSection(SECTION_STATEMENTS, TempList, FParentStatements);
    finally
      TempList.Free;
    end;
  end;
end;

procedure TDBEngineProfile.SaveToFile(FileName: String);
var
  TempList: TStringList;
begin
  if FileName = '' then
    FileName := FFileName;

  TempList := TStringList.Create;
  try
    // Save to ini file
    if FileExists(FileName) then
      TempList.LoadFromFile(FileName);
    ReplaceSection(SECTION_ENGINE, TempList, FEngineProps);
    ReplaceSection(SECTION_PROPERTIES, TempList, FProperties);
    ReplaceSection(SECTION_STATEMENTS, TempList, FStatements);
    ReplaceSection(SECTION_FIELDTYPES, TempList, FFieldTypeMap);
    ReplaceSection(SECTION_SYNONYMS, TempList, FSynonyms);
    ReplaceSection(SECTION_INFOSCHEMASQL, TempList, FInfoSchemaSQL);
    ReplaceSection(SECTION_INFOSCHEMAVALUEMAP, TempList, FInfoSchemaValueMap);
    TempList.SaveToFile(FileName);
  finally
    TempList.Free;
  end;
  FFileName := FileName;
end;

procedure TDBEngineProfile.GetProperties(const ObjType: String;
  Props: TStrings);
var
  I: Integer;
begin
  for I := 0 to FProperties.Count - 1 do
    if AnsiPos(UpperCase(ObjType+'.'), UpperCase(FProperties[I])) = 1 then
      Props.Add(copy(FProperties.Names[I], Length(ObjType) + 2, MaxInt));
end;

function TDBEngineProfile.GetPropInfo(const ObjType,
  PropName: String): String;
begin
  Result := FProperties.Values[ObjType + '.' + PropName];
end;

function TDBEngineProfile.GetExpression(const Name: String): TFmtExpression;
var
  I: Integer;
begin
  if FExpressions = nil then RebuildExpressions;
  for I := 0 to FExpressions.Count - 1 do
  begin
    Result := Expressions[I];
    if AnsiSameText(Result.Name, Name) then exit;
  end;
  Result := nil;
end;

function TDBEngineProfile.GetExpressionCount: Integer;
begin
  if FExpressions = nil then RebuildExpressions;
  Result := FExpressions.Count;
end;

function TDBEngineProfile.GetExpressions(Idx: Integer): TFmtExpression;
begin
  if FExpressions = nil then RebuildExpressions;
  Result := FExpressions[Idx] as TFmtExpression;
end;

procedure TDBEngineProfile.RebuildExpressions;

  procedure ExtractKeywords(const Str: String);
  var
    I: Integer;
    Keyword: String;
  begin
    I := 1;
    while I <= Length(Str) do
    begin
      Keyword := '';
      while (I <= Length(Str)) and CharInSet(Str[I], ['_', 'a'..'z', 'A'..'Z']) do
      begin
        Keyword := Keyword + Str[I];
        Inc(I);
      end;
      if Keyword <> '' then
        FKeywords.Add(Keyword);
      while (I <= Length(Str)) and not CharInSet(Str[I], ['_', 'a'..'z', 'A'..'Z']) do
        Inc(I);
    end;
  end;

  procedure AddKeywords(Expression: TFmtExpression);
  var
    I: Integer;
  begin
    for I := 0 to Expression.Count - 1 do
    with TFmtOperand(Expression[I]) do
      case OpType of
        otText: ExtractKeywords(Value);
        otExpression, otCase: AddKeywords(Expression);
      end;
  end;

var
  I: Integer;
  ExprName: String;
  Expr: TFmtExpression;
  TempStatements: TStringList;
begin
  if FExpressions = nil then
    FExpressions := TObjectList.Create(True)
  else FExpressions.Clear;
  FKeywords.Clear;

  TempStatements := TStringList.Create;
  try
    if FParentStatements.Count > 0 then
    begin
      TempStatements.Assign(FParentStatements);
      for I := 0 to FStatements.Count - 1 do
        if (FStatements[I] <> '') and (FStatements[I][1] <> ';') then
          TempStatements.Values[FStatements.Names[I]] := ValueFromIndex(FStatements, I);
    end else
      TempStatements.Assign(FStatements);

    for I := 0 to TempStatements.Count - 1 do
    begin
      ExprName := TempStatements.Names[I];
      if ExprName = '' then continue;
      Expr := TFmtExpression.Create(ValueFromIndex(TempStatements, I));
      Expr.FName := ExprName;
      FExpressions.Add(Expr);
      AddKeywords(Expr);
    end;
  finally
    TempStatements.Free;
  end;
end;

procedure TDBEngineProfile.StatementChanged(Sender: TObject);
var
  QuoteStr: String;
begin
  FreeAndNil(FExpressions);
  FKeywords.Clear;
  // Update engine properties
  EngineName := EngineProps.Values[ENTRY_ENGINENAME];
  FDisplayName := EngineProps.Values[ENTRY_DISPLAYNAME];
  FSupportsDomains := AnsiSameText(EngineProps.Values[ENTRY_DOMAINS], 'yes');
  FSupportsTriggers := AnsiSameText(EngineProps.Values[ENTRY_TRIGGERS], 'yes');
  FIndexNamesUnique := AnsiSameText(EngineProps.Values[ENTRY_INDEXNAMESUNIQUE], 'yes');
  FNamedPrimaryKeys := AnsiSameText(EngineProps.Values[ENTRY_NAMEDPRIMARYKEYS], 'yes');
  FNamesCaseSensitive := AnsiSameText(EngineProps.Values[ENTRY_NAMESCASESENSITIVE], 'yes');
  FTopLevelObjects.CommaText := EngineProps.Values[ENTRY_TOPLEVELOBJECTS];
  FCustomObjectTypes.CommaText := EngineProps.Values[ENTRY_CUSTOMOBJECTS];
  FNamePatterns.CommaText := EngineProps.Values[ENTRY_NAMEPATTERNS];
  FMaxIdLength := StrToIntDef(EngineProps.Values[ENTRY_MAXIDLENGTH], DefaultMaxIDLength);
  FViewObjects := AnsiUpperCase(EngineProps.Values[ENTRY_VIEWOBJECTS]);
  QuoteStr := EngineProps.Values[ENTRY_QUOTECHAR];
  if QuoteStr <> '' then
    FQuoteChar := QuoteStr[1];

  FEncloseNames := EngineProps.Values[ENTRY_ENCLOSENAMES];
  if Length(FEncloseNames) = 1 then
    FEncloseNames := FEncloseNames + FEncloseNames
  else if Length(FEncloseNames) > 2 then
    FEncloseNames := copy(FEncloseNames, 1, 2);

  FCommentChars := Trim(EngineProps.Values[ENTRY_COMMENTCHARS]);
  if FCommentChars = '' then
    FCommentChars := DefaultCommentChars;
  FSupportsScripts := AnsiSameText(EngineProps.Values[ENTRY_SUPPORTSSCRIPTS], 'yes');
  FDefaultTerm := Trim(EngineProps.Values[ENTRY_DEFAULTTERM]);
  if FDefaultTerm = '' then
    FDefaultTerm := ';';
  FOutputTerm := Trim(EngineProps.Values[ENTRY_OUTPUTTERM]);
  if FOutputTerm = '' then
    FOutputTerm := FDefaultTerm;
  FSetTermCommand := Trim(EngineProps.Values[ENTRY_SETTERM]);
  FCommentFormat := EngineProps.Values[ENTRY_COMMENTFORMAT];
  if FCommentFormat = '' then
    FCommentFormat := '-- %s';

  FIdentProps.CommaText := EngineProps.Values[ENTRY_IDENTPROPS] + ',' + DefaultIdentProps;
  TStringList(FIdentProps).Duplicates := dupIgnore;
  TStringList(FIdentProps).Sorted := True;
end;

function TDBEngineProfile.GetKeywords: TStrings;
begin
  if FExpressions = nil then RebuildExpressions;
  Result := FKeywords;
end;

procedure TDBEngineProfile.SetEngineProps(const Value: TStrings);
begin
  FEngineProps.Assign(Value);
end;

procedure TDBEngineProfile.SetProperties(const Value: TStrings);
begin
  FProperties.Assign(Value);
end;

function TDBEngineProfile.FormatName(const Name: String): String;
begin
  if FEncloseIdentifiersInQuotes and (FEncloseNames <> '') then
    Result := FEncloseNames[1] + Name + FEncloseNames[2]
  else Result := Name;
  if AnsiSameText(EngineProps.Values['IdSpaces'], 'no') then
    Result := StringReplace(Result, ' ', '_', [rfReplaceAll]);
end;

function TDBEngineProfile.FormatNameList(const NameList: String): String;
begin
  Result := Trim(NameList);
  if Result = '' then exit;
  if FEncloseIdentifiersInQuotes and (FEncloseNames <> '') then
    Result := FEncloseNames[1] + StringReplace(Result, ';', FEncloseNames[2]+','+FEncloseNames[1], [rfReplaceAll]) + FEncloseNames[2]
  else Result := StringReplace(Result, ';', ',', [rfReplaceAll]);
end;

function TDBEngineProfile.FormatSQLString(const Str: String): String;
begin
  Result := AnsiQuotedStr(Str, QuoteChar);
end;

procedure TDBEngineProfile.Refresh;
var
  ParentFileName: String;
  Reload: Boolean;
begin
  if FFileName = '' then exit;
  Reload := FileExists(FFileName) and (FFileDateTime < FileDateToDateTime(FileAge(FFileName)));
  if not Reload then
  begin
    ParentFileName := Trim(FEngineProps.Values[ENTRY_USES]);
    if ParentFileName <> '' then
    begin
      ParentFileName := IncludeTrailingPathDelimiter(ExtractFilePath(FileName)) + ParentFileName;
      Reload := FileExists(ParentFileName) and (FFileDateTime < FileDateToDateTime(FileAge(ParentFileName)));
    end;
  end;
  if Reload then
    LoadFromFile;
end;

function TDBEngineProfile.IsIdentProp(const PropName: String): Boolean;
begin
  Result := (PropName <> '') and (FIdentProps.IndexOf(PropName) >= 0);
end;

function TDBEngineProfile.IsTopLevelObject(const ObjClassName: String): Boolean;
begin
  Result := FTopLevelObjects.IndexOf(ObjClassName) >= 0;
end;

function TDBEngineProfile.GetSupportsInfoSchema: Boolean;
begin
  Result := InfoSchemaSQL.Count > 0;
end;

procedure TDBEngineProfile.SetInfoSchemaSQL(const Value: TStrings);
begin
  FInfoSchemaSQL.Assign(Value);
end;

procedure TDBEngineProfile.SetSynonyms(const Value: TStrings);
begin
  FSynonyms.Assign(Value);
end;

procedure TDBEngineProfile.SetInfoSchemaValueMap(const Value: TStrings);
begin
  FInfoSchemaValueMap.Assign(Value);
end;

function TDBEngineProfile.GetDisplayName: string;
begin
  Result := FDisplayName;
  if Trim(Result) = '' then
    Result := FEngineName;
end;

function TDBEngineProfile.GetHRName(const AToken: string): string;
var
  S: string;
  P: integer;
begin
  P := 1;
  Result := '';
  while True do
  begin
    S := NextToken(AToken, '.', P);
    if S <> '' then
      Result := S else
      break;
  end;
  if Result = '' then
    Exit;
  Result := LowerCase(Result);
  if Result = 'domains' then
    Result := 'Domains' else
  if Result = 'sequences' then
    Result := 'Sequences' else
  if Result = 'tabledefs' then
    Result := 'Tables' else
  if Result = 'fielddefs' then
    Result := 'Fields' else
  if Result = 'indexdefs' then
    Result := 'Indices' else
  if Result = 'indexfields' then
    Result := 'Indices' else
  if Result = 'constraints' then
    Result := 'Constraints' else
  if Result = 'triggers' then
    Result := 'Triggers' else
  if Result = 'relationships' then
    Result := 'Relations' else
  if Result = 'viewdefs' then
    Result := 'Views' else
  if Result = 'storedprocs' then
    Result := 'Stored procedures' else
  if Result = 'customobjects' then
    Result := 'Custom Objects' else
    Result := AToken;
end;

procedure TDBEngineProfile.ReverseEngineer(Schema: TDatabaseSchema; ExecSQLProc: TExecSQLProc);
type
  TImportType = (itNormal, itList, itListAdd, itIterate, itIterateParse);

var
  I,C,ImpCnt: Integer;
  SQL, ObjPath: String;
  ListCol: TSchemaItemsCollection;
  LastObj: TSchemaCollectionItem;
  LastCol: TSchemaItemsCollection;
  ObjectFound: Boolean;
  NameList: TStringList;
  ParseFld: TField;
  ImpType: TImportType;

  procedure DoExecSQL(const ASQL, AParam: string);
  var
    ResultSet: TDataSet;
    S: string;
    fI: integer;
  begin
    ResultSet := nil;
    try
      if AParam <> '' then
        ExecSQLProc(Format(ASQL, [AParam]), @ResultSet) else
        ExecSQLProc(ASQL, @ResultSet);
    except
      on E:Exception do
      begin
        FreeAndNil(ResultSet);
        if Assigned(OnReverse) then
        begin
          OnReverse(-1, 'Importing '+GetHRName(ObjPath) + ' hase error:');
          OnReverse(-1, E.Message);
        end;
      end;
    end;
    if ResultSet <> nil then
    try
      ResultSet.First;
      ParseFld := ResultSet.FindField('_CtxParse_');
      if (ImpType = itIterateParse) and (ParseFld = nil) then
      begin
        for fI := 0 to ResultSet.FieldCount-1 do
          if (ResultSet.FieldCount = 1) or (ResultSet.Fields[fI].Size > 128) then
          begin
            ParseFld := ResultSet.Fields[fI];
            Break;
          end;
      end;
      while not ResultSet.EOF do
      begin
        if ImpType in [itList, itListAdd] then
        begin
          S := ResultSet.Fields[0].AsString;
          if (ListCol <> nil) and (ListCol.Find(S) = nil) then
          begin
            if ImpType = itListAdd then
              ListCol.Add.Name := S;
            NameList.Add(S);
          end;
        end;
        if (ImpType in [itList, itListAdd, itIterateParse]) and (ParseFld <> nil) then
        begin
          ParseSQL(Schema, ParseFld.AsString);
          inc(ImpCnt);
        end else
        if (ImpType = itIterate) and (ListCol <> nil) then
        begin
          LastObj := ListCol.Find(AParam);
          if LastObj <> nil then
          begin
            AssignObject(LastObj, ResultSet, ObjPath);
            inc(ImpCnt);
          end;
        end else
        begin
          ObjectFound := LocateObject(Schema, ObjPath, ResultSet, LastObj, LastCol);
          if (not ObjectFound) and (LastCol <> nil) and LastCol.InheritsFrom(TSchemaItemsCollection) then
          begin
            if not ((LastCol is TTableDefinitions) and AnsiSameText(Trim(ResultSet.Fields[0].AsString), 'systable')) then
            begin
              LastObj := LastCol.Add;
              inc(ImpCnt);
              ObjectFound := True;
            end;
          end;
          if ObjectFound then
            AssignObject(LastObj, ResultSet, ObjPath);
        end;
        ResultSet.Next;
      end;
    finally
      FreeAndNil(ResultSet);
    end;
  end;

var
  TempObj: TObject;
begin
  if (Schema = nil) or not Assigned(ExecSQLProc) then exit;
  if not SupportsInfoSchema then
    DatabaseError(SInfoSchemaNotSupported);
  // Reverse Engineer using SQLExecuteStatement using ExecSQLProc
  NameList := nil;
  Schema.BeginUpdate;
  try
    LastCol := nil;
    LastObj := nil;
    NameList := TStringList.Create;
    for I := 0 to FInfoSchemaSQL.Count - 1 do
    begin
      ImpCnt := 0;
      ObjPath := Trim(FInfoSchemaSQL.Names[I]);
      if Pos(';', ObjPath) = 1 then
        Continue;

      // Update relationships in case relations where affected by previous parse operation
      Schema.UpdateRelationships;

      if Pos('*', ObjPath) = 1 then
        ImpType := itList else
      if Pos('+', ObjPath) = 1 then
        ImpType := itListAdd else
      if Pos('-', ObjPath) = 1 then
        ImpType := itIterate else
      if Pos('$', ObjPath) = 1 then
        ImpType := itIterateParse else
        ImpType := itNormal;
      if ImpType <> itNormal then
        Delete(ObjPath, 1, 1);
      SQL := Trim(ValueFromIndex(FInfoSchemaSQL, I));
      if (ObjPath = '') or (SQL = '') then
        Continue;
      // Execute SQL
      if ImpType in [itList, itListAdd, itIterate, itIterateParse] then
      begin
        if TypInfo.GetPropInfo(Schema, ObjPath) <> nil then
          TempObj := TObject(GetOrdProp(Schema, ObjPath)) else
          TempObj := nil;
        if (TempObj <> nil) and (TempObj is TSchemaItemsCollection) then
          ListCol := TSchemaItemsCollection(TempObj) else
          ListCol := nil;
      end;
      if Assigned(OnReverse) then
        OnReverse(1, 'Importing '+GetHRName(ObjPath) + '...');
      if ImpType in [itIterate, itIterateParse] then
      begin
        LastCol := ListCol;
        {if NameList.Count = 0 then
          DoExecSQL(SQL, '') else}
          for C := 0 to NameList.Count-1 do
            DoExecSQL(SQL, NameList[C]);
      end else
      begin
        NameList.Clear;
        DoExecSQL(SQL, '');
      end;
      if Assigned(OnReverse) then
        OnReverse(2, Format('Importing %s complete. %d object(s) imported.', [GetHRName(ObjPath), ImpCnt]));
    end;
  finally
    Schema.EndUpdate;
    Schema.UpdateRelationshipCardinality;
    NameList.Free;
  end;
end;

function TDBEngineProfile.HasInvalidSQLFieldTypes(Schema: TDatabaseSchema): Boolean;
var
  I, J: Integer;
begin
  Result := True;
  for I := 0 to Schema.TableDefs.Count - 1 do
    for J := 0 to Schema.TableDefs[I].FieldDefs.Count - 1 do
      with Schema.TableDefs[I].FieldDefs[J] do
        if (SQLFieldType <> '') and not IsValidFieldType(SQLFieldType) then
          exit;
  for I := 0 to Schema.Domains.Count - 1 do
    with Schema.Domains[I] do
      if (SQLFieldType <> '') and not IsValidFieldType(SQLFieldType) then
        exit;
  Result := False;
end;

procedure TDBEngineProfile.ConvertSQLFieldTypes(Schema: TDatabaseSchema);
var
  I, J: Integer;
  Temp: String;
begin
  for I := 0 to Schema.TableDefs.Count - 1 do
    for J := 0 to Schema.TableDefs[I].FieldDefs.Count - 1 do
      with Schema.TableDefs[I].FieldDefs[J] do
        if (SQLFieldType <> '') and not IsValidFieldType(SQLFieldType) then
        begin
          Temp := FSynonyms.Values[SQLFieldType];
          if Temp <> '' then
            SQLFieldType := Temp
          else
          begin
            Temp := GetEngineFieldType(GetPropValue('DataType'));
            if Temp <> '' then
              SQLFieldType := Temp;
          end;
        end;
  for I := 0 to Schema.Domains.Count - 1 do
    with Schema.Domains[I] do
      if (SQLFieldType <> '') and not IsValidFieldType(SQLFieldType) then
      begin
        Temp := FSynonyms.Values[SQLFieldType];
        if Temp <> '' then
          SQLFieldType := Temp
        else
        begin
          Temp := GetEngineFieldType(GetPropValue('DataType'));
          if Temp <> '' then
            SQLFieldType := Temp;
        end;
      end;
end;

function DeleteBrackets(const Str: String): String;
begin
  Result := Trim(Str);
  if Length(Result) < 2 then exit;
  if (Result[1] = '(') and (Result[Length(Result)] = ')') then
    Result := copy(Result, 2, Length(Result) - 2);
end;

function NormalizeCRLF(const Str: String): String;
var
  I: Integer;
begin
  Result := Str;
  I := 1;
  while I <= Length(Result) do
  begin
    if Result[I] = #13 then
    begin
      if I > Length(Result) then
        Result := Result + #10
      else if Result[I+1] <> #10 then
        Insert(#10, Result, I+1);
      Inc(I, 2);
    end else if Result[I] = #10 then
    begin
      if (I < 2) or (Result[I-1] <> #13) then
        Insert(#13, Result, I);
      Inc(I, 2);
    end else begin
      if Result[I] < #32 then
        Result[I] := #32;
      Inc(I);
    end;
  end;
end;

function TDBEngineProfile.FormatIBDataType(const FieldType, SubType,
  Scale, Precision, Size, SegLen, CharSet: String): String;
var
  Temp: String;
begin
  Temp := FSynonyms.Values[FieldType];
  if AnsiSameText(Temp, 'BLOB') then
  begin
    if SubType <> '' then
      Temp := Temp + ' SUB_TYPE ' + SubType;
    if SegLen <> '' then
      Temp := Temp + ' SEGMENT SIZE ' + SegLen;
    if (CharSet <> '') and not AnsiSameText(CharSet, 'NONE') then
      Temp := Temp + ' CHARACTER SET ' + CharSet;
  end else
  begin
    if Size <> '' then
      Temp := Temp + '(' + Size + ')'
    else if ((Precision <> '') and (Precision <> '0')) or ((Scale <> '') and (Scale <> '0')) then
    begin
      Temp := Temp + '(' + Precision;
      if (Scale <> '') and (Scale <> '0') then
        Temp := Temp + ', ' + Scale;
      Temp := Temp + ')';
    end;
  end;
  Result := Temp;  
end;

procedure TDBEngineProfile.ParseSQL(ASchema: TDatabaseSchema; const ASQL: string);
begin
  dbSQLParser.ParseSQL(ASQL, ASchema, Self);
end;

procedure TDBEngineProfile.AssignObject(Obj: TSchemaCollectionItem;
  ResultSet: TDataSet; const ObjPath: String);
var
  I: Integer;
  FldName, V, V2: String;
begin
  for I := 0 to ResultSet.FieldCount - 1 do
  with ResultSet.Fields[I] do
  begin
    FldName := FieldName;
    if Pos('_', FldName) <> 1 then
    begin
      V := AsString;
      if not (AnsiSameText('AddDefinition', FldName)
        or AnsiSameText('Definition', FldName)
        or AnsiSameText('AddDefinitionInLine', FldName)) then
      begin
        V := Trim(V);
        if V = '' then continue;
      end;
      // Handle field type synonyms right here
      if AnsiSameText(FldName, 'SQLFieldType') and not IsValidFieldType(V) then
      begin
        V2 := FSynonyms.Values[V];
        if V2 <> '' then
          V := V2;
      end else if AnsiSameText(FldName, 'IBDefaultExpression') then
      begin
        if AnsiSameText(copy(V, 1, 7), 'DEFAULT') then
          Delete(V, 1, 7);
        V := DeleteBrackets(V);
        FldName := 'DefaultExpression';
      end else if AnsiSameText(FldName, 'IBCheck') then
      begin
        if AnsiSameText(copy(V, 1, 5), 'CHECK') then
          Delete(V, 1, 5);
        V := DeleteBrackets(V);
        FldName := 'Check';
      end else if AnsiSameText(FldName, 'IBFieldType') then
      begin
        V := FormatIBDataType(V,
          ResultSet.FieldByName('_IBSubType').AsString,
          ResultSet.FieldByName('_IBScale').AsString,
          ResultSet.FieldByName('_IBPrecision').AsString,
          ResultSet.FieldByName('_IBSize').AsString,
          ResultSet.FieldByName('_IBSegLen').AsString,
          ResultSet.FieldByName('_IBCharSet').AsString);
        FldName := 'SQLFieldType';
      end else if AnsiSameText(FldName, 'IBParamType') then
      begin
        V := FormatIBDataType(V,
          ResultSet.FieldByName('_IBSubType').AsString,
          ResultSet.FieldByName('_IBParamScale').AsString,
          ResultSet.FieldByName('_IBParamPrecision').AsString,
          ResultSet.FieldByName('_IBParamSize').AsString,
          ResultSet.FieldByName('_IBParamSegLen').AsString,
          ResultSet.FieldByName('_IBParamCharSet').AsString);
        FldName := 'AddDefinitionInLine';
      end else begin
        V2 := InfoSchemaValueMap.Values[ObjPath + '.' + FldName + '.' + V];
        if V2 <> '' then
          V := V2;
      end;
      V := NormalizeCRLF(V);
      try
        Obj.SetPropValue(FldName, V);
      except
        on E: Exception do
        begin
          E.Message := Format('"%s" is invalid value for property "%s" of object %s', [V, FldName, Obj.GetSchemaClassName]);
          Application.HandleException(E);
        end;
      end;
    end;
  end;
end;

function TDBEngineProfile.LocateObject(Schema: TDatabaseSchema; const ObjPath: String;
  ResultSet: TDataSet; var Obj: TSchemaCollectionItem; var Col: TSchemaItemsCollection): Boolean;
var
  CurKeyFld, P: Integer;
  CurObj: TObject;
  S: String;
begin
  Result := False;
  P := 1;
  CurKeyFld := 0;
  CurObj := Schema;
  Obj := nil;
  Col := nil;
  while True do
  begin
    S := NextToken(ObjPath, '.', P);
    if S = '' then
    begin
      Obj := TSchemaCollectionItem(CurObj);
      Result := Obj <> nil;
      exit;
    end;
    if CurObj = nil then
    begin
      Obj := nil;
      Col := nil;
      exit;
    end;
    CurObj := TObject(GetOrdProp(CurObj, S));
    if (CurObj = nil) or not CurObj.InheritsFrom(TSchemaItemsCollection) then exit;
    if CurKeyFld >= ResultSet.FieldCount then exit;

    Col := TSchemaItemsCollection(CurObj);
    if ResultSet.Fields[CurKeyFld].DataType in [db.ftInteger, db.ftAutoInc, db.ftSmallInt, db.ftWord] then
    begin
      if ResultSet.Fields[CurKeyFld].AsInteger < Col.Count then
        CurObj := Col.Items[ResultSet.Fields[CurKeyFld].AsInteger]
      else CurObj := nil;
    end else CurObj := Col.Find(Trim(ResultSet.Fields[CurKeyFld].AsString));
    Inc(CurKeyFld);
  end;
end;



initialization
  DBEngineProfiles := TList.Create;
finalization
  DBEngineProfiles.Free;
end.


