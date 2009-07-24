(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Contains: ParseSQL procedures implementing generic SQL parsing for
(*            use with TDBEngineProfile component.
(*
(*  Copyright (c) 2005-2009, Context Software LLC
(*
(*  ------------------------------------------------------------
(*  FILE        : dbSQLParser.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.03
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009
(*
(******************************************************************************)
unit dbSQLParser;

{$I CtxVer.inc}

interface

uses Classes, SysUtils, dbSQLLexer, DB, dbSchema, dbEngProfile, Forms;

  (*
    Trigger:
      CREATE TRIGGER <name> [AFTER INSERT] ON <tablename> <definition>;

    TODO:

    DONE:
    . Foreign key defined in field clause Field <type> REFERENCE Table(Field)
    . Parsing fully qualified Index names in case there's table name also (IndexName field)
    . Parsing constraint name (import of drop constraint clause)
    . Parsing [FIELDTYPE] in square brackets or quotes
    . Triggers, view, stored procs
    . Index names (prefixing names will not work for parsing)
    . Test alter table: ADD [UNIQUE] [NOCASE] PRIMARY KEY parsing problem! chain items with same first text token.
    . TRelation: Add Key/Foreign Key Field
    . Single quoted values
    . Comments /*, ##, --
    . select, insert, delete
    . Skipping unknown statements without parsing
    . Data structure
    . check
    . Set term statement
  *)
type
  TParseStatusType = (pstProgress, pstError, pstMessage);

  TParseStatusEvent = procedure (Schema: TDatabaseSchema;
    StatusType: TParseStatusType; const Msg: String; Value: Integer) of object;

  procedure ParseSQL(const SQL: String; Schema: TDatabaseSchema;
    Profile: TDBEngineProfile = nil; StatusEvent: TParseStatusEvent = nil); overload;
  procedure ParseSQL(AStream: TStream; Schema: TDatabaseSchema;
    Profile: TDBEngineProfile = nil; StatusEvent: TParseStatusEvent = nil); overload;
  procedure ParseSQLFile(const FileName: String; Schema: TDatabaseSchema;
    Profile: TDBEngineProfile = nil; StatusEvent: TParseStatusEvent = nil);

  procedure UpdateSchemaItem(Item: TSchemaCollectionItem; const Definition: String);

resourcestring
  STableNotFound = 'Table not found: %s near line %d in pos %d';
  SUnexpectedToken = 'Unexpected token at line %d in pos %d';
  SUnexpectedEndOfFile = 'Unexpected end of file';
  SSyntaxError = 'Syntax error near "%s" at line %d in pos %d';
  SInvalidIndexFieldDefinition = 'Invalid index field definition near line %d in pos %d';
  SInvalidExpressionInGrammar = 'Invalid expression in grammar %s. Assignment <%s> is coded prior to the final statement recognition. Occured in script near line %d, pos %d';
  SErrorParsingDefinition = 'Error parsing object declaration';
  SDoneParsing = 'Parsing complete (%d lines parsed).';
  SBeginParsingFile = 'Begin parsing file "%s"';

implementation

{$I CtxD2009.inc}

const
  // This array contains names of the top most entities that may be
  // encountered in SQL script
  // ObjNames: array [0..7] of String = ('table', 'index', 'sequence', 'domain',
  // 'object', 'view', 'trigger', 'storedproc');
  DefaultImportObjects = 'table,index,sequence,domain,view,trigger,storedproc,module';

type
  TClassStatement = (csUndefined, csField, csComputedField, csPrimaryKey, csForeignKey, csUnique, csIndex, csTrigger,
    csTable, csView, csStoredProc, csModule, csSequence, csDomain, csIndexField, csTableConstraint, csConstraints,
    csCustomObject);

const
  StatementNames: array [TClassStatement] of String = ('',
    'field', 'computedfield', 'primarykey', 'foreignkey', 'unique', 'index', 'trigger',
    'table', 'view', 'storedproc', 'module', 'sequence', 'domain', 'indexfield', 'constraint', 'constraints',
    'custom object');

  ItemClasses: array [TClassStatement] of TSchemaCollectionItemClass = (nil,
    TFieldDefinition, TFieldDefinition, TIndexDefinition, TRelation, TIndexDefinition,
    TIndexDefinition, TTriggerDefinition,  TTableDefinition, TViewDefinition,
    TStoredProcDefinition, TModuleDefinition, TSequence, TDomain, TIndexField, TTableConstraint, nil,
    TCustomObject
  );

function ExtractTableName(const Definition: String): String;
var
  I: Integer;
  Temp: String;
begin
  // CREATE TRIGGER name SKIP_TOKENS ON table_name
  I := 1;
  Temp := StringReplace(Definition, #13#10, ' ', [rfReplaceAll]);
  while I <= Length(Temp) do
  begin
    Result := NextToken(Temp, ' ', I);
    if AnsiSameText(Result, 'on') or AnsiSameText(Result, 'for') then
    begin
      Result := NextToken(Temp, ' ', I);
      exit;
    end;
  end;
  Result := '';
end;

procedure UpdateSchemaItem(Item: TSchemaCollectionItem; const Definition: String);
var
  TempSchema: TDatabaseSchema;
  Col: TSchemaItemsCollection;
  Profile: TDBEngineProfile;
  SaveTerm: String;
begin
  TempSchema := TDatabaseSchema.Create(nil);
  try
    TempSchema.TargetDB := Item.Schema.TargetDB;
    if Item.InheritsFrom(TTableCollectionItem) then
    begin
      TempSchema.TableDefs.Add.Name := TTableCollectionItem(Item).TableName;
      Col := GetItemCollection(TempSchema.TableDefs[0], TSchemaCollectionItemClass(Item.ClassType));
    end else Col := GetItemCollection(TempSchema, TSchemaCollectionItemClass(Item.ClassType));

    Profile := GetDBEngineProfile(TempSchema.TargetDB);
    try
      SaveTerm := Profile.DefaultTerm;
      Profile.DefaultTerm := '$$$$$$$$$$$$$$$$$$'; // Make sure to specify impossible term
      ParseSQL(Definition, TempSchema, Profile);
    except
    end;
    Profile.DefaultTerm := SaveTerm;

    if Col.Count < 1 then // Col[0].Name = ''
      raise Exception.Create(SErrorParsingDefinition)
    else begin
      Item.Name := Col[0].Name;
      Item.SetPropValue('Definition', Definition);
      if Item is TStoredProcDefinition then
        TStoredProcDefinition(Item).IsFunction := TStoredProcDefinition(Col[0]).IsFunction;
    end;
  finally
    TempSchema.Free;
  end;
end;

procedure ParseSQL(AStream: TStream; Schema: TDatabaseSchema; Profile: TDBEngineProfile = nil; StatusEvent: TParseStatusEvent = nil);
var
  EqPos, I, TokenPos: Integer;
  Lexer: TSQLLexer;
  Token: String;
  KeyToken: Boolean;
  LastDelimiter: String;
  LastTableDef: TTableDefinition;
  LastIndexDef: TIndexDefinition;
  ValueBuffer: TStringList;
  ExprList: TList;
  DataTypes: TList;
  // Term,
  NextTerm: String;
  ExpressionFields: TStringList;
  StatementFields: TStringList;
  ImportExpr: TFmtExpression;
  DummyItem: TSchemaCollectionItem;
  // StatementStartPos: Integer;
  ExprStack: TList;
  SkippedExpression: String;

  function BeginWithText(AExpression: TFmtExpression; var Idx: Integer): String; forward;

  function ExprDisplayName(AExpr: TFmtExpression): String;
  var
    Idx: Integer;
  begin
    Result := AExpr.Name;
    if Result = '' then
    begin
      Idx := 0;
      Result := BeginWithText(AExpr, Idx);
    end;
  end;

  procedure ParseError(const Msg: string);
  var
    I: Integer;
    Pad: String;
    Tmp: String;
  begin
    Tmp := '';
    (* DEBUG ONLY *)
    Pad := '';
    for I := ExprStack.Count - 1 downto 0 do
    begin
      Tmp := Tmp + #13#10 + Pad + ExprDisplayName(TFmtExpression(ExprStack[I]));
      Pad := Pad + '  ';
    end;
    raise Exception.Create(Msg + Tmp);
  end;

  procedure ParseErrorFmt(const Msg: string; const Args: array of const);
  begin
    ParseError(Format(Msg, Args));
  end;

  procedure GetItemOperation(const ExprName: String;
    var ItemClassIdx: TClassStatement; var ItemOperation: TItemOperation);
  var
    I: TClassStatement;
    P: Integer;
    StmtName, OpName: String;
  begin
    if ExprName = '' then exit;
    P := 1;
    StmtName := NextToken(ExprName, '.', P);
    OpName := ExtractObjectName(ExprName);
    for I := Low(StatementNames) to High(StatementNames) do
      if AnsiSameText(StatementNames[I], StmtName) then
      begin
        if AnsiSameText(OpName, 'alter') then
        begin
          ItemClassIdx := I;
          ItemOperation := ioAlter;
        end else if AnsiSameText(OpName, 'drop') then
        begin
          ItemClassIdx := I;
          ItemOperation := ioDrop;
        end else if AnsiSameText(OpName, 'add') or AnsiSameText(OpName, 'create') then
        begin
          ItemClassIdx := I;
          ItemOperation := ioCreate;
        end else if AnsiSameText(OpName, 'define') then
          ItemClassIdx := I;
        exit;
      end;
    // Assume custom object
    if Profile.CustomObjectTypes.IndexOf(StmtName) >= 0 then
    begin
      if AnsiSameText(OpName, 'alter') then
      begin
        ItemClassIdx := csCustomObject;
        ItemOperation := ioAlter;
      end else if AnsiSameText(OpName, 'drop') then
      begin
        ItemClassIdx := csCustomObject;
        ItemOperation := ioDrop;
      end else if AnsiSameText(OpName, 'add') or AnsiSameText(OpName, 'create') then
      begin
        ItemClassIdx := csCustomObject;
        ItemOperation := ioCreate;
      end else if AnsiSameText(OpName, 'define') then
        ItemClassIdx := csCustomObject;
      exit;
    end;
  end;

  procedure SetItemProp(Item: TSchemaCollectionItem; PropName: String; const PropValue: String);
  begin
    while (PropName <> '') and CharInSet(PropName[1], ['~', '!', '?', '/', '^']) do
      Delete(PropName, 1, 1);
    if PropName <> '' then
      Item.SetPropValue(PropName, PropValue);
  end;

  procedure AddValue(var Item: TSchemaCollectionItem; PropName: String;
    const PropValue: String; ItemOperation: TItemOperation; ItemClassIdx: TClassStatement);
  var
    I: Integer;
    Parent: TObject;
    Coll: TSchemaItemsCollection;
    IndexDef: TIndexDefinition;
    TempStr, AdjPropValue, ParentName, ItemName: String;
    ItemClass: TSchemaCollectionItemClass;
    Dequote: Boolean;
  begin
    // We remove all possible prefixes except ^, which means "old value".
    Dequote := False;
    while (PropName <> '') and CharInSet(PropName[1], ['~', '!', '?', '/', '^']) do
    begin
      if PropName[1] = '/' then
        Dequote := True;
      Delete(PropName, 1, 1);
    end;
    if PropName = '' then exit;

    if (Item <> nil) and (ItemClassIdx <> csUndefined)
      and not Item.InheritsFrom(ItemClasses[ItemClassIdx])
    then
      Item := nil;

    if Dequote then
    begin
      AdjPropValue := Trim(PropValue);
      if AdjPropValue <> '' then
        AdjPropValue := AnsiDequotedStr(AdjPropValue, AdjPropValue[1]);
    end else if Profile.IsIdentProp(PropName) then
      AdjPropValue := ExtractObjectName(PropValue)
    else AdjPropValue := Trim(PropValue);

    if Item = nil then
    begin
      if StatementFields.IndexOf(PropName) < 0 then
        ValueBuffer.Add(PropName + '='+ StringReplace(AdjPropValue, #13#10, ' ', [rfReplaceAll]));

      ItemClass := ItemClasses[ItemClassIdx];
      if ItemClass = nil then
        exit;
      ItemName := '';
      ParentName := '';
      case ItemClassIdx of
        csUndefined:
          Parent := nil;
        csField, csComputedField, csPrimaryKey, csForeignKey, csUnique, csTableConstraint:
          Parent := LastTableDef;
        csIndex: begin
          ParentName := ValueBuffer.Values['tablename'];
          if ParentName = '' then
          begin
            // Extract IndexName, parse into schema, table & indexname +++
            if Profile.IndexNamesUnique then
            begin
              ItemName := ExtractObjectName(Trim(ValueBuffer.Values['indexname']));
              // Locate table in schema by unique index name dbSchema
              IndexDef := Schema.FindIndexGlobal(ItemName);
              if IndexDef <> nil then
                ParentName := IndexDef.TableName
            end else begin
              // [schema].[table].[index]
              ItemName := ExtractObjectName(Trim(ValueBuffer.Values['indexname']), ParentName);
              ParentName := ExtractObjectName(ParentName);
            end;
          end;
          if ParentName = '' then
            Parent := LastTableDef
          else Parent := Schema.TableDefs.Find(ParentName);
          (*
          if Parent = nil then
            ParseErrorFmt(STableNotFound, [ParentName, Lexer.LineNo, Lexer.LinePos]);
          *)
        end;
        csIndexField: begin
          Parent := LastIndexDef;
          if Parent = nil then
            ParseErrorFmt(SInvalidIndexFieldDefinition, [Lexer.LineNo, Lexer.LinePos]);
        end;
        csTrigger: begin
          Parent := nil;
          if AnsiSameText(PropName, 'definition') then
          begin
            ParentName := ExtractObjectName(ExtractTableName(PropValue));
            Parent := Schema.TableDefs.Find(ParentName);
            if Parent = nil then
              ParseErrorFmt(STableNotFound, [ParentName, Lexer.LineNo, Lexer.LinePos]);
          end;
        end;
        else Parent := Schema;
      end;
      if Parent = nil then exit;
      Coll := GetItemCollection(Parent, ItemClass);
      if Coll = nil then exit;

      if ItemName = '' then
        ItemName := Trim(ValueBuffer.Values['name']);
      try
        if ItemOperation in [ioAlter, ioDrop] then
        begin
          // Locate item
          if ItemName <> '' then
            Item := Coll.Find(ItemName);
          if (Item = nil) and Parent.InheritsFrom(TTableDefinition) and
            (ItemClassIdx in [csPrimaryKey, csForeignKey, csUnique, csTableConstraint, csConstraints])
          then
            Item := TTableDefinition(Parent).FindConstraint(ItemName);

          // Drop indexfields before processing alter for index collection item
          if (Item <> nil) and (ItemOperation = ioAlter) and Item.InheritsFrom(TIndexDefinition) then
            TIndexDefinition(Item).IndexFields.Clear;

        end else begin
          Item := Coll.Add;
          // Initialization of objects by type
          case ItemClassIdx of
            csPrimaryKey:
              with TIndexDefinition(Item) do
                Options := Options + [ixPrimary, ixUnique];
            csForeignKey:
              with TRelation(Item) do
              begin
                RequireOneRecord := True;
                RelationType := rtManyToOne;
                UpdateAction := raError;
                DeleteAction := raError;
                TempStr := ExtractObjectName(Trim(ValueBuffer.Values['name']));
                if TempStr <> '' then
                  RelationshipName := Relationship.GetAutoName(TempStr);
              end;
            csUnique:
              with TIndexDefinition(Item) do
              begin
                Options := Options + [ixUnique];
                if Name = '' then
                  Name := GetAutoName;
              end;
            csTrigger:
              with TTriggerDefinition(Item) do
              begin
                Definition := PropValue;
                // ClientSide := False;
              end;
            csCustomObject:
              TCustomObject(Item).SchemaClassName := Trim(ValueBuffer.Values['schemaclassname']);
          end;
        end;
        // Assign properties from ValueBuffer
        if Item <> nil then
        begin
          if ItemOperation = ioDrop then
            FreeAndNil(Item)
          else begin
            for I := 0 to ValueBuffer.Count - 1 do
              SetItemProp(Item, ValueBuffer.Names[I], ValueFromIndex(ValueBuffer, I));
          end;
          ValueBuffer.Clear;
        end;
      except
        if (Item <> nil) and (ItemOperation = ioCreate) then
          FreeAndNil(Item);
        raise;
      end;
    end else
      SetItemProp(Item, PropName, AdjPropValue);
    // Setup table & index context
    if (Item <> nil) then
    begin
      if Item.InheritsFrom(TTableDefinition) then
        LastTableDef := TTableDefinition(Item)
      else if Item.InheritsFrom(TIndexDefinition) then
        LastIndexDef := TIndexDefinition(Item);
    end;
  end;

  function IsKeyword(const Str: String): Boolean;
  begin
    Result := Profile.Keywords.IndexOf(Str) >= 0;
  end;

  procedure GetNextToken;

    procedure ReadValueTokens;
    begin
      repeat
        case Lexer.TokenID of
          tokenEOLN:;
          Ord('+'): Token := Token + '+';
          tokenLiteral: Token := Token + Lexer.Token;
          else break;
        end;
        Lexer.GetNextToken;
      until false;
    end;

  var
    TempStr: String;

  begin
    // We process two types of tokens: KeyTokens, Values (including quoted).
    while (Lexer.TokenID = tokenEOLN) or (Lexer.TokenID = tokenComment) do
    begin
      if Lexer.TokenID = tokenComment then
      begin
        TempStr := Trim(Lexer.Token);
        if AnsiPos('GO', TempStr) = 1 then
        begin
          KeyToken := True;
          Token := Lexer.Term; // Term
          Lexer.GetNextToken;
          exit;
        end else if copy(TempStr, 1, 2) = '##' then
          Lexer.ParsePragmaComments(copy(TempStr, 3, MaxInt));
      end;
      Lexer.GetNextToken;
    end;
    TokenPos := Lexer.TokenBeginPos;
    case Lexer.TokenID of
      tokenTerm: begin
        Token := Lexer.Term; // Term
        KeyToken := True;
        Lexer.GetNextToken;
      end;
      tokenToken: begin // value
        Token := Lexer.Token;
        KeyToken := False;
        Lexer.GetNextToken;
        if not KeyToken then
        begin
          KeyToken := IsKeyword(Token);
          if KeyToken then
          begin
            while (Lexer.TokenID = tokenToken) and IsKeyword(Lexer.Token) do
            begin
              Token := Token + ' ' + Lexer.Token;
              Lexer.GetNextToken;
            end;
          end;
          // else ReadValueTokens; 2.13 - 12.26.06 Reading Name 'Value' as one token
          Token := Trim(Token);
        end;
      end;
      tokenIdentifier: begin // "value" or [value]
        Token := Lexer.Token;
        Lexer.GetNextToken;
        KeyToken := False;
      end;
      tokenLiteral: begin // 'value'
        Token := '';
        ReadValueTokens;
        KeyToken := False;
      end;
      tokenEOF: begin
        Token := #0;
        KeyToken := True;
      end;
      33..128: begin
        Token := Lexer.Token;
        Lexer.GetNextToken;
        KeyToken := True;
      end;
      else
        ParseErrorFmt(SUnexpectedToken, [Lexer.LineNo, Lexer.LinePos]);
    end;
  end;

  function ExtractExpression: Boolean;
  var
    BracketCounter: Integer;
    StartPos: Integer;
  begin
    // Extracts subexpression within '(' ')' and saves to Token
    Result := not (KeyToken and (Token = ')'));
    if not Result then exit;
    StartPos := TokenPos;
    BracketCounter := 1;
    if Token = '(' then
      Inc(BracketCounter);
    repeat
      TokenPos := Lexer.TokenBeginPos;
      case Lexer.TokenID of
        Ord('('): Inc(BracketCounter);
        Ord(')'): begin
          Dec(BracketCounter);
          if BracketCounter = 0 then break;
        end;
        tokenEOF: ParseError(SUnexpectedEndOfFile);
      end;
      Lexer.GetNextToken;
    until False;
    Lexer.ExtractBlock(StartPos, TokenPos, Token);
    Token := StringReplace(Token, #13#10, ' ', [rfReplaceAll]);
    KeyToken := False;
  end;

  function ExtractStatement(Item: TSchemaCollectionItem; CountBeginEnd: Boolean = False): Boolean;
  var
    StatementName, SaveTerm: String;
    StartPos: Integer;
    BeginEndCounter: Integer;
    FoundStatementName: Boolean;
  begin
    Result := not (KeyToken and AnsiSameText(Token, Lexer.Term)); // Term
    if not Result then exit;
    StartPos := TokenPos;
    BeginEndCounter := 0;
    SaveTerm := Lexer.Term; // Term
    try
      if Item <> nil then
        StatementName := Item.Name
      else StatementName := ExtractObjectName(ValueBuffer.Values['name']);

      if CountBeginEnd and (Lexer.Term = ';') then
        Lexer.Term := '';
      CountBeginEnd := Lexer.Term = '';
      // Lexer.Term := Term;
      while not ((Lexer.Term <> '') and AnsiSameText(Token, Lexer.Term)) do
      begin
        if Lexer.TokenID = tokenEOF then
          if CountBeginEnd and (BeginEndCounter > 0) then
            ParseError(SUnexpectedEndOfFile)
          else break;
        Lexer.GetNextToken;
        Token := Lexer.Token;
        if CountBeginEnd and (Lexer.TokenID = tokenToken) then
        begin
          if AnsiSameText(Token, 'begin') then
            Inc(BeginEndCounter)
          else if AnsiSameText(Token, 'end') then
          begin
            Lexer.GetNextToken;
            // Skip statement name, that might follow last end before ';'
            FoundStatementName := (Lexer.TokenID = tokenToken)
              and (StatementName <> '')
              and AnsiSameText(StatementName, ExtractObjectName(Lexer.Token));
            if FoundStatementName then
              Lexer.GetNextToken;
            // END must follow by EOF or semicolon
            if (Lexer.TokenID = tokenEOF) or AnsiSameText(Lexer.Token, ';') then
            begin
              if FoundStatementName then
                BeginEndCounter := 0
              else Dec(BeginEndCounter);
              if BeginEndCounter <= 0 then
                break
              else Lexer.GetNextToken;
            end;
          end;
        end;
      end;
      TokenPos := Lexer.TokenBeginPos;
    finally
      Lexer.Term := SaveTerm;
    end;
    if AnsiSameText(Lexer.Token, Lexer.Term) then
      Lexer.GetNextToken;
    // +++ StatementStartPos
    Lexer.ExtractBlock(StartPos, TokenPos, Token); 
    KeyToken := False;
  end;

  function TrimValue(const Str: String): String;
  var
    B, E: Integer;
  begin
    B := 1;
    E := Length(Str);
    while (B <= E) and CharInSet(Str[B], [#1..#32,  '"', '''']) do Inc(B);
    while (E >= B) and CharInSet(Str[E], [#1..#32, '"', '''']) do Dec(E);
    if E >= B then
      Result := copy(Str, B, E - B + 1)
    else Result := '';
  end;

  function NextStrToken(const Str: String; var StrPos: Integer): String;
  begin
    Result := '';
    if StrPos > Length(Str) then exit;
    while (StrPos <= Length(Str)) and CharInSet(Str[StrPos], [#1..#32, '"', '''']) do Inc(StrPos);
    if StrPos <= Length(Str) then
    case Str[StrPos] of
      '_', 'a'..'z', 'A'..'Z': begin
        Result := Str[StrPos];
        Inc(StrPos);
        // MB: Comment to the line below - I'm not sure why there was a space ' '?
        while (StrPos <= Length(Str)) and CharInSet(Str[StrPos], [' ', '_', 'a'..'z', 'A'..'Z', '0'..'9']) do
        begin
          Result := Result + Str[StrPos];
          Inc(StrPos);
        end;
        Result := Trim(Result);
      end;
      else begin
        Result := Str[StrPos];
        // if (Result = ';') and (Term <> ';') then Result := Term; +++
        Inc(StrPos);
      end;
    end;
  end;

  procedure UpdateNextTerm;
  begin
    if NextTerm <> '' then
    begin
      Lexer.Term := NextTerm;
      NextTerm := '';
    end;
  end;

  function MatchText(Str: String): Boolean;
  var
    P: Integer;
    TempToken: String;
  begin
    P := 1;
    Result := False;
    while P <= Length(Str) do
    begin
      TempToken := UpperCase(NextStrToken(Str, P));
      if TempToken = '' then break;

      if not KeyToken or (AnsiPos(TempToken, UpperCase(Token)) <> 1) then exit;

      if Length(TempToken) < Length(Token) then
      begin
        if CharInSet(Token[Length(TempToken)+1], ['A'..'Z', 'a'..'z', '_', '0'..'9']) then
          exit;

        Delete(Token, 1, Length(TempToken));
        Token := Trim(Token);
        if Token = '' then
          GetNextToken;
      end else
        GetNextToken;
    end;
    Result := True;
  end;

  function IsOptionalExpression(Expr: TFmtExpression): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    if Expr.Count > 0 then
    case TFmtOperand(Expr[0]).OpType of
      otExpression: begin
        for I := 1 to Expr.Count - 1 do
          if TFmtOperand(Expr[I]).OpType <> otExpression then
          begin
            Result := False;
            break;
          end;
      end;
      otValue: begin
        for I := 0 to Expr.Count - 1 do
          with TFmtOperand(Expr[I]) do
          if (OpType <> otValue) or (Value = '') or (Value[1] <> '&') then
          begin
            Result := False;
            break;
          end;
      end;
      else Result := False;
    end;
  end;

  function BeginWithText(AExpression: TFmtExpression; var Idx: Integer): String;
  var
    I: Integer;
  begin
    Result := '';
    if AExpression <> nil then
    for I := 0 to AExpression.Count - 1 do
    with TFmtOperand(AExpression[I]) do
    begin
      case OpType of
        otText: Result := TrimValue(Value);
        otCase,
        otExpression: Result := BeginWithText(Expression, Idx);
        otValue: if (Value <> '') and (Value[1] = '$') then
          Result := BeginWithText(Profile.GetExpression(copy(Value, 2, MaxInt)), Idx);
      end;
      Idx := I;
      if Result <> '' then exit;
    end;
  end;

  function ProcessExpression(AExpression: TFmtExpression; var Item: TSchemaCollectionItem;
    ItemClassIdx: TClassStatement; ItemOperation: TItemOperation;
    Expressions: TList = nil; StartIdx: Integer = 0): Boolean; forward;

  function ProcessExpressions(List: TList; var Item: TSchemaCollectionItem;
    ItemClassIdx: TClassStatement; ItemOperation: TItemOperation;
    ProcessOnce: Boolean = False): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    List.Pack;
    for I := 0 to List.Count - 1 do
      if ProcessExpression(TFmtExpression(List[I]), Item, ItemClassIdx, ItemOperation, List) then
      begin
        if ProcessOnce then
          List.Delete(I); // Delete from list, so it's not processed again
        exit;
      end;
    Result := False;
  end;

  procedure HandleSyntaxAmbiguity(AExpression: TFmtExpression; Expressions: TList;
    ItemClassIdx: TClassStatement; ItemOperation: TItemOperation;
    var Item: TSchemaCollectionItem; MatchCount: Integer; Res: Boolean);
  var
    Temp: String;
    I, P: Integer;
  begin
    if Res then
    begin
      if (Expressions <> nil) and (MatchCount <= 1) then
      begin
        Temp := BeginWithText(AExpression, P);
        I := Expressions.IndexOf(AExpression) + 1;
        while I < Expressions.Count do
        with TFmtExpression(Expressions[I]) do
        begin
          if AnsiSameText(Temp, BeginWithText(TFmtExpression(Expressions[I]), P))
            and ProcessExpression(TFmtExpression(Expressions[I]), Item,
              ItemClassIdx, ItemOperation, Expressions{nil}, P + 1)
          then exit;
          Inc(I);
        end;
      end;
      ParseErrorFmt(SSyntaxError, [Token, Lexer.LineNo, Lexer.LinePos]);
    end;
  end;

  function ProcessExpression(AExpression: TFmtExpression; var Item: TSchemaCollectionItem;
    ItemClassIdx: TClassStatement; ItemOperation: TItemOperation;
    Expressions: TList = nil; StartIdx: Integer = 0): Boolean;
  var
    MatchCount, I, J, P: Integer;
    List: TList;
    Temp, ObjClasses, ObjClass, Delimiter: String;
    OptionalExpr, NotEmptyToken: Boolean;
    TempExpr, DataTypeExpression: TFmtExpression;
    TempItem: TSchemaCollectionItem;
  begin
    ExprStack.Add(AExpression);
    try

    I := StartIdx;
    Result := False;
    if (AExpression = nil) or (AExpression.Count = 0) then exit;

    GetItemOperation(AExpression.Name, ItemClassIdx, ItemOperation);
    MatchCount := 0;
    while I < AExpression.Count do
    with TFmtOperand(AExpression[I]) do
    begin
      case OpType of
        otText: begin
          Temp := TrimValue(Value);
          if (Temp <> '') and (Temp <> LastDelimiter) then
          begin
            if not MatchText(Value) then
            begin
              HandleSyntaxAmbiguity(AExpression, Expressions,
                ItemClassIdx, ItemOperation, Item, MatchCount, Result);
              exit;
            end;
            Result := True;
            Inc(MatchCount);
            LastDelimiter := '';
          end else if Temp = LastDelimiter then
            LastDelimiter := '';
          Inc(I);
        end;
        otValue: if (Value <> '') and (Value[1] <> '?') and (Value[1] <> '!') then
        begin
          if Value[1] = '$' then
          begin
            // Process subexpression
            TempExpr := Profile.GetExpression(copy(Value, 2, MaxInt));
            if (TempExpr <> nil) and (TempExpr.Count > 0) then
            begin
              // If TempExpr is optional then don't exit if it returns false
              OptionalExpr := IsOptionalExpression(TempExpr);
              if ProcessExpression(TempExpr, Item, ItemClassIdx, ItemOperation) then
              begin
                Result := True;
                Inc(MatchCount, 2);
              end else if not OptionalExpr then
              begin
                HandleSyntaxAmbiguity(AExpression, Expressions,
                  ItemClassIdx, ItemOperation, Item, MatchCount, Result);
                exit;
              end;
            end;
            Inc(I);
          end else if Value[1] = '&' then begin
            // Process optionally delimited list of expressions
            List := TList.Create;
            try
              P := 2;
              ObjClasses := NextToken(Value, '|', P);
              Delimiter := Trim(NextToken(Value, '|', P));
              P := 1;
              while P <= Length(ObjClasses) do
              begin
                ObjClass := Trim(NextToken(ObjClasses, ';', P));
                if ObjClass = '' then continue;
                List.Add(Profile.GetExpression(ObjClass));
              end;
              for P := 0 to List.Count - 1 do
                if ExprList.IndexOf(List[P]) >= 0 then
                  List[P] := nil;
              while ProcessExpressions(List, Item, ItemClassIdx, ItemOperation, True) do
              begin
                // skip delimiter;
                Result := True;
                Inc(MatchCount, 2);
                LastDelimiter := '';
                if Delimiter = '' then continue;
                if KeyToken and (Token = Delimiter) then
                begin
                  LastDelimiter := Delimiter;
                  GetNextToken;
                end else
                  break;
              end;
            finally
              List.Free;
            end;
            Inc(I);
          end else if Value[1] = '*' then begin
            // Process sub expressions
            List := TList.Create;
            try
              P := 2;
              ObjClasses := NextToken(Value, '|', P);
              Delimiter := Trim(NextToken(Value, '|', P));
              P := 1;
              while P <= Length(ObjClasses) do
              begin
                ObjClass := Trim(NextToken(ObjClasses, ';', P));
                if ObjClass = '' then continue;

                if ItemOperation = ioCreate then
                  List.Add(Profile.GetExpression(ObjClass + '.create'))
                else if ItemOperation = ioDrop then
                  List.Add(Profile.GetExpression(ObjClass + '.drop'))
                else begin
                  List.Add(Profile.GetExpression(ObjClass + '.add'));
                  List.Add(Profile.GetExpression(ObjClass + '.alter'));
                  List.Add(Profile.GetExpression(ObjClass + '.drop'));
                end;
              end;
              for P := 0 to List.Count - 1 do
                if ExprList.IndexOf(List[P]) >= 0 then
                  List[P] := nil;

              while True do
              begin
                TempItem := Item;
                if not ProcessExpressions(List, TempItem, ItemClassIdx, ItemOperation) then break;
                // skip delimiter;
                Result := True;
                Inc(MatchCount, 2);
                LastDelimiter := '';
                if Delimiter = '' then continue;
                if KeyToken and (Token = Delimiter) then
                begin
                  LastDelimiter := Delimiter;
                  GetNextToken;
                end else
                  break;
              end;
            finally
              List.Free;
            end;
            Inc(I);
          end else begin
            // Set value
            EqPos := AnsiPos('=', Value);
            if EqPos > 0 then
            begin
              if AnsiPos('!', Value) <= 0 then
              begin
                if not Result then
                  ParseErrorFmt(SInvalidExpressionInGrammar, [AExpression.Name, Value, Lexer.LineNo, Lexer.LinePos]);
                Temp := copy(Value, 1, EqPos - 1);
                AddValue(Item, Temp, copy(Value, EqPos + 1, MaxInt), ItemOperation, ItemClassIdx);
                Inc(MatchCount, 2);
              end;
            end else if AnsiSameText(Value, 'setterm') then
            begin
              if Token <> '' then
                NextTerm := Token;
              GetNextToken;
              Result := True;
              Inc(MatchCount, 2);
            end else if AnsiSameText(Value, 'sqldatatype') then
            begin
              if not KeyToken then
              begin
                Temp := ExtractObjectName(Token);
                // If Temp is a valid data type name OR ALIAS! +++
                DataTypeExpression := Profile.GetExpression('datatype.'+Temp);
                if DataTypeExpression <> nil then
                begin
                  KeyToken := True;
                  Token := Temp;
                end;
              end;
              Result := ProcessExpressions(DataTypes, Item, ItemClassIdx, ItemOperation);
              if Result then
                Inc(MatchCount, 2);
            end else if AnsiSameText(Value, 'sqlfieldtype') then
            begin
              // If it's key token, we shouldn't process it here
              Result := not KeyToken;
              if not Result then exit;
              // Locate data type
              Temp := ExtractObjectName(Token); // Profile.GetSchemaFieldType(ExtractObjectName(Token));
              AddValue(Item, 'sqlfieldtype', Temp, ItemOperation, ItemClassIdx);
              GetNextToken;
              Result := True;
              Inc(MatchCount, 2);
            end else if AnsiSameText(Value, 'tab') or AnsiSameText(Value, 'br')
              or AnsiSameText(Value, '!') or AnsiSameText(Value, 'before_alter')
              or AnsiSameText(Value, 'alter_failed') or AnsiSameText(Value, 'alter_succeed')
              or AnsiSameText(Value, 'after_alter') or AnsiSameText(Value, 'term')
            then
            begin
              // Skip and do nothing
            end else if AnsiSameText(Value, 'dummy') then
            begin
              // Skip dummy value
              if not KeyToken then
              begin
                Result := True;
                GetNextToken;
              end;
            end else if AnsiSameText(Value, 'go') then
            begin
              if KeyToken and AnsiSameText(Lexer.Term, Token) then
              begin
                Result := True;
                UpdateNextTerm;
                GetNextToken;
              end;
            end else
            begin
              NotEmptyToken := True;

              Temp := Value;

              while (Length(Temp) > 0) and CharInSet(Temp[1], ['^', '?', '!', '~', '/']) do
                Delete(Temp, 1, 1);

              if ExpressionFields.IndexOf(Temp) >= 0 then
                NotEmptyToken := ExtractExpression
              else if StatementFields.IndexOf(Temp) >= 0 then
                NotEmptyToken := ExtractStatement(Item, ItemClassIdx in [csStoredProc, csModule]);

              if NotEmptyToken and KeyToken then
              begin
                HandleSyntaxAmbiguity(AExpression, Expressions,
                  ItemClassIdx, ItemOperation, Item, MatchCount, Result);
                exit;
              end;

              if NotEmptyToken then
              begin
                AddValue(Item, Value, Token, ItemOperation, ItemClassIdx);
                GetNextToken;
              end;
              Result := True;
              Inc(MatchCount, 2);
            end;
            Inc(I);
          end;
        end else begin
          Inc(I);
        end;
        otExpression: begin
          if ProcessExpression(Expression, Item, ItemClassIdx, ItemOperation) then
          begin
            Result := True;
            Inc(MatchCount, 2);
          end;
          Inc(I);
        end;
        otCase: begin
          // build expression list
          List := TList.Create;
          try
            for J := 0 to Expression.Count - 1 do
              List.Add(TFmtOperand(Expression[J]).Expression);

            // process until result found
            if ProcessExpressions(List, Item, ItemClassIdx, ItemOperation) then
            begin
              Result := True;
              Inc(MatchCount, 2);
            end else
            begin
              HandleSyntaxAmbiguity(AExpression, Expressions,
                ItemClassIdx, ItemOperation, Item, MatchCount, Result);
              exit;
            end;
          finally
            List.Free;
          end;
          Inc(I);
        end;
      end;
    end;
    // Set new term
    if Result then
      UpdateNextTerm;

    finally
      ExprStack.Delete(ExprStack.Count - 1);
    end;
  end;

  procedure SkipExpression;
  var
    StartPos: Integer;
  begin
    SkippedExpression := Token;
    if not (KeyToken and AnsiSameText(Token, Lexer.Term)) then
    begin
      StartPos := TokenPos; // Lexer.TokenBeginPos;
      while (Lexer.TokenID <> tokenEOF) and not AnsiSameText(Token, Lexer.Term) do
      begin
        Token := Lexer.Token;
        TokenPos := Lexer.TokenBeginPos;
        Lexer.GetNextToken;
      end;
      Lexer.ExtractBlock(StartPos, TokenPos, SkippedExpression);
    end;
    GetNextToken;
  end;

  procedure BuildExprList;
  var
    I: Integer;
    Temp: String;
    ObjNames: TStringList;
  begin
    ObjNames := TStringList.Create;
    with Profile do
    try
      Temp := Trim(EngineProps.Values[ENTRY_IMPORTOBJECTS]);
      if Temp = '' then
        Temp := DefaultImportObjects;
      ObjNames.CommaText := Temp;
      for I := 0 to ObjNames.Count - 1 do
      begin
        ImportExpr := GetExpression(ObjNames[I] + '.import.create');
        if ImportExpr <> nil then
          ExprList.Add(ImportExpr)
        else
          ExprList.Add(GetExpression(ObjNames[I] + '.create'));
        ImportExpr := GetExpression(ObjNames[I] + '.import.alter');
        if ImportExpr <> nil then
          ExprList.Add(ImportExpr)
        else
          ExprList.Add(GetExpression(ObjNames[I] + '.alter'));
        ImportExpr := GetExpression(ObjNames[I] + '.import.drop');
        if ImportExpr <> nil then
          ExprList.Add(ImportExpr)
        else
          ExprList.Add(GetExpression(ObjNames[I] + '.drop'));
      end;
      ExprList.Add(GetExpression('setterm'));
      ExprList.Pack;
    finally
      ObjNames.Free;
    end;
  end;

begin
  if Profile = nil then
    Profile := GetDBEngineProfile(Schema.TargetDB);
  if Profile = nil then exit;

  ExpressionFields := nil;
  StatementFields := nil;

  ExprStack := TList.Create;
  Lexer := TSQLLexer.Create(AStream);
  Schema.BeginUpdate;
  try
    Lexer.Comments := Profile.CommentChars;
    Lexer.LiteralPrefixes := Profile.EngineProps.Values[ENTRY_LITERALPREFIXES];
    //Term := Profile.DefaultTerm;
    Lexer.Term := Profile.DefaultTerm;
    NextTerm := '';
    LastTableDef := nil;
    LastIndexDef := nil;

    Lexer.GetNextToken;
    GetNextToken;

    ValueBuffer := TStringList.Create;
    ExprList := TList.Create;
    DataTypes := TList.Create;
    with Profile do
    try
      if Assigned(StatusEvent) then
        StatusEvent(Schema, pstProgress, '', 0);

      ExpressionFields := TStringList.Create;
      StatementFields := TStringList.Create;
      ExpressionFields.CommaText := Profile.EngineProps.Values[ENTRY_EXPRESSIONFELDS];
      StatementFields.CommaText := Profile.EngineProps.Values[ENTRY_STATEMENTFIELDS];
      BuildExprList;

      for I := 0 to ExpressionCount - 1 do
        if AnsiPos('datatype.', AnsiLowerCase(Expressions[I].Name)) = 1 then
          DataTypes.Add(Expressions[I]);
      DataTypes.Pack;

      while Lexer.TokenID <> tokenEOF do
      begin
        try
          // StatementStartPos := TokenPos; +++
          DummyItem := nil;
          LastTableDef := nil;
          LastIndexDef := nil;
          if not ProcessExpressions(ExprList, DummyItem, csUndefined, ioCreate) then
          begin
            SkipExpression;
            if Assigned(StatusEvent) then
              StatusEvent(Schema, pstMessage, 'Skipped: ' + SkippedExpression, 0);
          end;
          if Assigned(StatusEvent) then
            StatusEvent(Schema, pstProgress, '', (Lexer.Position * 98) div Lexer.Stream.Size);
        except
          on E: Exception do
          begin
            SkipExpression;
            if Assigned(StatusEvent) then
            begin
              StatusEvent(Schema, pstMessage, 'Skipped: ' + SkippedExpression, 0);
              StatusEvent(Schema, pstError, E.Message, 0);
            end;
          end;
        end;
      end;

      // Update names of all indexes
      Schema.ForEachItem(Schema.UpdateItemName, TIndexDefinition, True);
      // Update names of all check constraints
      Schema.ForEachItem(Schema.UpdateItemName, TTableConstraint, True);

      // Update SQL fields where not assigned directly
      UpdateSchemaSQLFieldTypes(Schema);

    finally
      if Assigned(StatusEvent) then
        StatusEvent(Schema, pstProgress, '', 100);
      if Assigned(StatusEvent) then
        StatusEvent(Schema, pstMessage, Format(SDoneParsing, [Lexer.LineNo]), 0);
      FreeAndNil(ExpressionFields);
      FreeAndNil(StatementFields);
      DataTypes.Free;
      ExprList.Free;
      ValueBuffer.Free;
    end;
  finally
    Lexer.Free;
    ExprStack.Free;
    Schema.EndUpdate;
  end;
end;

procedure ParseSQL(const SQL: String; Schema: TDatabaseSchema;
  Profile: TDBEngineProfile = nil; StatusEvent: TParseStatusEvent = nil);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(SQL);
  try
    ParseSQL(Stream, Schema, Profile, StatusEvent);
  finally
    Stream.Free;
  end;
end;

procedure ParseSQLFile(const FileName: String; Schema: TDatabaseSchema;
  Profile: TDBEngineProfile = nil; StatusEvent: TParseStatusEvent = nil);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    if Assigned(StatusEvent) then
      StatusEvent(Schema, pstMessage, Format(SBeginParsingFile, [FileName]), 0);
    ParseSQL(Stream, Schema, Profile, StatusEvent);
  finally
    Stream.Free;
  end;
end;

end.

