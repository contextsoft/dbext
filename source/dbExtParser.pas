(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Generic expression parser
(*
(*  Copyright (c) 2005-2011, Context Software LLC
(*
(*  ------------------------------------------------------------
(*  FILE        : dbExtParser.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.26
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009, 2010, XE
(*
(******************************************************************************)
unit dbExtParser;

{$I CtxVer.inc}

interface

uses
  Classes;

const
  bOmitBlank     = $01;
  bOmitComment   = $02;
  bQuotedString  = $04;
  bQuotedIdent   = $08;
  bUpperedIdent  = $10;

type
  TokenType = (tChar, tSymbol, tKeyword, tIdentifier, tNumber, tString, tBlank,
    tComment, tBegin, tEnd);

  TToken = record
    TType: TokenType;
    Value: variant;
    Id: integer;
    StartPosition: integer;
    EndPosition: integer;
    LineBegin: integer;
  end;

  TLexerBehavior = word;

  TLexer = class
  private
    FText: String;
    FSymbols: TStringList;
    FKeywords: TStringList;
    FBehavior: TLexerBehavior;
    FStringQuotes: String;
    FIdentQuotes: String;
    FIdentQuoteBegin: String;
    FIdentQuoteEnd: String;
    FInlineComment: String;
    FCommentBegin: String;
    FCommentEnd: String;
    FCurrent: TToken;
    FPrevious: TToken;
    FPrevPrev: TToken;
    FNext: TToken;
    FNextAvailable: boolean;
    function GetOmitBlank: boolean;
    function GetOmitComment: boolean;
    function GetQuotedIdent: boolean;
    function GetQuotedString: boolean;
    function GetUpperedIdent: boolean;
    procedure SetOmitBlank(const value: boolean);
    procedure SetOmitComment(const value: boolean);
    procedure SetQuotedIdent(const value: boolean);
    procedure SetQuotedString(const value: boolean);
    procedure SetUpperedIdent(const value: boolean);
    function PeekNextToken: TToken;
    procedure SetText(const Value: String);
    function GetCurrent: TToken;
    function GetPrevious: TToken;
    function GetPrevPrev: TToken;
  public
    constructor Create(atext: String; behavior: TLexerBehavior); overload;
    constructor Create(atext: String; asymbols: TStringList; akeywords: TStringList;
      behavior: TLexerBehavior); overload;
    procedure Reset;
    function MoveNext: TToken;
    function GetNextToken(var from: TToken): TToken;
    property Text: String read FText write SetText;
    property Symbols: TStringList read FSymbols;
    property Keywords: TStringList read FKeywords;
    property OmitBlank: boolean read GetOmitBlank write SetOmitBlank;
    property OmitComment: boolean read GetOmitComment write SetOmitComment;
    property QuotedString: boolean read GetQuotedString write SetQuotedString;
    property QuotedIdent: boolean read GetQuotedIdent write SetQuotedIdent;
    property UpperedIdent: boolean read GetUpperedIdent write SetUpperedIdent;
    property StringQuotes: String read FStringQuotes write FStringQuotes;
    property IdentQuotes: String read FIdentQuotes write FIdentQuotes;
    property IdentQuoteBegin: String read FIdentQuoteBegin write FIdentQuoteBegin;
    property IdentQuoteEnd: String read FIdentQuoteEnd write FIdentQuoteEnd;
    property InlineComment: String read FInlineComment write FInlineComment;
    property CommentBegin: String read FCommentBegin write FCommentBegin;
    property CommentEnd: String read FCommentEnd write FCommentEnd;
    property PrevPrev: TToken read GetPrevPrev;
    property Previous: TToken read GetPrevious;
    property Current: TToken read GetCurrent;
    property Next: TToken read PeekNextToken;
  end;

  TOperationCode = (opNone, opOr, opAnd, opAdd, opMinus, opSub, opMul, opDiv,
    opEqual, opLess, opMore, opLessEqual, opMoreEqual, opNotEqual, opLike,
    opNot, opIdent, opConst, opTrue, opFalse, opNull, opIsNull, opIsNotNull,
    opFunc, opDot, opConcat);

  PExpression = ^TExpression;
  PEvaluator = ^TEvaluator;

  TFunction = function (evaluator: PEvaluator; expression: PExpression): variant;

  TExpression = record
    Token: TToken;
    Func: TFunction;
    Operation: TOperationCode;
    Arg1: PExpression;
    Arg2: PExpression;
    Arg3: PExpression;
    Args: array of PExpression;
  end;

  TExpressionContext = class;
  TContextClass = class of TExpressionContext;
  TExpressionContext = class
  public
    class function GetItem(context: pointer; const name: String): variant; virtual;
    class function GetSubContext(context: pointer; const name: String; out contextClass: TContextClass): pointer; virtual;
  end;

  TEvaluator = class
  private
    FRoot: PExpression;
    FItems: array of TExpression;
    FContext: pointer;
    FExpression: String;
    FItemCount: integer;
    FFunctions: TStringList;
    FContextClass: TContextClass;
    function Like(const st, Mask: String): boolean;
    function ContextPath(expr: PExpression): variant;
    procedure Error(const Msg: String);
  public
    constructor Create(const aexpression: String; acontext: pointer; acontextclass: TContextClass); overload;
    constructor Create(const aexpression: String; acontext: pointer; functions: TStringList; acontextclass: TContextClass); overload;
    function AllocNode: PExpression;
    function Evaluate: variant; overload;
    function Evaluate(expr: PExpression): variant; overload;
    property Context: pointer read FContext;
    property ContextClass: TContextClass read FContextClass write FContextClass;
  end;

  TExpressionParser = class
  private
    FEvaluator: TEvaluator;
    FLexer: TLexer;
    function OrExpr: PExpression;
    function AndExpr: PExpression;
    function Condition: PExpression;
    function Negation: PExpression;
    function Argument: PExpression;
    function AddExpr: PExpression;
    function MulExpr: PExpression;
    function Identifier: PExpression;
  public
    constructor Create(const expression: String; var evaluator: TEvaluator);
    destructor Destroy; override;
    procedure Parse;
    procedure Error(const Msg: String; const Token: TToken);
  end;

  function EvaluateExpression(const Expression: String; Context: Pointer; ContextClass: TContextClass): Variant; overload;
  function EvaluateExpression(const Expression: String): Variant; overload;

var
  CommonSymbols: TStringList = nil;
  CommonKeywords: TStringList = nil;
  CommonFunctions: TStringList = nil;
  DecimalPoint: Char = '.';

resourcestring
  SIllegalExpression = 'Illegal expression';
  SRightBracketExpected = ''')'' expected but ''%s'' found';
  SNullExpected = '''NULL'' expected but ''%s'' found';
  SUnknownChar = 'Unknown char ''%s''';
  SUnexpectedSymbol = 'Unexpected symbol ''%s''';
  SUnexpectedKeyword = 'Unexpected keyword ''%s''';
  SUnexpectedEnd = 'Unexpected end of expression';
  SInternalError = 'Internal error';
  SAtPosition = ' at position %d ';
  SIdentifierExpected = 'Identifier expected';
  SUnknownFunction = 'Unknown function ''%s''';
  SInvalidArgumentType = 'Invalid %d argument type for %s function';
  SUnableToResolveName = 'Unable to resolve identifier: "%s"';

implementation

uses
  SysUtils
{$IFnDEF VER130}
  , Variants
{$ENDIF}
  , Math;

{$I CtxD2009.inc}

const
  lxOR           = 1;
  lxAND          = 2;
  lxPlus         = 3;
  lxMinus        = 4;
  lxMul          = 5;
  lxDiv          = 6;
  lxNot          = 7;
  lxTrue         = 8;
  lxFalse        = 9;
  lxNull         = 10;
  lxLeftBracket  = 11;
  lxIs           = 12;
  lxEqual        = 13;
  lxMore         = 14;
  lxLess         = 15;
  lxMoreEqual    = 16;
  lxLessEqual    = 17;
  lxNotEqual     = 18;
  lxLike         = 19;
  lxRightBracket = 20;
  lxDot          = 21;
  lxComma        = 22;
  lxConcat       = 23;

const
  TokenBegin: TToken = (TType: tBegin; Id: 0; StartPosition: 1; EndPosition: 1;
    LineBegin: 1);

function EvaluateExpression(const Expression: String; Context: Pointer; ContextClass: TContextClass): Variant;
begin
  with TEvaluator.Create(Expression, Context, ContextClass) do
  try
    Result := Evaluate;
  finally
    Free;
  end;
end;

function EvaluateExpression(const Expression: String): Variant;
begin
  Result := EvaluateExpression(Expression, nil, TExpressionContext);
end;

procedure NewToken(var atoken: TToken; aTType: TokenType; aValue: variant;
  aId: integer; aStartPosition: integer; aEndPosition: integer; aLineBegin: integer);
begin
  with atoken do begin
    TType := aTType;
    Value := aValue;
    Id := aId;
    StartPosition := aStartPosition;
    EndPosition := aEndPosition;
    LineBegin := aLineBegin;
  end;
end;

{ TLexer }

constructor TLexer.Create(atext: String; behavior: TLexerBehavior);
begin
  Create(atext, CommonSymbols, CommonKeywords, behavior);
end;

constructor TLexer.Create(atext: String; asymbols, akeywords: TStringList;
  behavior: TLexerBehavior);
begin
  inherited Create;
  FText := atext;
  FSymbols := asymbols;
  FKeywords := akeywords;
  FBehavior := behavior;
  FStringQuotes := '''';
  FIdentQuotes := '"';
  FIdentQuoteBegin := '[';
  FIdentQuoteEnd := ']';
  FInlineComment := '--';
  FCommentBegin := '/*';
  FCommentEnd := '*/';
  Reset;
end;

function TLexer.GetCurrent: TToken;
begin
  result := FCurrent;
end;

function TLexer.GetNextToken(var from: TToken): TToken;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  Numeric = ['0'..'9'];
  AlphaNumeric = Alpha + Numeric;
var
  start: integer;
  index: integer;
  lineBegin: integer;
  omit: boolean;
  tmp: PChar;
  i: integer;
  ch: char;
  lenBegin: integer;
  lenEnd: integer;
  identQuote: String;
  val: String;
  sym: String;
  id: integer;
  tType: TokenType;
  scale: integer;
  v: variant;
  name: String;
begin
  start := from.EndPosition;
  index := start;
  lineBegin := from.LineBegin;

  repeat
    omit := false;
    // end
    if (start = Length(text) + 1) then begin
      NewToken(result, tEnd, '', 0, start, start, lineBegin);
      exit;
    end;

    // separator
    while (index < Length(text) + 1) and (text[index] <= ' ') do begin
      if (text[index] = #13) or (text[index] = #10) then
        lineBegin := index + 1;
      Inc(index);
    end;

    if (index > start) then begin
      omit := true;
      if not OmitBlank then begin
        NewToken(result, tBlank, copy(text, start, index - start), 0, start, index,
          lineBegin);
        exit;
      end else
        start := index;
    end;

    // inline comment
    if (StrLComp(PChar(text) + index - 1, PChar(InlineComment), Length(InlineComment)) = 0)
    then begin
      omit := true;
      tmp := StrScan(PChar(text) + index - 1, #13);
      if (tmp = nil) then
        index := Length(text) + 1
      else begin
        index := tmp - PChar(text) + 2;
        if (text[index] = #10) then
          lineBegin := index + 1;
      end;
      if (index < Length(text) + 1) and (text[index] = #10) then
        Inc(index);
      if not OmitComment then begin
        NewToken(result, tComment, copy(text, start, index - start), 0, start, index,
          lineBegin);
        exit;
      end else
        start := index;
    end;

    // comment
    if (StrLComp(PChar(text) + index - 1, PChar(CommentBegin), Length(CommentBegin)) = 0)
    then begin
      omit := true;
      tmp := StrPos(PChar(text) + index - 1, PChar(CommentEnd));
      if (tmp = nil) then
        index := Length(text) + 1
      else
        index := tmp - PChar(text) + Length(CommentEnd) + 1;
      if not OmitComment then begin
        NewToken(result, tComment, copy(text, start, index - start), 0, start, index,
          lineBegin);
        exit;
      end else
        start := index;
    end;
  until not omit;

  // String
  for i := 1 to Length(StringQuotes) do begin
    ch := StringQuotes[i];
    if (text[index] = ch) then begin
      tmp := StrScan(PChar(text) + index, ch);
      lenEnd := 1;
      if (tmp = nil) then begin
        index := Length(text) + 1;
        lenEnd := 0;
      end else
        index := tmp - PChar(text) + lenEnd + 1;
      if not QuotedString then
        Inc(Start)
      else
        lenEnd := 0;
      NewToken(result, tString, copy(text, start, index - start - lenEnd), 0,
        start, index, lineBegin);
      exit;
    end;
  end;

  // quoted identifier
  identQuote := '';
  for i := 1 to Length(IdentQuotes) do begin
    ch := StringQuotes[i];
    if (text[index] = ch) then begin
      identQuote := ch;
      break;
    end;
  end;
  if (identQuote <> '') or
     (StrLComp(PChar(text) + index - 1, PChar(IdentQuoteBegin), Length(IdentQuoteBegin)) = 0)
  then begin
    if (identQuote <> '') then begin
      lenBegin := 1;
    end
    else begin
      identQuote := identQuoteEnd;
      lenBegin := Length(IdentQuoteBegin);
    end;
    tmp := StrPos(PChar(text) + index, PChar(identQuote));
    lenEnd := Length(identQuote);
    if (index < 0) then begin
      index := Length(text) + 1;
      lenEnd := 0;
    end else
      index := tmp - PChar(text) + lenEnd + 1;
    val := copy(text, start + lenBegin, index - start - lenBegin - lenEnd);
    if QuotedIdent then
      Inc(start, lenBegin)
    else
      val := copy(text, start, index - start);
    NewToken(result, tIdentifier, val, 0, start, index, lineBegin);
    exit;
  end;

  // number
  if CharInSet(text[start], (Numeric)) then begin
    Inc(index);
    while (index < Length(text) + 1) and CharInSet(text[index], Numeric) do
      Inc(index);
    scale := 0;
    if (index < Length(text) + 1) and (text[index] = DecimalPoint) then begin
      Inc(index);
      while (index < Length(text) + 1) and CharInSet(text[index], Numeric) do begin
        Inc(index);
        Inc(scale);
      end;
    end;
    val := copy(text, start, index - start);
    if scale = 0 then
      v := StrToInt(val)
    else if scale <= 4 then
      v := StrToCurr(val)
    else
      v := StrToFloat(val);
    NewToken(result, tNumber, v, 0, start, index, lineBegin);
    exit;
  end;

  // identifier / keyword
  if CharInSet(text[start], Alpha) then begin
    Inc(index);
    while (index < Length(text) + 1) and CharInSet(text[index], AlphaNumeric) do
      Inc(index);
    val := copy(text, start, index - start);
    name := UpperCase(val);
    if UpperedIdent then begin
      val := name;
      Id := Keywords.IndexOf(val);
    end else
      Id := keywords.IndexOf(name);
    if Id < 0 then begin
      Id := 0;
      tType := tIdentifier;
    end
    else begin
      Id := integer(Keywords.Objects[id]);
      tType := tKeyword;
    end;
    NewToken(result, tType, val, id, start, index, lineBegin);
    exit;
  end;

  // symbols
  sym := '';
  id := 0;
  for i := 0 to Symbols.Count - 1 do begin
    val := Symbols[i];
    if (StrLComp(PChar(text) + index - 1, PChar(val), Length(val)) = 0) then begin
      if Length(val) > Length(sym) then begin
        id := integer(Symbols.Objects[i]);
        sym := val;
      end;
    end;
  end;
  if id <> 0 then begin
    NewToken(result, tSymbol, sym, id, start, start + Length(sym), lineBegin);
    exit;
  end;

  // undefined symbol
  NewToken(result, tChar, text[start], 0, start, start + 1, lineBegin);
end;

function TLexer.GetOmitBlank: boolean;
begin
  result := (FBehavior and bOmitBlank <> 0);
end;

function TLexer.GetOmitComment: boolean;
begin
  result := (FBehavior and bOmitComment <> 0);
end;

function TLexer.GetPrevious: TToken;
begin
  result := FPrevious;
end;

function TLexer.GetPrevPrev: TToken;
begin
  result := FPrevPrev;
end;

function TLexer.GetQuotedIdent: boolean;
begin
  result := (FBehavior and bQuotedIdent <> 0);
end;

function TLexer.GetQuotedString: boolean;
begin
  result := (FBehavior and bQuotedString <> 0);
end;

function TLexer.GetUpperedIdent: boolean;
begin
  result := (FBehavior and bUpperedIdent <> 0);
end;

function TLexer.MoveNext: TToken;
begin
  FPrevPrev := FPrevious;
  FPrevious := FCurrent;
  if FNextAvailable then begin
    FNextAvailable := false;
    FCurrent := FNext;
  end else
    FCurrent := GetNextToken(FCurrent);
  result := FCurrent;
end;

function TLexer.PeekNextToken: TToken;
begin
  if not FNextAvailable then begin
    FNext := GetNextToken(FCurrent);
    FNextAvailable := true;
  end;
  result := FNext;
end;

procedure TLexer.Reset;
begin
  FCurrent := TokenBegin;
  FPrevious := TokenBegin;
  FPrevPrev := TokenBegin;
  FNextAvailable := false;
end;

procedure TLexer.SetOmitBlank(const value: boolean);
begin
  if value then
    FBehavior := FBehavior or bOmitBlank
  else
    FBehavior := FBehavior and not bOmitBlank;
end;

procedure TLexer.SetOmitComment(const value: boolean);
begin
  if value then
    FBehavior := FBehavior or bOmitComment
  else
    FBehavior := FBehavior and not bOmitComment;
end;

procedure TLexer.SetQuotedIdent(const value: boolean);
begin
  if value then
    FBehavior := FBehavior or bQuotedIdent
  else
    FBehavior := FBehavior and not bQuotedIdent;
end;

procedure TLexer.SetQuotedString(const value: boolean);
begin
  if value then
    FBehavior := FBehavior or bQuotedString
  else
    FBehavior := FBehavior and not bQuotedString;
end;

procedure TLexer.SetText(const Value: String);
begin
  FText := Value;
  Reset;
end;

procedure TLexer.SetUpperedIdent(const value: boolean);
begin
  if value then
    FBehavior := FBehavior or bUpperedIdent
  else
    FBehavior := FBehavior and not bUpperedIdent;
end;

{ TContext }

class function TExpressionContext.GetItem(context: pointer;
  const name: String): variant;
begin
  raise Exception.CreateFmt(SUnableToResolveName, [name]);
end;

class function TExpressionContext.GetSubContext(context: pointer; const name: String;
  out contextClass: TContextClass): pointer;
begin
  (*
var
  S: String;
  if Context <> nil then
    S := ' of class ' + TObject(Context).ClassName
  else S := '';
   + S
  *)
  raise Exception.CreateFmt(SUnableToResolveName, [name]);
end;

{ TEvaluator }

const
  BlockSize = 64;

function TEvaluator.AllocNode: PExpression;
begin
  if Length(FItems) <= FItemCount then
    SetLength(FItems, Length(FItems) + BlockSize);
  result := @FItems[FItemCount];
  Inc(FItemCount);
end;

constructor TEvaluator.Create(const aexpression: String; acontext: pointer;
  acontextclass: TContextClass);
begin
  Create(aexpression, acontext, CommonFunctions, acontextclass);
end;

constructor TEvaluator.Create(const aexpression: String; acontext: pointer;
  functions: TStringList; acontextclass: TContextClass);
begin
  inherited Create;
  FFunctions := functions;
  FContext := acontext;
  FContextClass := acontextclass;
  FExpression := aexpression;
  FItemCount := 0;
  FRoot := nil;
  if aexpression <> '' then
  with TExpressionParser.Create(aexpression, Self) do
  try
    Parse;
  finally
    Free;
  end;
end;

function TEvaluator.Evaluate: variant;
begin
  result := Evaluate(FRoot);
end;

procedure TEvaluator.Error(const Msg: String);
begin
  raise Exception.Create(Msg);
end;

function TEvaluator.Evaluate(expr: PExpression): variant;
begin
  if expr = nil then begin
    result := Unassigned;
    exit;
  end;
  case expr.Operation of
    opOr:
      result := boolean(Evaluate(expr.Arg1)) or boolean(Evaluate(expr.Arg2));
    opAnd:
      result := boolean(Evaluate(expr.Arg1)) and boolean(Evaluate(expr.Arg2));
    opAdd:
      result := Evaluate(expr.Arg1) + Evaluate(expr.Arg2);
    opMinus:
      result := -Evaluate(expr.Arg1);
    opSub:
      result := Evaluate(expr.Arg1) - Evaluate(expr.Arg2);
    opMul:
      result := Evaluate(expr.Arg1) * Evaluate(expr.Arg2);
    opDiv:
      result := Evaluate(expr.Arg1) / Evaluate(expr.Arg2);
    opEqual:
      result := (Evaluate(expr.Arg1) = Evaluate(expr.Arg2));
    opLess:
      result := (Evaluate(expr.Arg1) < Evaluate(expr.Arg2));
    opMore:
      result := (Evaluate(expr.Arg1) > Evaluate(expr.Arg2));
    opLessEqual:
      result := (Evaluate(expr.Arg1) <= Evaluate(expr.Arg2));
    opMoreEqual:
      result := (Evaluate(expr.Arg1) >= Evaluate(expr.Arg2));
    opNotEqual:
      result := (Evaluate(expr.Arg1) <> Evaluate(expr.Arg2));
    opLike:
      result := Like(Evaluate(expr.Arg1), Evaluate(expr.Arg2));
    opConcat:
      result := concat(Evaluate(expr.Arg1), Evaluate(expr.Arg2));
    opNot:
      result := not boolean(Evaluate(expr.Arg1));
    opConst:
      result := expr.Token.Value;
    opTrue:
      result := true;
    opFalse:
      result := false;
    opNull:
      result := null;
    opIsNull:
      result := VarIsNull(Evaluate(expr.Arg1));
    opIsNotNull:
      result := not VarIsNull(Evaluate(expr.Arg1));
    opFunc:
      if Assigned(expr.Func) then
        result := expr.Func(@Self, expr)
      else
        result := Unassigned;
    opIdent:
      result := FContextClass.GetItem(FContext, expr.Token.Value);
    opDot:
      result := ContextPath(expr);
  end;
end;

function TEvaluator.Like(const st, Mask: String): boolean;
const
  Wildcard = '*';
  WildcardOne = '_';
type
  TMatchesResult = (mrFalse,mrTrue,mrEnd);

  function MatchesMask(St: PChar; Mask: PChar): TMatchesResult;
  begin
    while (St^ <> #0) and (Mask^ <> #0) or (Mask^ = Wildcard) do begin
      if Mask^ = Wildcard then begin
        if (Mask + 1)^ = #0 then begin  //-
          Result := mrTrue;             // Speed up
          Exit;                         // with mask '*'
        end                             //-
        else
          case MatchesMask(St, Mask + 1) of
            mrTrue: begin
              Result := mrTrue;
              Exit;
            end;
            mrFalse:
              if St^ = #0 then begin
                Result := mrEnd;
                Exit;
              end
              else
                Inc(St);
            mrEnd: begin
              Result := mrEnd;
              Exit;
            end;
          end;
      end
      else
        if (St^ = Mask^) or (Mask^ = WildcardOne) then begin
          Inc(St);
          Inc(Mask);
        end
        else begin
          Result := mrFalse;
          Exit;
        end;
    end;

    if St^ = #0 then
      if Mask^ = #0 then
        Result := mrTrue
      else
        Result := mrEnd
    else
      Result := mrFalse;
  end;

begin
  Result := MatchesMask(PChar(St), PChar(Mask)) = mrTrue;
end;

function TEvaluator.ContextPath(expr: PExpression): variant;
var
  subcontext: pointer;
  acontextclass: TContextClass;
  i: integer;
begin
  subcontext := FContextClass.GetSubContext(context, expr.Arg1.Token.Value, acontextclass);
  if expr.Arg3 = nil then begin
    result := acontextclass.GetItem(subcontext, expr.Arg2.Token.Value);
    exit;
  end;
  subcontext := acontextclass.GetSubContext(subcontext, expr.Arg2.Token.Value, acontextclass);
  if Length(expr.Args) = 0 then begin
    result := acontextclass.GetItem(subcontext, expr.Arg3.Token.Value);
    exit;
  end;
  subcontext := acontextclass.GetSubContext(subcontext, expr.Arg3.Token.Value, acontextclass);
  i := 0;
  while i < Length(expr.Args) - 1 do begin
    subcontext := acontextclass.GetSubContext(subcontext, expr.Args[i].Token.Value, acontextclass);
    Inc(i);
  end;
  result := acontextclass.GetItem(subcontext, expr.Args[i].Token.Value);
end;

{ TExpressionParser }

function TExpressionParser.AddExpr: PExpression;
var
  expr: PExpression;
begin
  result := MulExpr;
  while FLexer.FCurrent.Id in [lxPlus, lxMinus, lxConcat] do begin
    expr := FEvaluator.AllocNode;
    expr.Token := FLexer.FCurrent;
    case FLexer.FCurrent.Id of
      lxPlus:
        expr.Operation := opAdd;
      lxMinus:
        expr.Operation := opSub;
      lxConcat:
        expr.Operation := opConcat;
    end;
    FLexer.MoveNext;
    expr.Arg1 := result;
    expr.Arg2 := MulExpr;
    result := expr;
  end;
end;

function TExpressionParser.AndExpr: PExpression;
var
  expr: PExpression;
begin
  Result := Negation;
  while FLexer.FCurrent.Id = lxAND do begin
    expr := FEvaluator.AllocNode;
    expr.Token := FLexer.FCurrent;
    FLexer.MoveNext;
    expr.Operation := opAnd;
    expr.Arg1 := result;
    expr.Arg2 := Negation;
    result := expr;
  end;
end;

function TExpressionParser.Argument: PExpression;
var
  operation: TOperationCode;
begin
  operation := opNone;
  case FLexer.FCurrent.Id of
    lxLeftBracket: begin
      FLexer.MoveNext;
      result := OrExpr;
      if FLexer.FCurrent.Id <> lxRightBracket then
        Error(SRightBracketExpected, FLexer.FCurrent);
      FLexer.MoveNext;
      exit;
    end;
    lxMinus: begin
      result := FEvaluator.AllocNode;
      result.Token := FLexer.FCurrent;
      FLexer.MoveNext;
      result.Operation := opMinus;
      result.Arg1 := Argument;
      exit;
    end;
    lxPlus: begin
      FLexer.MoveNext;
      result := Argument;
      exit;
    end;
    lxNull:
      operation := opNull;
    lxTrue:
      operation := opTrue;
    lxFalse:
      operation := opFalse;
  end;
  if operation = opNone then begin
    case FLexer.FCurrent.TType of
      tChar:
        Error(SUnknownChar, FLexer.FCurrent);
      tSymbol:
        Error(SUnexpectedSymbol, FLexer.FCurrent);
      tKeyword:
        Error(SUnexpectedKeyword, FLexer.FCurrent);
      tIdentifier: begin
        result := Identifier;
        exit;
      end;
      tNumber,tString:
        operation := opConst;
      tEnd:
        Error(SUnexpectedEnd, FLexer.FCurrent);
    else
      Error(SInternalError, FLexer.FCurrent);
    end;
  end;
  result := FEvaluator.AllocNode;
  result.Token := FLexer.FCurrent;
  result.Operation := operation;
  FLexer.MoveNext;
end;

function TExpressionParser.Condition: PExpression;
var
  operation: TOperationCode;
  expr: PExpression;
begin
  result := AddExpr;
  case FLexer.FCurrent.Id of
    lxIs: begin
      expr := FEvaluator.AllocNode;
      expr.Token := FLexer.FCurrent;
      if FLexer.MoveNext.Id = lxNot then begin
        expr.operation := opIsNotNull;
        FLexer.MoveNext;
      end else
        expr.operation := opIsNull;
      if FLexer.FCurrent.Id <> lxNull then
        Error(SNullExpected, FLexer.FCurrent);
      FLexer.MoveNext;
      expr.Arg1 := result;
      result := expr;
      exit;
    end;
    lxEqual:
      operation := opEqual;
    lxMore:
      operation := opMore;
    lxLess:
      operation := opLess;
    lxMoreEqual:
      operation := opMoreEqual;
    lxLessEqual:
      operation := opLessEqual;
    lxNotEqual:
      operation := opNotEqual;
    lxLike:
      operation := opLike;
  else
    exit;
  end;
  expr := FEvaluator.AllocNode;
  expr.Token := FLexer.FCurrent;
  FLexer.MoveNext;
  expr.Operation := operation;
  expr.Arg1 := result;
  expr.Arg2 := AddExpr;
  result := expr;
end;

constructor TExpressionParser.Create(const expression: String;
  var evaluator: TEvaluator);
begin
  inherited Create;
  FLexer := TLexer.Create(expression, bOmitBlank or bOmitComment);
  FEvaluator := evaluator;
end;

destructor TExpressionParser.Destroy;
begin
  FreeAndNil(FLexer);
  inherited;
end;

procedure TExpressionParser.Error(const Msg: String; const Token: TToken);
begin
  raise Exception.CreateFmt(Msg + Format(SAtPosition, [Token.StartPosition]), [Token.Value]);
end;

function TExpressionParser.Identifier: PExpression;
var
  ident: TToken;
  expr: PExpression;
  i: integer;
begin
  ident := FLexer.FCurrent;
  case FLexer.MoveNext.Id of
    lxDot: begin
      result := FEvaluator.AllocNode;
      result.Operation := opDot;
      expr := FEvaluator.AllocNode;
      expr.Token := ident;
      expr.Operation := opIdent;
      result.Arg1 := expr;
      i := 2;
      repeat
        if FLexer.MoveNext.TType <> tIdentifier then
          Error(SIdentifierExpected, FLexer.FCurrent);
        if i > 3 then
          SetLength(result.Args, i - 3);
        expr := FEvaluator.AllocNode;
        expr.Token := FLexer.FCurrent;
        expr.Operation := opIdent;
        case i of
          2: result.Arg2 := expr;
          3: result.Arg3 := expr;
        else
          result.Args[i - 4] := expr;
        end;
        Inc(i);
      until FLexer.MoveNext.Id <> lxDot;
    end;
    lxLeftBracket: begin
      result := FEvaluator.AllocNode;
      result.Operation := opFunc;
      i := FEvaluator.FFunctions.IndexOf(ident.Value);
      if i < 0 then
        Error(SUnknownFunction, ident);
      result.Func := pointer(FEvaluator.FFunctions.Objects[i]);
      result.Token := ident;
      FLexer.MoveNext;
      if FLexer.FCurrent.Id <> lxRightBracket then begin
        result.Arg1 := OrExpr;
        i := 2;
        while FLexer.FCurrent.Id = lxComma do begin
          FLexer.MoveNext;
          if i > 3 then
            SetLength(result.Args, i - 3);
          case i of
            2: result.Arg2 := OrExpr;
            3: result.Arg3 := OrExpr;
          else
            result.Args[i - 4] := OrExpr;
          end;
          inc(i);
        end;
        if FLexer.FCurrent.Id <> lxRightBracket then
          Error(SRightBracketExpected, FLexer.FCurrent);
      end;
      FLexer.MoveNext;
    end;
  else
    result := FEvaluator.AllocNode;
    result.Token := ident;
    result.Operation := opIdent;
  end;
end;

function TExpressionParser.MulExpr: PExpression;
var
  expr: PExpression;
begin
  result := Argument;
  while FLexer.FCurrent.Id in [lxMul, lxDiv] do begin
    expr := FEvaluator.AllocNode;
    expr.Token := FLexer.FCurrent;
    if FLexer.FCurrent.Id = lxMul then
      expr.Operation := opMul
    else
      expr.Operation := opDiv;
    FLexer.MoveNext;
    expr.Arg1 := result;
    expr.Arg2 := Argument;
    result := expr;
  end;
end;

function TExpressionParser.Negation: PExpression;
begin
  if FLexer.FCurrent.Id = lxNOT then begin
    result := FEvaluator.AllocNode;
    result.Token := FLexer.FCurrent;
    FLexer.MoveNext;
    result.Operation := opNot;
    result.Arg1 := Condition;
  end else
    result := Condition;
end;

function TExpressionParser.OrExpr: PExpression;
var
  expr: PExpression;
begin
  result := AndExpr;
  while FLexer.FCurrent.Id = lxOR do begin
    expr := FEvaluator.AllocNode;
    expr.Token := FLexer.FCurrent;
    FLexer.MoveNext;
    expr.Operation := opOr;
    expr.Arg1 := result;
    expr.Arg2 := AndExpr;
    result := expr;
  end;
end;

procedure TExpressionParser.Parse;
begin
  FLexer.MoveNext;
  FEvaluator.FRoot := OrExpr;
  if FLexer.FCurrent.TType <> tEnd then
    Error(SIllegalExpression, FLexer.FCurrent);
end;

procedure AddSymbol(const symbol: String; id: integer);
begin
  CommonSymbols.AddObject(symbol, pointer(id));
end;

procedure AddKeyword(const keyword: String; id: integer);
begin
  CommonKeywords.AddObject(keyword, pointer(id));
end;

procedure AddFunction(const name: String; func: TFunction);
begin
  CommonFunctions.AddObject(name, @func);
end;

function Nvlf(evaluator: PEvaluator; expression: PExpression): variant;
var
  i: integer;
begin
  result := evaluator.Evaluate(expression.Arg1);
  if not(VarIsNull(result) or VarIsEmpty(result)) then
    exit;
  result := evaluator.Evaluate(expression.Arg2);
  if not(VarIsNull(result) or VarIsEmpty(result)) then
    exit;
  result := evaluator.Evaluate(expression.Arg3);
  if not(VarIsNull(result) or VarIsEmpty(result)) then
    exit;
  for i := 0 to Length(expression.Args) - 1 do begin
    result := evaluator.Evaluate(expression.Args[i]);
    if not(VarIsNull(result) or VarIsEmpty(result)) then
      exit;
  end;
end;

function Iiff(evaluator: PEvaluator; expression: PExpression): variant;
var
  condition: variant;
begin
  condition := evaluator.Evaluate(expression.Arg1);
  if VarType(condition) <> varBoolean then
    evaluator.Error(Format(SInvalidArgumentType, [1, 'IIF']));
  if condition then
    result := evaluator.Evaluate(expression.Arg2)
  else
    result := evaluator.Evaluate(expression.Arg3)
end;

function fLOWER(evaluator: PEvaluator; expression: PExpression): variant;
var
  s: String;
begin
  s := evaluator.Evaluate(expression.Arg1);
  result := AnsiLowerCase(s);
end;

function fUPPER(evaluator: PEvaluator; expression: PExpression): variant;
var
  s: String;
begin
  s := evaluator.Evaluate(expression.Arg1);
  result := AnsiUpperCase(s);
end;

function fLENGTH(evaluator: PEvaluator; expression: PExpression): variant;
var
  s: String;
begin
  s := evaluator.Evaluate(expression.Arg1);
  result := Length(s);
end;

function fSUBSTRING(evaluator: PEvaluator; expression: PExpression): variant;
var
  s: String;
  index, count: integer;
begin
  s := evaluator.Evaluate(expression.Arg1);
  index := evaluator.Evaluate(expression.Arg2);
  count := evaluator.Evaluate(expression.Arg3);
  result := copy(s, index, count);
end;

function fLEFT(evaluator: PEvaluator; expression: PExpression): variant;
var
  s: String;
  count: integer;
begin
  s := evaluator.Evaluate(expression.Arg1);
  count := evaluator.Evaluate(expression.Arg2);
  result := copy(s, 1, count);
end;

function fRIGHT(evaluator: PEvaluator; expression: PExpression): variant;
var
  s: String;
  count: integer;
begin
  s := evaluator.Evaluate(expression.Arg1);
  count := evaluator.Evaluate(expression.Arg2);
  result := copy(s, length(s)-count+1, count);
end;

function fTRIM(evaluator: PEvaluator; expression: PExpression): variant;
var
  s: String;
begin
  s := evaluator.Evaluate(expression.Arg1);
  result := Trim(s);
end;

function fLTRIM(evaluator: PEvaluator; expression: PExpression): variant;
var
  s: String;
begin
  s := evaluator.Evaluate(expression.Arg1);
  result := TrimLeft(s);
end;

function fRTRIM(evaluator: PEvaluator; expression: PExpression): variant;
var
  s: String;
begin
  s := evaluator.Evaluate(expression.Arg1);
  result := TrimRight(s);
end;

function fPOSITION(evaluator: PEvaluator; expression: PExpression): variant;
var
  substr: String;
  s: String;
begin
  substr := evaluator.Evaluate(expression.Arg1);
  s := evaluator.Evaluate(expression.Arg2);
  result := Pos(substr, s);
end;

function fREPLACE(evaluator: PEvaluator; expression: PExpression): variant;
var
  old: String;
  new: String;
  s: String;
begin
  old := evaluator.Evaluate(expression.Arg1);
  new := evaluator.Evaluate(expression.Arg2);
  s := evaluator.Evaluate(expression.Arg3);
  result := StringReplace(s, old, new, [rfReplaceAll]);
end;

{
TODO:
  OCCURS Finds the number of times one String value is present within another String value.
}

function fREPEAT(evaluator: PEvaluator; expression: PExpression): variant;
var
  pattern: String;
  count: integer;
  s: String;
  i: integer;
  p: integer;
  l: integer;
begin
  pattern := evaluator.Evaluate(expression.Arg1);
  count := evaluator.Evaluate(expression.Arg2);
  l := length(pattern);
  SetLength(s, l * count);
  p := 1;
  for i := 1 to count do begin
    move(pattern[1], s[p], l);
    inc(p, l);
  end;
  result := s;
end;

function fCONCAT(evaluator: PEvaluator; expression: PExpression): variant;
var
  s: String;
  i: integer;
begin
  s := evaluator.Evaluate(expression.Arg1);
  if (expression.Arg2 <> nil) then
    s := concat(s, evaluator.Evaluate(expression.Arg2));
  if (expression.Arg3 <> nil) then
    s := concat(s, evaluator.Evaluate(expression.Arg3));
  for i := 0 to Length(expression.Args) - 1 do begin
    s := concat(s, evaluator.Evaluate(expression.Args[i]));
  end;
  result := s;
end;

function fABS(evaluator: PEvaluator; expression: PExpression): variant;
var
  v: variant;
  i: integer;
{$IFnDEF VER130}
  i64: int64;
{$ENDIF}
  e: extended;
begin
  v := evaluator.Evaluate(expression.Arg1);
  case VarType(v) of
    {$IFnDEF VER130}
    varShortInt,
    varWord,
    {$ENDIF}
    varSmallint,varByte,varInteger: begin
      i := v;
      result := abs(i);
    end;
    {$IFnDEF VER130}
    varInt64: begin
      i64 := v;
      result := abs(i64);
    end;
    {$ENDIF}
    else begin
      e := v;
      result := abs(e);
    end;
  end;
end;

function fACOS(evaluator: PEvaluator; expression: PExpression): variant;
var
  e: extended;
begin
  e := evaluator.Evaluate(expression.Arg1);
  result := ArcCos(e);
end;

function fASIN(evaluator: PEvaluator; expression: PExpression): variant;
var
  e: extended;
begin
  e := evaluator.Evaluate(expression.Arg1);
  result := ArcSin(e);
end;

function fATAN(evaluator: PEvaluator; expression: PExpression): variant;
var
  e: extended;
begin
  e := evaluator.Evaluate(expression.Arg1);
  result := ArcTan(e);
end;

function fATAN2(evaluator: PEvaluator; expression: PExpression): variant;
var
  y, x: extended;
begin
  y := evaluator.Evaluate(expression.Arg1);
  x := evaluator.Evaluate(expression.Arg2);
  result := ArcTan2(y, x);
end;

function fCOS(evaluator: PEvaluator; expression: PExpression): variant;
var
  e: extended;
begin
  e := evaluator.Evaluate(expression.Arg1);
  result := Cos(e);
end;

function fSIN(evaluator: PEvaluator; expression: PExpression): variant;
var
  e: extended;
begin
  e := evaluator.Evaluate(expression.Arg1);
  result := Sin(e);
end;

function fTAN(evaluator: PEvaluator; expression: PExpression): variant;
var
  e: extended;
begin
  e := evaluator.Evaluate(expression.Arg1);
  result := Tan(e);
end;

function fCEILING(evaluator: PEvaluator; expression: PExpression): variant;
var
  x: extended;
begin
  x := evaluator.Evaluate(expression.Arg1);
  result := Ceil(x);
end;

{$IFnDEF VER130}
function fCOT(evaluator: PEvaluator; expression: PExpression): variant;
var
  x: extended;
begin
  x := evaluator.Evaluate(expression.Arg1);
  result := Cot(x);
end;
{$ENDIF}

function fDEGREES(evaluator: PEvaluator; expression: PExpression): variant;
var
  x: extended;
begin
  x := evaluator.Evaluate(expression.Arg1);
  result := RadToDeg(x);
end;

function fEXP(evaluator: PEvaluator; expression: PExpression): variant;
var
  x: extended;
begin
  x := evaluator.Evaluate(expression.Arg1);
  result := Exp(x);
end;

function fFLOOR(evaluator: PEvaluator; expression: PExpression): variant;
var
  x: extended;
begin
  x := evaluator.Evaluate(expression.Arg1);
  result := Floor(x);
end;

function fLOG(evaluator: PEvaluator; expression: PExpression): variant;
var
  x: extended;
begin
  x := evaluator.Evaluate(expression.Arg1);
  result := Ln(x);
end;

function fLOG10(evaluator: PEvaluator; expression: PExpression): variant;
var
  x: extended;
begin
  x := evaluator.Evaluate(expression.Arg1);
  result := Log10(x);
end;

function fPI(evaluator: PEvaluator; expression: PExpression): variant;
begin
  result := pi;
end;

function fMOD(evaluator: PEvaluator; expression: PExpression): variant;
var
  a, b: integer;
begin
  a := evaluator.Evaluate(expression.Arg1);
  b := evaluator.Evaluate(expression.Arg2);
  result := a mod b;
end;

function fPOWER(evaluator: PEvaluator; expression: PExpression): variant;
var
  x, y: extended;
begin
  x := evaluator.Evaluate(expression.Arg1);
  y := evaluator.Evaluate(expression.Arg2);
  result := Power(x, y);
end;

function fRADIANS(evaluator: PEvaluator; expression: PExpression): variant;
var
  x: extended;
begin
  x := evaluator.Evaluate(expression.Arg1);
  result := DegToRad(x);
end;

function fRAND(evaluator: PEvaluator; expression: PExpression): variant;
var
  r: integer;
begin
  if (expression.Arg1 = nil) then
    result := random
  else begin
    r := evaluator.Evaluate(expression.Arg1);
    result := random(r);
  end;
end;

function fROUND(evaluator: PEvaluator; expression: PExpression): variant;
var
  x: extended;
begin
  x := evaluator.Evaluate(expression.Arg1);
  result := Integer(Round(x));
end;

function fTRUNC(evaluator: PEvaluator; expression: PExpression): variant;
var
  x: extended;
begin
  x := evaluator.Evaluate(expression.Arg1);
  result := Integer(Trunc(x));
end;

function fSIGN(evaluator: PEvaluator; expression: PExpression): variant;
var
  x: extended;
begin
  x := evaluator.Evaluate(expression.Arg1);
  if (x < 0) then
    result := -1
  else if (x > 0) then
    result := 1
  else
    result := 0;
end;

function fSQRT(evaluator: PEvaluator; expression: PExpression): variant;
var
  x: extended;
begin
  x := evaluator.Evaluate(expression.Arg1);
  result := Sqrt(x);
end;


initialization
  CommonSymbols := TStringList.Create;
  CommonKeywords := TStringList.Create;
  AddSymbol('+', lxPlus);
  AddSymbol('-', lxMinus);
  AddSymbol('*', lxMul);
  AddSymbol('/', lxDiv);
  AddSymbol('(', lxLeftBracket);
  AddSymbol('=', lxEqual);
  AddSymbol('>', lxMore);
  AddSymbol('<', lxLess);
  AddSymbol('>=', lxMoreEqual);
  AddSymbol('<=', lxLessEqual);
  AddSymbol('<>', lxNotEqual);
  AddSymbol(')', lxRightBracket);
  AddSymbol('.', lxDot);
  AddSymbol(',', lxComma);
  AddSymbol('||', lxConcat);
  AddKeyword('OR', lxOR);
  AddKeyword('AND', lxAND);
  AddKeyword('NOT', lxNot);
  AddKeyword('TRUE', lxTrue);
  AddKeyword('FALSE', lxFalse);
  AddKeyword('NULL', lxNull);
  AddKeyword('IS', lxIs);
  AddKeyword('LIKE', lxLike);
  CommonSymbols.Sorted := true;
  CommonKeywords.Sorted := true;
  CommonFunctions := TStringList.Create;
  AddFunction('COALESCE', Nvlf);
  AddFunction('NVL', Nvlf);
  AddFunction('IIF', Iiff);
  AddFunction('IF', Iiff);
  AddFunction('LOWER', fLOWER);
  AddFunction('LCASE', fLOWER);
  AddFunction('UPPER', fUPPER);
  AddFunction('UCASE', fUPPER);
  AddFunction('LENGTH', fLENGTH);
  AddFunction('LEN', fLENGTH);
  AddFunction('SUBSTRING', fSUBSTRING);
  AddFunction('SUBSTR', fSUBSTRING);
  AddFunction('LEFT', fLEFT);
  AddFunction('RIGHT', fRIGHT);
  AddFunction('TRIM', fTRIM);
  AddFunction('LTRIM', fLTRIM);
  AddFunction('RTRIM', fRTRIM);
  AddFunction('POS', fPOSITION);
  AddFunction('POSITION', fPOSITION);
  AddFunction('REPLACE', fREPLACE);
  AddFunction('REPEAT', fREPEAT);
  AddFunction('CONCAT', fCONCAT);
  AddFunction('ABS', fABS);
  AddFunction('ACOS', fACOS);
  AddFunction('ASIN', fASIN);
  AddFunction('ATAN', fATAN);
  AddFunction('ATAN2', fATAN2);
  AddFunction('COS', fCOS);
  AddFunction('SIN', fSIN);
  AddFunction('TAN', fTAN);
  AddFunction('CEIL', fCEILING);
  AddFunction('CEILING', fCEILING);
  {$IFnDEF VER130}
  AddFunction('COT', fCOT);
  {$ENDIF}
  AddFunction('DEGREES', fDEGREES);
  AddFunction('EXP', fEXP);
  AddFunction('FLOOR', fFLOOR);
  AddFunction('LOG', fLOG);
  AddFunction('LOG10', fLOG10);
  AddFunction('PI', fPI);
  AddFunction('MOD', fMOD);
  AddFunction('POWER', fPOWER);
  AddFunction('RADIANS', fRADIANS);
  AddFunction('RAND', fRAND);
  AddFunction('ROUND', fROUND);
  AddFunction('TRUNC', fTRUNC);
  AddFunction('TRUNCATE', fTRUNC);
  AddFunction('SIGN', fSIGN);
  AddFunction('SQRT', fSQRT);
  CommonFunctions.Sorted := true;
finalization
  FreeAndNil(CommonSymbols);
  FreeAndNil(CommonKeywords);
  FreeAndNil(CommonFunctions);
end.

