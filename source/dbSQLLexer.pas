(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Contains: TSQLLexer class implementing generic SQL lexer & parser. 
(*
(*  Copyright (c) 2005-2011, Context Software LLC
(*
(*  ------------------------------------------------------------
(*  FILE        : dbSQLLexer.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 3.31
(*  DELPHI\BCB  : Delphi 7, 2005, 2006, 2007, 2009, 2010, XE
(*
(******************************************************************************)
unit dbSQLLexer;

{$I CtxVer.inc}

interface

uses Windows, SysUtils, Classes;

type
  (*
    SQLLexer is a generic Lexer parser, that can be customizing with
    the following parameters:
    - Comments in form of: '--;//;/*,*/;#'
    - List of Literal Prefixes, like: N,X
    - SetTermCommand
    - Term
  *)
  TSQLLexerState = record
    Position: Integer;
    TokenBeginPos: Integer;
    Term: String;
    SetTermCommand: String;
    LineNo: Integer;
    LinePos: Integer;
    NextChar: Char;
    Token: String;
    TokenID: Integer;
  end;

  TSQLLexerPragmaEvent = procedure (Sender: TObject; const Pragma: String) of object;

  TSQLLexer = class
  protected
    FStream: TStream;
    FBuffer: String;
    FBufPos: Integer;
    FBufSize: Integer;
    FBufLen: Integer;
    FNextChar: Char;
    FToken: String;
    FTokenID: Integer;
    FLinePos: Integer;
    FLineNo: Integer;
    FTokenBeginPos: Integer;
    FTokenLen: Integer;
    FTokenBuf: String;
    FTokenBufSize: Integer;

    FComments: TStringList;
    FLiteralPrefixes: TStringList;
    FCommentChars: set of AnsiChar;
    FTermChar: Char;
    FOwnStream: Boolean;
    FTerm: String;
    FSetTermCommand: String;
    FSizeOfChar: Integer;
    FOnPragma: TSQLLexerPragmaEvent;

    procedure SetTerm(const Value: String);
    function GetComments: String;
    function GetPrefixes: String;
    procedure SetComments(const Value: String);
    procedure SetPrefixes(const Value: String);
    procedure ReadBuffer;
    function GetPosition: Integer;
    function ExtractQuotedToken(const Quote: Char; ATokenID: Integer): Integer;
    function ParseComments: Integer;
    procedure AddTokenChar(const C: Char);
  public
    constructor Create(AStream: TStream; BufSize: Integer = 1024; ASizeOfChar: Integer = 1; const ATerm: string =''; const AComments: string = ''); overload;
    constructor Create(const SQLString: String; const ATerm: string =''; const AComments: string = ''); overload;
    destructor Destroy; override;

    procedure GetNextChar;
    function GetNextToken: Integer;

    function GetLexerState: TSQLLexerState;
    procedure SetLexerState(const AState: TSQLLexerState);

    // Reads next statement and returns false if end of script is reached
    function NextStatement(var Statement: String): Boolean;

    procedure ExtractBlock(StartPos, EndPos: Integer; var Buffer: String);
    procedure ParsePragmaComments(const Pragma: String);

    property Stream: TStream read FStream;
    property Position: Integer read GetPosition;
    property TokenBeginPos: Integer read FTokenBeginPos;

    property Comments: String read GetComments write SetComments;
    property LiteralPrefixes: String read GetPrefixes write SetPrefixes;
    property Term: String read FTerm write SetTerm;
    property SetTermCommand: String read FSetTermCommand write FSetTermCommand;

    property LineNo: Integer read FLineNo;
    property LinePos: Integer read FLinePos;
    property NextChar: Char read FNextChar;
    property Token: String read FToken;
    property TokenID: Integer read FTokenID;
    property SizeOfChar: Integer read FSizeOfChar;
    property OnPragma: TSQLLexerPragmaEvent read FOnPragma write FOnPragma;
  end;

const
  // SQL Token Types
  tokenUnknown = -1;
  tokenEOF = 0;
  tokenEOLN = 1;
  tokenComment = 2;
  tokenToken = 3;
  tokenIdentifier = 4;
  tokenLiteral = 5;
  tokenSymbols = 6;
  tokenTerm = 7;

  setDelimiters = [#1..#32] - [#13];
  setSpecialChars = ['''','"','`',';',',','[',']','(',')','=']; // ,'.','-','+'
  setTokenChars = [#33..#255] - setSpecialChars;
  setAlphaNum = ['A'..'Z', 'a'..'z', '_', '0'..'9'];

  DefaultCommentChars = '--;/*,*/;//';

  chrEOLN = #13;

resourcestring
  SUnexpectedEndOfToken = 'Unexpected end of token';

implementation

{$I CtxD2009.inc}

{ Helper Routines }

function MatchText(SubStr, Str: String; var SP: Integer): Boolean;
var
  I: Integer;
begin
  I := 1;
  Result := False;
  if Length(SubStr) > Length(Str) - SP + 1 then exit;
  while (I <= Length(SubStr)) and (SP <= Length(Str)) do
  begin
    if AnsiUpperCase(SubStr[I]) <> AnsiUpperCase(Str[SP]) then exit;
    Inc(I);
    Inc(SP);
  end;
  if I <= Length(SubStr) then exit;
  Result := (SP > Length(Str)) or (Str[SP] = ' ');
  while (SP <= Length(Str)) and (Str[SP] = ' ') do Inc(SP);
end;

function NextToken(const Str: String; Delimiter: Char; var SP: Integer): String;
var
  BP: Integer;
begin
  // Skip leading delimiters
  while (SP <= Length(Str)) and (Ord(Str[SP]) <= 32) do Inc(SP);
  BP := SP;
  if (SP <= Length(Str)) and (Str[SP] = '"') then
  begin
    Inc(BP);
    Inc(SP);
    while (SP <= Length(Str)) and (Str[SP] <> '"') do Inc(SP);
    Result := copy(Str, BP, SP - BP);
    while (SP <= Length(Str)) and (Str[SP] <> Delimiter) do Inc(SP);
  end else
  begin
    while (SP <= Length(Str)) and (Str[SP] <> Delimiter) do Inc(SP);
    Result := copy(Str, BP, SP - BP);
  end;
  if SP <= Length(Str) then Inc(SP); // Skip delimiter
end;

function GetDelimitedText(Strings: TStrings): string;
const
  ctQuoteChar = '"';
  ctDelimiter = ';';
var
  S: string;
  P: PChar;
  I, ACount: Integer;
begin
  with Strings do
  begin
    ACount := Count;
    if (ACount = 1) and (Strings[0] = '') then
      Result := ctQuoteChar + ctQuoteChar
    else
    begin
      Result := '';
      for I := 0 to ACount - 1 do
      begin
        S := Strings[I];
        P := PChar(S);
        while not CharInSet(P^, [#0..' ', ctQuoteChar, ctDelimiter]) do
        {$IFDEF MSWINDOWS}
          P := CharNext(P);
        {$ELSE}
          Inc(P);
        {$ENDIF}
        if (P^ <> #0) then S := AnsiQuotedStr(S, ctQuoteChar);
        Result := Result + S + ctDelimiter;
      end;
      System.Delete(Result, Length(Result), 1);
    end;
  end;
end;

procedure SetDelimitedText(Strings: TStrings; const Value: string);
const
  ctQuoteChar = '"';
  ctDelimiter = ';';
var
  P, P1: PChar;
  S: string;
begin
  with Strings do
  begin
    BeginUpdate;
    try
      Clear;
      P := PChar(Value);
      while CharInSet(P^, [#1..' ']) do
      {$IFDEF MSWINDOWS}
        P := CharNext(P);
      {$ELSE}
        Inc(P);
      {$ENDIF}
      while P^ <> #0 do
      begin
        if P^ = ctQuoteChar then
          S := AnsiExtractQuotedStr(P, ctQuoteChar)
        else
        begin
          P1 := P;
          while (P^ > ' ') and (P^ <> ctDelimiter) do
          {$IFDEF MSWINDOWS}
            P := CharNext(P);
          {$ELSE}
            Inc(P);
          {$ENDIF}
          SetString(S, P1, P - P1);
        end;
        Add(S);
        while CharInSet(P^, [#1..' ']) do
        {$IFDEF MSWINDOWS}
          P := CharNext(P);
        {$ELSE}
          Inc(P);
        {$ENDIF}
        if P^ = ctDelimiter then
        begin
          P1 := P;
          {$IFDEF MSWINDOWS}
          if CharNext(P1)^ = #0 then
          {$ELSE}
          Inc(P1);
          if P1^ = #0 then
          {$ENDIF}
            Add('');
          repeat
            {$IFDEF MSWINDOWS}
            P := CharNext(P);
            {$ELSE}
            Inc(P);
            {$ENDIF}
          until not CharInSet(P^, [#1..' ']);
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

function LeftPart(const Str, Delimiter: String): String;
var
  P: Integer;
begin
  P := AnsiPos(Delimiter, Str);
  if P <= 0 then
    Result := Str
  else Result := copy(Str, 1, P - 1);
end;

function RightPart(const Str, Delimiter: String): String;
var
  P: Integer;
begin
  P := AnsiPos(Delimiter, Str);
  if P <= 0 then
    Result := ''
  else Result := copy(Str, P + Length(Delimiter), MaxInt);
end;

{ TSQLLexer }

constructor TSQLLexer.Create(AStream: TStream; BufSize: Integer = 1024; ASizeOfChar: Integer = 1; const ATerm: string =''; const AComments: string = '');
begin
  inherited Create;
  FBufSize := BufSize;
  FStream := AStream;
  FOwnStream := False;
  SetLength(FBuffer, FBufSize);
  FTokenBufSize := 255;
  SetLength(FTokenBuf, FTokenBufSize);
  FComments := TStringList.Create;
  // +++ FComments.NameValueSeparator := ',';
  // +++ FComments.Delimiter := ';';
  FSetTermCommand := '';
  FTermChar := #0;
  if AComments = '' then
    Comments := DefaultCommentChars else
    Comments := AComments;
  if ATerm <> '' then
    Term := ATerm;
  FLiteralPrefixes := TStringList.Create;
  FLinePos := 1;
  FLineNo := 1;
  FBufPos := 1;
  FTokenID := tokenUnknown;
  FSizeOfChar := ASizeOfChar;
  ReadBuffer;
  GetNextChar;
end;

constructor TSQLLexer.Create(const SQLString: String; const ATerm: string =''; const AComments: string = '');
begin
  {$IFDEF D2009_ORLATER}
  Create(TStringStream.Create(SQLString, TEncoding.Unicode), 1024, 2, ATerm, AComments);
  {$ELSE}
  Create(TStringStream.Create(SQLString), 1024, 1, ATerm, AComments);
  {$ENDIF}
  FOwnStream := True;
end;

destructor TSQLLexer.Destroy;
begin
  FComments.Free;
  FLiteralPrefixes.Free;
  if FOwnStream then
    FreeAndNil(FStream);
  inherited;
end;

function TSQLLexer.GetComments: String;
begin
  Result := GetDelimitedText(FComments);
end;

function TSQLLexer.GetPrefixes: String;
begin
  Result := FLiteralPrefixes.CommaText;
end;

procedure TSQLLexer.SetComments(const Value: String);
var
  I: Integer;
begin
  SetDelimitedText(FComments, Value);
  FCommentChars := [];
  I := 0;
  while I < FComments.Count do
    if FComments[I] <> '' then
    begin
      Include(FCommentChars, AnsiChar(FComments[I][1]));
      Inc(I);
    end else
      FComments.Delete(I);
end;

procedure TSQLLexer.SetPrefixes(const Value: String);
begin
  FLiteralPrefixes.CommaText := Value;
end;

procedure TSQLLexer.ReadBuffer;
var
  P: integer;
begin
  P := FBufLen-FBufPos+1;
  Move(FBuffer[FBufPos], FBuffer[1], P);
  FBufLen := P+FStream.Read(FBuffer[P+1], (FBufSize-P) * FSizeOfChar) div FSizeOfChar;
  FBufPos := 1;
end;

function TSQLLexer.GetPosition: Integer;
begin
  Result := (FStream.Position div FSizeOfChar)-FBufLen+FBufPos - 1;
end;

procedure TSQLLexer.GetNextChar;

  procedure DoNextChar;
  begin
    if FBufPos >= FBufLen then
      ReadBuffer;
    if FBufPos <= FBufLen then
    begin
      FNextChar := FBuffer[FBufPos];
      Inc(FBufPos);
      Inc(FLinePos);
    end else FNextChar := #0;
  end;

begin
  DoNextChar;
  if (FNextChar = #10) or (FNextChar = #13) then
  begin
    if (FBufPos <= FBufLen) and (FBuffer[FBufPos] = #10) or (FBuffer[FBufPos] = #13) then
      DoNextChar;
    FNextChar := chrEOLN;
  end;
end;

procedure TSQLLexer.ExtractBlock(StartPos, EndPos: Integer; var Buffer: String);
var
  SavePos: Integer;
begin
  SavePos := Stream.Position;
  try
    Stream.Position := StartPos * FSizeOfChar;
    SetLength(Buffer, EndPos - StartPos);
    if Length(Buffer) > 0 then
      Stream.Read(Buffer[1], (EndPos-StartPos) * FSizeOfChar);
  finally
    Stream.Position := SavePos;
  end;
end;

function TSQLLexer.ExtractQuotedToken(const Quote: Char; ATokenID: Integer): Integer;
begin
  Result := ATokenID;
  AddTokenChar(FNextChar);
  GetNextChar;
  repeat
    if FNextChar = Quote then
    begin
      AddTokenChar(FNextChar);
      GetNextChar;
      if not (CharInSet(Quote, ['''', '"', '`']) and (FNextChar = Quote)) then break;
    end;
    if FNextChar = #0 then
      raise Exception.Create(SUnexpectedEndOfToken);
    AddTokenChar(FNextChar);
    if FNextChar = chrEOLN then
    begin
      Inc(FLineNo);
      FLinePos := 1;
    end;
    GetNextChar;
  until False;
end;

function TSQLLexer.ParseComments: Integer;
var
  MatchCount, Len, P, I: Integer;
  CommentBegin, CommentEnd, Str: String;
begin
  Result := tokenUnknown;
  Str := FNextChar; // this is already begin of comments
  I := 0;
  while I < FComments.Count do
  begin
    CommentBegin := LeftPart(FComments[I], ',');
    P := AnsiPos(Str, CommentBegin);
    if P = 1 then
    begin
      GetNextChar; // this char has played
      if Length(Str) = Length(CommentBegin) then
      begin
        CommentEnd := copy(FComments[I], Length(CommentBegin) + 2, MaxInt);
        if CommentEnd = '' then
          CommentEnd := chrEOLN;
        MatchCount := 0;
        Len := Length(CommentEnd);

        // We now need to skeep comments till CommentEnd
        while (FNextChar <> #0) and (MatchCount < Len) do
        begin
          AddTokenChar(FNextChar);
          if FNextChar = CommentEnd[MatchCount + 1] then
            Inc(MatchCount)
          else begin
            MatchCount := 0;
            if FNextChar = CommentEnd[1] then
              MatchCount := 1;
          end;
          if FNextChar = chrEOLN then
          begin
            Inc(FLineNo);
            FLinePos := 1;
          end;
          GetNextChar;
        end;
        if MatchCount = Len then
          Dec(FTokenLen, Len);
        Result := tokenComment;
        exit;
      end else
        Str := Str + FNextChar;
      // Restart search
      I := 0;
    end else
      Inc(I);
  end;
end;

function TSQLLexer.GetNextToken: Integer;
var
  IsTerm, DotDelimitedID: Boolean;
  PrevChar: Char;
begin
  Result := tokenUnknown;
  if FTokenID = tokenEOLN then
  begin
    Inc(FLineNo);
    FLinePos := 1;
  end;
  // Skip delimiters
  while CharInSet(FNextChar, setDelimiters) do
    GetNextChar;

  DotDelimitedID := False;
  FToken := '';
  FTokenLen := 0;
  FTokenBeginPos := Position - 1;
  repeat
    case FNextChar of
      #0: Result := tokenEOF; // End of file
      chrEOLN:
      begin
        // End of line
        GetNextChar;
        Result := tokenEOLN;
      end;
      '''': Result := ExtractQuotedToken('''', tokenLiteral);
      '`': Result := ExtractQuotedToken('`', tokenIdentifier);
      '"': Result := ExtractQuotedToken('"', tokenIdentifier); // extract after .
      '[': Result := ExtractQuotedToken(']', tokenIdentifier);
      ';', ',': begin
        if FNextChar = FTerm then
          Result := tokenTerm
        else Result := Ord(FNextChar);
        GetNextChar;
      end
      else begin
        // Extract comments
        PrevChar := #0;
        if CharInSet(FNextChar, FCommentChars) and not DotDelimitedID then
        begin
          PrevChar := FNextChar;
          Result := ParseComments;
          if Result <> tokenComment then
            Result := Ord(PrevChar)
          else PrevChar := #0;
        end else if CharInSet(FNextChar, setSpecialChars) and not DotDelimitedID then
        begin
          if FNextChar = FTerm then
            Result := tokenTerm
          else Result := Ord(FNextChar);
          GetNextChar;
        end;

        if (Result = tokenUnknown) or (Result = tokenIdentifier) or (Result = tokenToken) or CharInSet(PrevChar, FCommentChars) then
        begin
          IsTerm := FNextChar = FTermChar;
          if PrevChar > #0 then
            AddTokenChar(PrevChar);
          while CharInSet(FNextChar, setTokenChars - FCommentChars) and (IsTerm or (FNextChar <> FTermChar)) do
          begin
            AddTokenChar(FNextChar);
            GetNextChar;
            if FNextChar = '.' then break;
          end;
          if not DotDelimitedID then
          begin
            FToken := copy(FTokenBuf, 1, FTokenLen);
            if (FTerm <> '') and AnsiSameText(FToken, FTerm) then
              Result := tokenTerm
            else if (FLiteralPrefixes.IndexOf(FToken) >= 0) and (FNextChar = '''') then
              Result := ExtractQuotedToken('''', tokenLiteral)
            else if (FToken <> '') and (FToken[1] = '#') then
              Result := tokenLiteral
            else Result := tokenToken;
          end;
        end;
      end;
    end;

    DotDelimitedID := ((Result = tokenIdentifier) or (Result = tokenToken)) and (FNextChar = '.');
    if DotDelimitedID then
    begin
      AddTokenChar('.');
      Result := tokenIdentifier;
      GetNextChar;
      if CharInSet(FNextChar, [#0..#32, '''', ';', ',', '(', ')']) then break;
    end;
  until not DotDelimitedID;

  FTokenID := Result;
  if FTokenID = tokenTerm then
    FToken := FTerm
  else if (Result > 32) and (Result < 255) then
    FToken := Char(Result)
  else FToken := copy(FTokenBuf, 1, FTokenLen);
end;

procedure TSQLLexer.AddTokenChar(const C: Char);
begin
  if FTokenLen >= FTokenBufSize then
  begin
    Inc(FTokenBufSize, 255);
    SetLength(FTokenBuf, FTokenBufSize);
  end;
  Inc(FTokenLen);
  FTokenBuf[FTokenLen] := C;
end;

procedure TSQLLexer.SetTerm(const Value: String);
begin
  if FTerm <> Value then
  begin
    FTerm := Value;
    if (FTerm <> '') and CharInSet(FTerm[1], setTokenChars)
      and not CharInSet(FTerm[1], setAlphaNum)
    then
      FTermChar := FTerm[1]
    else FTermChar := #0;
  end;
end;

procedure TSQLLexer.ParsePragmaComments(const Pragma: String);
var
  SP: Integer;
  PropName, PropValue: String;
begin
  // Engine: DBISAM; Comments: "//"; Delimiter: ";"; SetTerm: "SET TERM";
  SP := 1;
  while (SP <= Length(Pragma)) do
  begin
    PropName := Trim(NextToken(Pragma, ':', SP));
    PropValue := Trim(NextToken(Pragma, ';', SP));
    if AnsiSameText(PropName, 'comments') then
      Comments := PropValue
    else if AnsiSameText(PropName, 'delimiter') then
      Term := PropValue
    else if AnsiSameText(PropName, 'setterm') then
      SetTermCommand := PropValue
    else if Assigned(OnPragma) then
      OnPragma(Self, Pragma);
  end;
end;

function TSQLLexer.NextStatement(var Statement: String): Boolean;
var
  CmdPos, StartPos, EndPos: Integer;
  Matching, FirstToken: Boolean;
begin
  if TokenID = tokenUnknown then
    GetNextToken;
  if TokenID = tokenEOF then
  begin
    Statement := '';
    Result := False;
    exit;
  end;
  CmdPos := 1;
  StartPos := TokenBeginPos;
  EndPos := -1;
  FirstToken := True;
  while (TokenID <> tokenEOF) and (TokenID <> tokenTerm) do
  begin
    try
      if FirstToken and (FSetTermCommand <> '') and (TokenID = tokenToken) and MatchText(Token, FSetTermCommand, CmdPos) then
      begin
        Matching := True;
        GetNextToken;
        while (TokenID = tokenToken) and Matching and (CmdPos <= Length(FSetTermCommand)) do
        begin
          Matching := MatchText(Token, FSetTermCommand, CmdPos);
          GetNextToken;
        end;
        if CmdPos > Length(FSetTermCommand) then
        begin
          // Process set term
          Term := Token;
          GetNextToken;
          while (TokenID <> tokenEOLN) and (TokenID <> tokenEOF) do
            GetNextToken;
          if TokenID = tokenEOF then break;
          StartPos := TokenBeginPos;
        end else
          FirstToken := False;
      end else if (TokenID = tokenComment) then
      begin
        // Parse first comments
        FToken := Trim(FToken);
        if copy(Token, 1, 2) = '##' then
          ParsePragmaComments(copy(Token, 3, MaxInt))
        else if AnsiPos('GO ', FToken) = 1 then
        begin
          EndPos := TokenBeginPos;
          GetNextToken;
          break;
        end;
      end else if TokenID >= tokenToken then
        FirstToken := False;
    finally
      GetNextToken;
      if FirstToken then
        StartPos := TokenBeginPos;
    end;
  end;
  if EndPos < 0 then
    if TokenID = tokenTerm then
    begin
      EndPos := TokenBeginPos;
      GetNextToken
    end else if TokenID = tokenEOF then
      EndPos := Position;
  Result := (TokenID <> tokenEOF) or (EndPos > StartPos);
  if Result then
    ExtractBlock(StartPos, EndPos, Statement);
  Statement := Trim(Statement);
end;

function TSQLLexer.GetLexerState: TSQLLexerState;
begin
  Result.Position := Position;
  Result.TokenBeginPos := TokenBeginPos;
  Result.Term := Term;
  Result.SetTermCommand := SetTermCommand;
  Result.LineNo := LineNo;
  Result.LinePos := LinePos;
  Result.NextChar := NextChar;
  Result.Token := Token;
  Result.TokenID := TokenID;
end;

procedure TSQLLexer.SetLexerState(const AState: TSQLLexerState);
begin
  FTokenBeginPos := AState.TokenBeginPos;
  FTerm := AState.Term;
  FSetTermCommand := AState.SetTermCommand;
  FLineNo := AState.LineNo;
  FLinePos := AState.LinePos;
  FNextChar := AState.NextChar;
  FToken := AState.Token;
  FTokenID := AState.TokenID;
  FStream.Position := AState.Position;
  FBufPos := 1;
  FBufLen := 0;
  ReadBuffer;
end;

end.
