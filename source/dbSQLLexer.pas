(******************************************************************************)
(*
(*  Context Database Extensions Suite
(*
(*  Contains: TSQLLexer class implementing generic SQL lexer & parser.
(*
(*  Copyright (c) 2004-2007 Michael Baytalsky
(*
(*  ------------------------------------------------------------
(*  FILE        : dbSQLLexer.pas
(*  AUTHOR(S)   : Michael Baytalsky (mike@contextsoft.com)
(*  VERSION     : 2.15
(*  DELPHI\BCB  : Delphi 5,6,7,2005,2006, 2007; C++Builder 6.0, 2006
(*
(******************************************************************************)
unit dbSQLLexer;

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
  TSQLLexer = class
  protected
    FStream: TStream;
    FBuffer: AnsiString;
    FBufPos: Integer;
    FBufSize: Integer;
    FBufLen: Integer;
    FNextChar: AnsiChar;
    FToken: AnsiString;
    FTokenID: Integer;
    FLinePos: Integer;
    FLineNo: Integer;
    FTokenBeginPos: Integer;
    FTokenLen: Integer;
    FTokenBuf: AnsiString;
    FTokenBufSize: Integer;

    FComments: TStringList;
    FLiteralPrefixes: TStringList;
    FCommentChars: set of AnsiChar;
    FTermChar: AnsiChar;
    FOwnStream: Boolean;
    FTerm: AnsiString;
    FSetTermCommand: AnsiString;

    procedure SetTerm(const Value: AnsiString);
    function GetComments: AnsiString;
    function GetPrefixes: AnsiString;
    procedure SetComments(const Value: AnsiString);
    procedure SetPrefixes(const Value: AnsiString);
    procedure ReadBuffer;
    function GetPosition: Integer;
    function ExtractQuotedToken(const Quote: AnsiChar; ATokenID: Integer): Integer;
    function ParseComments: Integer;
    procedure AddTokenChar(const C: AnsiChar);
  public
    constructor Create(AStream: TStream; BufSize: Integer = 1024); overload;
    constructor Create(const SQLString: AnsiString); overload;
    destructor Destroy; override;

    procedure GetNextChar;
    function GetNextToken: Integer;

    // Reads next statement and returns false if end of script is reached
    function NextStatement(var Statement: AnsiString): Boolean;

    procedure ExtractBlock(StartPos, EndPos: Integer; var Buffer: AnsiString);
    procedure ParsePragmaComments(const Pragma: AnsiString);

    property Stream: TStream read FStream;
    property Position: Integer read GetPosition;
    property TokenBeginPos: Integer read FTokenBeginPos;

    property Comments: AnsiString read GetComments write SetComments;
    property LiteralPrefixes: AnsiString read GetPrefixes write SetPrefixes;
    property Term: AnsiString read FTerm write SetTerm;
    property SetTermCommand: AnsiString read FSetTermCommand write FSetTermCommand;

    property LineNo: Integer read FLineNo;
    property LinePos: Integer read FLinePos;
    property NextChar: AnsiChar read FNextChar;
    property Token: AnsiString read FToken;
    property TokenID: Integer read FTokenID;
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
  setSpecialChars = ['''','"','`',';',',','+','[',']','(',')','=']; // ,'.'
  setTokenChars = [#33..#255] - setSpecialChars;
  setAlphaNum = ['A'..'Z', 'a'..'z', '_', '0'..'9'];

  DefaultCommentChars = '--;/*,*/;//';

resourcestring
  SUnexpectedEndOfToken = 'Unexpected end of token';

implementation

{$IFnDEF VER200}
  {$I D2009.inc}
{$ENDIF}

{ Helper Routines }

function MatchText(SubStr, Str: AnsiString; var SP: Integer): Boolean;
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

function NextToken(const Str: AnsiString; Delimiter: AnsiChar; var SP: Integer): AnsiString;
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

function GetDelimitedText(Strings: TStrings): AnsiString;
const
  ctQuoteChar = '"';
  ctDelimiter = ';';
var
  S: String;
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

procedure SetDelimitedText(Strings: TStrings; const Value: String);
const
  ctQuoteChar = '"';
  ctDelimiter = ';';
var
  P, P1: PChar;
  S: String;
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

function LeftPart(const Str, Delimiter: AnsiString): AnsiString;
var
  P: Integer;
begin
  P := AnsiPos(Delimiter, Str);
  if P <= 0 then
    Result := Str
  else Result := copy(Str, 1, P - 1);
end;

function RightPart(const Str, Delimiter: AnsiString): AnsiString;
var
  P: Integer;
begin
  P := AnsiPos(Delimiter, Str);
  if P <= 0 then
    Result := ''
  else Result := copy(Str, P + Length(Delimiter), MaxInt);
end;

{ TSQLLexer }

constructor TSQLLexer.Create(AStream: TStream; BufSize: Integer = 1024);
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
  Comments := DefaultCommentChars;
  FTerm := '';
  FSetTermCommand := '';
  FTermChar := #0;
  FLiteralPrefixes := TStringList.Create;
  FLinePos := 1;
  FLineNo := 1;
  FBufPos := 1;
  FTokenID := tokenUnknown;
  ReadBuffer;
  GetNextChar;
end;

constructor TSQLLexer.Create(const SQLString: AnsiString);
begin
  Create(TStringStream.Create(SQLString));
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

function TSQLLexer.GetComments: AnsiString;
begin
  Result := GetDelimitedText(FComments);
end;

function TSQLLexer.GetPrefixes: AnsiString;
begin
  Result := FLiteralPrefixes.CommaText;
end;

procedure TSQLLexer.SetComments(const Value: AnsiString);
var
  I: Integer;
begin
  SetDelimitedText(FComments, Value);
  FCommentChars := [];
  I := 0;
  while I < FComments.Count do
    if FComments[I] <> '' then
    begin
      Include(FCommentChars, AnsiString(FComments[I])[1]);
      Inc(I);
    end else
      FComments.Delete(I);
end;

procedure TSQLLexer.SetPrefixes(const Value: AnsiString);
begin
  FLiteralPrefixes.CommaText := Value;
end;

procedure TSQLLexer.ReadBuffer;
begin
  FBufLen := FStream.Read(FBuffer[1], FBufSize);
  FBufPos := 1;
end;

function TSQLLexer.GetPosition: Integer;
begin
  Result := FStream.Position - FBufLen + FBufPos - 1;
end;

procedure TSQLLexer.GetNextChar;
begin
  if FBufPos <= FBufLen then
  begin
    FNextChar := FBuffer[FBufPos];
    Inc(FBufPos);
    Inc(FLinePos);
  end else begin
    ReadBuffer;
    if FBufPos <= FBufLen then
    begin
      FNextChar := FBuffer[FBufPos];
      Inc(FBufPos);
      Inc(FLinePos);
    end else FNextChar := #0;
  end;
end;

procedure TSQLLexer.ExtractBlock(StartPos, EndPos: Integer; var Buffer: AnsiString);
var
  SavePos: Integer;
begin
  SavePos := Stream.Position;
  try
    Stream.Position := StartPos;
    SetLength(Buffer, EndPos - StartPos);
    if Length(Buffer) > 0 then
      Stream.Read(Buffer[1], EndPos - StartPos);
  finally
    Stream.Position := SavePos;
  end;
end;

function TSQLLexer.ExtractQuotedToken(const Quote: AnsiChar; ATokenID: Integer): Integer;
begin
  Result := ATokenID;
  AddTokenChar(FNextChar);
  GetNextChar;
  repeat
    if FNextChar = Quote then
    begin
      AddTokenChar(FNextChar);
      GetNextChar;
      if not ((Quote in ['''', '"', '`']) and (FNextChar = Quote)) then break;
    end;
    if FNextChar = #0 then
      raise Exception.Create(SUnexpectedEndOfToken);
    AddTokenChar(FNextChar);
    if FNextChar = #10 then
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
  CommentBegin, CommentEnd, Str: AnsiString;
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
          CommentEnd := #13;
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
          if FNextChar = #10 then
          begin
            Inc(FLineNo);
            FLinePos := 1;
          end;
          GetNextChar;
        end;
        if (CommentEnd = #13) and (FNextChar = #10) then
        begin
          GetNextChar;
          Inc(FLineNo);
          FLinePos := 1;
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
  PrevChar: AnsiChar;
begin
  Result := tokenUnknown;
  if FTokenID = tokenEOLN then
  begin
    Inc(FLineNo);
    FLinePos := 1;
  end;
  // Skip delimiters
  while FNextChar in setDelimiters do
    GetNextChar;

  DotDelimitedID := False;
  FToken := '';
  FTokenLen := 0;
  FTokenBeginPos := Position - 1;
  repeat
    case FNextChar of
      #0: Result := tokenEOF; // End of file
      #10, #13: begin
        // End of line
        GetNextChar;
        if FNextChar = #10 then
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
        if (FNextChar in FCommentChars) and not DotDelimitedID then
        begin
          PrevChar := FNextChar;
          Result := ParseComments;
          if Result <> tokenComment then
            Result := Ord(PrevChar);
        end else if (FNextChar in setSpecialChars) and not DotDelimitedID then
        begin
          if FNextChar = FTerm then
            Result := tokenTerm
          else Result := Ord(FNextChar);
          GetNextChar;
        end else
        begin
          IsTerm := FNextChar = FTermChar;
          while (FNextChar in setTokenChars - FCommentChars) and (IsTerm or (FNextChar <> FTermChar)) do
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
      if FNextChar in [#0..#32, '''', ';', ',', '(', ')'] then break;
    end;
  until not DotDelimitedID;

  FTokenID := Result;
  if FTokenID = tokenTerm then
    FToken := FTerm
  else if (Result > 32) and (Result < 255) then
    FToken := Char(Result)
  else FToken := copy(FTokenBuf, 1, FTokenLen);
end;

procedure TSQLLexer.AddTokenChar(const C: AnsiChar);
begin
  if FTokenLen >= FTokenBufSize then
  begin
    Inc(FTokenBufSize, 255);
    SetLength(FTokenBuf, FTokenBufSize);
  end;
  Inc(FTokenLen);
  FTokenBuf[FTokenLen] := C;
end;

procedure TSQLLexer.SetTerm(const Value: AnsiString);
begin
  if FTerm <> Value then
  begin
    FTerm := Value;
    if (FTerm <> '') and (FTerm[1] in setTokenChars) and not (FTerm[1] in setAlphaNum) then
      FTermChar := FTerm[1]
    else FTermChar := #0;
  end;
end;

procedure TSQLLexer.ParsePragmaComments(const Pragma: AnsiString);
var
  SP: Integer;
  PropName, PropValue: AnsiString;
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
      SetTermCommand := PropValue;
  end;
end;

function TSQLLexer.NextStatement(var Statement: AnsiString): Boolean;
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
    GetNextToken;
    if FirstToken then
      StartPos := TokenBeginPos;
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

end.
