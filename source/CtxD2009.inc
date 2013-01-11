{$IFnDEF D2009_ORLATER}
{$IFnDEF VER240}
type
  CharSet = set of AnsiChar;

function CharInSet(C: AnsiChar; const CharSet: CharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}
{$ENDIF}
