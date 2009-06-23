@echo off

touch source\*.*

cd packages

SET package=dbExtPkg
SET dpath=E:\programs\Borland
set include=e:\projects\Context\Common\source;

set buildto=..\lib\c2009
"%dpath%\BDS\4.0\Bin\dcc32.exe" -JPHNE -NO%buildto% -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%C2009.dpk
"%dpath%\BDS\4.0\Bin\dcc32.exe" -JL -NO%buildto% -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%C2009.dpk

move ..\source\*.hpp %buildto%
move ..\source\*.obj %buildto%
move ..\source\designtime\*.hpp %buildto%
move ..\source\designtime\*.obj %buildto%
move *.bpi %buildto%
move *.bpl %buildto%
move *.lib %buildto%
move *.obj %buildto%
move *.hpp %buildto%

del %buildto%\*.dcu
del *.lsp
del %buildto%\CtxGridView.*
del %buildto%\CtxProfiles.*
del %buildto%\CtxPropView.*

cd ..
