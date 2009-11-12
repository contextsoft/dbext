@echo off

touch source\*.*

cd packages

SET package=dbExtPkg
SET dpath=e:\programs\Embarcadero
set include=e:\projects\Context\Common\source;

set buildto=..\lib\d2010

"%dpath%\RAD Studio\7.0\bin\dcc32.exe" -JL -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2010.dpk

move ..\source\*.hpp %buildto%
move ..\source\*.obj %buildto%
move ..\source\designtime\*.hpp %buildto%
move ..\source\designtime\*.obj %buildto%
move *.bpi %buildto%
move *.bpl %buildto%
move *.lib %buildto%
move *.obj %buildto%
move *.hpp %buildto%

del %buildto%\CtxGridView.*
del %buildto%\CtxPropView.*

cd ..


