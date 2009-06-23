@echo off

touch source\*.*

cd packages

SET package=dbExtPkg
SET dpath=e:\Programs\Borland
set include=e:\projects\Context\Common\source;

set buildto=..\lib\d2009
"%dpath%\..\CodeGear\RAD Studio\6.0\bin\dcc32.exe" -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2009.dpk

cd ..