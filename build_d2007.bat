@echo off

touch source\*.*

cd packages

SET package=dbExtPkg
SET dpath=e:\Programs\Borland
set include=E:\projects\Context\Common\source;

set buildto=..\lib\d2007
"%dpath%\..\CodeGear\RAD Studio\5.0\bin\dcc32.exe" -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2007.dpk

cd ..