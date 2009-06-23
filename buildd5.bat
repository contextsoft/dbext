@echo off

cd packages

SET package=dbExtPkg
SET dpath=e:\programs\Borland
set include=e:\projects\Context\Common\source;

set buildto=..\lib\d5
"%dpath%\Delphi5\Bin\dcc32.exe" -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D5.dpk

cd ..
