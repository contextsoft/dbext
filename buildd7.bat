@echo off

cd packages

SET package=dbExtPkg
SET dpath=e:\programs\Borland
set include=E:\projects\Context\Common\source;

set buildto=..\lib\d7
"%dpath%\Delphi7\Bin\dcc32.exe" -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D7.dpk
