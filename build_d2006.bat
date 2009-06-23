@echo off

touch source\*.*

cd packages

SET package=dbExtPkg
SET dpath=e:\Programs\Borland
set include=E:\projects\Context\Common\source;

set buildto=..\lib\d2006
"%dpath%\BDS\4.0\Bin\dcc32.exe" -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2006.dpk


cd ..