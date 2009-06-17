@echo off

cd packages

SET package=dbExtPkg
SET dpath=e:\programs\Borland
set include=e:\projects\Context\Common\source;

set buildto=..\lib\d5
"%dpath%\Delphi5\Bin\dcc32.exe" -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D5.dpk

set buildto=..\lib\d6
"%dpath%\Delphi6\Bin\dcc32.exe" -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D6.dpk

set buildto=..\lib\d7
"%dpath%\Delphi7\Bin\dcc32.exe" -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D7.dpk

set buildto=..\lib\d2005
"%dpath%\BDS\3.0\Bin\dcc32.exe" -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2005.dpk

set buildto=..\lib\d2006
"%dpath%\BDS\4.0\Bin\dcc32.exe" -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2006.dpk

set buildto=..\lib\d2007
"C:\Program Files\CodeGear\RAD Studio\5.0\bin\dcc32.exe" -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2007.dpk

cd ..
