@echo off

touch source\*.*

cd source\packages

SET package=DBExtPkg
SET dpath=E:

SET buildto=..\..\libt\d5
"%dpath%\D5\Bin\dcc32.exe" -DCTX_TRIAL -LE%buildto% -LN%buildto% -N%buildto% -B %package%D5.dpk
set buildto=..\..\lib\d5
"%dpath%\D5\Bin\dcc32.exe" -LE%buildto% -LN%buildto% -N%buildto% -B %package%D5.dpk

if "%1"=="5" goto _exit

set buildto=..\..\libt\d6
"%dpath%\D6\Bin\dcc32.exe" -DCTX_TRIAL -LE%buildto% -LN%buildto% -N%buildto% -B %package%D6.dpk
set buildto=..\..\lib\d6
"%dpath%\D6\Bin\dcc32.exe" -LE%buildto% -LN%buildto% -N%buildto% -B %package%D6.dpk

if "%1"=="6" goto _exit

set buildto=..\..\libt\d7
"%dpath%\D7\Bin\dcc32.exe" -DCTX_TRIAL -LE%buildto% -LN%buildto% -N%buildto% -B %package%D7.dpk
set buildto=..\..\lib\d7
"%dpath%\D7\Bin\dcc32.exe" -LE%buildto% -LN%buildto% -N%buildto% -B %package%D7.dpk

SET dpath=E:\Program Files\Borland

set buildto=..\..\libt\d2005
"%dpath%\BDS\3.0\Bin\dcc32.exe" -DCTX_TRIAL -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2005.dpk
set buildto=..\..\lib\d2005
"%dpath%\BDS\3.0\Bin\dcc32.exe" -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2005.dpk

set buildto=..\..\libt\d2006
"%dpath%\BDS\4.0\Bin\dcc32.exe" -DCTX_TRIAL -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2006.dpk
set buildto=..\..\lib\d2006
"%dpath%\BDS\4.0\Bin\dcc32.exe" -LE%buildto% -LN%buildto% -N%buildto% -B %package%D2006.dpk

:_exit

cd ..\..
