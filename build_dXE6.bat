@echo off

touch source\*.*

cd packages

SET package=dbExtPkg
SET dpath=e:\programs\Embarcadero
set include=e:\projects\Context\Common\source;
set ns=Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi;Data.Win;Bde;System.Win;IBX

set buildto=..\lib\dXE6

"%dpath%\RAD Studio\14.0\bin\dcc32.exe" -JL -NS%ns% -I%include% -U%include% -LE%buildto% -LN%buildto% -N%buildto% -B %package%dXE6.dpk

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


