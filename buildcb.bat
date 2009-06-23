@echo off

cd packages

SET package=dbExtPkg
set buildto=..\lib\cb6

e:\programs\Borland\CBuilder6\Bin\make.exe -B -f %package%CB6.mak

move ..\source\*.hpp %buildto%
move ..\source\*.obj %buildto%
move ..\source\designtime\*.hpp %buildto%
move ..\source\designtime\*.obj %buildto%
move *.bpi %buildto%
move *.bpl %buildto%
move *.lib %buildto%
move *.obj %buildto%
del ..\source\*.dcu
del ..\source\designtime\*.dcu
del *.drc
del *.tds
del E:\projects\Context\Common\source\*.hpp
del E:\projects\Context\Common\source\*.obj
del %buildto%\CtxGridView.*
del %buildto%\CtxProfiles.*
del %buildto%\CtxPropView.*


cd ..


