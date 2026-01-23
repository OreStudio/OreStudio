rem @echo off
rem Author:  Brian Lynn, GEM, for ISDA
rem Oct 2015
rem Copyright (c) 2002-2015
rem
rem add references to a recordkeeping view example
rem from one with none.
rem
rem usage:
rem rpt-pre [sourcedir] [destdir] [filename]

call env.bat
set SRC=%1
set DESTDIR=%2
set FN=%3

set DESTFN=%FN%

echo Generate ver %SRC%\%FN% to %DESTDIR%\%DESTFN%, view=%VIEW%

%SAXON8%  -o %DESTDIR%\%DESTFN%.xml %SRC%/%FN%.xml %SCRIPTDIR%\recordkeeping-preprocess.xsl 

if errorlevel 1 del  %DESTDIR%\%DESTFN%.xml 
