@echo off
rem Author:  Brian Lynn, GEM, for ISDA
rem Oct 2015
rem Copyright (c) 2002-2015
rem
rem convert a product to a generic regulatory reporting product
rem
rem usage:
rem rpt-conv-reg-report [sourcedir] [destdir] [filename]

call env.bat recordkeeping
set SRC=%1
set DESTDIR=%2
set FN=%3

set DESTFN=%FN%

echo Generate ver %SRC%\%FN% to %DESTDIR%\%DESTFN% with xpathdir = "%XPATHDIR%"

%SAXON8%  -o %DESTDIR%\%DESTFN%.xml %SRC%/%FN%.xml %SCRIPTDIR%\recordkeeping-convert-to-flat-product.xsl xpathdir="%XPATHDIR%"

if errorlevel 1 del  %DESTDIR%\%DESTFN%.xml 
