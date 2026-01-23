@echo off
rem Author:  Brian Lynn, GEM, for ISDA
rem Oct 2015
rem Copyright (c) 2002-2015
rem
rem Convert products to a generic regulatory reporting product
rem
rem Usage:
rem 	rpt-convert-to-reg-disclosure [targdir]
rem
rem E.g.:
rem


echo ***** convert to generic reporting product

if not "%1" == "" set TARGET= %1

call env.bat recordkeeping
:generate

rem set SRC=%SCRIPTTMPDIR%
set SRC=intermediate
rem echo SRC1 is %SRC%

del /q %SRC%\*.*~

if NOT EXIST %SRC% goto notfound
rem set DEST=intermediate\reg-report-full-product
set DEST=%TARGET%\full-product

if EXIST %DEST% goto conv
MD %DEST%

:conv
echo Converting to generic reporting product [%SRC%] 

FOR %%I IN (%SRC%\*.xml) DO call rpt-conv-reg-disclosure %SRC% %DEST% %%~nI 



goto end

:skip
echo Skipping %SRC% in %SRCVIEW%
goto end

:notfound
echo Input %SRC% not found

:end
