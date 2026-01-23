@echo off
rem Author:  Brian Lynn, GEM, for ISDA
rem Oct 2015
rem Copyright (c) 2002-2015
rem
rem Convert reg reports from full product to flattenened product infom
rem
rem Usage:
rem 	rpt-convert-regrort-to-flat-product [targdir]
rem
rem E.g.:
rem


echo ***** convert to flat product

if not "%1" == "" set TARGET= %1

call env.bat recordkeeping
:generate

set SRC=%TARGET%\full-product
rem echo SRC1 is %SRC%

del /q %SRC%\*.*~

if NOT EXIST %SRC% goto notfound
rem set DEST=intermediate\reg-report-full-product
rem set DEST=xml\reg-reporting\reg-report-flat-product
set DEST=%TARGET%\flat-product

if EXIST %DEST% goto conv
MD %DEST%

:conv
echo Converting to generic reporting product [%SRC%] 

FOR %%I IN (%SRC%\*.xml) DO call rpt-conv-regrpt-to-flat-prod %SRC% %DEST% %%~nI 



goto end

:skip
echo Skipping %SRC% in %SRCVIEW%
goto end

:notfound
echo Input %SRC% not found

:end
