@echo off
rem Author:  Brian Lynn, GEM, for ISDA
rem Oct 2015
rem Copyright (c) 2002-2015
rem
rem Extract and add explicit references for reg reporting
rem
rem Usage:
rem 	rpt-preprocess
rem
rem E.g.:
rem


echo ***** Generate Refs  


call env.bat recordkeeping
:generate

set SRC=%SOURCEDIR%\recordkeeping-view-examples\samples
rem echo SRC1 is %SRC%

del /q %SRC%\*.*~

if NOT EXIST %SRC% goto notfound
set DEST=%SCRIPTTMPDIR%

if EXIST %DEST% goto conv
MD %DEST%

:conv
echo Adding references to examples in  [%SRC%] 

FOR %%I IN (%SRC%\*.xml) DO call rpt-pre %SRC% %DEST% %%~nI 



goto end

:skip
echo Skipping %SRC% in %SRCVIEW%
goto end

:notfound
echo Input %SRC% not found

:end
