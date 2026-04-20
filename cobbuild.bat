@echo off
REM Wrapper til GnuCOBOL på Windows
REM Sætter include- og lib-stier automatisk

set COBINC=C:\GnuCobol\include
set COBLIB=C:\GnuCobol\lib
set COB_COPY_DIR=Copybooks

REM Tilføj bin-mappen til PATH midlertidigt
set PATH=C:\GnuCobol\bin;%PATH%

REM Kald cobc med de rigtige flags
cobc -I"%COBINC%" -L"%COBLIB%" -x "src/%*" -o "build/%*"
pause
