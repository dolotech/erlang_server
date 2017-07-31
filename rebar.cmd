@echo off
setlocal
set rebarscript=%~f0
rem escript.exe "%rebarscript:.cmd=%i" %*
escript.exe "rebar"  %*
