@echo off
rem Ë½³×Â·¾¶
@set ppk_path=/work/tools/rolong_rsa.ppk
@set code_path=\work\myserver\src\data

pscp.exe -2 -4 -i "%ppk_path%" root@42.121.111.191:/data/myserver/src/data/data* %code_path%
pause
