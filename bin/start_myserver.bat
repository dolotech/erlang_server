@echo off
taskkill /F /IM werl.exe
c:
cd \work\myserver
start werl -pa ebin -smp enable -config rel/files/sys -args_file rel/files/vm.args -s myserver start
cd bin
