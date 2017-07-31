@echo off
taskkill /F /IM werl.exe

start werl -pa ebin -smp enable -config rel/files/sys -args_file rel/files/vm.args -s myserver start

pause
