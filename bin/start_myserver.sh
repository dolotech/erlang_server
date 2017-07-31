#!/bin/sh

cd ..
escript src/pt_tool.erl
erl -pa ebin -smp enable -config rel/files/sys -args_file rel/files/vm.args -s myserver start
cd bin
pause
