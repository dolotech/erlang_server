#!/bin/sh
cd ..
escript src/pt/pt_tool.erl
escript rebar  compile
cd bin
