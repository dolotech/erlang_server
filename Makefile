.SUFFIXES:
.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<
ERL = erl -boot start_clean 

all:
	./rebar  compile

eunit:
	./rebar compile eunit

ctags:
	ctags -R .

#compile: ${FILES:%=%.beam}
#	@echo "-OK-"
#

start:
	erl -pa ebin -smp enable -config rel/files/sys -args_file rel/files/vm.args -s myserver start

# download data
# (c)7900-ffa
# ssh-copy-id -i /root/.ssh/rolong_rsa.pub root@42.121.111.191
# ssh -i /root/.ssh/rolong_rsa 'root@42.121.111.191'
# dl:
# 	scp -i ~/work/mystore/key/rolong_rsa root@42.121.111.191:/data/myserver/src/data/* src/data/
# 	@echo "Download data ok!"
# 
up:
	svn up ~/work/策划/
	svn up ~/work/myserver/
	svn up ~/work/webtool/

aliyun:
	ssh root@42.121.111.191 -i /Users/rolong/work/mystore/key/rolong_rsa

send:
	scp -i ~/work/mystore/key/rolong_rsa ~/work/code/hammer.zip root@42.121.111.191:~

clean:	
	./rebar clean
