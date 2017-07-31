all:
	gcc -fPIC -bundle -flat_namespace -undefined suppress -o ../priv/myudf.so myudf.c -I/usr/local/mysql/include
	sudo cp ../priv/myudf.so /usr/local/mysql/lib/plugin/
	mysql -uroot -p`cat ~/.mysqlroot |grep "mysqlroot_passwd"|cut -f 2 -d "="` -f < ./myudf.sql

linux:
	gcc -fPIC -shared -o ../priv/myudf.so myudf.c -I/usr/local/mysql/include
	cp ../priv/myudf.so /usr/local/mysql/lib/plugin/
	mysql -uroot -p`cat ~/.mysqlroot |grep "mysqlroot_passwd"|cut -f 2 -d "="` -f < ./myudf.sql

clean:
	rm ./testnif.o
