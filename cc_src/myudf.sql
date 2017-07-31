drop function show_item;
drop function show_hero;
create function show_item returns string soname 'myudf.so';
create function show_hero returns string soname 'myudf.so';
