#!/usr/bin/env bash

USER_NAME=snakes
HOST=`LC_ALL=C ifconfig  | grep 'inet addr:'| grep -v '127.0.0.1' | cut -d: -f2 | awk '{ print $1}'`

defPath=src/auto/def/
defFile=src/auto/def/def.hrl
protoPath=src/auto/proto/
protoFile=src/auto/proto/all_proto.hrl
mysqlPath=src/auto/mysql
mysqlFile=src/auto/mysql/mysql_tab_record.hrl

show_help()
{
    echo
    "Usage:
    -h | -help show help
    st
    t
    default show info"
}

create_hrl()
{
    if [ ! -f "$defFile" ]; then
    mkdir -p "$defPath"
	touch "$defFile"
	fi
	if [ ! -f "$protoFile" ]; then
	mkdir -p "$protoPath"
	touch "$protoFile"
	fi
	if [ ! -f "$mysqlFile" ]; then
	mkdir -p "$mysqlPath"
	touch "$mysqlFile"
	fi
}

close()
{
    local PID=$(ps aux | grep $USER_NAME@$HOST | grep -v grep | awk '{print($2)}')
    kill $PID
    echo $PID closed
}


show_info()
{
    ps aux | grep $USER_NAME@$HOST | grep -v grep
}

mkdir -p log
if [ "-$2" = "-" ]
then
    num="_192.168.120.53_8088"

elif [ "-$2" = "-1" ]
then
    num="_192.168.120.53_8088"

elif [ "-$2" = "-2" ]
then
    num="_192.168.120.53_8089"

else
    num="_$2"
fi

echo ${num}

case $1 in
    '-h'|'--help')
        show_help ;;

    'st' | 'start')
        erl -config priv/etc/sys${num} -config priv/etc/test/sys_dev -args_file priv/etc/vm${num}.args  -args_file priv/etc/test/vm_dev.args;;

    'ct')
        erl  -pa ebin/ deps/*/ebin apps/*/ebin -s t_player_server start_link;;

    'mysql')
        erl -pa ebin/ deps/*/ebin apps/*/ebin -config priv/etc/sys -s emysql_extra main -s erlang halt;;

    'test' )
        erl -config priv/etc/test/sys${num} -config priv/etc/test/sys_dev -args_file priv/etc/test/vm${num}.args  -args_file priv/etc/test/vm_dev.args;;

    'create_hrl')
        create_hrl;;

    'sh')
        erl -pa ebin apps/*/ebin deps/*/ebin -setcookie f8743d19661dc53699c8b0079361a2fb -name snakes_game_debug@127.0.0.1 -remsh snakes_8088@121.199.30.213;;

    'bg')
        erl -config priv/etc/pro/sys${num} -config priv/etc/pro/sys_pro -args_file priv/etc/pro/vm${num}.args -args_file priv/etc/pro/vm_pro.args -noshell -noinput -detached;;

     'screen')
        erl -config priv/etc/pro/sys${num} -config priv/etc/pro/sys_pro -args_file priv/etc/pro/vm${num}.args -args_file priv/etc/pro/vm_pro.args;;


    'cl'| 'top')
        close ;;

    *)
        show_info ;;
esac
