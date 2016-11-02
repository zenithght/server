All_ERL = `find -L src/ -name '*.erl'`
DATE=`date +%F_%H_%M`
GAME_TAR=snakes_$(DATE).tar.gz


make:
	##./start.sh create_hrl
	./rebar co


    ##erlc -o ebin/ c_src/map_math_nif.erl
    ##gcc -fPIC -shared -o priv/map_math_nif.so c_src/map_math_nif.c -I /usr/local/lib/erlang/usr/include/ -std=c99


cl:
	./rebar clean


rebuild:
	#rm -rf deps/*/ebin/*.beam;
	#rm -rf deps/*/.rebar/
	rm -rf apps/*/ebin/*.beam;
	rm -rf apps/*/.rebar/
	rm -rf ebin/
	./rebar clean
	make auto
	make


releases:
	rm -rf rel/snakes;
	cd rel && ../rebar generate;


ssh:
	ssh root@115.159.70.142

rar:
	tar -zcf ${GAME_TAR} --exclude=deps/jiffy start.sh ebin/ priv/ deps/*/ebin apps/*/ebin apps/*/priv apps/*/start.sh

scp:
	scp ${GAME_TAR} root@115.159.70.142:~/project/


def:
	escript apps/parse_tool/t_def_2 apps/parse_tool/priv/t_yecc_json.yrl priv/def/ src/auto/def/ priv/docroot/api/


proto:
	escript apps/parse_tool/t_proto_2 apps/parse_tool/priv/t_yecc_json.yrl priv/proto/ src/auto/proto/ priv/docroot/api/


mysql:
	./start.sh mysql


auto:
	rm -rf src/auto
	mkdir src/auto
	make def
	make proto
#	make mysql



