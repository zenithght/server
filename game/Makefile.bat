@echo off

rd /q /s ebin

escript apps/parse_tool/t_def_2 apps/parse_tool/priv/t_yecc_json.yrl priv/def/ src/auto/def/ priv/docroot/api/
escript apps/parse_tool/t_proto_2 apps/parse_tool/priv/t_yecc_json.yrl priv/proto/ src/auto/proto/ priv/docroot/api/

escript rebar co

erlc -o ebin/ c_src/map_math_nif.erl

pause