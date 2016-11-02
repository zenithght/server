@echo off
set %cd% 

start werl -config priv/etc/sys -args_file priv/etc/vm.args -args_file priv/etc/test/vm_dev.args

EXIT
