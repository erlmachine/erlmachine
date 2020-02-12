PROJECT = erlmachine
PROJECT_DESCRIPTION = An application level virtual machine based on erlang OTP
PROJECT_VERSION = 0.0.1

DEPS = gen-batch-server rabbitmq-common yamerl jsx cowboy

dep_gen-batch-server = git https://github.com/rabbitmq/gen-batch-server
dep_rabbitmq-common = git https://github.com/rabbitmq/rabbitmq-common
dep_jsx = git https://github.com/talentdeficit/jsx.git
dep_yamerl = git https://github.com/yakaz/yamerl.git
dep_poolboy = git https://github.com/devinus/poolboy
dep_gun = git https://github.com/ninenines/gun
dep_cowboy = git https://github.com/ninenines/cowboy
dep_rabbitmq-erlang-client = git https://github.com/rabbitmq/rabbitmq-erlang-client
dep_syn = git https://github.com/ostinelli/syn

DEP_PLUGINS = cowboy

include $(if $(ERLANG_MK_FILENAME),$(ERLANG_MK_FILENAME),erlang.mk)
