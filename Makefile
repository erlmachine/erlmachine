PROJECT = erlmachine
PROJECT_DESCRIPTION = An application level virtual machine based on erlang OTP
PROJECT_VERSION = 0.0.1

DEPS = gen-batch-server rabbitmq-common poolboy cowboy gun rabbitmq-erlang-client syn

dep_gen-batch-server = git https://github.com/rabbitmq/gen-batch-server
dep_rabbitmq-common = git https://github.com/rabbitmq/rabbitmq-common
dep_poolboy = git https://github.com/devinus/poolboy
dep_cowboy = git https://github.com/ninenines/cowboy
dep_gun = git https://github.com/ninenines/gun
dep_rabbitmq-erlang-client = git https://github.com/rabbitmq/rabbitmq-erlang-client
dep_syn = git https://github.com/ostinelli/syn

include erlang.mk
