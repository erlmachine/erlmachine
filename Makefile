PROJECT = erlmachine
PROJECT_DESCRIPTION = The first flow-based Erlang OTP programming framework
PROJECT_VERSION = 3.2.0

DEPS = erlbox ra syn yamerl jesse

TEST_DEPS = meck

dep_erlbox = git https://github.com/erlmachine/erlbox.git

dep_ra = git https://github.com/rabbitmq/ra.git v2.2.0

dep_yamerl = git https://github.com/yakaz/yamerl.git v0.8.0
dep_jesse = git https://github.com/for-GET/jesse.git 1.5.6

dep_syn = hex 3.0.1

dep_meck = git https://github.com/eproxus/meck.git 0.9.0

include erlang.mk
