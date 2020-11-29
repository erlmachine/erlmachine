PROJECT = erlmachine
PROJECT_DESCRIPTION = The first flow-based Erlang OTP programming framework
PROJECT_VERSION = 2.0.0

DEPS = jsx yamerl jessy

TEST_DEPS = meck jsx yamerl jessy

dep_jsx = git https://github.com/talentdeficit/jsx.git main 3.0.0
dep_yamerl = git https://github.com/yakaz/yamerl.git v0.8.0
dep_jessy = git https://github.com/for-GET/jesse.git 1.5.6

dep_meck = git https://github.com/eproxus/meck.git 0.9.0

include erlang.mk
