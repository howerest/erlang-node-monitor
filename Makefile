PROJECT = erlang_node_monitor

DEPS = cowboy jiffy
dep_cowboy_commit = 2.6.3
dep_jiffy = git https://github.com/davisp/jiffy

DEP_PLUGINS = cowboy

PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

include erlang.mk
