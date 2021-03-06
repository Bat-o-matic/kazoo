PROJECT = pivot
ROOT = ../..

EBINS = $(shell find $(ROOT)/core -maxdepth 2 -name ebin -print) $(shell find $(ROOT)/deps -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

ERLC_OPTS = -Werror +debug_info +warn_export_all -I$(ROOT)/core -I$(ROOT)/deps $(PA) \
	+warn_missing_spec # +bin_opt_info

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl
	@mkdir -p ebin/
	erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $?

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.erl
	@mkdir -p test/
	erlc -v $(ERLC_OPTS)  -o test/ -pa test/  $?

clean:
	rm -f ebin/*
	rm -f test/*.beam test/$(PROJECT).app
	rm -f erl_crash.dump

test: clean compile-test eunit

eunit: compile-test
	erl -noshell -pa test -eval "eunit:test([$(MODULES)], [verbose])" -s init stop
