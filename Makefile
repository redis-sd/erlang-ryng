PROJECT = ryng

COVERAGE ?= 0
TEST ?= 0

V ?= 0

## dialyzer

DIALYZER ?= dialyzer

dialyzer_verbose_0 = @echo ":: DIALYZER" $(@F);
dialyzer_verbose = $(dialyzer_verbose_$(V))

dialyzer = $(dialyzer_verbose) $(DIALYZER)

## rebar

REBAR_URL ?= https://github.com/Pagoda/rebar/releases/download/2.1.0/rebar

define get_rebar
	wget -O $(REBAR) $(REBAR_URL) || rm $(REBAR)
	chmod +x $(REBAR)
endef

REBAR := $(shell command -v rebar 2>&1 >/dev/null; if [ $$? -eq 0 ]; then command -v rebar; else echo $(shell pwd)/rebar; fi)
REBAR_BUILD_DIR := $(shell pwd)/.rebar-build

rebar_args_3 = -v 3
rebar_args_2 = -v 2
rebar_args_1 = -v 1
rebar_args = $(rebar_args_$(V))

rebar_verbose_0 = @echo ":: REBAR" $(@F);
rebar_verbose = $(rebar_verbose_$(V))

rebar = $(rebar_verbose) V=$(V) TEST=$(TEST) $(REBAR) $(rebar_args)

.PHONY: deps update-deps deps-compile compile build clean clean-app distclean

all: deps build

deps: $(REBAR)
	$(rebar) get-deps
	$(rebar) check-deps

update-deps: $(REBAR)
	$(rebar) update-deps

deps-compile: $(REBAR)
	$(rebar) skip_apps=$(PROJECT) compile

compile: $(REBAR)
	$(rebar) skip_deps=true compile

build: $(REBAR)
	$(rebar) compile

clean: $(REBAR)
	$(rebar) clean

clean-app: $(REBAR)
	$(rebar) skip_deps=true clean

distclean: clean
	$(rebar) delete-deps

##
## Docs
##
.PHONY: docs xref

docs: $(REBAR)
	$(rebar) skip_deps=true doc

xref: $(REBAR)
	$(rebar) xref

##
## Dialyzer
##
PLT ?= .$(PROJECT).plt
PLT_DEPS ?= asn1 compiler crypto edoc erts gs hipe inets kernel \
	observer public_key runtime_tools sasl ssl stdlib syntax_tools \
	tools webtool xmerl
PLT_APPS ?= .
DIALYZER_OPTS ?= -Werror_handling -Wno_return -Wrace_conditions \
	-Wunmatched_returns

.PHONY: build-plt check-plt dialyze

build-plt: clean compile
	$(dialyzer) --build_plt --output_plt $(PLT) --apps $(PLT_DEPS) $(PLT_APPS) \
		deps/*/ebin ebin

check-plt: $(PLT)
	$(dialyzer) --check_plt --plt $(PLT) --apps $(PLT_DEPS) $(PLT_APPS) \
		deps/*/ebin ebin

dialyze: $(PLT)
	$(dialyzer) $(DIALYZER_OPTS) --plt $(PLT) deps/*/ebin ebin

$(PLT):
	$(MAKE) build-plt

##
## Tests
##
.PHONY: coverage ct eunit test-deps test-deps-compile test-compile test-build test-clean test test-app

coverage: COVERAGE=1
coverage: test

ct: TEST=1
ct: $(REBAR)
	$(rebar) skip_deps=true ct

eunit: TEST=1
eunit: $(REBAR)
	$(rebar) skip_deps=true eunit

test-deps: TEST=1
test-deps: deps

test-deps-compile: TEST=1 EUNIT_NOAUTO=1
test-deps-compile: deps-compile

test-compile: TEST=1 EUNIT_NOAUTO=1
test-compile: compile

test-build: TEST=1 EUNIT_NOAUTO=1
test-build: build

test-clean: TEST=1
test-clean: clean

test: test-deps test-clean test-build ct

test-app: TEST=1 EUNIT_NOAUTO=1
test-app: COVERAGE=1
test-app: clean-app compile ct

##
## rebar
##
.PHONY: rebar update-rebar

rebar: $(REBAR)

$(REBAR):
	@$(call get_rebar)

update-rebar:
	@rm -rf $(REBAR_BUILD_DIR)
	git clone git://github.com/rebar/rebar.git $(REBAR_BUILD_DIR)
	cd $(REBAR_BUILD_DIR) && ./bootstrap
	mv $(REBAR_BUILD_DIR)/rebar $(REBAR)
	@rm -rf $(REBAR_BUILD_DIR)
