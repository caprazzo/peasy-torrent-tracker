-include Makefile.config

SHELL=/bin/sh
PEASY_LIB=./lib/peasy
PEASY_PLT=$(PEASY_LIB)/dialyzer_plt
MOCHIWEB_LIB=./lib/mochiweb
LOG4ERL_LIB=./lib/log4erl
all: libs

libs:
	cd $(LOG4ERL_LIB) && $(MAKE)
	cd $(MOCHIWEB_LIB) && $(MAKE)
	cd $(PEASY_LIB) && $(MAKE)
	
test: all
	cd $(PEASY_LIB) && $(MAKE) test

$(PEASY_PLT):
	$(DIALYZER) $(DIALYZER_OPTS) --build_plt --output_plt $(PEASY_PLT) --verbose -I $(PEASY_LIB)/include -r $(DIALYZER_BUILD_PATHS) $(PEASY_LIB)/ebin 

dialyzer: libs $(PEASY_PLT)
	$(DIALYZER) $(DIALYZER_OPTS) --plt $(PEASY_PLT) --verbose -I $(PEASY_LIB)/include -r $(PEASY_LIB)/ebin

dialyzer-succ: libs $(PEASY_PLT)
	$(DIALYZER) --plt $(PEASY_PLT) --verbose --succ_typings -I $(PEASY_LIB)/include -r $(PEASY_LIB)

#db is new each time, so sjip this
#run-create-db: libs 
#	erl -noinput $(ERL_FLAGS) -pa $(PEASY_LIB)/ebin \
#	-config $(PEASY_LIB)/priv/peasy.config \
#	-sname peasy -s peasy db_create_schema

run: libs
	erl $(ERL_FLAGS) \
	-boot start_sasl \
	-pa $(PEASY_LIB)/ebin \
	-pa $(MOCHIWEB_LIB)/ebin \
	-pa $(LOG4ERL_LIB)/ebin \
	-sname peasy -s peasy start

tracer:
	erl -pa $(PEASY_LIB)/ebin -noinput \
	-sname tracer -s tr client

clean:
	cd $(PEASY_LIB) && $(MAKE) clean
	cd $(MOCHIWEB_LIB) && $(MAKE) clean
	cd $(LOG4ERL_LIB) && $(MAKE) clean

release:
	mkdir -p $(RELEASE_PREFIX)/log
	cp -r config $(RELEASE_PREFIX)/
	for i in src ebin include priv; do \
		cp -r $(PEASY_LIB)/$$i $(RELEASE_PREFIX)/lib/peasy-$(VER) ; \
		cp -r $(MOCHIWEB_LIB)/$$1 $(RELEASE_PREFIX)/lib/mochiweb ; \
		cp -r $(LOG4ERL_LIB)/$$1 $(RELEASE_PREFIX)/lib/log4erl ; \
	done

	mkdir -p $(BIN_PREFIX)
	sed -e "s|%%%PATHS%%%|-pa peasy/lib/peasy-$(VER)/ebin -pa peasy/lib/mochiweb/ebin -pa peasy/lib/log4erl/ebin|;" \
	    -e "s|%%%CONFIGFILE%%%|peasy/lib/peasy-$(VER)/priv/peasy.config|;" \
	    -e "s|%%%ERL_FLAGS%%%|\"$(ERL_FLAGS)\"|" < ./bin/peasyctl.in > $(BIN_PREFIX)/peasyctl
	chmod +x $(BIN_PREFIX)/peasyctl



