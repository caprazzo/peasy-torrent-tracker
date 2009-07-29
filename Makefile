-include Makefile.config

VER=1.0

SHELL=/bin/sh
PEASY_LIB=./lib/peasy-1.0
MOCHIWEB_LIB=./lib/mochiweb
all: libs

libs:
	cd lib/peasy-1.0 && $(MAKE)
	cd lib/mochiweb && $(MAKE)
	
test:
	cd lib/peasy-1.0 && $(MAKE) test

dialyzer: libs
	$(DIALYZER) $(DIALYZER_OPTS) --verbose -I $(PEASY_LIB)/include -r $(PEASY_LIB)/ebin

dialyzer-succ: libs
	$(DIALYZER) --verbose --succ_typings -I $(PEASY_LIB)/include -r $(PEASY_LIB)

#db is new each time, so sjip this
#run-create-db: libs 
#	erl -noinput $(ERL_FLAGS) -pa $(PEASY_LIB)/ebin \
#	-config $(PEASY_LIB)/priv/peasy.config \
#	-sname peasy -s peasy db_create_schema

run: libs
	erl $(ERL_FLAGS) \
	-pa $(PEASY_LIB)/ebin \
	-pa $(MOCHIWEB_LIB)/ebin \
	-config $(PEASY_LIB)/priv/peasy.config \
	-sname peasy -s peasy start

tracer:
	erl -pa $(PEASY_LIB)/ebin -noinput \
	-sname tracer -s tr client

clean:
	cd lib/peasy-1.0 && $(MAKE) clean
	cd lib/mochiweb && $(MAKE) clean

release:
	mkdir -p $(RELEASE_PREFIX)/lib/peasy-1.0
	for i in src ebin include priv; do \
		cp -r $(PEASY_LIB)/$$i $(RELEASE_PREFIX)/lib/peasy-$(VER) ; \
		cp -r $(MOCHIWEB_LIB)/$$1 $(RELEASE_PREFIX)/lib/mochiweb ; \
	done

	mkdir -p $(BIN_PREFIX)
	sed -e "s|%%%PATHS%%%|-pa $(RELEASE_PREFIX)/lib/peasy-$(VER)/ebin -pa $(RELEASE_PREFIX)/lib/peasy-$(VER)/ebin|;" \
	    -e "s|%%%CONFIGFILE%%%|$(RELEASE_PREFIX)/lib/peasy-$(VER)/priv/peasy.config|;" \
	    -e "s|%%%ERL_FLAGS%%%|\"$(ERL_FLAGS)\"|" < ./bin/peasyctl.in > $(BIN_PREFIX)/peasyctl
	chmod +x $(BIN_PREFIX)/peasyctl


