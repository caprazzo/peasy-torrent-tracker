# leave these lines alone

ERL = erl -boot start_clean 

EBIN = ./ebin
EBIN_TEST = ./ebin/test
EBIN_MOCHIWEB = ./3rd/mochiweb/ebin
ESRC = ./src/main
ESRC_TEST = ./src/test

BIN = ./bin
RELEASE = ./release
CONFIG = ./config
INC = ./include 
CC = erlc -I ${INC} -W0

SRC			=	$(wildcard $(ESRC)/*.erl)
SRC_TEST	=	$(wildcard $(ESRC_TEST)/*.erl)

TARGET		=	$(addsuffix .beam, $(basename \
				$(addprefix $(EBIN)/, $(notdir $(SRC)))))
				
TARGET_TEST =	$(addsuffix .beam, $(basename \
				$(addprefix $(EBIN_TEST)/, $(notdir $(SRC_TEST)))))
             
all: dirs mochiweb ${TARGET} test
	cp peasy.app $(EBIN)
	
test: ${TARGET_TEST}
	@echo Running test suite...
	@erl -pa $(EBIN) -pa $(EBIN_TEST) -noshell -s all_tests test -s init stop

release: all test
	mkdir -p $(RELEASE)
	mkdir -p $(RELEASE)/log
	cp -R $(EBIN) $(RELEASE)/
	cp -R $(CONFIG) $(RELEASE)/
	cp -R $(BIN) $(RELEASE)/
	cp -R $(EBIN_MOCHIWEB) $(RELEASE)/ebin/mochiweb
	
# run development version
run: ${TARGET}
	erl -boot start_sasl -config  $(CONFIG)/cfg_dev -pa $(EBIN) -pa $(EBIN_MOCHIWEB) -s peasy start
	
# run qa
run_qa: ${TARGET}
	erl -boot start_sasl -config  $(CONFIG)/cfg_qa -detached -pa $(EBIN) -pa $(EBIN_MOCHIWEB) -s peasy start

docs:
	erl -noshell -s edoc files ${MODS:%=%.erl} -s init stop

dirs:
	@echo "Creating dirs ..." 
	@mkdir -p  ${EBIN} ${EBIN_TEST} 
	
$(EBIN)/%.beam: $(ESRC)/%.erl 
	@echo  "Compiling  $< ... "
	@$(CC) -o $(EBIN) $<

$(EBIN_TEST)/%.beam: $(ESRC_TEST)/%.erl
	@echo "Compiling test $< ... "
	@$(CC) +debug_info -o $(EBIN_TEST) $<
	
# remove all the code

mochiweb:
	@echo Building mochiweb...
	@cd 3rd/mochiweb && make

clean:	
	rm -rf $(EBIN)/*.beam $(EBIN_TEST)/*.beam erl_crash.dump
	rm -rf $(RELEASE)
