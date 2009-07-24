# leave these lines alone

ERL = erl -boot start_clean 

EBIN = ./ebin
EBIN_TEST = ./ebin/test
EBIN_MOCHIWEB = ./3rd/mochiweb/ebin
ESRC = ./src/main
ESRC_TEST = ./src/test

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
	erl -pa $(EBIN) -pa $(EBIN_TEST) -noshell -s peasy_web_test test -s init stop

# run development version
run: ${TARGET}
	erl -boot start_sasl -config config/cfg_dev -pa $(EBIN) -pa $(EBIN_MOCHIWEB) -s peasy start
	
# run qa
run_qa: ${TARGET}
	erl -boot start_sasl -config config/cfg_qa -detached -pa $(EBIN) -pa $(EBIN_MOCHIWEB) -s peasy start

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
	@$(CC) -o $(EBIN_TEST) $<
	
# remove all the code

mochiweb:
	cd 3rd/mochiweb && make

clean:	
	rm -rf $(EBIN)/*.beam $(EBIN_TEST)/*.beam erl_crash.dump
