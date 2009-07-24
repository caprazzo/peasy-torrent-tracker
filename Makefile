# leave these lines alone

ERL = erl -boot start_clean 

EBIN = ./ebin
ESRC = ./src
SRC      =  $(wildcard $(ESRC)/*.erl)
TARGET   = $(addsuffix .beam, $(basename \
             $(addprefix $(EBIN)/, $(notdir $(SRC)))))
             
all: ${TARGET} 

test:
	erl -noshell -s mt_test test -s init stop
	
docs:
	erl -noshell -s edoc files ${MODS:%=%.erl} -s init stop
	
ebin/%.beam: src/%.erl 
	@echo  "Compiling  $< ... "
	@erlc -W0 -o ebin $<
# remove all the code

clean:	
	rm -rf ebin/*.beam erl_crash.dump
