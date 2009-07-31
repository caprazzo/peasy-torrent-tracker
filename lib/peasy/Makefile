-include ../../Makefile.config

.PHONY: peasy all test

all: peasy

test:
	$(ERL)  -pa ./ebin -pa ../mochiweb/ebin -s all_tests test -s init stop

peasy:
	$(ERL) -make

clean:
	rm ebin/*.beam

