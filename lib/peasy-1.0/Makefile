-include ../../Makefile.config

.PHONY: peasy-1.0 all dialyzer

all: peasy-1.0

tags:
	cd src && $(MAKE) tags

peasy-1.0:
	$(ERL) -make

clean:
	rm ebin/*.beam

