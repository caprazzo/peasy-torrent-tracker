# leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc +debug_info -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean 

# Edit the lines below
MODS = peasy_web db db_setup peasy_supervisor peasy hex \
       loadtest peasy_web_test mock
all: compile 

compile: ${MODS:%=%.beam}

test:
	erl -noshell -s mt_test test -s init stop
	
docs:
	erl -noshell -s edoc files ${MODS:%=%.erl} -s init stop
	
## special compilation requirements are added here

# special1.beam: special1.erl    
#	${ERL} -Dflag1 -W0 special1.erl

## run an application from the makefile

# application1: compile
#	${ERL} -pa Dir1  -s application1 start Arg1 Arg2 

#subdirs:
#	echo .
#	cd dir1; make
#	cd dir2; make
#	...

# remove all the code

clean:	
	rm -rf *.beam erl_crash.dump
#	cd dir1; make clean
#	cd dir2; make clean 