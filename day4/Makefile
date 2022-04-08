# leave these lines alone
.SUFFIXES: .erl .beam .yrl
.erl.beam:
	erlc -W $<
.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = linalg day4

Dir1 = "/home/fabiolusha/Desktop/DeskArch-07-21/projects/erlang/advent-of-code-2021/day4"

## run an application from the makefile
all: compile
	${ERL} -pa Dir1 -noshell -s day4 start -s init stop

compile: ${MODS:%=%.beam}


# remove all the code
clean:
	rm -rf *.beam erl_crash.dump
