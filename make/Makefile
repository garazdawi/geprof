SRC=$(wildcard src/*.erl)
BEAM=$(patsubst src/%.erl,ebin/%.beam,$(SRC))
INCLUDE=$(wildcard */*.hrl)

all: $(BEAM)

ebin:
	@mkdir ebin

ebin/%.beam: src/%.erl $(INCLUDE) ebin
	@echo ERLC $(notdir $<)
	@erlc -I src -I include -o ebin $<

clean:
	@echo Cleaning `pwd`
	@rm -rf ebin