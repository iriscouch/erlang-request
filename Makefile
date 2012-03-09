ERL          ?= erl
ERLC	     ?= erlc

.PHONY: deps doc

%.beam: %.erl
	erlc -o test/ $<

all: deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

check: test/etap.beam test/util.beam
	@prove test/*.t

check_verbose: test/etap.beam test/util.beam
	@prove -v test/*.t

clean:
	@./rebar clean
	@rm -f t/*.beam

distclean: clean
	@./rebar delete-deps
	@rm -rf deps

dialyzer: compile
	@dialyzer -Wno_return -c ebin
