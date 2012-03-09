ERL          ?= erl
ERLC	     ?= erlc

.PHONY: deps doc

all: deps compile

compile:
	@./rebar compile

deps:
	@./rebar get-deps

test: all
	@$(ERLC) -o t/ t/etap.erl
	prove t/*.t

clean:
	@./rebar clean
	@rm -f t/*.beam

distclean: clean
	@./rebar delete-deps
	@rm -rf deps

dialyzer: compile
	@dialyzer -Wno_return -c ebin
