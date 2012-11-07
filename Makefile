ERL	?= erl
ERLC	?= erlc

all:
	./rebar compile

doc:
	./rebar doc

test: all
	prove t/*.t

verbose-test: all
	prove -v t/*.t

cover: all
	COVER=1 prove t/*.t
	@$(ERL) -detached -noshell -eval 'etap_report:create()' -pa ebin -s init stop

clean:
	./rebar clean
	rm -rf cover/
	rm -f doc/*.html doc/*.css doc/edoc-info doc/*.png

.PHONY: doc
