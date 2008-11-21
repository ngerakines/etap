ERL_RUN = erl -run 
ERL_STOP = -run init stop -noshell

all:
	(cd src;$(MAKE))

test:
	(cd t;$(MAKE) test)

clean:
	(cd src;$(MAKE) clean)
	(cd t;$(MAKE) clean)
