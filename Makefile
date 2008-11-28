
all:
	(cd src;$(MAKE))

test: all
	(cd t;$(MAKE))
	(cd t;$(MAKE) test)

clean:
	(cd src;$(MAKE) clean)
	(cd t;$(MAKE) clean)
