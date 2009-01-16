LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
.PHONY: doc

all:
	mkdir -p ebin
	(cd src;$(MAKE))

doc:
	(cd src; $(MAKE) doc)

test: all
	(cd t;$(MAKE))
	(cd t;$(MAKE) test)

prove: all
	(cd t;$(MAKE))
	prove t/*.t

clean:
	(cd src;$(MAKE) clean)
	(cd t;$(MAKE) clean)

dist-src:
	mkdir etap-0.3.2/ && cp -rfv src support Makefile etap-0.3.2/
	tar zcf etap-0.3.2.tgz etap-0.3.2

install: all
	mkdir -p ${LIBDIR}/etap-0.3.2/{ebin,include}
	for i in ebin/*.beam; do install $$i $(LIBDIR)/etap-0.3.2/$$i ; done
