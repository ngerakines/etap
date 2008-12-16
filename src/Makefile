include ../support/include.mk

all: $(EBIN_FILES)

doc:
	mkdir -p $(DOC_DIR)
	$(ERL) -noshell -run edoc files $(ERL_SOURCES) -run init stop
	mv *.html $(DOC_DIR)
	mv *.png $(DOC_DIR)
	mv *.css $(DOC_DIR)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
	rm -rf edoc-info
	rm -rf $(DOC_DIR)

test: all
	echo "No tests for yaml_erlang."
