include ../support/include.mk

all: $(EBIN_FILES)

clean:
	rm -rf $(EBIN_FILES)

test: all $(MODULES)

./$(MODULES):
	@echo "Running tests for $@"
	erl -pa ../ebin -run $@ start -run init stop -noshell
