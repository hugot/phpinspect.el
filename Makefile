export EMACS ?= $(shell which emacs)

ELC_FILES = $(patsubst %.el, %.elc, $(shell ls -1 ./*.el ./test/*.el ./benchmarks/*.el))
DEP_DIRECTORY = $(CURDIR)/.deps
RUN_EMACS := emacs -batch -L $(CURDIR) --eval '(package-initialize)'

export HOME = ${DEP_DIRECTORY}

$(CURDIR): deps
$(CURDIR):$(ELC_FILES)
$(CURDIR): ./data/builtin-stubs-index.eld.gz

./.deps: ./phpinspect.el
./.deps:
	emacs -batch -l ./scripts/install-deps.el

./stubs/builtins.php: ./scripts/generate-builtin-stubs.php
	mkdir -p ./stubs/
	php ./scripts/generate-builtin-stubs.php > ./stubs/builtins.php

./data/builtin-stubs-index.eld.gz: ./stubs/builtins.php | ./.deps
	mkdir -p ./data/
	$(RUN_EMACS) -l phpinspect-cache -f phpinspect-dump-stub-index

%.elc: %.el
	$(RUN_EMACS) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $<

.PHONY: deps
deps: ./.deps

.PHONY: stub-index
stub-index: ./data/builtin-stubs-index.eld.gz

.PHONY: clean
clean:
	rm -f $(ELC_FILES) ./data/builtin-stubs-index.eld.gz

.PHONY: clean-all
clean-all: clean
	rm -f ./stubs/builtins.php

.PHONY: compile
compile: ./.deps
compile: $(ELC_FILES)

.PHONY: compile-native
compile-native: ./.deps
	bash ./scripts/native-compile.bash

.PHONY: test
test: deps
	$(RUN_EMACS) -L ./test -l ./test/phpinspect-test -f ert-run-tests-batch
