export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
compile: generate-stubs
	bash ./scripts/compile.bash

.PHONY: compile-native
compile-native: cask
compile-native: generate-stubs
	bash ./scripts/native-compile.bash

.PHONY: generate-stubs
generate-stubs: cask
	php ./scripts/generate-builtin-stubs.php > ./stubs/builtins.php

.PHONY: test
test: compile
	cask emacs --batch -L . -L test -l ./test/phpinspect-test.el e -f ert-run-tests-batch
