export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	bash ./compile.bash

.PHONY: compile-native
compile-native: cask
	bash ./native-compile.bash

.PHONY: test
test: compile
	cask emacs --batch -L . -L test -l ./test/phpinspect-test.el e -f ert-run-tests-batch
