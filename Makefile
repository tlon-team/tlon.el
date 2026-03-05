EMACS ?= emacs

# Elpaca profile root.  Override via ELPACA_DIR= if needed.
ELPACA_DIR ?= $(shell dirname $(CURDIR))/..

# Build load-path from all elpaca builds directories plus the repo itself.
LOAD_PATH := -L . -L test \
  $(shell for d in $(ELPACA_DIR)/builds/*/; do printf -- '-L %s ' "$$d"; done)

# Collect every test file under test/.
TEST_FILES := $(wildcard test/*-test.el)

.PHONY: test test-core test-yaml test-url test-cleanup test-counterpart

## Run the full test suite.
test:
	$(EMACS) -Q --batch $(LOAD_PATH) \
	  --eval "(setq debug-on-error nil)" \
	  $(foreach f,$(TEST_FILES),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

## Run individual test modules.
test-core:
	$(EMACS) -Q --batch $(LOAD_PATH) \
	  --eval "(setq debug-on-error nil)" \
	  -l test/tlon-core-test.el \
	  -f ert-run-tests-batch-and-exit

test-yaml:
	$(EMACS) -Q --batch $(LOAD_PATH) \
	  --eval "(setq debug-on-error nil)" \
	  -l test/tlon-yaml-test.el \
	  -f ert-run-tests-batch-and-exit

test-url:
	$(EMACS) -Q --batch $(LOAD_PATH) \
	  --eval "(setq debug-on-error nil)" \
	  -l test/tlon-url-test.el \
	  -f ert-run-tests-batch-and-exit

test-cleanup:
	$(EMACS) -Q --batch $(LOAD_PATH) \
	  --eval "(setq debug-on-error nil)" \
	  -l test/tlon-cleanup-test.el \
	  -f ert-run-tests-batch-and-exit

test-counterpart:
	$(EMACS) -Q --batch $(LOAD_PATH) \
	  --eval "(setq debug-on-error nil)" \
	  -l test/tlon-counterpart-test.el \
	  -f ert-run-tests-batch-and-exit
