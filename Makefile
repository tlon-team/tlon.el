EMACS ?= emacs

# Elpaca profile root.  Override via ELPACA_DIR= if needed.
ELPACA_DIR ?= $(shell dirname $(CURDIR))/..

# Build load-path from all elpaca builds directories plus the repo itself.
LOAD_PATH := -L . -L test \
  $(shell for d in $(ELPACA_DIR)/builds/*/; do printf -- '-L %s ' "$$d"; done)

# Collect every test file under test/.
TEST_FILES := $(wildcard test/*-test.el)

# Minimal load-path for CI (no elpaca).  Uses transient from MELPA
# installed into DEPS_DIR, plus stubs for other external packages.
DEPS_DIR ?= .deps
CI_LOAD_PATH := -L . -L test -L $(DEPS_DIR)

.PHONY: test test-ci deps test-core test-yaml test-url test-cleanup test-counterpart test-dispatch

## Run the full test suite (local, with elpaca).
test:
	$(EMACS) -Q --batch $(LOAD_PATH) \
	  --eval "(setq debug-on-error nil)" \
	  $(foreach f,$(TEST_FILES),-l $(f)) \
	  -f ert-run-tests-batch-and-exit

## Install CI dependencies (transient from MELPA).
deps:
	mkdir -p $(DEPS_DIR)
	$(EMACS) -Q --batch \
	  --eval "(setq package-user-dir (expand-file-name \"$(DEPS_DIR)\" default-directory))" \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	  --eval "(package-initialize)" \
	  --eval "(package-refresh-contents)" \
	  --eval "(unless (package-installed-p 'transient) (package-install 'transient))"

## Run the full test suite in CI (with stubs).
test-ci: deps
	$(EMACS) -Q --batch $(CI_LOAD_PATH) \
	  --eval "(setq package-user-dir (expand-file-name \"$(DEPS_DIR)\" default-directory))" \
	  --eval "(require 'package)" \
	  --eval "(package-initialize)" \
	  --eval "(setq debug-on-error nil)" \
	  -l test/ci-stubs.el \
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

test-dispatch:
	$(EMACS) -Q --batch $(LOAD_PATH) \
	  --eval "(setq debug-on-error nil)" \
	  -l test/tlon-dispatch-test.el \
	  -f ert-run-tests-batch-and-exit
