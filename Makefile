SHELL = bash
CASK ?= $(shell command -v cask)

ifneq ($(TAGS),)
override TAGS := -t $(TAGS)
endif

ifneq ($(PATTERN),)
override PATTERN := -p $(PATTERN)
endif

.PHONY: cask
cask:
ifeq ($(CASK),)
	$(error "Install cask (https://github.com/cask/cask)")
endif

.PHONY: init
init: cask
	$(CASK) --dev install
	$(CASK) --dev update

.PHONY: test
test: cask
	$(CASK) exec ert-runner --script $(TAGS) $(PATTERN)
