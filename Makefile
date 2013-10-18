# Note that this is basically a carbon copy of cloud-haskell/build.mk,
# so that since https://github.com/haskell-distributed/cloud-haskell/commit/e23ff71049e77d9feb278fd4492aacbe32e93331
# we might as well integrate this repo into the umbrella and change *this* makefile
# to just include ../build.mk

GHC ?= $(shell which ghc)
CABAL ?= $(shell which cabal)
CABAL_DEV ?= $(shell which cabal-dev)
PWD = $(shell pwd)
SANDBOX ?= $(PWD)/cabal-dev
BRANCH=$(subst * ,,$(shell git branch))
GIT_BASE ?= git://github.com/haskell-distributed
USE_LOCAL_UMBRELLA ?=
TEST_SUITE ?=
REPO_NAMES=$(shell cat REPOS | sed '/^$$/d')
REPOS=$(patsubst %,$(PWD)/build/%.repo,$(REPO_NAMES))

.PHONY: all
all: install

.PHONY: test
test: $(REPOS)
	$(CABAL_DEV) test $(TEST_SUITE) --show-details=always

.PHONY: info
info:
	$(info branch = ${BRANCH})
	$(info ghc = ${GHC})

.PHONY: clean
clean:
	rm -rf ./build ./dist ./cabal-dev

.PHONY: dev-install
ifneq (,$(CABAL_DEV))
install: $(REPOS)
	$(CABAL_DEV) install --enable-tests
else
install:
	$(error install cabal-dev to proceed)
endif

ifneq (,$(USE_LOCAL_UMBRELLA))
define clone
	git clone $(GIT_BASE)/$1 ./build/$1
endef
else
define clone
    git clone $(GIT_BASE)/$1.git ./build/$1
endef
endif

%.repo:
	$(call clone,$(*F))
	git --git-dir=$(@D)/$(*F)/.git \
		--work-tree=$(@D)/$(*F) \
		checkout $(BRANCH)
	cd $(@D)/$(*F) && $(CABAL_DEV) install --sandbox=$(SANDBOX)
	touch $@

./build:
	mkdir -p build
