SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(value)/*.hs)

PROJECTNAME=haskell-notes
DOCUMENTATION_DIR=docs

default: help

install: ## Install 
	make compile

uninstall: ## Uninstall
	make clean
	stack purge

compile: ## Compile
	stack build 

compile-profile: ## Compile with profiler
	stack build --profile 

document: ## Build haddock documentation 
	stack haddock --no-haddock-deps --haddock-arguments "-o $(DOCUMENTATION_DIR)" 

lint: ## Lint
	@stack exec -- hlint --verbose -h=.hlint.yaml src test

benchmark: ## Run benchmark
	@stack bench

test: ## Run tests
	@echo "Tests"
	@stack test --test-arguments="+RTS -N"

ide: ## Check IDE setup
	haskell-language-server-wrapper ./lib/
	haskell-language-server-wrapper ./src/
	haskell-language-server-wrapper ./test/

debug: ## Print variables
	@echo SHELL=$(SHELL)
	@echo PROJECTNAME=$(PROJECTNAME)

clean: ## Clean
	stack clean
	rm -f *.hi *.o *.cabal *.png *.svg *.html
	rm -rf $(DOCUMENTATION_DIR)
	find src 
		-iname '*.o' \
		-o -iname '*.hi' \
		-o -iname '*.dyn_hi' \
		-o -iname '*.dyn_o' \
		-o -iname '*dump-*' \
		-delete

help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: default install uninstall document compile compile-profile run  run-profile benchmark test ide debug clean help
