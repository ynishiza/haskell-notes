SHELL=/usr/bin/env bash
map=$(foreach value,$(2),$(call $(1),$(value)))
get_source_in_directory=$(wildcard $(1)/20*.hs)

PROJECTNAME=haskell-notes
DOCUMENTATION_DIR=docs

SRCFILES=$(call get_source_in_directory,src)
SRCFILESCOUNT=27

default: help

.PHONY: install
install: ## Install 
	make compile

.PHONY: uninstall
uninstall: ## Uninstall
	make clean
	stack purge

.PHONY: compile
compile: ## Compile
	# IMPORTANT: need to build dynamic too
	# for library to be linked with scripts
	# e.g. using lib/Utils.hs 
	stack build --ghc-options -dynamic-too
	make compile-scripts

.PHONY: compile-profile
compile-profile: ## Compile with profiler
	stack build --profile 

.PHONY: compile-scripts
compile-scripts: ## Compile each script
	set -euo pipefail ; \
	FILES=($(SRCFILES)) ;\
	FILECOUNT="$${#FILES[@]}" ;\
	[[ "$$FILECOUNT" != $(SRCFILESCOUNT) ]] && exit "Expected $(SRCFILESCOUNT) but found $$FILECOUNT. Check 'make debug'"; \
	for file in "$${FILES[@]}"; do stack ghc -- $$file; done

.PHONY: test-scripts
test-scripts: ## Test scripts
	set -euo pipefail ; \
	FILES=($(SRCFILES)) ;\
	for file in "$${FILES[@]}"; do stack exec -- $$file; [[ $$? != "0" ]] && exit 10; done; exit 0;

.PHONY: document
document: ## Build haddock documentation 
	stack haddock --no-haddock-deps --haddock-arguments "-o $(DOCUMENTATION_DIR)" 

.PHONY: lint
lint: ## Lint
	@stack exec -- hlint --verbose -h=.hlint.yaml src test

.PHONY: benchmark
benchmark: ## Run benchmark
	@stack bench

.PHONY: test
test: ## Run tests
	@echo "Tests"
	@stack test --test-arguments="+RTS -N"
	make test-scripts

.PHONY: ide
ide: ## Check IDE setup
	haskell-language-server-wrapper ./lib/
	haskell-language-server-wrapper ./src/
	haskell-language-server-wrapper ./test/

.PHONY: debug
debug: ## Print variables
	@echo SHELL=$(SHELL)
	@echo PROJECTNAME=$(PROJECTNAME)
	@echo SRCFILES=$(SRCFILES)
	@echo SRCFILESCOUNT=$(SRCFILESCOUNT)

.PHONY: clean
clean: ## Clean
	stack clean
	rm -f *.hi *.o *.png *.svg *.html
	rm -rf $(DOCUMENTATION_DIR)
	find lib/ src/ \( \! -iname '*.hs' \) -type f -delete

.PHONY: help
help: ## Display this help
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
