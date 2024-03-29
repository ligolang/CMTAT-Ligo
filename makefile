SHELL := /bin/bash

ligo_compiler?=docker run --rm -v "$(PWD)":"$(PWD)" -w "$(PWD)" ligolang/ligo:1.2.0
# ^ Override this variable when you run make command by make <COMMAND> ligo_compiler=<LIGO_EXECUTABLE>
# ^ Otherwise use default one (you'll need docker)
PROTOCOL_OPT?=

project_root=--project-root .
# ^ required when using packages

help:
	@grep -E '^[ a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | \
	awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

compile = $(ligo_compiler) compile contract $(project_root) ./lib/$(1) -o ./compiled/$(2) $(3) $(PROTOCOL_OPT)
# ^ compile contract to michelson or micheline

test = $(ligo_compiler) run test $(project_root) ./test/$(1) $(PROTOCOL_OPT)
# ^ run given test file

compile: ## compile contracts
	@if [ ! -d ./compiled ]; then mkdir -p ./compiled/cmtat/nft && mkdir -p ./compiled/cmtat/asset ; fi
	@echo "Compiling contracts..."
	@$(call compile,cmtat/asset/cmtat_single_asset.impl.mligo,cmtat/asset/cmtat_single_asset.impl.mligo.tz)
	@$(call compile,cmtat/asset/cmtat_single_asset.impl.mligo,cmtat/asset/cmtat_single_asset.impl.mligo.json,--michelson-format json)
	@$(call compile,../test/cmtat/extended_cmtat_single_asset.instance.mligo,cmtat/extended_cmtat_single_asset.mligo.tz)
	@$(call compile,../test/cmtat/extended_cmtat_single_asset.instance.mligo,cmtat/extended_cmtat_single_asset.mligo.json,--michelson-format json)

	@$(call compile,cmtat/asset/cmtat_multi_asset.impl.mligo,cmtat/asset/cmtat_multi_asset.impl.mligo.tz)
	@$(call compile,cmtat/asset/cmtat_multi_asset.impl.mligo,cmtat/asset/cmtat_multi_asset.impl.mligo.json,--michelson-format json)
	@$(call compile,../test/cmtat/extended_cmtat_multi_asset.instance.mligo,cmtat/extended_cmtat_multiple_asset.mligo.tz)
	@$(call compile,../test/cmtat/extended_cmtat_multi_asset.instance.mligo,cmtat/extended_cmtat_multiple_asset.mligo.json,--michelson-format json)

	@$(call compile,cmtat/asset/cmtat_nft_asset.impl.mligo,cmtat/asset/cmtat_nft_asset.impl.mligo.tz)
	@$(call compile,cmtat/asset/cmtat_nft_asset.impl.mligo,cmtat/asset/cmtat_nft_asset.impl.mligo.json,--michelson-format json)
	@$(call compile,../test/cmtat/extended_cmtat_nft_asset.instance.mligo,cmtat/extended_cmtat_nft_asset.mligo.tz)
	@$(call compile,../test/cmtat/extended_cmtat_nft_asset.instance.mligo,cmtat/extended_cmtat_nft_asset.mligo.json,--michelson-format json)
	@echo "Compiled contracts!"
clean: ## clean up
	@rm -rf compiled

compile_example: 
	@echo "Compiling example contracts..."
	@if [ ! -d ./compiled/example ]; then mkdir -p ./compiled/example ; fi
	@$(call compile,../example/extended_cmtat_single_asset.mligo,example/extended_cmtat_single_asset.mligo.json,--michelson-format json)

deploy: deploy_deps deploy.js

deploy_default_single: deploy_deps deploy_default_single.js
deploy_default_multi: deploy_deps deploy_default_multi.js

deploy_default_single.js:
	@echo "Running deploy script (Default Single asset)\n"
	@cd deploy && npm i && npm run deploy_default_single

deploy_default_multi.js:
	@echo "Running deploy script (Default Multi asset)\n"
	@cd deploy && npm i && npm run deploy_default_multi


deploy.js:
	@echo "Running deploy script\n"
	@cd deploy && npm i && npm deploy_example

deploy_deps:
	@echo "Installing deploy script dependencies"
	@cd deploy && npm install
	@echo ""

install: ## install dependencies
	@$(ligo_compiler) install

.PHONY: test
test: ## run tests (SUITE=permit make test)
ifndef SUITE
	@$(call test,cmtat/default_cmtat_single_asset.test.mligo)
	@$(call test,cmtat/default_cmtat_multi_asset.test.mligo)
	@$(call test,cmtat/default_cmtat_nft_asset.test.mligo)
	
	@$(call test,cmtat/extended_cmtat_single_asset.fa2.test.mligo)
	@$(call test,cmtat/extended_cmtat_single_asset.test.mligo)
	@$(call test,cmtat/extended_cmtat_single_asset.snapshots.test.mligo)

	@$(call test,cmtat/extended_cmtat_multi_asset.fa2.test.mligo)
	@$(call test,cmtat/extended_cmtat_multi_asset.test.mligo)
	@$(call test,cmtat/extended_cmtat_multi_asset.snapshots.test.mligo)

	@$(call test,cmtat/extended_cmtat_nft_asset.fa2.test.mligo)
	@$(call test,cmtat/extended_cmtat_nft_asset.test.mligo)
##  @$(call test,fa2/nft/e2e_mutation.test.mligo)

else
	@$(call test,$(SUITE).test.mligo)
endif

lint: ## lint code
	@npx eslint ./scripts --ext .ts