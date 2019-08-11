help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

arew:
	cd submodules/arew/src/srfi && scheme --program link-dirs.chezscheme.sps

init: arew  ## init the repository for development

check:
	cd submodules/arew && PATH=$(PWD)/local/bin:$(PATH) make check2

repl:  ## Run the REPL
	cd submodules/arew && PATH=$(PWD)/local/bin:$(PATH) make repl
