help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort

init-arew:
	cd submodules/arew/src/srfi && scheme --program link-dirs.chezscheme.sps

init: init-arew  ## init the repository for development

check-srfi-167:
	./scheme --program submodules/srfi-167/srfi/tests.scm

check-srfi-168:
	./scheme --program submodules/srfi-168/srfi/tests.scm


repl:  ## Run the REPL
	cd submodules/arew && PATH=$(PWD)/local/bin:$(PATH) make repl
