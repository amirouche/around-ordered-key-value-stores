help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort


wiredtiger:
	cd submodules/wiredtiger && ./autogen.sh
	cd submodules/wiredtiger && ./configure
	cd submodules/wiredtiger && make
	cd submodules/wiredtiger && sudo make install
	sudo ldconfig

chez:
	cd submodules/ChezScheme/ && ./configure --threads
	cd submodules/ChezScheme/ && make
	cd submodules/ChezScheme/ && sudo make install

arew:
	cd submodules/arew/src/srfi && scheme --program link-dirs.chezscheme.sps

install: wiredtiger chez arew  ## compile and install wiredtiger and chez scheme fork

check:
	cd submodules/arew && make check2

repl:
	rm -rf /tmp/wt
	cd submodules/arew && make repl
