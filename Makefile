help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST) | sort


wiredtiger:
	cd submodules/wiredtiger && ./autogen.sh
	cd submodules/wiredtiger && ./configure --prefix=$(PWD)/local
	cd submodules/wiredtiger && make
	cd submodules/wiredtiger && make install

chez:
	cd submodules/ChezScheme/ && ./configure --threads --installprefix=$(PWD)/local
	cd submodules/ChezScheme/ && make
	cd submodules/ChezScheme/ && make install

arew:
	cd submodules/arew/src/srfi && scheme --program link-dirs.chezscheme.sps

init: wiredtiger chez arew  ## compile and install wiredtiger and chez scheme fork

check:
	cd submodules/arew && make check2

repl:
	rm -rf /tmp/wt
	mkdir -p /tmp/wt
	cd submodules/arew && LD_PRELOAD=$(PWD)/local/lib/libwiredtiger.so PATH=$(PWD)/local/bin:$(PATH) make repl
