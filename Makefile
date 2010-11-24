
.PHONY: force

all: geprof

apps: force
	@$(MAKE) -C apps

geprof: apps

clean:
	@$(MAKE) -C apps clean
	@rm -rf geprof