APPS=$(patsubst %/src,%,$(wildcard */src))

.PHONY: force

all: $(APPS)

$(APPS): force
	@$(MAKE) -C $@ all

clean: $(patsubst %,%_clean,$(APPS))


%_clean:
	@$(MAKE) -C $* clean