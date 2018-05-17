.PHONY: build install uninstall reinstall test clean

build:
	jbuilder build @install

install:
	jbuilder install

uninstall:
	jbuilder uninstall

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

test:
	jbuilder runtest -f

clean:
	jbuilder clean
