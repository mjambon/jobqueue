.PHONY: build install test clean

build:
	jbuilder build

install:
	jbuilder install

test:
	jbuilder runtest -f

clean:
	jbuilder clean
