.PHONY: build install test clean

build:
	jbuilder build

install:
	jbuilder install

test:
	jbuilder runtest

clean:
	jbuilder clean
