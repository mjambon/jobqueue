.PHONY: build install test clean

build:
	jbuilder build

install:
	jbuilder install

test:
	ALCOTEST_QUICK_TESTS=false jbuilder runtest -f

clean:
	jbuilder clean
