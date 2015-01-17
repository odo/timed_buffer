.PHONY: all test
test:
	./rebar com skip_deps=true && deps/etest/bin/etest-runner $*
