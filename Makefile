.PHONY: all test
test:
	ETEST_ARGS="-pa  _build/default/lib/timed_buffer/ebin/"  _build/default/lib/etest/bin/etest-runner
