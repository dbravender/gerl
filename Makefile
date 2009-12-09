all: compile test

compile:
	mkdir -p ebin
	erl -make

clean:
	rm -rf ./coverage/*.*
	rm -rf ./ebin/*.*
	rm -rf ./test/ebin/*.*

test: compile
	erl -noshell \
		-pa ebin \
		-s gerl_test test \
		-s init stop

coverage: compile
	git submodule init lib/coverize
	git submodule update lib/coverize
	make -C lib/coverize
	mkdir -p coverage
	erl -noshell \
		-pa ebin \
		-pa test/ebin \
		-pa lib/coverize/ebin \
		-s gerl_test run_cover \
		-s init stop
