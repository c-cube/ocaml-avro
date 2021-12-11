

DUNE_OPTS=--profile=release
build:
	@dune build @all $(DUNE_OPTS)

test:
	@dune runtest -f --no-buffer $(DUNE_OPTS)

perfs:
	@dune build perfs/employee_test.exe $(DUNE_OPTS)
	@ln -sf _build/default/perfs/employee_test.exe employee_test.exe

clean:
	@dune clean

watch:
	@dune build @all $(DUNE_OPTS) -w

.PHONY: perfs clean
