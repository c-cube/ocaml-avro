

DUNE_OPTS=--profile=release
build:
	@dune build @all $(DUNE_OPTS)

test:
	@dune runtest -f --no-buffer

clean:
	@dune clean

watch:
	@dune build @all $(DUNE_OPTS) -w
