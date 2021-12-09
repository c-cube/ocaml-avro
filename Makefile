

DUNE_OPTS=--profile=release
build:
	@dune build @all $(DUNE_OPTS)

clean:
	@dune clean

watch:
	@dune build @all $(DUNE_OPTS) -w
