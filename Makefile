all: erlang haskell idris

.PHONY: erlang haskell idris

erlang:
	@ $(MAKE) -C lib/erlang

haskell:
	@ stack build

idris:
	@ $(MAKE) -C lib/idris
