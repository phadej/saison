.PHONY: all build test test-all bench-laureates

build :
	cabal build saison

all :
	cabal build all

test :
	cabal build saison-tests
	$$(cabal-plan list-bin saison-tests)

test-all :
	cabal build all:tests
	@for t in $$(cabal-plan list-bins '*:test:*' | awk '{ print $$2 }'); do echo "==> $$(basename $$t)"; $$t; done

bench-laureates :
	cabal build bench-laureates
	$$(cabal-plan list-bin bench-laureates) --csv bench-laureates.csv -o bench-laureates.html
