
all:
	cabal build

lint:
	hlint .

doc:
	cabal haddock

