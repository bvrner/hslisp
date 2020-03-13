# not patient enough to setup a cabal os stack thing
FILES = Main.hs Parser.hs
FLAGS = -Wall -Werror -Wextra -O2

all:
	ghc $(FILES) $(FLAGS)

.PHONY: clear
clear:
	rm *.o *.hi
