# not patient enough to setup a cabal os stack thing
FILES = Main.hs Parser.hs Eval.hs
FLAGS = -Wall -Werror -Wextra -O2

all: $(FILES)
	ghc $(FILES) $(FLAGS)

.PHONY: clear
clear:
	rm *.o *.hi
