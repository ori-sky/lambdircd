EXECUTABLE=lambdircd
FLAGS=-W -O2
CFLAGS=

all: build-plugins build

build-plugins:
	find plugins -name '*.hs' -print0 | xargs -0 ghc $(FLAGS) $(CFLAGS) -isrc

build:
	ghc $(FLAGS) $(CFLAGS) -package ghc -package ghc-paths -isrc src/Main -o $(EXECUTABLE)

clean-all: clean-plugins clean

clean-plugins:
	find plugins -name '*.o' -print0 | xargs -0 rm -fv
	find plugins -name '*.hi' -print0 | xargs -0 rm -fv

clean:
	rm -fv $(EXECUTABLE)
	find src -name '*.o' -print0 | xargs -0 rm -fv
	find src -name '*.hi' -print0 | xargs -0 rm -fv

test:
	sh test.sh
