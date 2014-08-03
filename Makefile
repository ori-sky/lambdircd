EXECUTABLE=lambdircd
FLAGS=-W -O2
CFLAGS=

all: build-plugins build

build-plugins:
	ghc $(FLAGS) $(CFLAGS) -isrc plugins/*.hs

build:
	ghc $(FLAGS) $(CFLAGS) -package ghc -package ghc-paths -isrc src/Main -o $(EXECUTABLE)

clean-all: clean-plugins clean

clean-plugins:
	rm -fv plugins/*.o plugins/*.hi

clean:
	rm -fv $(EXECUTABLE)
	find src -name '*.o' -print0 | xargs -0 rm -fv
	find src -name '*.hi' -print0 | xargs -0 rm -fv

test:
	sh test.sh
