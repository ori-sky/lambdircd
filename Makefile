EXECUTABLE=lambdircd
FLAGS=-W -O2
CFLAGS=

all: build-plugins build

build-plugins:
	ghc $(FLAGS) $(CFLAGS) -isrc plugins/*.hs

build:
	ghc $(FLAGS) $(CFLAGS) -isrc src/Main -o $(EXECUTABLE)

rts:
	ghc $(FLAGS) $(CFLAGS) -isrc src/Main -o $(EXECUTABLE) -rtsopts

clean:
	rm -fv $(EXECUTABLE)
	rm -fv plugins/*.o plugins/*.hi
	find src -name '*.o' -print0 | xargs -0 rm -fv
	find src -name '*.hi' -print0 | xargs -0 rm -fv

test:
	sh test.sh
