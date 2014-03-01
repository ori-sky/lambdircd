EXECUTABLE=lambdircd

all: build-plugins build-main

build-plugins:
	ghc -W -O2 -isrc plugins/*.hs

build-main:
	ghc -W -O2 -isrc src/Main -o $(EXECUTABLE)

clean:
	rm -fv $(EXECUTABLE)
	rm -fv plugins/*.o plugins/*.hi
	find src -name '*.o' -print0 | xargs -0 rm -fv
	find src -name '*.hi' -print0 | xargs -0 rm -fv

test:
	sh test.sh
