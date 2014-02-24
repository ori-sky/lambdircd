EXECUTABLE=lambdircd

all: build-plugins build-main

build-plugins:
	ghc -W -isrc plugins/*.hs

build-main:
	ghc -W -isrc src/Main -o $(EXECUTABLE)

clean:
	rm -fv $(EXECUTABLE)
	rm -fv src/*.o src/**/*.o
	rm -fv plugins/*.o plugins/*.hi
