EXECUTABLE=lambdircd

all: build-plugins build-main

build-plugins:
	ghc -W -isrc plugins/*.hs

build-main:
	ghc -W -isrc src/Main -o $(EXECUTABLE)

clean:
	rm -fv $(EXECUTABLE) \
		src/*.o src/*.hi \
		src/**/*.o src/**/*.hi \
		plugins/*.o \
		plugins/*.hi
