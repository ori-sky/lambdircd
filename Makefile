CFLAGS=-W -Werror

all: main
again: clean main

main:
	ghc --make $(CFLAGS) main

clean: 
	rm -f main *.o *.hi
