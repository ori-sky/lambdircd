all: main
again: clean main

main:
	ghc --make main

strict:
	ghc --make -W -Werror main

clean: 
	rm -f main *.o *.hi
