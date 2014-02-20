all: build-plugins build-main

build-plugins:
	ghc -W plugins/*.hs

build-main:
	ghc -W Main

clean:
	rm -f Main *.o *.hi
	rm -f IRC/*.o IRC/*.hi IRC/**/*.o IRC/**/*.hi
	rm -f Plugin/*.o Plugin/*.hi Plugin/**/*.o Plugin/**/*.hi
	rm -f plugins/*.o plugins/*.hi
