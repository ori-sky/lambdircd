all: build-plugins build-main

build-plugins:
	ghc -W Plugins/*.hs

build-main:
	ghc -W Main

clean: clean-irc clean-plugins clean-main
#	rm -f Main PluginsTest *.o *.hi IRC/*.o IRC/*.hi Plugins/*.o Plugins/*.hi

clean-irc:
	rm -f IRC/*.o IRC/*.hi

clean-plugins:
	rm -f Plugins/*.o Plugins/*.hi

clean-main:
	rm -f Main *.o *.hi
