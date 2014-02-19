build:
	ghc -W Plugins/*.hs
	ghc -W Main

clean:
	rm -f Main PluginsTest *.o *.hi Plugins/*.o Plugins/*.hi
