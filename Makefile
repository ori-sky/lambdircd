build:
	ghc -W Plugins/*.hs
	mv Plugins/*.o .
	ghc -W Main

clean:
	rm -f Main PluginsTest *.o *.hi Plugins/*.hi
