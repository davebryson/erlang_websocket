all:
	(cd deps/mochiweb/src;$(MAKE))
	(cd src;$(MAKE))
	(cd examples/basic/src;$(MAKE))

clean:
	(cd src;$(MAKE) clean)
	(cd deps/mochiweb/src;$(MAKE) clean)
	(cd examples/basic/src;$(MAKE) clean)
