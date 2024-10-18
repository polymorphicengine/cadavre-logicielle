execpath := $(shell cabal list-bin cl-server)
start:
	cabal build --write-ghc-environment-files=always
	-rm -r build
	mkdir build
	cp $(execpath) build
	cp -r static build
	npm start
