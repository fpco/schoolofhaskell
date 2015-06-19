default: docker

cabal.config:
	@wget http://www.stackage.org/lts/cabal.config

cabal.sandbox.config:
	@cabal sandbox init

.cabal-sandbox/bin/sohs: | cabal.config cabal.sandbox.config
	@cabal install -j

docker: | .cabal-sandbox/bin/sohs
	@docker build -t sohs .

clean:
	@cabal clean

dist-clean:
	@rm -rf .cabal-sandbox cabal*.config dist

.PHONY: default docker clean dist-clean
