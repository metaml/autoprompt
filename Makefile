.DEFAULT_GOAL = help

export SHELL := $(shell type --path bash)

buildc: build ## build continuously
	@fswatch --latency 1 --one-per-batch --recursive --extended --exclude ".*" --include ".*\.hs$$|.*\.cabal$$|cabal\.project$$" . \
	| xargs --no-run-if-empty -I{} cabal build --jobs='$$ncpus' \
	| source-highlight --src-lang=haskell --out-format=esc

build: # lint (breaks on multiple readers) ## build
	cabal build --jobs='$$ncpus' | source-highlight --src-lang=haskell --out-format=esc

install: ## install
	cabal install --install-method=copy --overwrite-policy=always --installdir=bin exe:autoprompt

test: ## test
	cabal test

lint: ## lint
	hlint app src

clean: ## clean
	cabal clean
	find . -name \*~ | xargs rm -f

clobber: clean ## clobber
	rm -rf dist-newstyle/*

run: ## run autoprompt
	cabal run autoprompt

repl: ## repl
	cabal repl autoprompt

dev: ## nix develop
	nix develop

package: ## nix build default package
	nix build --impure --verbose --option sandbox relaxed

image: ## nix build docker image
	nix build --impure --verbose --option sandbox relaxed .#docker
	nix build --impure --verbose --option sandbox relaxed .#autoprompt

help: ## help
	-@grep --extended-regexp '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sed 's/^Makefile://1' \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
	-@ghc --version
	-@cabal --version
	-@hlint --version

KEY=etc/ssl/key.pem
CSR=etc/ssl/server.csr
CERT=etc/ssl/cert.pem

cert-ssl: cert-ec ## create self-signed ssl certificate

cert-ec:
	openssl ecparam -name prime256v1 -genkey -noout -out $(KEY)
	openssl req -new -sha256 -key $(KEY) -out $(CSR)
	openssl x509 -req -sha256 -days 3652 -in $(CSR) -signkey $(KEY) -out $(CERT)

cert-rsa:
	openssl req -x509 -newkey rsa:2048 -nodes -out etc/ssl/cert.pem -keyout etc/ssl/key.pem -days 3652
