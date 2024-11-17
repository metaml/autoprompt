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
	rm -rf logs packages store
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

ssl-cert: ## create a self signed cert for SSL
	openssl req -x509 -newkey rsa:4096 -nodes -out etc/ssl/cert.pem -keyout etc/ssl/key.pem -days 3650
