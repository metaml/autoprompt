.DEFAULT_GOAL = help

export SHELL := $(shell type --path bash)
# the following vars are required for targets, "repl" and "rds-db"
DBDATABASE = aip
DBHOST     = aip.c7eaoykysgcc.us-east-2.rds.amazonaws.com
DBPASSWORD = $(shell aws secretsmanager get-secret-value --secret-id=db-password |awk '{print $$4}')
DBUSER     = $(shell aws secretsmanager get-secret-value --secret-id=db-user     |awk '{print $$4}')

buildc: build ## build continuously
	@fswatch --latency 1 --one-per-batch --recursive --extended --exclude ".*" --include ".*\.hs$$|.*\.cabal$$|cabal\.project$$" . \
	| xargs --no-run-if-empty -I{} cabal build --jobs='$$ncpus' \
	| source-highlight --src-lang=haskell --out-format=esc

build: # lint (breaks on multiple readers) ## build
	cabal build --jobs='$$ncpus' | source-highlight --src-lang=haskell --out-format=esc

install: ## install
	cabal install --enable-prof --install-method=copy --overwrite-policy=always --installdir=bin exe:autoprompt

test: ## test
	cabal test

lint: ## lint
	hlint app src

clean: ## clean
	cabal clean
	find . -name '*~' -o -name '#*' | xargs rm -f

clobber: clean ## clobber
	rm -rf dist-newstyle/*
run: export PGDATABASE = $(DBDATABASE)
run: export PGHOST     = $(DBHOST)
run: export PGPASSWORD = $(DBPASSWORD)
run: export PGUSER     = $(DBUSER)
run: export OPENAI_API_KEY ?= $(shell aws secretsmanager get-secret-value --secret-id=openai-api-key --output json | jq --raw-output '.SecretString')
run: ## run autoprompt
	cabal run autoprompt

run-bin: install
run-bin: export PGDATABASE = $(DBDATABASE)
run-bin: export PGHOST     = $(DBHOST)
run-bin: export PGPASSWORD = $(DBPASSWORD)
run-bin: export PGUSER     = $(DBUSER)
run-bin: export OPENAI_API_KEY ?= $(shell aws secretsmanager get-secret-value --secret-id=openai-api-key --output json | jq --raw-output '.SecretString')
run-bin: ## run autoprompt
	./bin/autoprompt +RTS -xc

repl: export PGDATABASE = $(DBDATABASE)
repl: export PGHOST     = $(DBHOST)
repl: export PGPASSWORD = $(DBPASSWORD)
repl: export PGUSER     = $(DBUSER)
repl: ## repl
	cabal repl autoprompt

ghcid: export PGDATABASE = $(DBDATABASE)
ghcid: export PGHOST     = $(DBHOST)
ghcid: export PGPASSWORD = $(DBPASSWORD)
ghcid: export PGUSER     = $(DBUSER)
ghcid: ## ghcid
	ghcid --verbose

# @todo: broken and 'ghcid:'
ghcid-dot: ## ghcid's .ghci file
	echo ':set -fwarn-unused-binds -fwarn-unused-imports' > .ghci
	echo ':set -isrc -iapp' >> .ghci
	echo ':load Main.main'

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

# @todo: use a lesser role
login-aws: ## login to aws to fetch/refresh token
	aws sso login # AdministratorAccess-975050288432

api-test: METHOD ?= chat/chat
api-test: ## curl an endpoint
	curl \
	--insecure \
	--request POST \
	--header "Content-Type: application/json" \
	--data @etc/test/chatreq.json \
	https://localhost:8443/$(METHOD)

rds-db: export PGDATABASE = $(DBDATABASE)
rds-db: export PGHOST     = $(DBHOST)
rds-db: export PGPASSWORD = $(DBPASSWORD)
rds-db: export PGUSER     = $(DBUSER)
rds-db: ## connect to the postgresql instance
	psql aip

cert-ssl: KEY=etc/ssl/key.pem
cert-ssl: CSR=etc/ssl/server.csr
cert-ssl: CERT=etc/ssl/cert.pem
cert-ssl: ## create self-signed ssl certificate for dev only
	openssl ecparam -name prime256v1 -genkey -noout -out $(KEY)
	openssl req -new -sha256 -key $(KEY) -out $(CSR)
	openssl x509 -req -sha256 -days 3652 -in $(CSR) -signkey $(KEY) -out $(CERT)

cert-rsa:
	openssl req -x509 -newkey rsa:2048 -nodes -out etc/ssl/cert.pem -keyout etc/ssl/key.pem -days 3652
