.PHONY: all clean cleanConfig

all: Cargo.toml stack.yaml default.nix
	stack build
	cargo build

cleanConfig:
	rm -rf ./target
	rm -f Cargo.lock Cargo.toml stack.yaml default.nix

clean: cleanConfig
	rm -rf .stack-work
	rm -rf pkg/*/.stack-work
	rm -f pkg/*/*.cabal

##############################################################################################################

default.nix: Config
	echo '(./Config).nix' | dhall-to-text >default.nix

Cargo.lock: Config
	echo '(./Config).cargo-lock' | dhall-to-text >Cargo.lock

Cargo.toml: Config
	echo '(./Config).cargo' | dhall-to-json | remarshal -if json -of toml >Cargo.toml

stack.yaml: Config
	echo '(./Config).stack' | dhall-to-yaml >stack.yaml
