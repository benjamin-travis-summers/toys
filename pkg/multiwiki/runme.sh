set -e
set -x

tgzfile="$(realpath "$1")"

cd $(dirname "$(realpath "$0")")

test -e "$tgzfile"

hspkgs='attoparsec classy-prelude lens text taggy taggy-lens'

nix-shell                                                                     \
  -p go-pup                                                                   \
  -p "haskell.packages.ghc802.ghcWithPackages (pkgs: with pkgs; [ $hspkgs ])" \
  --run "bash mkwiki.sh '$tgzfile'"
