set -e
set -x

hspkgs='classy-prelude lens text scotty'

nix-shell                                                                     \
  -p "haskell.packages.ghc802.ghcWithPackages (pkgs: with pkgs; [ $hspkgs ])" \
  --run "runhaskell takeout-upload.hs"
