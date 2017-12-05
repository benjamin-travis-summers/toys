set -e
set -x

hspkgs='classy-prelude lens text scotty wai-middleware-static sqlite-simple'

nix-shell                                                                     \
  -p "haskell.packages.ghc802.ghcWithPackages (pkgs: with pkgs; [ $hspkgs ])" \
  --run "runhaskell takeout-upload.hs $*"
