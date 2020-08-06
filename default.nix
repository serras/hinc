with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/ea25964565074e73d4052b56b60b6e101fa08bc5.tar.gz";
  sha256 = "1yb9yvc0ln4yn1jk2k5kwwa1s32310abawz40yd8cqqkm1z7w6wg";
}) {});
let
  haskell-src-exts-src = pkgs.fetchFromGitHub { 
     owner = "haskell-suite";
     repo = "haskell-src-exts";
     rev = "62e545855dd07839c06c750dc68e9b546260c25d";
     sha256 = "0sqa6ylmmycllanvpbm3iq8pr0ccjx274mxlyz9symknri5q4f2x";
  };
 haskell-src-exts = pkgs.haskell.packages.ghcjs.callCabal2nix "haskell-src-exts" haskell-src-exts-src {};
in
  pkgs.haskell.packages.ghcjs.callCabal2nix "hinc" ./. { inherit haskell-src-exts; }