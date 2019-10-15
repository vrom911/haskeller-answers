{}:
with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/1114ba461605cef11ef4e1a96b5adfc4b4c9af18.tar.gz";
  sha256 = "03x323yjpx3wq87kb2i202cw6sxj3sb79j4h712jiv4yd993gjlz";
}) {});
let
  inherit (pkgs) runCommand closurecompiler;
  inherit (pkgs.haskell.packages) ghcjs86;
  client = ghcjs86.callCabal2nix "haskeller-answers" ./. {};
in
  runCommand "haskeller-answers" { inherit client; } ''
    mkdir -p $out/static
    ${closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
      --jscomp_off=checkVars \
      --externs=${client}/bin/client.jsexe/all.js.externs \
      ${client}/bin/client.jsexe/all.js > temp.js
    mv temp.js $out/static/all.js
  ''
