{}:
with (import ./miso {});
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
