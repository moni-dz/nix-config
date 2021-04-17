{ lib, writeShellScriptBin, spotify, spotify-adblock-linux }:

writeShellScriptBin "spotify-adblock" ''
  LD_PRELOAD=${spotify-adblock-linux}/lib/spotify-adblock.so ${spotify}/bin/spotify  
''
