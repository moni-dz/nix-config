{ inputs, ... }:

{
  flake.overlays = {
    default = final: prev: {
      lib = prev.lib.extend (
        lfinal: lprev: {
          infuse = (import "${inputs.infuse.outPath}/default.nix" { lib = lprev; }).v1.infuse;
        }
      );

      pokemon-colorscripts-mac = final.lib.infuse prev.pokemon-colorscripts-mac {
        __output = {
          src.__assign = final.fetchFromGitLab {
            owner = "phoneybadger";
            repo = "pokemon-colorscripts";
            rev = "5802ff67520be2ff6117a0abc78a08501f6252ad";
            hash = "sha256-gKVmpHKt7S2XhSxLDzbIHTjJMoiIk69Fch202FZffqU=";
          };
          buildInputs.__assign = [ final.python3 ];
          preBuild.__assign = ''
            patchShebangs ./install.sh
            substituteInPlace install.sh --replace /usr/local $out
          '';
        };
      };
    };
  };
}
