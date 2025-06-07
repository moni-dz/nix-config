{ inputs, ... }:

{
  flake.overlays = {
    default = final: prev: {
      lib = prev.lib.extend (
        lfinal: lprev: {
          inherit ((import "${inputs.infuse.outPath}/default.nix" { inherit (inputs.nixpkgs) lib; }).v1) infuse;
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
