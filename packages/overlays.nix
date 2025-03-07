{ inputs, ... }:

{
  flake.overlays = {
    default = final: prev: {
      iosevka-ft = prev.iosevka.override {
        privateBuildPlan = __readFile ./patches/iosevka-ft-build-plan.toml;
        set = "Ft";
      };

      pokemon-colorscripts-mac = prev.pokemon-colorscripts-mac.overrideAttrs {
        src = prev.fetchFromGitLab {
          owner = "phoneybadger";
          repo = "pokemon-colorscripts";
          rev = "5802ff67520be2ff6117a0abc78a08501f6252ad";
          hash = "sha256-gKVmpHKt7S2XhSxLDzbIHTjJMoiIk69Fch202FZffqU=";
        };

        buildInputs = [ final.python3 ];

        preBuild = ''
          patchShebangs ./install.sh
          substituteInPlace install.sh --replace /usr/local $out
        '';
      };
    };
  };
}
