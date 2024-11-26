{ self, inputs, ... }:

{
  flake.overlays.default = final: prev: {
    iosevka-ft = prev.iosevka.override {
      privateBuildPlan = __readFile ./patches/iosevka-ft-build-plan.toml;
      set = "Ft";
    };
  };

  perSystem =
    { lib, system, ... }:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        allowUnfree = true;
        allowUnsupportedSystem = true;
      };
    in
    {
      packages = removeAttrs (self.overlays.default pkgs pkgs) [ "lib" ];
    };
}
