{ inputs, ... }:

{
  flake.overlays = {
    default =
      final: prev:
      {
        iosevkaft = prev.iosevka.override {
          privateBuildPlan = __readFile ./patches/iosevka-ft-build-plan.toml;
          set = "Ft";
        };

        inherit (inputs.nixpkgs-f2k.overlays.default final prev) lib;
      }
      // (inputs.nvim.overlays.default final prev);
  };
}
