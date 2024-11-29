{ inputs, ... }:

{
  flake.overlays = {
    default = final: prev: {
      iosevka-ft = prev.iosevka.override {
        privateBuildPlan = __readFile ./patches/iosevka-ft-build-plan.toml;
        set = "Ft";
      };
    };
  };
}
