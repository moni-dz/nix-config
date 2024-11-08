_:

{
  flake.overlays.default = final: prev: {
    asitop = prev.callPackage ./derivations/asitop.nix { };

    python3 = prev.python3.override {
      packageOverrides = pfinal: pprev: {
        pydashing = pfinal.callPackage ./derivations/dashing.nix { };
      };
    };

    iosevka-ft = prev.iosevka.override {
      privateBuildPlan = __readFile ./patches/iosevka-ft-build-plan.toml;
      set = "Ft";
    };
  };
}
