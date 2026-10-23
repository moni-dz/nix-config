{
  services = {
    desktopManager.plasma6.enable = true;

    displayManager.sddm = {
      enable = true;
      wayland.enable = true;
    };
  };

  jovian = {
    devices.steamdeck = {
      enable = true;
      autoUpdate = true;
    };

    steam = {
      enable = true;
      autoStart = true;
      user = "lyt";
      desktopSession = "plasma";
    };
  };

  nixpkgs.overlays = [
    (_: prev: {
      jovianPackages = prev.jovianPackages.overrideScope (
        _: super: {
          gamescope = super.gamescope.overrideAttrs (old: {
            patches = builtins.filter (
              p:
              !(builtins.any (s: prev.lib.hasSuffix s (toString p)) [
                # wlroots libinput backport, already in 3.16.30.1
                "c08d99437ec8bb56a703f04ad1ef199502c62d10.diff"
              ])
            ) old.patches;

            # unit tests need catch2
            mesonFlags = old.mesonFlags ++ [ (prev.lib.mesonBool "enable_tests" false) ];
          });
        }
      );
    })
  ];
}
