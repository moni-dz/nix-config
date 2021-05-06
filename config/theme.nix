let
  wallpaperPath = ../assets/wallpapers;
in
rec {
  borderWidth = "2";
  primaryColor = "red";
  lightModeEnabled = false;

  # colorscheme to load, see colors/flowtune.nix for a reference impl
  colors =
    if !lightModeEnabled
    then import ./colors/horizon.nix { inherit primaryColor wallpaperPath; }
    else import ./colors/horizon-light.nix { inherit wallpaperPath; };
}
