rec {
  borderWidth = "2";
  lightModeEnabled = false;
  primaryColor = "yellow";
  colors =
    if lightModeEnabled
    then (import ./colors-light.nix)
    else (import ./colors.nix { inherit primaryColor; });
  wallpaper =
    let
      wallpaperPath = "/etc/nixos/config/wallpapers";
    in
    if lightModeEnabled then "${wallpaperPath}/horizon_lightmode.jpg"
    else if primaryColor == "red"
    then "${wallpaperPath}/horizon.jpg"
    else "${wallpaperPath}/horizonyellow.jpg";
}
