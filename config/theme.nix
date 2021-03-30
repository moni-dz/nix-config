rec {
  borderWidth = "2";
  flowtuneEnabled = true;
  lightModeEnabled = false;
  primaryColor = "green";
  colors =
    if lightModeEnabled
    then (import ./colors-light.nix)
    else if flowtuneEnabled then (import ./colors-flowtune.nix { inherit primaryColor; })
    else (import ./colors.nix { inherit primaryColor; });
  wallpaper =
    let
      wallpaperPath = ./wallpapers;
    in
    if lightModeEnabled then "${wallpaperPath}/horizon_lightmode.jpg"
    else if primaryColor == "red"
    then "${wallpaperPath}/horizon.jpg"
    else if primaryColor == "yellow"
    then "${wallpaperPath}/horizonyellow.jpg"
    else if primaryColor == "blue"
    then "${wallpaperPath}/enigma.png"
    else "${wallpaperPath}/riki.jpg";
}
