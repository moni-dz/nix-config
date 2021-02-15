rec {
  lightModeEnabled = false;
  colors = if lightModeEnabled then (import ./colors-light.nix) else (import ./colors.nix);
  wallpaper = let
    wallpaperPath = "/etc/nixos/nixos/config/wallpapers";
  in if lightModeEnabled 
    then "${wallpaperPath}/horizon_lightmode.jpg"
    else "${wallpaperPath}/horizon.jpg";
}
