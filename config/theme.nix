let
  wallpaperPath = ../img/wallpapers;
in
rec {
  borderWidth = "2";
  primaryColor = "red";
  lightModeEnabled = false;

  # colorscheme to load, see colors-flowtune.nix for a reference impl
  colors =
    if !lightModeEnabled
    then import ./colors-horizon.nix { inherit primaryColor; }
    else import ./colors-horizon-light.nix;

  wallpaper =
    if primaryColor == "white"
    then "${wallpaperPath}/horizon_lightmode.jpg"
    else if primaryColor == "red"
    then "${wallpaperPath}/horizon.jpg"
    # then "${wallpaperPath}/slark.png"
    else if primaryColor == "green"
    then "${wallpaperPath}/riki.jpg"
    else if primaryColor == "yellow"
    then "${wallpaperPath}/dota_stars_wallpaper.jpg"
    else if primaryColor == "blue"
    then "${wallpaperPath}/enigma.png"
    else if primaryColor == "magenta"
    then "${wallpaperPath}/dota_stars_wallpaper.jpg"
    else "${wallpaperPath}/voidclose.png";

  tiledWallpaper = wallpaper == "${wallpaperPath}/slark.png";
}
