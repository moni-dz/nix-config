rec {
  borderWidth = "2";
  primaryColor = "red";

  # colorscheme to load, see colors-flowtune.nix for a reference impl
  colors = import ./colors-flowtune.nix { inherit primaryColor; };

  wallpaper =
    let
      wallpaperPath = ../img/wallpapers;
    in
    if lightModeEnabled then "${wallpaperPath}/horizon_lightmode.jpg"
    else if primaryColor == "red"
    # then "${wallpaperPath}/horizon.jpg"
    then "${wallpaperPath}/slark.png"
    else if primaryColor == "green"
    then "${wallpaperPath}/riki.jpg"
    else if primaryColor == "yellow"
    then "${wallpaperPath}/dota_stars_wallpaper.jpg"
    else if primaryColor == "blue"
    then "${wallpaperPath}/enigma.png"
    else if primaryColor == "magenta"
    then "${wallpaperPath}/dota_stars_wallpaper.jpg"
    else "${wallpaperPath}/voidclose.png";

  tiledWallpaper = let wallpaperPath = ../img/wallpapers; in wallpaper == "${wallpaperPath}/slark.png";
}
