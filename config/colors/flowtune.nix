{ primaryColor, wallpaperPath }:

rec {
  fg = "e2ecf7";
  bg = "151515";

  c0 = "252525";
  c1 = "ea2e58";
  c2 = "3cef85";
  c3 = "fcd148";
  c4 = "2f93fc";
  c5 = "bc2deb";
  c6 = "40c9f2";
  c7 = "f1f6fb";

  c8 = "323232";
  c9 = "ec456b";
  c10 = "54f194";
  c11 = "fcd761";
  c12 = "48a0fc";
  c13 = "c344ed";
  c14 = "58cff4";
  c15 = "f6f9fd";

  primary = if primaryColor == "red" then
    c1
  else if primaryColor == "green" then
    c2
  else if primaryColor == "yellow" then
    c3
  else if primaryColor == "magenta" then
    c5
  else
    c4;
  secondary = if primaryColor == "red" then
    c2
  else if primaryColor == "green" then
    c5
  else if primaryColor == "yellow" then
    c1
  else if primaryColor == "magenta" then
    c3
  else
    c5;
  primaryBright = if primaryColor == "red" then
    c9
  else if primaryColor == "green" then
    c10
  else if primaryColor == "yellow" then
    c11
  else if primaryColor == "magenta" then
    c13
  else
    c12;
  muted = "505050";
  transparent = "00000000";
  textColor = bg;
  activeBorderColor = primary;
  inactiveBorderColor = muted;
  highlightColor = primary;
  vimColorscheme = "flowtune";

  wallpaper = if primaryColor == "white" then
    "${wallpaperPath}/horizon_lightmode.jpg"
  else if primaryColor == "red" then
    "${wallpaperPath}/slark.png"
  else if primaryColor == "green" then
    "${wallpaperPath}/riki.jpg"
  else if primaryColor == "yellow" then
    "${wallpaperPath}/dota_stars_wallpaper.jpg"
  else if primaryColor == "blue" then
    "${wallpaperPath}/enigma.png"
  else if primaryColor == "magenta" then
    "${wallpaperPath}/dota_stars_wallpaper.jpg"
  else
    "${wallpaperPath}/voidclose.png";

  tiledWallpaper = wallpaper == "${wallpaperPath}/slark.png";
}
