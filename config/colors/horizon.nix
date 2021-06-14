{ pkgs, primaryColor, wallpaperPath }:

rec {
  fg = "fdf0ed";
  bg = "16161c";
  c0 = "232530";
  c1 = "e95678";
  c2 = "29d398";
  c3 = "fab795";
  c4 = "26bbd9";
  c5 = "ee64ae";
  c6 = "59e3e3";
  c7 = "fadad1";
  c8 = "2e303e";
  c9 = "ec6a88";
  c10 = "3fdaa4";
  c11 = "fbc3a7";
  c12 = "3fc6de";
  c13 = "f075b7";
  c14 = "6be6e6";
  c15 = "fdf0ed";

  primary =
    if primaryColor == "red"
    then c1
    else if primaryColor == "green"
    then c2
    else if primaryColor == "yellow"
    then c3
    else if primaryColor == "magenta"
    then c5
    else c4;

  secondary =
    if primaryColor == "red"
    then c2
    else if primaryColor == "green"
    then c1
    else if primaryColor == "yellow"
    then c1
    else if primaryColor == "magenta"
    then c3
    else c5;

  primaryBright =
    if primaryColor == "red"
    then c9
    else if primaryColor == "green"
    then c10
    else if primaryColor == "yellow"
    then c11
    else if primaryColor == "magenta"
    then c13
    else c12;

  secondaryBright =
    if primaryColor == "red"
    then c10
    else if primaryColor == "green"
    then c9
    else if primaryColor == "yellow"
    then c11
    else if primaryColor == "magenta"
    then c13
    else c12;

  muted = "204571";
  transparent = "00000000";
  textColor = bg;
  activeBorderColor = primary;
  inactiveBorderColor = c8;
  highlightColor = primary;
  vimColorscheme = "horizon";

  wallpaper =
    if primaryColor == "red"
    then "${pkgs.nixos-wallpapers}/png/horizon/nineish/01.png"
    else if primaryColor == "green"
    then "${pkgs.nixos-wallpapers}/png/horizon/nineish/01.png"
    else if primaryColor == "yellow"
    then "${wallpaperPath}/dota_stars_wallpaper.jpg"
    else if primaryColor == "blue"
    then "${wallpaperPath}/enigma.png"
    else if primaryColor == "magenta"
    then "${wallpaperPath}/dota_stars_wallpaper.jpg"
    else "${wallpaperPath}/voidclose.png";

  tiledWallpaper = false;
}
