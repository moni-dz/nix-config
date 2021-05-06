{ primaryColor, wallpaperPath }:

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
  vt-red = "vt.default_red=0x16,0xe9,0x29,0xfa,0x26,0xee,0x59,0xfd,0x23,0xec,0x3f,0xfb,0x3f,0xf0,0x6b,0xfd";
  vt-grn = "vt.default_grn=0x16,0x56,0xd3,0xb7,0xbb,0x64,0xe3,0xf0,0x35,0x6a,0xda,0xc3,0xc6,0x75,0xe6,0xf0";
  vt-blu = "vt.default_blu=0x1c,0x78,0x98,0x95,0xd9,0xae,0xe3,0xed,0x30,0x88,0xa4,0xa7,0xde,0xb7,0xe6,0xed";

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
    then c5
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

  muted = "204571";
  transparent = "00000000";
  textColor = bg;
  activeBorderColor = primary;
  inactiveBorderColor = c8;
  highlightColor = primary;
  vimColorscheme = "horizon";

  wallpaper =
    if primaryColor == "red"
    then "${wallpaperPath}/horizon.jpg"
    else if primaryColor == "green"
    then "${wallpaperPath}/riki.jpg"
    else if primaryColor == "yellow"
    then "${wallpaperPath}/dota_stars_wallpaper.jpg"
    else if primaryColor == "blue"
    then "${wallpaperPath}/enigma.png"
    else if primaryColor == "magenta"
    then "${wallpaperPath}/dota_stars_wallpaper.jpg"
    else "${wallpaperPath}/voidclose.png";

  tiledWallpaper = false;
}
