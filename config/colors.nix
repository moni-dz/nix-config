{ primaryColor }:

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
  primary = if primaryColor == "red" then c1 else c3;
  transparent = "00000000";
  textColor = bg;
  activeBorderColor = primary;
  inactiveBorderColor = c8;
  highlightColor = primary;
  termiteBg = "rgba(22, 22, 28, 0.97)";
}
