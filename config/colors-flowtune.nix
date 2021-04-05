{ primaryColor }:

rec {
  # Version 1
  #fg = "edfdf8";
  #bg = "21212a";

  #c0 = "383847";
  #c1 = "e95b6e";
  #c2 = "75e897";
  #c3 = "e8b358";
  #c4 = "588de8";
  #c5 = "e875c6";
  #c6 = "5be9d6";
  #c7 = "edfafd";

  #c8 = "434355";
  #c9 = "ec7182";
  #c10 = "8beca7";
  #c11 = "ebbd6e";
  #c12 = "6e9ceb";
  #c13 = "ec8bcf";
  #c14 = "71ecdc";
  #c15 = "edfafd";

  # Version 2
  fg = "e2ecf7";
  bg = "060d16";

  c0 = "0f2034";
  c1 = "ea2e58";
  c2 = "3cef85";
  c3 = "fcd148";
  c4 = "2f93fc";
  c5 = "e03cef";
  c6 = "40c9f2";
  c7 = "f1f6fb";

  c8 = "132943";
  c9 = "ec456b";
  c10 = "54f194";
  c11 = "fcd761";
  c12 = "48a0fc";
  c13 = "e454f1";
  c14 = "58cff4";
  c15 = "f6f9fd";

  vt-red = "vt.default_red=0x06,0xea,0x3c,0xfc,0x2f,0xe0,0x40,0xf1,0x0f,0xec,0x54,0xfc,0x48,0xe4,0x58,0xf6";
  vt-grn = "vt.default_grn=0x0d,0x2e,0xef,0xd1,0x93,0x3c,0xc9,0xf6,0x20,0x45,0xf1,0xd7,0xa0,0x54,0xcf,0xf4";
  vt-blu = "vt.default_blu=0x16,0x58,0x85,0x48,0xfc,0xef,0xf2,0xfb,0x34,0x6b,0x94,0x61,0xfc,0xf1,0xf4,0xfd";

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
  inactiveBorderColor = muted;
  highlightColor = primary;
  termiteBg = "rgba(22, 22, 28, 0.97)";
}
