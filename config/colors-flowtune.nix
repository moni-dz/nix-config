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

  primary = 
    if primaryColor == "red"
    then c1
    else if primaryColor == "yellow"
    then c3
    else c4;
  transparent = "00000000";
  textColor = bg;
  activeBorderColor = primary;
  inactiveBorderColor = c8;
  highlightColor = primary;
  termiteBg = "rgba(22, 22, 28, 0.97)";
}
