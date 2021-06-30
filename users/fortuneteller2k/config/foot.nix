{ theme }:

with theme.colors;

{
  main = {
    term = "xterm-256color";
    font = "Iosevka FT:size=10.5";
    letter-spacing = -1;
    line-height = 13.5;
    dpi-aware = "yes";
    pad = "8x8";
  };

  cursor.style = "beam";

  colors = {
    background = bg;
    foreground = fg;
    regular0 = c0;
    regular1 = c1;
    regular2 = c2;
    regular3 = c3;
    regular4 = c4;
    regular5 = c5;
    regular6 = c6;
    regular7 = c7;
    bright0 = c8;
    bright1 = c9;
    bright2 = c10;
    bright3 = c11;
    bright4 = c12;
    bright5 = c13;
    bright6 = c14;
    bright7 = c15;
  };
}

