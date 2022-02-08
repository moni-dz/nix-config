{ colorscheme }:

with colorscheme.colors; {
  main = {
    term = "xterm-256color";
    font = "Iosevka FT Light:size=10.5";
    letter-spacing = -1;
    line-height = 13.5;
    dpi-aware = "yes";
    pad = "8x8";
  };

  cursor.style = "beam";

  colors = {
    background = base00;
    foreground = base05;
    regular0 = base01;
    regular1 = base08;
    regular2 = base0B;
    regular3 = base0A;
    regular4 = base0D;
    regular5 = base0E;
    regular6 = base0C;
    regular7 = base06;
    bright0 = base02;
    bright1 = base08;
    bright2 = base0B;
    bright3 = base0A;
    bright4 = base0D;
    bright5 = base0E;
    bright6 = base0C;
    bright7 = base07;
  };
}

