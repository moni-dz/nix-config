{ colors }:

{
  env.TERM = "xterm-256color";
  font = {
    normal = {
      family = "Iosevka Nerd Font Mono";
      style = "Light";
    };
    size = 11.4;
  };
  window.padding = {
    x = 8;
    y = 8;
  };
  cursor.style = "Beam";
  colors = {
    primary = {
      background = "0x${colors.bg}";
      foreground = "0x${colors.fg}";
    };
    normal = {
      black = "0x${colors.c0}";
      red = "0x${colors.c1}";
      green = "0x${colors.c2}";
      yellow = "0x${colors.c3}";
      blue = "0x${colors.c4}";
      magenta = "0x${colors.c5}";
      cyan = "0x${colors.c6}";
      white = "0x${colors.c7}";
    };
    bright = {
      black = "0x${colors.c8}";
      red = "0x${colors.c9}";
      green = "0x${colors.c10}";
      yellow = "0x${colors.c11}";
      blue = "0x${colors.c12}";
      magenta = "0x${colors.c13}";
      cyan = "0x${colors.c14}";
      white = "0x${colors.c15}";
    };
  };
}
