{ theme }:

with theme.colors; {
  background_opacity = 0.90;
  env.TERM = "xterm-256color";
  font = {
    normal = {
      family = "Iosevka FT";
      style = "Regular";
    };
    size = 11.4;
  };
  window = {
    dynamic_padding = true;
    padding = {
      x = 8;
      y = 8;
    };
  };
  cursor.style = "Beam";
  colors = {
    primary = {
      background = "0x${bg}";
      foreground = "0x${fg}";
    };
    normal = {
      black = "0x${c0}";
      red = "0x${c1}";
      green = "0x${c2}";
      yellow = "0x${c3}";
      blue = "0x${c4}";
      magenta = "0x${c5}";
      cyan = "0x${c6}";
      white = "0x${c7}";
    };
    bright = {
      black = "0x${c8}";
      red = "0x${c9}";
      green = "0x${c10}";
      yellow = "0x${c11}";
      blue = "0x${c12}";
      magenta = "0x${c13}";
      cyan = "0x${c14}";
      white = "0x${c15}";
    };
  };
}
