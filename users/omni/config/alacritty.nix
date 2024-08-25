{
  colorscheme,
  isWayland ? false,
}:

with colorscheme.colors;
{
  env.TERM = "xterm-256color";

  font = {
    normal = {
      family = "Iosevka FT";
      style = "Light";
    };

    bold = {
      family = "Iosevka FT";
      style = "Medium";
    };

    # Weird, but it works...
    size = if isWayland then 12.5 else 11;

    offset = {
      x = 0;
      y = -1;
    };
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
      background = "0x${base00}";
      foreground = "0x${base05}";
    };

    normal = {
      black = "0x${base01}";
      red = "0x${base08}";
      green = "0x${base0B}";
      yellow = "0x${base0A}";
      blue = "0x${base0D}";
      magenta = "0x${base0E}";
      cyan = "0x${base0C}";
      white = "0x${base06}";
    };

    bright = {
      black = "0x${base02}";
      red = "0x${base08}";
      green = "0x${base0B}";
      yellow = "0x${base0A}";
      blue = "0x${base0D}";
      magenta = "0x${base0E}";
      cyan = "0x${base0C}";
      white = "0x${base07}";
    };
  };
}
