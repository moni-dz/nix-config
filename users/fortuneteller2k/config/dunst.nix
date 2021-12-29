{ colorscheme }:

with colorscheme.colors; {
  global = {
    padding = 8;
    markup = "full";
    alignment = "center";
    word_wrap = "yes";
    horizontal_padding = 8;
    show_indicators = false;
    frame_width = "2";
    format = "<b>%s</b>\\n\\n%b";
    font = "Sarasa Gothic J 10.4";
    frame_color = "#${base0B}";
    separator_color = "auto";
    icon_position = "left";
    max_icon_size = 80;
    geometry = "330x5-8+25";
  };

  urgency_low = {
    foreground = "#${base05}";
    background = "#${base01}";
    frame_color = "#${base0B}";
    timeout = 2;
  };

  urgency_normal = {
    foreground = "#${base05}";
    background = "#${base01}";
    frame_color = "#${base0A}";
    timeout = 4;
  };

  urgency_critical = {
    foreground = "#${base05}";
    background = "#${base01}";
    frame_color = "#${base08}";
  };
}
