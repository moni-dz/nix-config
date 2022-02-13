{ colorscheme }:

with colorscheme.colors; {
  global = {
    padding = 8;
    markup = "full";
    alignment = "center";
    word_wrap = "yes";
    horizontal_padding = 8;
    show_indicators = false;
    format = "<b>%s</b>\\n\\n%b";
    font = "Sarasa Gothic J 10.4";
    frame_width = 4;
    outer_frame_width = 5;
    separator_height = 4;
    separator_color = "#${base03}";
    icon_position = "left";
    max_icon_size = 80;
    offset = "9x70";
  };

  urgency_low = {
    foreground = "#${base05}";
    background = "#${base01}";
    frame_color = "#${base0B}";
    outer_frame_color = "#${base00}";
    timeout = 2;
  };

  urgency_normal = {
    foreground = "#${base05}";
    background = "#${base01}";
    frame_color = "#${base0A}";
    outer_frame_color = "#${base00}";
    timeout = 4;
  };

  urgency_critical = {
    foreground = "#${base05}";
    background = "#${base01}";
    frame_color = "#${base08}";
    outer_frame_color = "#${base00}";
  };
}
