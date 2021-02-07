{ colors }:

{
  global = {
    padding = 8;
    markup = "full";
    alignment = "center";
    word_wrap = "yes";
    horizontal_padding = 8;
    show_indicators = false;
    frame_width = 2;
    format = "<b>%s</b>: %b";
    font = "FantasqueSansMono Nerd Font Mono 10";
    frame_color = "#29d398";
    separator_color = "auto";
    icon_position = "left";
    max_icon_size = 80;
    geometry = "330x5-8+25";
  };
  urgency_low = {
    foreground = "#${colors.fg}";
    background = "#${colors.bg}";
    frame_color = "#${colors.c2}";
    timeout = 15;
  };
  urgency_normal = {
    foreground = "#${colors.fg}";
    background = "#${colors.bg}";
    frame_color = "#${colors.c3}";
    timeout = 30;
  };
  urgency_critical = {
    foreground = "#${colors.fg}";
    background = "#${colors.bg}";
    frame_color = "#${colors.c1}";
  };
}
