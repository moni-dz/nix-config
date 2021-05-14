{ theme }:

with theme.colors; {

  global = {
    padding = 8;
    markup = "full";
    alignment = "center";
    word_wrap = "yes";
    horizontal_padding = 8;
    show_indicators = false;
    frame_width = if theme.borderWidth == "0" then "2" else theme.borderWidth;
    format = "<b>%s</b>\\n\\n%b";
    font = "Sarasa Gothic J 10.4";
    frame_color = "#${c2}";
    separator_color = "auto";
    icon_position = "left";
    max_icon_size = 80;
    geometry = "330x5-8+25";
  };

  urgency_low = {
    foreground = "#${fg}";
    background = "#${bg}";
    frame_color = "#${c2}";
    timeout = 2;
  };

  urgency_normal = {
    foreground = "#${fg}";
    background = "#${bg}";
    frame_color = "#${c3}";
    timeout = 4;
  };

  urgency_critical = {
    foreground = "#${fg}";
    background = "#${bg}";
    frame_color = "#${c1}";
  };
}
