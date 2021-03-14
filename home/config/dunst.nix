{ theme }:

let opacity = "f0";
in
with theme.colors; {
  global = {
    padding = 8;
    markup = "full";
    alignment = "center";
    word_wrap = "yes";
    horizontal_padding = 8;
    show_indicators = false;
    frame_width = 2;
    format = "<b>%s</b>: %b";
    font = "Iosevka FT 10.4";
    frame_color = "#${c2}";
    separator_color = "auto";
    icon_position = "left";
    max_icon_size = 80;
    geometry = "330x5-8+25";
  };
  urgency_low = {
    foreground = "#${fg}";
    background = "#${bg}${opacity}";
    frame_color = "#${c2}";
    timeout = 2;
  };
  urgency_normal = {
    foreground = "#${fg}";
    background = "#${bg}${opacity}";
    frame_color = "#${c3}";
    timeout = 4;
  };
  urgency_critical = {
    foreground = "#${fg}";
    background = "#${bg}${opacity}";
    frame_color = "#${c1}";
  };
}
