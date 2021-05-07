{ colors }:

with colors;

{
  colors = {
    foreground = "#${fg}";
    background = "#${bg}";

    cursor_bg = "#${fg}";
    cursor_fg = "#${fg}";
    cursor_border = "#${fg}";

    selection_bg = "#${c3}";
    selection_fg = "#${c0}";

    ansi = [
      "#${c0}"
      "#${c1}"
      "#${c2}"
      "#${c3}"
      "#${c4}"
      "#${c5}"
      "#${c6}"
      "#${c7}"
    ];

    brights = [
      "#${c8}"
      "#${c9}"
      "#${c10}"
      "#${c11}"
      "#${c12}"
      "#${c13}"
      "#${c14}"
      "#${c15}"
    ];
  };
}
