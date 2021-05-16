{ pkgs }:

''
  #!/bin/sh

  COLOR=$(${pkgs.xcolor}/bin/xcolor)
  echo -n "$COLOR" | xclip -i -selection CLIPBOARD
  notify-desktop -u critical "Color under mouse cursor: " "$COLOR" || echo "notify-desktop isn't present"
  echo "$COLOR"
''
