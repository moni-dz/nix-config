{ theme }:

with theme.colors;

''
  sort=-time
  layer=overlay
  background-color=#${bg}
  width=300
  height=110
  border-size=2
  icons=1
  max-icon-size=64
  default-timeout=3000
  ignore-timeout=1

  [urgency=low]
  border-color=#${c2}
  default-timeout=2

  [urgency=normal]
  border-color=#${c3}
  default-timeout=4

  [urgency=high]
  border-color=#${c1}
  default-timeout=0

  [category=mpd]
  default-timeout=2000
  group-by=category
''
