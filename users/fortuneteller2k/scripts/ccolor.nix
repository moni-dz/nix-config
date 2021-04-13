''
  #!/usr/bin/env dash

  sleep 2;
  eval $(xdotool getmouselocation --shell)
  IMAGE=`import -window root -depth 8 -crop 1x1+$X+$Y txt:-`
  COLOR=`echo $IMAGE | grep -om1 '#\w\+'`
  echo -n $COLOR | xclip -i -selection CLIPBOARD
  notify-desktop "Color under mouse cursor: " $COLOR || echo "notify-desktop isn't present"
  echo $COLOR
''
