{ theme }:

with theme;

''
  # modkey
  set $mod Mod1

  # Use Mouse+$mod to drag floating windows to their wanted position
  floating_modifier $mod

  # start a terminal
  set $term alacritty
  bindsym $mod+Return exec $term

  # change focus
  bindsym $mod+Left focus left
  bindsym $mod+Down focus down
  bindsym $mod+Up focus up
  bindsym $mod+Right focus right

  # move focused window
  bindsym $mod+Shift+Left move left
  bindsym $mod+Shift+Down move down
  bindsym $mod+Shift+Up move up
  bindsym $mod+Shift+Right move right

  # split in horizontal orientation
  bindsym $mod+c split h

  # split in vertical orientation
  bindsym $mod+v split v

  # enter fullscreen mode for the focused container
  bindsym $mod+e fullscreen toggle

  # toggle tiling / floating
  bindsym $mod+t floating toggle

  # kill focused window
  bindsym $mod+q kill

  # Window rules
  for_window [window_role="pop-up"]      floating enable
  for_window [window_role="bubble"]      floating enable
  for_window [window_role="task_dialog"] floating enable
  for_window [window_role="Preferences"] floating enable
  for_window [window_type="dialog"]      floating enable
  for_window [window_type="menu"]        floating enable
  for_window [window_role="task_dialog"] floating enable
  for_window [class="Gimp"]              floating enable
  for_window [class="mpv"]               floating enable
  for_window [class=".*"]                inhibit_idle fullscreen

  # Define names for default workspaces for which we configure key bindings later on.
  # We use variables to avoid repeating the names in multiple places.
  set $ws1 "α"
  set $ws2 "β"
  set $ws3 "γ"
  set $ws4 "δ"
  set $ws5 "ε"
  set $ws6 "ζ"
  set $ws7 "η"
  set $ws8 "θ"
  set $ws9 "ι"
  set $ws10 "κ"

  # switch to named workspace
  bindsym $mod+1 workspace $ws1
  bindsym $mod+2 workspace $ws2
  bindsym $mod+3 workspace $ws3
  bindsym $mod+4 workspace $ws4
  bindsym $mod+5 workspace $ws5
  bindsym $mod+6 workspace $ws6
  bindsym $mod+7 workspace $ws7
  bindsym $mod+8 workspace $ws8
  bindsym $mod+9 workspace $ws9
  bindsym $mod+0 workspace $ws10

  # switch to prev/next workspace
  bindsym ctrl+Left  workspace prev
  bindsym ctrl+Right workspace next

  # move focused container to workspace
  bindsym $mod+Shift+1 move container to workspace $ws1
  bindsym $mod+Shift+2 move container to workspace $ws2
  bindsym $mod+Shift+3 move container to workspace $ws3
  bindsym $mod+Shift+4 move container to workspace $ws4
  bindsym $mod+Shift+5 move container to workspace $ws5
  bindsym $mod+Shift+6 move container to workspace $ws6
  bindsym $mod+Shift+7 move container to workspace $ws7
  bindsym $mod+Shift+8 move container to workspace $ws8
  bindsym $mod+Shift+9 move container to workspace $ws9
  bindsym $mod+Shift+0 move container to workspace $ws10

  # Workspace back-and-forth
  workspace_auto_back_and_forth yes

  # reload the configuration file
  bindsym $mod+Shift+r reload

  # start a program launcher
  bindsym $mod+d exec wofi --show drun

  # applications shortcuts
  bindsym $mod+F2 exec qutebrowser
  bindsym $mod+w  exec emacsclient -nc

  # Volume
  bindsym XF86AudioRaiseVolume exec ~/.config/scripts/volume.sh up
  bindsym XF86AudioLowerVolume exec ~/.config/scripts/volume.sh down
  bindsym XF86AudioMute       exec ~/.config/scripts/volume.sh mute

  # Brightness
  bindsym XF86MonBrightnessDown exec brightnessctl -q set 10%-
  bindsym XF86MonBrightnessUp   exec brightnessctl -q set 10%+

  # Screenshot
  bindsym $mod+Print   exec grimshot copy area
  bindsym Print        exec grimshot copy active
  bindsym $mod+Shift+s exec grimshot save screen ~/Pictures/screenshots/$(date "+%B-%d-%Y-%I:%M-%p").png

  # Toggle waybar
  bindsym $mod+b exec pkill -USR1 waybar

  bindsym $mod+Shift+q exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'swaymsg exit'

  default_border pixel ${borderWidth}

  gaps              inner 8
  smart_borders     on

  set $color0 #${colors.c0}
  set $color1 #${colors.c1}
  set $color2 #${colors.c2}
  set $color3 #${colors.c3}
  set $color4 #${colors.c4}
  set $color5 #${colors.c5}
  set $color6 #${colors.c6}
  set $color7 #${colors.c7}
  set $color8 #${colors.c8}
  set $color9 #${colors.c9}
  set $color10 #${colors.c10}
  set $color11 #${colors.c11}
  set $color12 #${colors.c12}
  set $color13 #${colors.c13}
  set $color14 #${colors.c14}
  set $color15 #${colors.c15}

  # class                 border    backgr    text    indicator
  client.focused          #${colors.primary} ${colors.primary} $color7 $color5
  client.focused_inactive #${colors.muted} ${colors.muted} $color7 $color5
  client.unfocused        #${colors.muted} ${colors.muted} $color7 $color5
  client.urgent           $color10 $color10 $color0 $color5

  output "*" bg ${wallpaper} fill

  exec swayidle -w \
      timeout 600 'swaylock && swaymsg "output * dpms off"' \
      resume 'swaymsg "output * dpms on"' \

  exec mako
''
