{ pkgs, theme }:

with theme.colors;

''
  exec autotiling

  # modkey
  set $mod Mod1

  # Use Mouse+$mod to drag floating windows to their wanted position
  floating_modifier $mod

  # start a terminal
  set $term footclient
  bindsym $mod+Return exec $term

  # spawn menu
  set $menu "${pkgs.bemenu}/bin/bemenu-run -H 18 -l 5 --fn 'Iosevka FT 10.5' --tb '#${primaryBright}' --tf '#${bg}' --hb '#${primaryBright}' --hf '#${bg}'"
  bindsym $mod+d exec $menu

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

  font "pango:FantasqueSansMono Nerd Font 5"

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
  for_window [app_id=".*"]               title_format ""
  for_window [class=".*"]                title_format ""

  titlebar_border_thickness 0
  titlebar_padding 0 0

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
  workspace_auto_back_and_forth no

  # reload the configuration file
  bindsym $mod+Shift+r reload

  # applications shortcuts
  bindsym $mod+F2 exec brave
  bindsym $mod+w  exec emacs

  # Volume
  bindsym XF86AudioRaiseVolume exec ~/.local/bin/volume up
  bindsym XF86AudioLowerVolume exec ~/.local/bin/volume down
  bindsym XF86AudioMute        exec ~/.local/bin/volume toggle

  # Brightness
  bindsym XF86MonBrightnessDown exec brightnessctl -q set 10%-
  bindsym XF86MonBrightnessUp   exec brightnessctl -q set 10%+

  # Screenshot
  bindsym $mod+Print   exec grimshot copy area
  bindsym Print        exec grimshot copy active
  bindsym Mod4+Print   exec grimshot save screen

  # Toggle waybar
  bindsym $mod+b exec pkill -USR1 waybar

  bindsym $mod+Shift+q exec swaynag -t warning -m 'Do you really want to exit sway?' -b 'Yes, exit sway' 'swaymsg exit'

  default_border normal ${theme.borderWidth}
  hide_edge_borders --i3 smart
  gaps inner 8
  smart_borders on

  set $color0 #${c0}
  set $color1 #${c1}
  set $color2 #${c2}
  set $color3 #${c3}
  set $color4 #${c4}
  set $color5 #${c5}
  set $color6 #${c6}
  set $color7 #${c7}
  set $color8 #${c8}
  set $color9 #${c9}
  set $color10 #${c10}
  set $color11 #${c11}
  set $color12 #${c12}
  set $color13 #${c13}
  set $color14 #${c14}
  set $color15 #${c15}

  # class                 border    backgr    text    indicator
  client.focused          #${primary} ${primary} $color0 $color5
  client.focused_inactive #${muted} ${muted} $color0 $color5
  client.unfocused        #${muted} ${muted} $color7 $color5
  client.urgent           $color10 $color10 $color0 $color5

  output "*" bg ${wallpaper} ${if tiledWallpaper then "tile" else "fill"}
  output "*" scale 1
  output "*" scale_filter nearest

  input type:touchpad {
    tap enabled
    natural_scroll enabled
  }

  input type:keyboard {
    repeat_rate 40
    repeat_delay 350
  }

  exec swayidle -w \
      timeout 300 'swaylock -c "${bg}" --font "Sarasa Gothic J"' \
      timeout 310 'swaymsg "output * dpms off"' \
      resume 'swaymsg "output * dpms on"' \
''
