''
  (defpoll active-workspace :interval "500ms" :initial "w" "sway-active-ws")

  (defwindow active-workspace-indicator
             :monitor 0
             :geometry (geometry :x "20px"
                                 :y "20px"
                                 :width "40px"
                                 :height "40px"
                                 :anchor "right bottom")
             :stacking "fg"
             :reserve (struts :distance "40px" :side "top")
             :windowtype "dock"
             :wm-ignore true
      (label :text active-workspace))
''
