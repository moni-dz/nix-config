''
bind k send-keys -t.- 'tmkill' Enter
set -sg escape-time 0
set -g base-index 1
set -g status-bg black
set -g status-fg white
set -g status-right ""
set -g status-left ""
set -g status-justify centre
set -g window-status-current-format "#[fg=magenta]#[fg=black]#[bg=magenta]#I #[bg=brightblack]#[fg=white] #W#[fg=brightblack]#[bg=black] "
set -g window-status-format "#[fg=yellow]#[fg=black]#[bg=yellow]#I #[bg=brightblack]#[fg=white] #W#[fg=brightblack]#[bg=black] "
set -g set-titles on
set -g set-titles-string '#W'
setw -g automatic-rename on
set -g focus-events on
''
