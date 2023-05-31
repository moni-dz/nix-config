{
  command_timeout = 3000;
  format = "$username$nix_shell$character";
  right_format = "$directory$git_branch$git_commit$git_state$git_status";

  character = {
    success_symbol = "[Σ](bold green)";
    error_symbol = "[Σ](bold red)";
    vimcmd_symbol = "[ζ](bold green)";
    vimcmd_replace_symbol = "[ζ](bold purple)";
    vimcmd_replace_one_symbol = "[ζ](bold purple)";
    vimcmd_visual_symbol = "[ζ](bold yellow)";
  };

  username = {
    format = "[$user]($style) ";
    disabled = false;
    show_always = true;
  };

  hostname = {
    ssh_only = false;
    format = "at [$hostname](bold blue) in ";
    disabled = true;
  };

  git_status = {
    ahead = "↑";
    behind = "↓";
    conflicted = "±";
    deleted = "×";
    diverged = "↕";
    modified = "‼";
    renamed = "≡";
    stashed = "⌂";
    format = ''( [\[$all_status$ahead_behind\]]($style))'';
  };

  git_branch = {
    format = " → [$symbol$branch(:$remote_branch)]($style)";
    symbol = "";
  };

  battery.disabled = true;
  line_break.disabled = true;
  
  directory = {
    read_only = "(ro)";
    format = "[$read_only]($read_only_style) [$path]($style)";
  };
  
  nix_shell.symbol = "(nix)";
}
