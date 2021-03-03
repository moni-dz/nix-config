{
  character = {
    success_symbol = "[^](bold green)";
    error_symbol = "[^](bold red)";
    vicmd_symbol = "[^](bold yellow)";
  };
  format = "$all";
  username = {
    style_user = "bold red";
    format = "[$user]($style) ";
    disabled = true;
    show_always = true;
  };
  hostname = {
    ssh_only = false;
    format = "at [$hostname](bold blue) in ";
    disabled = true;
  };
  line_break.disabled = true;
  directory.format = "[$path]($style)[$read_only]($read_only_style) ";
  nix_shell.symbol = "🌨  ";
  nodejs.symbol = " ";
  package.symbol = "📦  ";
}
