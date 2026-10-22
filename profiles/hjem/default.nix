{
  imports = [
    ./atuin.nix
    ./direnv.nix
    ./git.nix
    ./htop.nix
    ./jujutsu.nix
    ./neovim.nix
    ./nushell.nix
    ./packages.nix
    ./rex.nix
    ./starship.nix
    ./zoxide.nix
  ];

  environment.sessionVariables = {
    EDITOR = "nvim";
    DIRENV_LOG_FORMAT = "";
  };
}
