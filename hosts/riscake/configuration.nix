{ config, lib, pkgs, system, self, inputs, inputs', ... }:

{
  environment = {
    shells = __attrValues { inherit (pkgs) fish; };

    systemPackages = __attrValues {
      inherit (pkgs) gawk;
      inherit (inputs'.agenix.packages) agenix;
    };
  };

  services = {
    nix-daemon.enable = true;

    skhd = {
      enable = true;

      skhdConfig = ''
        cmd - return : open -na wezterm
      '';
    };
  };

  programs.fish.enable = true;
  security.pam.enableSudoTouchIdAuth = true;

  fonts = {
    fontDir.enable = true;

    fonts = __attrValues {
      inherit (pkgs)
        fira-code
        comic-neue
        fantasque-sans-mono
        maple-mono-otf
        emacs-all-the-icons-fonts;
    };
  };
}
