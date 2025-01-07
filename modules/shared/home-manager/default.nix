{
  config,
  self',
  inputs,
  inputs',
  lib,
  pkgs,
  ...
}:

/*
  home-manager configuration
  Useful links:
  - Home Manager Manual: https://nix-community.gitlab.io/home-manager/
  - Appendix A. Configuration Options: https://nix-community.gitlab.io/home-manager/options.html
*/
{
  imports = [
    ./fish.nix
    ./vcs.nix
  ];

  home = {
    packages = __attrValues {
      inherit (pkgs) eternal-terminal mosh ripgrep;
      inherit (inputs'.agenix.packages) agenix;

      nvim = self'.packages.neovim.override { libuv = pkgs.libuv.overrideAttrs { doCheck = false; }; };
    };

    sessionVariables = {
      EDITOR = "nvim";
      MANPAGER = "nvim +Man! -c 'nnoremap i <nop>'";
    };
  };

  programs = {
    eza = {
      enable = true;
      package = pkgs.eza;
    };

    home-manager = {
      enable = true;
      path = lib.mkForce "${inputs.home}";
    };

    htop = {
      enable = true;

      settings =
        {
          detailed_cpu_time = true;
          hide_kernel_threads = false;
          show_cpu_frequency = pkgs.stdenv.isLinux;
          show_cpu_usage = true;
          show_program_path = false;
          show_thread_names = true;

          fields = with config.lib.htop.fields; [
            PID
            USER
            PRIORITY
            NICE
            M_SIZE
            M_RESIDENT
            M_SHARE
            STATE
            PERCENT_CPU
            PERCENT_MEM
            TIME
            COMM
          ];
        }
        // (
          with config.lib.htop;
          leftMeters [
            (bar "AllCPUs")
            (bar "Memory")
            (bar "Swap")
          ]
        )
        // (
          with config.lib.htop;
          rightMeters [
            (text "Tasks")
            (text "LoadAverage")
            (text "Uptime")
          ]
        );
    };

    nix-index-database.comma.enable = lib.mkDefault true;
  };

  xdg.configFile."nvim".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.config/nix-config/nvim";
}
