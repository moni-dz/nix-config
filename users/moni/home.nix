{
  config,
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
  home = {
    packages = __attrValues {
      inherit (pkgs)
        curl
        nix-output-monitor
        parallel
        fd
        ripgrep
        jq
        krabby
        nixd
        nixfmt-rfc-style
        typst
        tinymist
        ;
    };
  };

  programs = {
    fish = {
      interactiveShellInit = ''
        function export
          if [ $argv ] 
            set var (echo $argv | cut -f1 -d=)
            set val (echo $argv | cut -f2 -d=)
            set -g -x $var $val
          else
            echo 'export var=value'
          end
        end

        . ${config.age.secrets.tokens.path}
        # . /Users/moni/.local/share/miniconda/etc/fish/conf.d/conda.fish
      '';

      shellInit = ''
        fish_add_path -amP /usr/bin
        fish_add_path -amP /opt/homebrew/bin
        fish_add_path -amP /Users/moni/.modular/bin
        fish_add_path -amP /opt/local/bin
        fish_add_path -amP /opt/homebrew/opt/llvm/bin
        fish_add_path -amP /Users/moni/.local/share/modular/pkg/packages.modular.com_mojo/bin
        fish_add_path -m /run/current-system/sw/bin
        fish_add_path -m /Users/moni/.nix-profile/bin

        set --export BUN_INSTALL "$HOME/.bun"
        set --export PATH $BUN_INSTALL/bin $PATH
      '';
    };

    nushell = {
      envFile.text = ''
        open $"(getconf DARWIN_USER_TEMP_DIR)/agenix/tokens" | split column " " | get column2 | split column "=" | reduce -f {} {|it, acc| $acc | upsert $it.column1 $it.column2 } | load-env
        $env.BUN_INSTALL = ($env.HOME | path join ".bun")
        $env.path ++= ["/Users/moni/.local/state/nix/profiles/profile/bin" "/usr/bin" "/opt/homebrew/bin" "/Users/moni/.modular/bin" "/opt/local/bin" "/opt/homebrew/opt/llvm/bin" "/Users/moni/.local/share/modular/pkg/packages.modular.com_mojo/bin" ($env.BUN_INSTALL | path join "bin") "/Users/Shared/DBngin/postgresql/17.0/bin"]
        $env.config.buffer_editor = "nvim"
        $env.config.show_banner = false
      '';
    };
  };
}
