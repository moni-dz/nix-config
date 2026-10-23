{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;

  cfg = config.rum.programs.neovim;

  mellifluous = pkgs.vimUtils.buildVimPlugin {
    pname = "mellifluous.nvim";
    version = inputs.mellifluous-nvim.shortRev or "dirty";
    src = inputs.mellifluous-nvim;
  };

  smart-splits-backend-rex = pkgs.vimUtils.buildVimPlugin {
    pname = "smart-splits-backend-rex";
    version = inputs.smart-splits-backend-rex.shortRev or "dirty";
    src = inputs.smart-splits-backend-rex;
  };

  plugins = __attrValues {
    inherit (pkgs.vimPlugins)
      fzf-lua
      gitsigns-nvim
      heirline-nvim
      jj-nvim
      smart-splits-nvim
      ;

    inherit mellifluous smart-splits-backend-rex;

    nvim-treesitter = pkgs.vimPlugins.nvim-treesitter.withPlugins (
      p:
      __attrValues {
        inherit (p)
          bash
          c
          cpp
          diff
          gitcommit
          jjdescription
          json
          lua
          markdown
          markdown_inline
          nix
          rust
          toml
          typst
          ;
      }
    );
  };
in
{
  options.rum.programs.neovim = {
    enable = mkEnableOption "Neovim, wrapped with its plugins, language servers and config";

    extraPackages = mkOption {
      type = types.listOf types.package;
      default = __attrValues {
        inherit (pkgs)
          clang-tools # clangd
          fd
          fzf
          git
          nixd
          nixfmt
          ripgrep
          rust-analyzer
          tinymist
          ;
      };
      description = "Language servers and tools put on Neovim's PATH, and nothing else's.";
    };

    package = mkOption {
      type = types.package;
      description = "The wrapped Neovim, the config in {file}`neovim/init.lua` is baked in.";
      default = pkgs.wrapNeovimUnstable pkgs.neovim-unwrapped {
        inherit plugins;
        luaRcContent = lib.concatMapStringsSep "\n" builtins.readFile [
          ./neovim/init.lua
          ./neovim/statusline.lua
        ];
        wrapperArgs = [
          "--suffix"
          "PATH"
          ":"
          (lib.makeBinPath cfg.extraPackages)
        ];
      };
    };
  };

  config = mkIf cfg.enable {
    packages = [ cfg.package ];
  };
}
