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

  plugins = [
    mellifluous
    pkgs.vimPlugins.fzf-lua
    pkgs.vimPlugins.gitsigns-nvim
    pkgs.vimPlugins.heirline-nvim
    pkgs.vimPlugins.jj-nvim
    pkgs.vimPlugins.smart-splits-nvim
    smart-splits-backend-rex
    (pkgs.vimPlugins.nvim-treesitter.withPlugins (p: [
      p.bash
      p.c
      p.cpp
      p.diff
      p.gitcommit
      p.jjdescription
      p.json
      p.lua
      p.markdown
      p.markdown_inline
      p.nix
      p.rust
      p.toml
      p.typst
    ]))
  ];
in
{
  options.rum.programs.neovim = {
    enable = mkEnableOption "Neovim, wrapped with its plugins, language servers and config";

    extraPackages = mkOption {
      type = types.listOf types.package;
      default = [
        pkgs.clang-tools # clangd
        pkgs.fd
        pkgs.fzf
        pkgs.git
        pkgs.nixd
        pkgs.nixfmt
        pkgs.ripgrep
        pkgs.rust-analyzer
        pkgs.tinymist
      ];
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
