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

  swapNerdGlyphs = ''
    HOME=$TMPDIR ${lib.getExe' pkgs.neovim-unwrapped "nvim"} --clean -l ${./neovim/nerd-to-unicode.lua} $(find lua -name '*.lua')
  '';

  moni-config = pkgs.vimUtils.buildVimPlugin {
    pname = "moni-config";
    version = "0";
    doCheck = false;
    src = lib.fileset.toSource {
      root = ./neovim;
      fileset = ./neovim/lua;
    };
  };

  mellifluous = pkgs.vimUtils.buildVimPlugin {
    pname = "mellifluous.nvim";
    version = inputs.mellifluous-nvim.shortRev or "dirty";
    src = inputs.mellifluous-nvim;
  };

  pulse = pkgs.vimUtils.buildVimPlugin {
    pname = "pulse.nvim";
    version = inputs.pulse-nvim.shortRev or "dirty";
    src = inputs.pulse-nvim;
    postPatch = swapNerdGlyphs;
  };

  blink-cmp = pkgs.vimPlugins.blink-cmp.overrideAttrs (old: {
    postPatch = (old.postPatch or "") + swapNerdGlyphs;
  });

  dropbar = pkgs.vimPlugins.dropbar-nvim.overrideAttrs (old: {
    postPatch = (old.postPatch or "") + swapNerdGlyphs;
  });

  vellum = pkgs.vimUtils.buildVimPlugin {
    pname = "vellum.nvim";
    version = inputs.vellum-nvim.shortRev or "dirty";
    src = inputs.vellum-nvim;
    nvimSkipModules = [ "build" ];
    postPatch = swapNerdGlyphs;
  };

  smart-splits-backend-rex = pkgs.vimUtils.buildVimPlugin {
    pname = "smart-splits-backend-rex";
    version = inputs.smart-splits-backend-rex.shortRev or "dirty";
    src = inputs.smart-splits-backend-rex;
  };

  plugins = __attrValues {
    inherit (pkgs.vimPlugins)
      flash-nvim
      gitsigns-nvim
      hunk-nvim
      heirline-nvim
      jj-nvim
      mini-ai
      mini-bracketed
      mini-hipatterns
      mini-pairs
      mini-surround
      nvim-tree-lua
      nvim-treesitter-textobjects
      nvim-ufo
      smart-splits-nvim
      snacks-nvim
      which-key-nvim
      ;

    inherit
      blink-cmp
      dropbar
      mellifluous
      moni-config
      pulse
      smart-splits-backend-rex
      vellum
      ;

    nvim-treesitter = pkgs.vimPlugins.nvim-treesitter.withPlugins (
      p:
      __attrValues {
        inherit (p)
          bash
          c
          cmake
          cpp
          css
          diff
          dockerfile
          fish
          git_config
          gitattributes
          gitcommit
          gitignore
          go
          graphql
          hcl
          html
          ini
          javascript
          jjdescription
          json
          json5
          just
          lua
          luadoc
          make
          markdown
          markdown_inline
          nix
          nu
          proto
          python
          query
          regex
          rust
          scss
          sql
          terraform
          toml
          tsx
          typescript
          typst
          vim
          vimdoc
          xml
          yaml
          zig
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
          bash-language-server
          clang-tools
          ghostscript
          git
          imagemagick
          lua-language-server
          nixd
          nixfmt
          ripgrep
          rust-analyzer
          taplo
          tinymist
          vscode-json-languageserver
          ;
      };
      description = "Language servers and tools put on Neovim's PATH, and nothing else's.";
    };

    package = mkOption {
      type = types.package;
      description = "The wrapped Neovim, the config in {file}`neovim/lua/moni` is baked in.";
      default =
        pkgs.wrapNeovimUnstable
          inputs.neovim-nightly-overlay.packages.${pkgs.stdenv.hostPlatform.system}.default
          {
            inherit plugins;
            luaRcContent = ''require("moni")'';
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
