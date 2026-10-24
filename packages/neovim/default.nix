{
  lib,
  pkgs,
  vimUtils,
  vimPlugins,
  wrapNeovimUnstable,
  # plugin sources as flake inputs.
  sources,
  neovim ? pkgs.neovim-unwrapped,
  defaultExtraPackages ? __attrValues {
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
      svelte-language-server
      taplo
      tinymist
      vscode-json-languageserver
      ;
  },
  extraPackages ? defaultExtraPackages,
  extraPlugins ? [ ],
  plugins ? null,
}:

let
  swapNerdGlyphs = ''
    HOME=$TMPDIR ${lib.getExe' neovim "nvim"} --clean -l ${./nerd-to-unicode.lua} $(find lua -name '*.lua')
  '';

  moni-config = vimUtils.buildVimPlugin {
    pname = "moni-config";
    version = "0";
    doCheck = false;
    src = lib.fileset.toSource {
      root = ./.;
      fileset = ./lua;
    };
  };

  mellifluous = vimUtils.buildVimPlugin {
    pname = "mellifluous.nvim";
    version = sources.mellifluous-nvim.shortRev or "dirty";
    src = sources.mellifluous-nvim;
  };

  pulse = vimUtils.buildVimPlugin {
    pname = "pulse.nvim";
    version = sources.pulse-nvim.shortRev or "dirty";
    src = sources.pulse-nvim;
    postPatch = swapNerdGlyphs;
  };

  blink-cmp = vimPlugins.blink-cmp.overrideAttrs (old: {
    postPatch = (old.postPatch or "") + swapNerdGlyphs;
  });

  dropbar = vimPlugins.dropbar-nvim.overrideAttrs (old: {
    postPatch = (old.postPatch or "") + swapNerdGlyphs;
  });

  vellum = vimUtils.buildVimPlugin {
    pname = "vellum.nvim";
    version = sources.vellum-nvim.shortRev or "dirty";
    src = sources.vellum-nvim;
    nvimSkipModules = [ "build" ];
    postPatch = swapNerdGlyphs;
  };

  smart-splits-backend-rex = vimUtils.buildVimPlugin {
    pname = "smart-splits-backend-rex";
    version = sources.smart-splits-backend-rex.shortRev or "dirty";
    src = sources.smart-splits-backend-rex;
  };

  defaultPlugins = __attrValues {
    inherit (vimPlugins)
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

    nvim-treesitter = vimPlugins.nvim-treesitter.withPlugins (
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
          svelte
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
(wrapNeovimUnstable neovim {
  plugins = (if plugins == null then defaultPlugins else plugins) ++ extraPlugins;
  luaRcContent = ''require("moni")'';

  wrapperArgs = [
    "--suffix"
    "PATH"
    ":"
    (lib.makeBinPath extraPackages)
  ];
}).overrideAttrs
  (old: {
    passthru = (old.passthru or { }) // {
      inherit defaultExtraPackages defaultPlugins;
    };
  })
