{ pkgs, colors }:

with pkgs;

let
  colorscheme =
    if colors.vimColorscheme == "horizon"
    then {
      plugin = vimUtils.buildVimPlugin rec {
        name = "vim-horizon";
        src = vim-horizon-src;
      };

      config = "colorscheme ${colors.vimColorscheme}";
    }
    else {
      plugin = vimUtils.buildVimPlugin rec {
        name = "vim-flowtune";
        src = vim-flowtune-src;
      };

      config = "colorscheme ${colors.vimColorscheme}";
    };
in
with vimPlugins; [
  coc-nvim
  colorscheme
  vim-hexokinase
  vim-nix

  {
    plugin = vim-closetag;

    config = ''
      let g:closetag_filenames = '*.html,*.xhtml,*.phtml,*.xml'
      let g:closetag_xhtml_filenames = '*.xhtml,*.jsx,*.xml'
      let g:closetag_filetypes = 'html,xhtml,phtml,xml'
      let g:closetag_xhtml_filetypes = 'xhtml,jsx,xml'
      let g:closetag_emptyTags_caseSensitive = 1
      let g:closetag_regions = {
          \ 'typescript.tsx': 'jsxRegion,tsxRegion',
          \ 'javascript.jsx': 'jsxRegion',
          \ }
      let g:closetag_shortcut = '>'
    '';
  }

  {
    plugin = lightline-vim;
    config = "let g:lightline = { 'colorscheme' : '${colors.vimColorscheme}' }";
  }
]
