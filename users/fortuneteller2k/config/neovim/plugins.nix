{ pkgs, colors }:

with pkgs;

let
  colorscheme =
    if colors.vimColorscheme == "horizon"
    then {
      plugin = vimUtils.buildVimPlugin rec {
        name = "vim-horizon";

        src = fetchFromGitHub {
          owner = "fortuneteller2k";
          repo = name;
          rev = "e0c0a68f59ba6337517ded340391bab3f8476363";
          sha256 = "sha256-3NT7ggghEljttcoWzgigK4m0sHOGA+M6k2sYooEAYdM=";
        };
      };

      config = "colorscheme ${colors.vimColorscheme}";
    }
    else {
      plugin = vimUtils.buildVimPlugin rec {
        name = "vim-flowtune";

        src = fetchFromGitHub {
          owner = "fortuneteller2k";
          repo = name;
          rev = "e4b54733e8e5aa7b1b0f17ca4476f2a9bd91abe9";
          sha256 = "sha256-mlIhQi6noq7Hb0LQ8cQzWyZRxiHSrsok56Zl4wLEaHI=";
        };
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
