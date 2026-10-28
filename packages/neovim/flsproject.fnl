 {:fennel-path "./fnl/?.fnl;./fnl/?/init.fnl"
  :macro-path "./fnl/?.fnlm;./fnl/?/init.fnlm;./.laurel/?.fnl;./fnl/?.fnl;./fnl/?/init-macros.fnl;./fnl/?/init.fnl"
  :lua-version :lua5.1
  :extra-globals "vim Snacks bit jit unpack"}
;; fennel-ls project for the Neovim config. The macros of nvim-laurel live in
;; the Nix store, so .laurel links to them; moni.lsp keeps that link current.
{:fennel-path "./fnl/?.fnl;./fnl/?/init.fnl"
 :macro-path "./fnl/?.fnlm;./fnl/?/init.fnlm;./.laurel/?.fnl;./fnl/?.fnl;./fnl/?/init-macros.fnl;./fnl/?/init.fnl"
 :lua-version :lua5.1
 :extra-globals "vim Snacks bit jit unpack"}
