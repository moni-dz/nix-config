(import-macros {: setup!} :moni.macros)

;; fnlfmt: skip
(setup! :mellifluous
  {:transparent_background {:enabled true :floating_windows false :cursor_line false}})

(vim.cmd.colorscheme :mellifluous)
