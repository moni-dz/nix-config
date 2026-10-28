(import-macros {: set! : map!} :laurel.macros)

(set vim.g.mapleader " ")

(set! :number false)
(set! :signcolumn :yes)
(set! :cursorline true)
(set! :scrolloff 8)
(set! :splitright true)
(set! :splitbelow true)
(set! :ignorecase true)
(set! :smartcase true)
(set! :undofile true)
(set! :expandtab true)
(set! :shiftwidth 2)
(set! :tabstop 2)
(set! :updatetime 250)
(set! :clipboard :unnamedplus)

;; fnlfmt: skip
(let [osc52 (require :vim.ui.clipboard.osc52)
      paste (λ [] [(vim.fn.split (vim.fn.getreg "") "\n") (vim.fn.getregtype "")])]
  (set vim.g.clipboard
    {:name "OSC 52"
     :copy {:+ (osc52.copy "+") :* (osc52.copy "*")}
     :paste {:+ paste :* paste}}))

(set! :winborder :rounded)

(set! :mouse :a)
(set! :mousemodel :extend)
(set! :mousescroll "ver:3,hor:6")

(set vim.g.loaded_netrw 1)
(set vim.g.loaded_netrwPlugin 1)

(set! :wildoptions "pum,fuzzy")

;; fnlfmt: skip
(map! :n :<leader>N [:desc "toggle line numbers"] #(set vim.wo.number (not vim.wo.number)))
