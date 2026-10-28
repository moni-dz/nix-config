(import-macros {: map!} :laurel.macros)

(local severity vim.diagnostic.severity)

;; fnlfmt: skip
(vim.diagnostic.config
  {:signs {:text {severity.ERROR "■" severity.WARN "▲" severity.INFO "●" severity.HINT "•"}}
   :virtual_text {:prefix "■" :spacing 2 :current_line false}
   :virtual_lines {:current_line true}
   :severity_sort true
   :float {:border :rounded :source true :header ""}})

(map! :n :<leader>e [:desc "diagnostics at cursor"] vim.diagnostic.open_float)
(map! :n "]e" [:desc "next error"]
      (partial vim.diagnostic.jump {:count 1 :severity severity.ERROR}))

(map! :n "[e" [:desc "previous error"]
      (partial vim.diagnostic.jump {:count -1 :severity severity.ERROR}))

(map! :nx :<leader>a [:desc "code action"] vim.lsp.buf.code_action)
(map! :n :<leader>xq [:desc "all diagnostics to quickfix"]
      (partial vim.diagnostic.setqflist {:open true}))

(map! :n :<leader>xl [:desc "this buffer's diagnostics to the location list"]
      (partial vim.diagnostic.setloclist {:open true}))

(map! :n :<leader>xx [:desc "toggle diagnostics"]
      #(vim.diagnostic.enable (not (vim.diagnostic.is_enabled))))
