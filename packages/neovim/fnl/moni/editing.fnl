(import-macros {: map!} :laurel.macros)
(import-macros {: setup! : on!} :moni.macros)

(on! :FileType [ev] (pcall vim.treesitter.start ev.buf))

(local flash-ns (vim.api.nvim_create_namespace :flash))

(λ flash-changed [buf]
  (let [last (vim.api.nvim_buf_line_count buf)
        first (math.min (. (vim.api.nvim_buf_get_mark buf "[") 1) last)
        final (math.min (. (vim.api.nvim_buf_get_mark buf "]") 1) last)]
    (when (<= 1 first)
      (vim.hl.range buf flash-ns :IncSearch [(- first 1) 0] [(- final 1) -1]
                    {:regtype :V :timeout 150}))))

(on! :TextYankPost [] (vim.hl.hl_op {:higroup :IncSearch :timeout 150}))

(each [key cmd (pairs {:u :u :<C-r> "\018"})]
  (vim.keymap.set :n key (λ []
                           (vim.cmd.normal {1 (.. vim.v.count1 cmd) :bang true})
                           (flash-changed 0))
                  {:desc (if (= key :u) "undo, flashed" "redo, flashed")}))

(vim.cmd.packadd :nvim.undotree)
(vim.cmd.packadd :nohlsearch)
(map! :n :<leader>u [:desc "undo history"] :<Cmd>Undotree<CR>)

(setup! :mini.pairs)

;; fnlfmt: skip
(setup! :mini.surround
  {:mappings
     {:add :gza
      :delete :gzd
      :find :gzf
      :find_left :gzF
      :highlight :gzh
      :replace :gzr
      :update_n_lines :gzn}})

(let [hipatterns (require :mini.hipatterns)
      hex-color (hipatterns.gen_highlighter.hex_color {:style :inline
                                                       :inline_text "■"})]
  (hipatterns.setup {:highlighters {:hex_color hex-color}}))
