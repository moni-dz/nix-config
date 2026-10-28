(import-macros {: map!} :laurel.macros)
(import-macros {: setup!} :moni.macros)

(local flash (require :flash))

;; fnlfmt: skip
(flash.setup
  {:labels :asdfghjklqwertyuiopzxcvbnm
   :prompt
     {:prefix [["› " :FlashPromptIcon]]}

   :modes {:search {:enabled false} :char {:jump_labels true}}})

(map! :nxo :s [:desc "jump to text"] flash.jump)

(map! :nxo :S [:desc "select a treesitter node"] flash.treesitter)

(map! :o :r [:desc "act on text elsewhere"] flash.remote)

(map! :ox :R [:desc "treesitter search"] flash.treesitter_search)

(map! :c :<C-s> [:desc "flash in the / search"] flash.toggle)

(let [ai (require :mini.ai)
      treesitter (λ [outer inner]
                   (ai.gen_spec.treesitter {:a outer :i inner}))]
  (ai.setup {:n_lines 300
             :mappings {:around_next :aN
                        :inside_next :iN
                        :around_last :aL
                        :inside_last :iL}
             :custom_textobjects {:f (treesitter "@function.outer"
                                                 "@function.inner")
                                  :c (treesitter "@class.outer" "@class.inner")
                                  :a (treesitter "@parameter.outer"
                                                 "@parameter.inner")
                                  :o (treesitter ["@conditional.outer"
                                                  "@loop.outer"]
                                                 ["@conditional.inner"
                                                  "@loop.inner"])}}))

;; fnlfmt: skip
(setup! :mini.bracketed
  {:comment {:suffix ""}
   :undo {:suffix ""}})
