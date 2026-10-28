(import-macros {: setup!} :moni.macros)

(λ on-attach [buf]
  (let [gs (require :gitsigns)
        map (λ [lhs rhs desc] (vim.keymap.set :n lhs rhs {:buffer buf : desc}))]
    (map "]h" (partial gs.nav_hunk :next) "next hunk")
    (map "[h" (partial gs.nav_hunk :prev) "previous hunk")
    (map :<leader>hp gs.preview_hunk "preview hunk")
    (map :<leader>hr gs.reset_hunk "reset hunk")
    (map :<leader>hd gs.diffthis "compare with the parent")
    (map :<leader>hb (partial gs.blame_line {:full true}) "blame line")))

;; fnlfmt: skip
(setup! :gitsigns 
  {:base :HEAD
   :signs {:add {:text "▎"}
           :change {:text "▎"}
           :delete {:text "▁"}
           :topdelete {:text "▔"}
           :changedelete {:text "▎"}
           :untracked {:text "▎"}}
   :linehl true
   :on_attach on-attach})

;; fnlfmt: skip
(setup! :hunk 
  {:icons 
    {:enable_file_icons false
     :selected "●"
     :deselected "○"
     :partially_selected "◘"
     :folder_open "▼"
     :folder_closed "►"
     :expanded "▼"
     :collapsed "►"}})
