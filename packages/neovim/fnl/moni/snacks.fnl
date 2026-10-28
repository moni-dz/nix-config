(import-macros {: map! : set!} :laurel.macros)
(import-macros {: setup!} :moni.macros)

(when (= vim.env.TERM_PROGRAM :rex)
  (set vim.env.SNACKS_KITTY :1))

(set! :smoothscroll false)

;; fnlfmt: skip
(setup! :snacks
  {:notifier {:timeout 3000 :icons {:error "■" :warn "▲" :info "●" :debug "•" :trace "·"}}
   :indent
     {:indent {:char "▏"}
      :scope {:char "▏"}
      :animate {:enabled false}}

   :words {:debounce 150}
   :image {:math {:enabled false} :icons {:math "∑ " :chart "≡ " :image "□ "}}})

(map! :n :<leader>i [:desc "preview image under cursor"] #(Snacks.image.hover))

(map! :n :<leader>bd [:desc "close buffer, keep the window"]
      #(Snacks.bufdelete))

(map! :n "]]" [:desc "next reference"] #(Snacks.words.jump vim.v.count1 true))
(map! :n "[[" [:desc "previous reference"]
      #(Snacks.words.jump (- vim.v.count1) true))
