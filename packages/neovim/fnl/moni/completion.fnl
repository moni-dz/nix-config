(import-macros {: setup! : lazy} :moni.macros)

(λ run-line? []
  (not= (: (vim.fn.getcmdline) :match "^%s*Run%s") nil))

(λ cmdline-position []
  (lazy :moni.cmdline :position))

(local label-width {:label {:width {:fill true :max 36}}})

;; fnlfmt: skip
(local 
  kind-icons
  {:Text "≡" 
   :Method "ƒ"
   :Function "ƒ"
   :Constructor "⌂"
   :Field "·"
   :Variable "●"
   :Class "◊"
   :Interface "○"
   :Module "□"
   :Property "·"
   :Unit "µ"
   :Value "#"
   :Enum "Σ"
   :Keyword "κ"
   :Snippet "§"
   :Color "☼"
   :File "▬"
   :Reference "→"
   :Folder "►"
   :EnumMember "ε"
   :Constant "π"
   :Struct "Δ"
   :Event "♪"
   :Operator "±"
   :TypeParameter "τ"
   :Command "▸"
   :Subcommand "›"
   :Option "-"
   :Path "▬"})

;; fnlfmt: skip
(setup! 
  :blink.cmp
  {:appearance {:kind_icons kind-icons}

   :sources 
     {:per_filetype {:moni-run [:shell]}
      :providers {:shell {:name :Shell :module :moni.shellsource} :cmdline {:enabled (λ [] (not (run-line?)))}}}

   :cmdline {:sources (λ []
                        (if (= (vim.fn.getcmdtype) ":")
                            [:cmdline :shell]
                            (if (= (vim.fn.getcmdtype) "@")
                                [:cmdline]
                                [:buffer])))}

   :keymap 
     {:preset :default
      :<Tab> [:select_and_accept :snippet_forward :fallback]
      :<S-Tab> [:snippet_backward :fallback]
      :<CR> [:accept :fallback]}

   :completion 
     {:list 
        {:selection {:preselect false :auto_insert false}}

      :menu 
        {:cmdline_position cmdline-position
         :min_width 24
         :border :rounded
         :scrollbar false
         :draw {:columns [[:kind_icon] [:label] [:kind]] :components label-width}}

      :documentation 
        {:auto_show true
         :auto_show_delay_ms 300
         :window {:min_width 30 :max_width 60 :max_height 16 :border :rounded :scrollbar false}}}

   :signature {:enabled true}
   :fuzzy {:implementation :prefer_rust}})

(λ over-blink-menu []
  (let [(ok menu) (pcall require :blink.cmp.completion.windows.menu)]
    (if (and ok (menu.win:is_open))
        (let [pos (vim.fn.getmousepos)]
          (if (= pos.winid (menu.win:get_win))
              pos)))))

(λ schedule-list [method opts]
  (vim.schedule (λ []
                  (lazy :blink.cmp.completion.list method opts))))

(each [_ mode (ipairs [:i :c])]
  (vim.keymap.set mode :<LeftMouse>
                  (λ []
                    (let [pos (over-blink-menu)]
                      (if (not pos)
                          :<LeftMouse>
                          (do
                            (schedule-list :accept {:index pos.line})
                            ""))))
                  {:expr true :desc "click completion item"})
  (each [key step (pairs {:<ScrollWheelDown> :select_next
                          :<ScrollWheelUp> :select_prev})]
    (vim.keymap.set mode key (λ []
                               (if (not (over-blink-menu))
                                   key
                                   (do
                                     (schedule-list step {:count 3})
                                     "")))
                    {:expr true :desc "scroll completion menu"})))
