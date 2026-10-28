(import-macros {: map!} :laurel.macros)
(import-macros {: setup!} :moni.macros)

(local which-key (require :which-key))

;; fnlfmt: skip
(local
  key-icons
  {:Up "↑"
   :Down "↓"
   :Left "←"
   :Right "→"
   :C "C-"
   :M "M-"
   :D "D-"
   :S "S-"
   :CR "CR"
   :Esc "Esc"
   :NL "NL"
   :BS "BS"
   :Space "Space"
   :Tab "Tab"
   :ScrollWheelDown "WheelDn"
   :ScrollWheelUp "WheelUp"
   :F1 "F1"
   :F2 "F2"
   :F3 "F3"
   :F4 "F4"
   :F5 "F5"
   :F6 "F6"
   :F7 "F7"
   :F8 "F8"
   :F9 "F9"
   :F10 "F10"
   :F11 "F11"
   :F12 "F12"})

;; fnlfmt: skip
(which-key.setup
  {:preset :helix
   :icons {:separator "→" :mappings false :rules false :keys key-icons}})

(which-key.add [{1 :<leader>f :group :find}
                {1 :<leader>h :group :hunk}
                {1 :<leader>b :group :buffer}
                {1 :<leader>j :group :jj}
                {1 :<leader>m :group :markdown}
                {1 :gz :group :surround}
                {1 :<leader>r :group :run}
                {1 :<leader>x :group :diagnostics}])

(map! :n :<leader>? [:desc "buffer-local keymaps"]
      (partial which-key.show {:global false}))

;; fnlfmt: skip
(setup! :dropbar
  {:icons
     {:enable false
      :ui {:bar {:separator " › " :extends "…"} :menu {:separator " " :indicator "► "}}}})
