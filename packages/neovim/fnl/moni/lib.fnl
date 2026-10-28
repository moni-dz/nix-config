;; fnlfmt: skip
(λ set-all! [target args]
  (for [i 1 (length args) 2] (tset target (. args i) (. args (+ i 1)))))

(λ bo! [buf ...]
  "Set buffer options in order: (bo! buf :modifiable false :bufhidden :wipe)."
  (set-all! (. vim.bo buf) [...]))

(λ wo! [win ...]
  "Set window options in order: (wo! win :wrap false :cursorline true)."
  (set-all! (. vim.wo win) [...]))

(λ float! [buf enter? config ?winhighlight]
  "Open a minimal, rounded float on `buf`. `config` is nvim_open_win's; a :title is padded and put on the left."
  (let [config (vim.tbl_extend :keep config {:style :minimal :border :rounded})]
    (when config.title
      (set config.title (.. " " config.title " "))
      (set config.title_pos :left))
    (let [win (vim.api.nvim_open_win buf enter? config)]
      (when ?winhighlight (wo! win :winhighlight ?winhighlight))
      win)))

(λ nmap! [buf keys f]
  "In `buf`, map each of `keys`, a key or a list of keys, to `f` in normal mode."
  (each [_ lhs (ipairs (if (= (type keys) :table) keys [keys]))]
    (vim.keymap.set :n lhs f {:buffer buf :nowait true})))

{: bo! : wo! : float! : nmap!}
