(local {: bo! : wo! : float! : nmap!} (require :moni.lib))

(local select-ns (vim.api.nvim_create_namespace :moni_select))

(λ title-of [?prompt]
  (vim.trim (: (or ?prompt "") :gsub "%s*:%s*$" "")))

(λ clip-width [wanted]
  "`wanted`, but no wider than most of the screen."
  (math.min wanted (math.floor (* vim.o.columns 0.7))))

;;; vim.ui.select

(λ select-lines [items format]
  "One numbered line per item; only the first nine can be picked by number."
  (icollect [i item (ipairs items)]
    (let [text (: (tostring (format item)) :gsub "\n.*" "")]
      (string.format " %s  %s " (if (<= i 9) (tostring i) " ") text))))

;; fnlfmt: skip
(λ highlight-numbers! [buf items]
  (for [i 1 (math.min (length items) 9)]
    (vim.api.nvim_buf_set_extmark buf select-ns (- i 1) 1 {:end_col 2 :hl_group :Comment})))

;; fnlfmt: skip
(λ select-window [buf lines title]
  (let [widest (accumulate [widest 0 _ line (ipairs lines)] (math.max widest (vim.fn.strdisplaywidth line)))
        width (clip-width (math.max (+ widest 1) (+ (vim.fn.strdisplaywidth title) 4) 24))
        win
          (float!
            buf
            true
            {:relative :cursor
             :row 1
             :col 0
             : width
             :height (math.min (length lines) 10)
             :title (if (not= title "") title)
             :zindex 60}
            (.. "Normal:NormalFloat,FloatBorder:FloatBorder,"
                "FloatTitle:FloatTitle,CursorLine:PmenuSel"))]
    (wo! win :wrap false :cursorline true)
    win))

;; fnlfmt: skip
(λ bind-select! [buf win items on-choice]
  "Wire the keys and mouse of the select window to `on-choice`."
  (var done false)

  (λ finish [?index]
    (when (not done)
      (set done true)
      (when (vim.api.nvim_win_is_valid win) (vim.api.nvim_win_close win true))
      (vim.schedule (λ [] (on-choice (and ?index (. items ?index)) ?index)))))

  (λ move [delta]
    (let [row (. (vim.api.nvim_win_get_cursor win) 1)
          next-row (+ (% (+ (- row 1) delta) (length items)) 1)]
      (vim.api.nvim_win_set_cursor win [next-row 0])))

  (nmap! buf [:<CR> :<Tab>] (λ [] (finish (. (vim.api.nvim_win_get_cursor win) 1))))
  (nmap! buf [:<Esc> :q :<C-c>] (λ [] (finish nil)))
  (nmap! buf [:j :<Down> :<C-n>] (λ [] (move 1)))
  (nmap! buf [:k :<Up> :<C-p> :<S-Tab>] (λ [] (move (- 1))))

  (for [n 1 (math.min (length items) 9)]
    (nmap! buf (tostring n) (λ [] (finish n))))

  (nmap!
    buf
    :<LeftMouse>
    (λ []
      (let [pos (vim.fn.getmousepos)]
        (finish
          (if (and (= pos.winid win) (<= 1 pos.line) (<= pos.line (length items)))
              pos.line)))))

  (vim.api.nvim_create_autocmd
    [:WinLeave :BufLeave]
    {:buffer buf :once true :callback (λ [] (finish nil) nil)}))

(λ select [items ?opts on-choice]
  (let [opts (or ?opts {})]
    (if (= (length items) 0)
        (on-choice nil nil)
        (let [lines (select-lines items (or opts.format_item tostring))
              buf (vim.api.nvim_create_buf false true)]
          (vim.api.nvim_buf_set_lines buf 0 -1 false lines)
          (bo! buf :modifiable false :bufhidden :wipe)
          (highlight-numbers! buf items)
          (let [win (select-window buf lines (title-of opts.prompt))]
            (bind-select! buf win items on-choice))))))

(set vim.ui.select select)

;;; vim.ui.input

;; fnlfmt: skip
(λ input-window [buf default title]
  (let [width
          (clip-width
            (math.max
              32
              (+ (vim.fn.strdisplaywidth default) 8)
              (+ (vim.fn.strdisplaywidth title) 4)))]
    (float!
      buf
      true
      {:relative :cursor
       :row 1
       :col 0
       : width
       :height 1
       :title (if (not= title "") title)
       :zindex 60}
      (.. "Normal:NormalFloat,FloatBorder:FloatBorder," "FloatTitle:FloatTitle"))))

;; fnlfmt: skip
(λ bind-input! [buf win on-confirm]
  "Wire the input window's keys to `on-confirm`, then start typing."
  (var done false)

  (λ finish [confirm]
    (when (not done)
      (set done true)
      (let [text (. (vim.api.nvim_buf_get_lines buf 0 1 false) 1)]
        (vim.cmd.stopinsert)
        (when (vim.api.nvim_win_is_valid win) (vim.api.nvim_win_close win true))
        (vim.schedule (λ [] (on-confirm (if confirm text)))))))

  (each [_ mode (ipairs [:i :n])]
    (vim.keymap.set mode :<CR> (λ [] (finish true)) {:buffer buf})
    (vim.keymap.set mode :<Esc> (λ [] (finish false)) {:buffer buf})
    (vim.keymap.set mode :<C-c> (λ [] (finish false)) {:buffer buf}))

  (vim.api.nvim_create_autocmd
    :WinLeave
    {:buffer buf :once true :callback (λ [] (finish false) nil)})
  (vim.cmd :startinsert!))

(λ input [?opts on-confirm]
  (let [opts (or ?opts {})
        default (or opts.default "")
        buf (vim.api.nvim_create_buf false true)]
    (vim.api.nvim_buf_set_lines buf 0 -1 false [default])
    (bo! buf :bufhidden :wipe)
    (let [win (input-window buf default (title-of opts.prompt))]
      (bind-input! buf win on-confirm))))

(set vim.ui.input input)
