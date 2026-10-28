(local cmdline (require :vim._core.ui2.cmdline))

(local {: bo! : wo! : float!} (require :moni.lib))

(local api vim.api)

(api.nvim_set_hl 0 :MoniCmdlinePrompt {:link :MoniMenuHint :default true})

(local ns (api.nvim_create_namespace :moni.cmdline))
(var buf nil)
(var win nil)
(var text "")
(var prefix 0)

(λ win-valid? []
  (and win (api.nvim_win_is_valid win)))

(λ ensure-buf []
  (when (not (and buf (api.nvim_buf_is_valid buf)))
    (set buf (api.nvim_create_buf false true))
    (bo! buf :bufhidden :hide)))

;; fnlfmt: skip
(λ place []
  (let [width (math.min (math.max 48 (math.floor (* vim.o.columns 0.5)))
                        (- vim.o.columns 4))]
    (api.nvim_win_set_config win {: width})
    (let [height (math.min (. (api.nvim_win_text_height win {}) :all)
                           (math.max 1 (- vim.o.lines 6)))
          row (math.max 1 (math.floor (* vim.o.lines 0.2)))
          col (math.floor (/ (- vim.o.columns width 2) 2))]
      (api.nvim_win_set_config win {:relative :editor : row : col : width : height}))))

;; fnlfmt: skip
(λ highlight-tree [tstree tree]
  (let [query (and tstree (vim.treesitter.query.get (tree:lang) :highlights))]
    (when query
      (each [capture node (query:iter_captures (tstree:root) text)]
        (let [(_start-row start-col _end-row end-col) (node:range)
              name (. query.captures capture)
              group (string.format "@%s.%s" name query.lang)]

          (when (not= (name:sub 1 1) "_")
            (api.nvim_buf_set_extmark 
              buf 
              ns 
              0 
              (+ prefix start-col)
              {:end_col (+ prefix end-col) :hl_group group})))))))

;; fnlfmt: skip
(λ highlight [lines ?hl-id]
  (api.nvim_buf_clear_namespace buf ns 0 -1)
  (let [last (- (length lines) 1)]
    (when (< 0 prefix)
      (api.nvim_buf_set_extmark 
        buf 
        ns 
        last 
        0
        {:end_col prefix :hl_group (if (and ?hl-id (< 0 ?hl-id)) ?hl-id :MoniCmdlinePrompt)}))

    (when (and (= (: (. lines 1) :sub 1 1) ":") (= last 0))
      (let [(ok parser) (pcall vim.treesitter.get_string_parser text :vim)]
        (when ok
          (parser:parse true)
          (parser:for_each_tree highlight-tree))))))

(λ redraw []
  (api.nvim__redraw {:flush true :cursor true : win}))

(λ cmdline.cmdline_pos [pos]
  (when (win-valid?)
    (let [last (api.nvim_buf_line_count buf)
          col (+ prefix (length (vim.fn.strtrans (text:sub 1 pos))))]
      (api.nvim_win_set_cursor win [last col])
      (redraw))))

;; fnlfmt: skip
(λ cmdline.cmdline_show [content pos firstc prompt indent level ?hl-id]
  (ensure-buf)

  (let [lines []]
    (each [line (: (.. (string.format "%s%s%s" firstc prompt (string.rep " " indent)) "\n") :gmatch "(.-)\n")]
      (tset lines (+ (length lines) 1) (vim.fn.strtrans line)))

    (set text "")

    (each [_ chunk (ipairs content)]
      (set text (.. text (. chunk 2))))

    (set prefix (length (. lines (length lines))))

    (tset lines (length lines)
          (.. (. lines (length lines)) (vim.fn.strtrans text) " "))

    (api.nvim_buf_set_lines buf 0 -1 false lines)

    (when (not (win-valid?))
      (set win
           (float! 
             buf 
             false 
             {:relative :editor
              :row 1
              :col 0
              :width 48
              :height 1
              :zindex 250
              :focusable false}
             "Normal:NormalFloat,FloatBorder:FloatBorder"))
      (wo! win :wrap true))

    (highlight lines ?hl-id)
    (place)
    (set cmdline.level level)
    (cmdline.cmdline_pos pos)))

;; fnlfmt: skip
(λ cmdline.cmdline_special_char [c ?shift]
  (when (win-valid?)
    (api.nvim_win_call win (λ [] (api.nvim_put [c] (if ?shift "" :c) false false)))))

(λ cmdline.cmdline_hide [level]
  (when (<= level (if (= (vim.fn.getcmdwintype) "") 1 2))
    (set cmdline.level 0)
    (when (win-valid?)
      (api.nvim_win_close win true))
    (set win nil)))

(λ cmdline.cmdline_block_show [])
(λ cmdline.cmdline_block_append [])
(λ cmdline.cmdline_block_hide [])

(λ position []
  "Where a popup menu should open"
  (if (win-valid?)
      (let [box (api.nvim_win_get_config win)]
        [(+ box.row box.height 1) (+ box.col 1)])
      [(- vim.o.lines 1) 0]))

{: position}
