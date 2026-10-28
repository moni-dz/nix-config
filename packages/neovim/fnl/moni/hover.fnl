(import-macros {: map! : set!} :laurel.macros)
(import-macros {: on!} :moni.macros)

(set! :mousemoveevent true)

(local hover {:timer (vim.uv.new_timer) :win nil :key nil})

(λ hover-close []
  (when (and hover.win (vim.api.nvim_win_is_valid hover.win))
    (vim.api.nvim_win_close hover.win true))
  (set hover.win nil)
  (set hover.key nil))

;; fnlfmt: skip
(λ editor-window? [pos]
  "Whether the mouse is over a normal window showing a normal buffer."
  (and (not= pos.winid 0)
       (not= pos.line 0)
       (= (. (vim.api.nvim_win_get_config pos.winid) :relative) "")
       (= (. vim.bo (vim.api.nvim_win_get_buf pos.winid) :buftype) "")))

;; fnlfmt: skip
(λ symbol-under-mouse []
  "A key for the symbol under the mouse, then the position, buffer and line text. Only the position when there is none."
  (let [pos (vim.fn.getmousepos)]
    (if (not (editor-window? pos))
        (values nil pos)
        (let [buf (vim.api.nvim_win_get_buf pos.winid)
              lines (vim.api.nvim_buf_get_lines buf (- pos.line 1) pos.line false)
              text (or (. lines 1) "")
              (word start) (unpack (vim.fn.matchstrpos text (.. "\\k*\\%" pos.column "c\\k\\+")))]
          (if (= word "")
              (values nil pos)
              (values (.. pos.winid ":" pos.line ":" start) pos buf text))))))

;; fnlfmt: skip
(λ open-hover [key lines title]
  "Show `lines` under the mouse, unless it has left the symbol `key`."
  (when (and (< 0 (length lines)) (= (symbol-under-mouse) key))
    (hover-close)
    (let [(_ win)
          (vim.lsp.util.open_floating_preview
            lines
            :markdown
            {:focus false
             :close_events []
             :max_width 80
             :max_height 20
             :border :rounded
             :title (.. " " title " ")
             :title_pos :left})]
      (vim.api.nvim_win_set_config win {:relative :mouse :row 1 :col 0})
      (set hover.win win)
      (set hover.key key))))

(λ hover-lines [diagnostics ?result]
  "The server's hover text, headed by the line's diagnostics."
  (var lines [])

  (when (and ?result ?result.contents)
    (set lines (vim.lsp.util.convert_input_to_markdown_lines ?result.contents))
    (set lines (vim.split (table.concat lines "\n") "\n" {:trimempty true})))

  (when (< 0 (length diagnostics))
    (table.insert lines 1 "---")
    (for [i (length diagnostics) 1 -1]
      (table.insert lines 1 (. diagnostics i))))

  lines)

;; fnlfmt: skip
(λ hover-show []
  (let [(key pos buf text) (symbol-under-mouse)]
    (when (and key (not= key hover.key))
      (let [diagnostics (vim.tbl_map #$.message (vim.diagnostic.get buf {:lnum (- pos.line 1)}))
            client (. (vim.lsp.get_clients {:bufnr buf :method :textDocument/hover}) 1)]
        (if (not client)
            (open-hover key diagnostics :diagnostics)
            (let [character
                   (vim.str_utfindex text client.offset_encoding (- pos.column 1) false)
                  params
                    {:textDocument {:uri (vim.uri_from_bufnr buf)} :position {:line (- pos.line 1) : character}}]
              (client:request
                :textDocument/hover
                params
                (λ [_err ?result] (open-hover key (hover-lines diagnostics ?result) client.name))
                buf)))))))

;; fnlfmt: skip
(map!
  :nv
  :<MouseMove>
  [:desc "hover tooltip"]
  (λ []
    (hover.timer:stop)
    (let [pos (vim.fn.getmousepos)]
      (when (not (and hover.win (= pos.winid hover.win)))
        (when (and hover.win (not= (symbol-under-mouse) hover.key))
          (hover-close))
        (hover.timer:start 500 0 (vim.schedule_wrap hover-show))))))

(on! [:InsertEnter :BufLeave :CursorMoved] [] (hover-close))
