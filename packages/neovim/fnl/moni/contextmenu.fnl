(import-macros {: map!} :laurel.macros)

(local menu (require :moni.menu))
(local {: format!} (require :moni.format))

(λ lsp-attached? [buf]
  (< 0 (length (vim.lsp.get_clients {:bufnr buf}))))

(λ press [lhs]
  (let [map (vim.fn.maparg lhs :n false true)]
    (if map.callback
        (map.callback)
        (and map.rhs (not= map.rhs ""))
        (let [(cmd) (: (map.rhs:gsub :^<Cmd> "") :gsub :<CR>$ "")]
          (vim.cmd cmd)))))

(λ copy [text what]
  (vim.fn.setreg "+" text)
  (vim.notify (.. "copied " what "\n" text)))

(λ in-hunk? [buf line]
  (let [(ok gitsigns) (pcall require :gitsigns)]
    (if (not ok)
        false
        (do
          (var found false)
          (each [_ hunk (ipairs (or (gitsigns.get_hunks buf) []))]
            (let [from hunk.added.start
                  count (math.max hunk.added.count 1)]
              (when (and (<= from line) (< line (+ from count)))
                (set found true))))
          found))))

(λ entry [label hint run]
  {: label : hint : run})

(λ normal! [keys]
  (λ []
    (vim.cmd (.. "normal! " keys))))

(λ format-buffer [visual]
  (if (and visual (not= vim.bo.filetype :fennel))
      (vim.lsp.buf.format {:range {:start (vim.fn.getpos "'<")
                                   :end (vim.fn.getpos "'>")}})
      (format!)))

(λ lsp-navigation [buf]
  (if (lsp-attached? buf)
      [(entry "Go to Definition" :gd vim.lsp.buf.definition)
       (entry "Go to Type Definition" :gy vim.lsp.buf.type_definition)
       (entry "Find References" :grr vim.lsp.buf.references)
       (entry "Go to Implementation" :gri vim.lsp.buf.implementation)
       (entry "Show Documentation" :K vim.lsp.buf.hover)]
      []))

(λ lsp-editing [buf visual]
  (if (lsp-attached? buf)
      [(entry "Rename Symbol" :grn vim.lsp.buf.rename)
       (entry "Code Action" "Space a" vim.lsp.buf.code_action)
       (entry (if visual "Format Selection" "Format Document") "Space f"
              (partial format-buffer visual))]
      []))

(λ text-editing [visual]
  (let [clipboard (if visual
                      [(entry :Cut :d (normal! :d))
                       (entry :Copy :y (normal! :y))]
                      [(entry "Copy Line" :yy (normal! :yy))])]
    (vim.list_extend clipboard
                     [(entry :Paste :p (normal! :p))
                      (entry "Select All" :ggVG (normal! :ggVG))
                      (entry "Toggle Comment" (if visual :gc :gcc)
                             (partial vim.cmd
                                      (if visual "normal gc" "normal gcc")))])))

(λ line-diagnostics [buf line]
  (if (< 0 (length (vim.diagnostic.get buf {:lnum (- line 1)})))
      [(entry "Show Line Diagnostics" "Space e" vim.diagnostic.open_float)
       (entry "Next Error" "]e"
              (partial vim.diagnostic.jump
                       {:count 1 :severity vim.diagnostic.severity.ERROR}))]
      []))

(λ version-control [buf line]
  (let [entries []]
    (when (vim.fs.root buf :.jj)
      (vim.list_extend entries
                       [(entry "Toggle Blame" "Space jb"
                               (partial press :<leader>jb))
                        (entry "Compare with Parent" "Space jD"
                               (partial press :<leader>jD))
                        (entry "File History" "Space jh"
                               (partial press :<leader>jh))]))
    (when (in-hunk? buf line)
      (let [gitsigns (require :gitsigns)]
        (vim.list_extend entries
                         [(entry "Preview Hunk" "Space hp"
                                 gitsigns.preview_hunk)
                          (entry "Reset Hunk" "Space hr" gitsigns.reset_hunk)])))
    entries))

(λ project-actions [buf]
  (if (vim.fs.root buf [:Cargo.toml
                        :go.mod
                        :package.json
                        :Makefile
                        :justfile
                        :flake.nix
                        :pyproject.toml])
      [(entry "Run Project" "Space rr" (partial press :<leader>rr))
       (entry "Test Nearest" "Space rt" (partial press :<leader>rt))]
      []))

(λ buffer-extras [buf]
  (let [entries []
        target (vim.fn.expand :<cfile>)
        name (vim.api.nvim_buf_get_name buf)]
    (when (target:match "^https?://")
      (table.insert entries
                    (entry "Open Link" :gx (partial vim.ui.open target))))
    (when (= (. vim.bo buf :filetype) :markdown)
      (table.insert entries
                    (entry "Markdown Preview" "Space mp"
                           (partial press :<leader>mp))))
    (when (not= name "")
      (vim.list_extend entries
                       [(entry "Copy Path" ""
                               (λ []
                                 (copy (vim.fn.fnamemodify name ":.")
                                       "the path")))
                        (entry "Reveal in File Tree" "Space T"
                               (partial vim.cmd :NvimTreeFindFile))]))
    entries))

(λ join-sections [sections]
  "The entries of the non-empty `sections`, with a separator between them."
  (let [entries []]
    (each [_ section (ipairs sections)]
      (when (< 0 (length section))
        (when (< 0 (length entries))
          (table.insert entries {:separator true}))
        (vim.list_extend entries section)))
    entries))

(λ editor-entries [buf visual]
  (let [line (vim.fn.line ".")]
    (join-sections [(lsp-navigation buf)
                    (lsp-editing buf visual)
                    (text-editing visual)
                    (line-diagnostics buf line)
                    (version-control buf line)
                    (project-actions buf)
                    (buffer-extras buf)])))

;; fnlfmt: skip
(λ tree-entries [has-node]
  (let [api (require :nvim-tree.api)
        entries []]
    (λ add [label hint run]
      (tset entries (+ (length entries) 1) {: label : hint : run}))

    (λ separator []
      (tset entries (+ (length entries) 1) {:separator true}))

    (when has-node
      (add :Open :Enter api.node.open.edit)
      (add "Open in Split" :C-x api.node.open.horizontal)
      (add "Open in Vertical Split" :C-v api.node.open.vertical)
      (add "Open with Default App" "" api.node.run.system)
      (separator))

    (add "New File or Folder…" :a api.fs.create)

    (when has-node
      (add "Rename…" :r api.fs.rename)
      (add :Delete :d api.fs.remove)
      (separator)
      (add :Cut :x api.fs.cut)
      (add :Copy :c api.fs.copy.node))

    (add :Paste :p api.fs.paste)

    (when has-node
      (separator)
      (add "Copy Absolute Path" :gy api.fs.copy.absolute_path)
      (add "Copy Relative Path" "" api.fs.copy.relative_path))

    (separator)
    (add :Refresh :R api.tree.reload)
    (add "Collapse All" :W api.tree.collapse_all)
    entries))

(λ below [pos]
  [pos.screenrow (math.max (- pos.screencol 1) 0)])

(λ visual-mode? [mode]
  (not= (mode:match "^[vV\022]") nil))

(λ tree-node-at? [buf line]
  (let [api (require :nvim-tree.api)
        (ok node) (pcall api.tree.get_node_under_cursor)]
    (and ok (not= node nil) (<= 1 line)
         (<= line (vim.api.nvim_buf_line_count buf)))))

(λ right-click []
  (let [pos (vim.fn.getmousepos)
        win pos.winid]
    (when (not (or (= win 0) (not= (. (vim.api.nvim_win_get_config win)
                                      :relative)
                                   "")))
      (let [buf (vim.api.nvim_win_get_buf win)
            mode (vim.fn.mode)]
        (var visual (visual-mode? mode))
        (var keep false)
        (when (and visual (= win (vim.api.nvim_get_current_win)))
          (let [a (vim.fn.line :v)
                b (vim.fn.line ".")]
            (set keep (and (<= (math.min a b) pos.line)
                           (<= pos.line (math.max a b))))))
        (when (not keep)
          (if visual (vim.cmd "normal! \027")
              (= mode :i) (vim.cmd :stopinsert))
          (set visual false)
          (vim.api.nvim_set_current_win win)
          (when (and (<= 1 pos.line)
                     (<= pos.line (vim.api.nvim_buf_line_count buf)))
            (vim.api.nvim_win_set_cursor win
                                         [pos.line
                                          (math.max (- pos.column 1) 0)])))
        (let [entries (if (= (. vim.bo buf :filetype) :NvimTree)
                          (tree-entries (tree-node-at? buf pos.line))
                          (= (. vim.bo buf :buftype) "")
                          (editor-entries buf visual))]
          (when entries
            (menu.open entries (below pos)
                       {:before (if visual
                                    (λ [] (vim.cmd "normal! gv")))})))))))

(λ at-cursor []
  "Open the context menu at the text cursor."
  (let [buf (vim.api.nvim_get_current_buf)
        visual (visual-mode? (vim.fn.mode))
        at [(vim.fn.screenrow) (- (vim.fn.screencol) 1)]
        entries (if (= (. vim.bo buf :filetype) :NvimTree)
                    (tree-entries true)
                    (= (. vim.bo buf :buftype) "")
                    (editor-entries buf visual))]
    (when entries
      (when visual
        (vim.cmd "normal! \027"))
      (menu.open entries at
                 {:before (if visual
                              (λ [] (vim.cmd "normal! gv")))}))))

(λ tab [buf]
  "Open the context menu for the tabline entry of buffer `buf`."
  (let [pos (vim.fn.getmousepos)]
    (λ add [label run]
      {: label : run})
    (menu.open [(add :Close (λ [] (Snacks.bufdelete buf)))
                (add "Close Others"
                     (λ []
                       (let [infos (vim.fn.getbufinfo {:buflisted 1})]
                         (each [_ info (ipairs infos)]
                           (when (not= info.bufnr buf)
                             (Snacks.bufdelete info.bufnr))))))
                {:separator true}
                (add "Copy Path"
                     (λ []
                       (copy (vim.fn.fnamemodify (vim.api.nvim_buf_get_name buf)
                                                 ":.")
                             "the path")))
                (add "Reveal in File Tree"
                     (λ []
                       (vim.cmd.buffer buf)
                       (vim.cmd :NvimTreeFindFile)))]
               (below pos))))

(map! :nvi :<RightMouse> [:desc "context menu"] right-click)
(map! :nx :<leader>. [:desc "context menu at the cursor"] at-cursor)

{:at_cursor at-cursor : tab}
