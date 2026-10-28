(import-macros {: on!} :moni.macros)

(local color (require :moni.colors))
(local set-hl vim.api.nvim_set_hl)

;; fnlfmt: skip
(λ number-colors []
  (let [fg (color.palette :fg)
        bg (color.theme_bg)
        tan (color.palette :yellow)]
    (when (and fg bg tan)
      (let [dim (color.mix fg bg 0.4)]
        (each [name fg (pairs {:LineNr dim :CursorLineNr tan})]
          (let [hl (vim.api.nvim_get_hl 0 {: name :link false})]
            (set-hl 0 name (vim.tbl_extend :force hl {: fg :bold (= name :CursorLineNr)}))))))))

(number-colors)
(on! :ColorScheme [] (number-colors))

;; fnlfmt: skip
(λ flat-gutter []
  (let [names [:LineNr :SignColumn :FoldColumn]]
    (each [_ kind (ipairs [:Add :Change :Delete :Topdelete :Changedelete :Untracked])]
      (table.insert names (.. :GitSigns kind)))
    (each [_ kind (ipairs [:Error :Warn :Info :Hint :Ok])]
      (table.insert names (.. :DiagnosticSign kind)))
    (each [_ name (ipairs names)]
      (let [hl (vim.api.nvim_get_hl 0 {: name :link false})]
        (when hl.bg
          (set hl.bg nil)
          (set-hl 0 name hl))))))

(on! :ColorScheme [] (vim.schedule flat-gutter))

;; fnlfmt: skip
(λ git-line-tints []
  (λ hl-color [name attr]
    (. (vim.api.nvim_get_hl 0 {: name :link false}) attr))
  (let [bg
          (or (hl-color :Normal :bg) (color.palette :bg) (if (= vim.o.background :dark) 0 16777215))]
    (each [line
           sign
           (pairs {:GitSignsAddLn    :GitSignsAdd
                   :GitSignsChangeLn :GitSignsChange
                   :GitSignsDeleteLn :GitSignsDelete})]
      (let [fg (hl-color sign :fg)]
        (when fg (set-hl 0 line {:bg (color.mix fg bg 0.12)}))))))

(git-line-tints)
(on! :ColorScheme [] (git-line-tints))
(flat-gutter)

(local mix color.mix)

(local dropbar-icons [:DropBarIconUISeparator :DropBarIconUIIndicator])

(λ set-all! [names spec]
  "Give every highlight group in `names` the definition `spec`."
  (each [_ name (ipairs names)] (set-hl 0 name spec)))

;; fnlfmt: skip
(λ editor-colors! [{: fg : dim : bg}]
  (set-hl 0 :SnacksIndent {:fg (mix fg bg 0.08)})
  (set-hl 0 :SnacksIndentScope {:fg (mix dim bg 0.7)})
  (set-all! [:LspReferenceText :LspReferenceRead :LspReferenceWrite] {:bg (mix fg bg 0.1)})
  (set-hl 0 :LspInlayHint {:fg (mix fg bg 0.38) :italic true})
  (set-hl 0 :FoldMark {:fg (mix fg bg 0.3)}))

(λ float-colors! [{: fg : bg}]
  (set-hl 0 :NormalFloat {: bg})
  (set-hl 0 :Pmenu {: bg})
  (set-hl 0 :FloatBorder {:fg (mix fg bg 0.3) : bg})
  (set-hl 0 :FloatTitle {:fg (mix fg bg 0.55) : bg :italic true}))

;; fnlfmt: skip
(λ completion-colors! [{: fg : tan : bg}]
  (set-all!
    [:BlinkCmpMenuBorder :BlinkCmpDocBorder :BlinkCmpSignatureHelpBorder]
    {:link :FloatBorder})

  (set-all! [:BlinkCmpMenu :BlinkCmpDoc :BlinkCmpSignatureHelp :BlinkCmpScrollBarGutter] {: bg})
  
  (let [selected (mix fg bg 0.14)]
    (set-all!
      [:PmenuSel
       :BlinkCmpMenuSelection
       :BlinkCmpDocCursorLine
       :BlinkCmpSignatureHelpActiveParameter]
      {:bg selected})
    (set-hl 0 :DropBarMenuHoverEntry {:bg selected}))

  (set-all! [:PmenuSbar :DropBarMenuSbar] {: bg})
  (set-hl 0 :BlinkCmpKind {:fg (mix fg bg 0.45)})
  (set-hl 0 :BlinkCmpLabelMatch {:fg tan :bold true}))

(λ foreground-of [name]
  (. (vim.api.nvim_get_hl 0 {: name :link false}) :fg))

;; fnlfmt: skip
(λ diagnostic-colors! [{: bg}]
  (each [_ kind (ipairs [:Error :Warn :Info :Hint])]
    (let [severity (foreground-of (.. :Diagnostic kind))]
      (when severity
        (set-hl 0 (.. :DiagnosticVirtualText kind) {:fg (mix severity bg 0.65)})
        (set-hl 0 (.. :DiagnosticVirtualLines kind) {:fg (mix severity bg 0.85)})))))

(λ jj-colors! [{: fg : bg}]
  (let [warn (foreground-of :DiagnosticWarn)]
    (set-hl 0 :JjBlame {:fg (mix fg bg 0.4) :italic true})
    (set-hl 0 :JjConflict {:bg (and warn (mix warn bg 0.14))})))

(λ flash-colors! [{: fg : tan : bg}]
  (set-hl 0 :FlashLabel {:fg bg :bg tan :bold true})
  (set-hl 0 :FlashMatch {: fg :bg (mix fg bg 0.22)})
  (set-hl 0 :FlashCurrent {: fg :bg (mix tan bg 0.4)})
  (set-hl 0 :FlashBackdrop {:fg (mix fg bg 0.35)}))

(λ menu-colors! [{: fg : bg}]
  (set-hl 0 :MoniMenuHint {:fg (mix fg bg 0.4)})
  (set-hl 0 :MoniMenuSeparator {:fg (mix fg bg 0.2)}))

(λ bar-colors! [{: fg : bg}]
  (set-hl 0 :WinBar {:fg (mix fg bg 0.55)})
  (set-hl 0 :WinBarNC {:fg (mix fg bg 0.4)})
  (each [_ name (ipairs [:StatusLine :StatusLineNC])]
    (let [hl (vim.api.nvim_get_hl 0 {: name :link false})]
      (set hl.bg nil)
      (set-hl 0 name hl)))
  (set-all! dropbar-icons {:fg (mix fg bg 0.4)}))

;; fnlfmt: skip
(λ quiet-colors []
  (let [palette
          {:fg (color.palette :fg)
           :dim (color.palette :comments)
           :tan (color.palette :yellow)
           :bg (color.theme_bg)}]
    (when (and palette.fg palette.dim palette.tan palette.bg)
      (editor-colors! palette)
      (float-colors! palette)
      (completion-colors! palette)
      (diagnostic-colors! palette)
      (jj-colors! palette)
      (flash-colors! palette)
      (menu-colors! palette)
      (bar-colors! palette))))

(quiet-colors)
(on! :ColorScheme [] (vim.schedule quiet-colors))
