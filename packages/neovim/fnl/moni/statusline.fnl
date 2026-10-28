(import-macros {: set!} :laurel.macros)
(import-macros {: lazy : on!} :moni.macros)

(local heirline (require :heirline))
(local conditions (require :heirline.conditions))
(local utils (require :heirline.utils))
(local color (require :moni.colors))

(λ colors []
  (let [{:bg base-bg :fg base-fg} (utils.get_highlight :Normal)
        {:fg bar-fg} (utils.get_highlight :StatusLine)
        {:fg dim} (utils.get_highlight :Comment)
        {:fg func} (utils.get_highlight :Function)
        {:fg str} (utils.get_highlight :String)
        {:fg stmt} (utils.get_highlight :Statement)
        {:fg const} (utils.get_highlight :Constant)
        {:fg type} (utils.get_highlight :Type)
        {:fg err} (utils.get_highlight :DiagnosticError)
        {:fg warn} (utils.get_highlight :DiagnosticWarn)]
    {:badge_fg (or (color.theme_bg) (color.palette :bg))
     :num (color.palette :ui_yellow)
     :base_bg (or base-bg (color.theme_bg))
     :base_fg base-fg
     :tab_bg (color.palette :bg4)
     :bar_fg bar-fg
     :chip_err (color.palette :red)
     :chip_warn (color.palette :orange)
     ;; mellifluous' own green is olive; this matches the Rex theme files
     :chip_ok (if (= vim.o.background :dark) 10009752 2322475)
     : dim
     : func
     : str
     : stmt
     : const
     : type
     : err
     : warn}))

(local Mode
       {:init (λ [self]
                (set self.mode (: (vim.fn.mode 1) :sub 1 1)))
        :static {:names {:n :NOR
                         :i :INS
                         :v :VIS
                         :V :V-L
                         "\022" :V-B
                         :s :SEL
                         :S :S-L
                         "\019" :S-B
                         :R :REP
                         :c :CMD
                         :r :PRM
                         :! :SH
                         :t :TRM
                         :no :OP}
                 :colors {:n :func
                          :i :str
                          :v :stmt
                          :V :stmt
                          "\022" :stmt
                          :R :const
                          :c :type
                          :t :str}}
        :provider (λ [self]
                    (.. " " (or (. self.names self.mode) self.mode) " "))
        :hl (λ [self]
              {:fg :badge_fg
               :bg (or (. self.colors self.mode) :func)
               :bold true})
        :update {1 :ModeChanged
                 :pattern "*:*"
                 :callback (vim.schedule_wrap (λ []
                                                (vim.cmd.redrawstatus)))}})

(λ search-count []
  (let [(ok count) (pcall vim.fn.searchcount {:maxcount 999})]
    (if (or (not ok) (not count.total) (= count.total 0))
        ""
        (string.format "[%d/%d%s] " count.current count.total
                       (if (< 0 count.incomplete) "+" "")))))

(local SearchCount {:condition (λ []
                                 (= vim.v.hlsearch 1))
                    :provider search-count
                    :hl {:fg :num}})

(local Branch
       {:condition (λ []
                     (not= vim.b.branch nil))
        :provider (λ []
                    (.. " " (vim.fn.nr2char 57504) " " vim.b.branch " "))
        :hl {:fg :badge_fg :bg :stmt :bold true}
        :update {1 :BufEnter 2 :User :pattern :BranchUpdated}})

(local severity vim.diagnostic.severity)

(λ count-diagnostics [level]
  (length (vim.diagnostic.get 0 {:severity level})))

(local Diagnostics {:condition conditions.has_diagnostics
                    :init (λ [self]
                            (set self.errors (count-diagnostics severity.ERROR))
                            (set self.warnings
                                 (count-diagnostics severity.WARN)))
                    :update [:DiagnosticChanged :BufEnter]
                    1 {:condition (λ [self]
                                    (< 0 self.errors))
                       :provider (λ [self]
                                   (.. " ■ " self.errors " "))
                       :hl {:fg :badge_fg :bg :chip_err :bold true}}
                    2 {:condition (λ [self]
                                    (< 0 self.errors))
                       :provider " "}
                    3 {:condition (λ [self]
                                    (< 0 self.warnings))
                       :provider (λ [self]
                                   (.. " ▲ " self.warnings " "))
                       :hl {:fg :badge_fg :bg :chip_warn :bold true}}
                    4 {:condition (λ [self]
                                    (< 0 self.warnings))
                       :provider " "}})

(local lsp-names {})
(local lsp-failed {})

(λ notify-lsp-failed []
  (vim.api.nvim_exec_autocmds :User {:pattern :MoniLspFailed}))

(on! :LspAttach [{:data {: client_id}}]
     (let [client (vim.lsp.get_client_by_id client_id)]
       (when client
         (tset lsp-names client.id client.name)
         (tset lsp-failed client.name nil))))

(vim.lsp.config "*"
                {:on_exit (λ [code signal client-id]
                            (let [name (. lsp-names client-id)]
                              (when (and name
                                         (or (not= code 0) (not= signal 0)))
                                (tset lsp-failed name true)
                                (vim.schedule notify-lsp-failed))))})

(λ failed-here []
  (let [names []]
    (each [name (pairs lsp-failed)]
      (let [filetypes (or (. (or (. vim.lsp.config name) {}) :filetypes) [])]
        (when (vim.tbl_contains filetypes vim.bo.filetype)
          (table.insert names name))))
    (table.sort names)
    names))

(local Lsp {:condition (λ []
                         (or (conditions.lsp_attached)
                             (< 0 (length (failed-here)))))
            :update {1 :LspAttach
                     2 :LspDetach
                     3 :BufEnter
                     4 :User
                     :pattern :MoniLspFailed}
            :init (λ [self]
                    (set self.failed (failed-here))
                    (set self.names [])
                    (each [_ c (ipairs (vim.lsp.get_clients {:bufnr 0}))]
                      (table.insert self.names c.name))
                    (vim.list_extend self.names self.failed))
            :provider (λ [self]
                        (.. " ♦ " (table.concat self.names ",") " "))
            :hl (λ [self]
                  {:fg :badge_fg
                   :bg (if (< 0 (length self.failed)) :chip_err :chip_ok)
                   :bold true})})

(local LspGap {:condition Lsp.condition :provider " "})

(λ cursor-position []
  (let [[row col] (vim.api.nvim_win_get_cursor 0)]
    (string.format " %s %d:%d " (vim.fn.nr2char 57505) row (+ col 1))))

;; fnlfmt: skip
(local
  Ruler
  {:provider cursor-position
   :hl {:fg :num}})

(λ buffer-size []
  (let [last (vim.fn.line "$")
        bytes (math.max (- (vim.fn.line2byte (+ last 1)) 1) 0)]
    (string.format " (%dl %db) " last bytes)))

;; fnlfmt: skip
(local
  Size
  {:provider buffer-size
   :hl {:fg :dim}})

(λ editing-window? [win]
  (and (= (. vim.bo (vim.api.nvim_win_get_buf win) :buftype) "")
       (= (. (vim.api.nvim_win_get_config win) :relative) "")))

(λ focus-editing-window []
  "Focus the first window showing a normal buffer, the current one first."
  (let [wins [(vim.api.nvim_get_current_win)
              (unpack (vim.api.nvim_tabpage_list_wins 0))]]
    (var found nil)
    (each [_ win (ipairs wins)]
      (when (and (not found) (editing-window? win))
        (set found win)))
    (when found
      (vim.api.nvim_set_current_win found))))

(λ buffer-number [self]
  self.bufnr)

(λ tab-title [self]
  (let [name (vim.fn.fnamemodify self.filename ":t")]
    (.. " " (if (= name "") "[No Name]" name) (if self.modified " ●" ""))))

(λ click-tab [_self minwid _nclicks button]
  (if (= button :m)
      (pcall Snacks.bufdelete minwid)
      (= button :r)
      (lazy :moni.contextmenu :tab minwid)
      (do
        (focus-editing-window)
        (vim.cmd.buffer minwid))))

;; fnlfmt: skip
(local
  TabFile
  {:provider tab-title
   :on_click {:callback click-tab :minwid buffer-number :name :heirline_tabline_buffer}})

(λ close-buffer [_self minwid]
  (let [(ok err) (pcall Snacks.bufdelete minwid)]
    (when (not ok)
      (vim.notify (tostring err) vim.log.levels.WARN))))

;; fnlfmt: skip
(local
  TabClose
  {:provider " × "
   :hl {:fg :dim}
   :on_click {:callback close-buffer :minwid buffer-number :name :heirline_tabline_close}})

(local Tab {:init (λ [self]
                    (set self.filename (vim.api.nvim_buf_get_name self.bufnr))
                    (set self.modified (. vim.bo self.bufnr :modified)))
            :hl (λ [self]
                  (if self.is_active
                      {:fg :base_fg :bg :base_bg :bold true}
                      {:fg :dim :bg :tab_bg}))
            1 TabFile
            2 TabClose})

(local TabLine {1 (utils.make_buflist Tab {:provider " ◄ " :hl {:fg :dim}}
                                      {:provider " ► " :hl {:fg :dim}})
                2 {:provider "%="}
                :hl {:bg :tab_bg}})

(heirline.load_colors (colors))

;; fnlfmt: skip
(heirline.setup
  {:statusline
     {:hl {:fg :bar_fg}
      1 Mode
      2 SearchCount
      3 {:provider "%="}
      4 Ruler
      5 Size
      6 Diagnostics
      7 Lsp
      8 LspGap
      9 Branch}

   :tabline TabLine})

(set! :laststatus 3)
(set! :showmode false)
(set! :ruler false)
(set! :cmdheight 0)
(set! :showcmd false)

(on! :ColorScheme [] (utils.on_colorscheme colors))

(λ branch-command [dir]
  (let [root (vim.fs.root dir [:.jj :.git])]
    (if (and root (vim.uv.fs_stat (.. root :/.jj)))
        [:jj
         :log
         :-R
         root
         :-r
         "@"
         :--no-graph
         :--ignore-working-copy
         :--color
         :never
         :-T
         "change_id.shortest(4) ++ \" \" ++ bookmarks"]
        [:git :-C dir :branch :--show-current])))

(λ show-branch [buf res]
  (when (vim.api.nvim_buf_is_valid buf)
    (let [branch (vim.trim (or res.stdout ""))]
      (tset (. vim.b buf) :branch (if (not= branch "") branch))
      (vim.api.nvim_exec_autocmds :User {:pattern :BranchUpdated})
      (vim.cmd.redrawstatus))))

(on! [:BufEnter :FocusGained] [ev]
     (let [dir (vim.fn.expand "%:p:h")]
       (when (not (or (not= (. vim.bo ev.buf :buftype) "") (= dir "")))
         (vim.system (branch-command dir) {:text true}
                     (λ [res]
                       (vim.schedule (partial show-branch ev.buf res)))))))
