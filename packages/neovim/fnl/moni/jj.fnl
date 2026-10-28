(import-macros {: map!} :laurel.macros)
(import-macros {: setup! : lazy} :moni.macros)

(local {: bo!} (require :moni.lib))

(setup! :jj {})

(λ jj [args root on-done]
  (vim.system (vim.list_extend [:jj] args) {:text true :cwd root}
              (vim.schedule_wrap on-done)))

(λ repo-root [?buf]
  (vim.fs.root (or ?buf 0) :.jj))

(λ relative [buf root]
  (vim.fs.relpath root (vim.api.nvim_buf_get_name buf)))

(λ runner [cmd]
  (let [root (repo-root)]
    (when root
      (lazy :moni.run :run [cmd] root))))

(λ notify-result [label]
  (λ [res]
    (let [out (vim.trim (.. (or res.stdout "") (or res.stderr "")))]
      (vim.notify (.. label (if (= res.code 0) "" " failed")
                      (if (not= out "") (.. "\n" out) ""))
                  (if (= res.code 0) vim.log.levels.INFO vim.log.levels.ERROR))
      (vim.cmd.checktime))))

(map! :n :<leader>jl [:desc "jj log"] "<Cmd>J log<CR>")
(map! :n :<leader>js [:desc "jj status"] "<Cmd>J status<CR>")
(map! :n :<leader>jd [:desc "jj describe"] "<Cmd>J describe<CR>")
(map! :n :<leader>jn [:desc "jj new"] "<Cmd>J new<CR>")
(map! :n :<leader>jf [:desc "jj diff of this file"] "<Cmd>J diff<CR>")

(map! :n :<leader>ju [:desc "jj undo"]
      (λ []
        (let [root (repo-root)]
          (when root
            (jj [:undo] root (notify-result "jj undo"))))))

(map! :n :<leader>jq [:desc "squash this change into its parent"]
      (λ []
        (let [root (repo-root)]
          (when root
            (jj [:squash :--use-destination-message] root
                (notify-result "squashed into the parent"))))))

(map! :n :<leader>jQ [:desc "squash, choosing hunks"]
      (partial runner "jj squash -i"))

(map! :n :<leader>jS [:desc "split this change, choosing hunks"]
      (partial runner "jj split"))

(map! :n :<leader>jP [:desc "jj git push"] (partial runner "jj git push"))
(map! :n :<leader>jF [:desc "jj git fetch"] (partial runner "jj git fetch"))

(local revision-template
       (.. "change_id.shortest(4) ++ \"\\t\" ++ author.name() ++ "
           "\"\\t\" ++ committer.timestamp().ago() ++ \"\\t\" ++ "
           "description.first_line() ++ \"\\n\""))

(λ parse-revisions [?stdout]
  (let [items []]
    (each [line (: (or ?stdout "") :gmatch "[^\n]+")]
      (let [(id author ago desc) (line:match "^(.-)\t(.-)\t(.-)\t(.*)$")]
        (when (and id (not= id ""))
          (table.insert items
                        {: id
                         : author
                         : ago
                         :desc (if (not= desc "") desc "(no description)")}))))
    items))

(λ format-revision [revision]
  (string.format "%s  %s  %s  %s" revision.id revision.ago revision.author
                 revision.desc))

(λ pick-revision [revset prompt on-choice]
  (let [root (repo-root)]
    (when root
      (jj [:log :--no-graph :-r revset :-T revision-template] root
          (λ [res]
            (vim.ui.select (parse-revisions res.stdout)
                           {: prompt :format_item format-revision}
                           (λ [?choice]
                             (when ?choice
                               (on-choice ?choice.id)))))))))

(λ open-comparison [buf rev rel contents]
  "Show `contents`, the version of `buf`'s file at `rev`, beside it as a diff."
  (let [ft (. vim.bo buf :filetype)]
    (vim.cmd :diffthis)
    (vim.cmd "leftabove vnew")
    (let [old (vim.api.nvim_get_current_buf)
          lines (vim.split contents "\n" {:plain true})]
      (when (= (. lines (length lines)) "")
        (table.remove lines))
      (vim.api.nvim_buf_set_lines old 0 -1 false lines)
      (bo! old :buftype :nofile :bufhidden :wipe :swapfile false :filetype ft
           :modifiable false)
      (vim.api.nvim_buf_set_name old (.. "jj:" rev ":" rel))
      (vim.cmd :diffthis)
      (vim.keymap.set :n :q :<Cmd>diffoff!<Bar>close<CR>
                      {:buffer old :desc "close the comparison"}))))

(λ diff-with [rev]
  (let [buf (vim.api.nvim_get_current_buf)
        root (repo-root buf)
        rel (and root (relative buf root))]
    (if (not rel)
        (vim.notify "this file is not in a jj repo" vim.log.levels.WARN)
        (jj [:file :show :-r rev rel] root
            (λ [res]
              (if (not= res.code 0)
                  (vim.notify (vim.trim (or res.stderr "")) vim.log.levels.WARN)
                  (open-comparison buf rev rel res.stdout)))))))

(map! :n :<leader>jD [:desc "compare this file with the parent"]
      (partial diff-with "@-"))

(map! :n :<leader>jc [:desc "compare this file with a revision"]
      (partial pick-revision "ancestors(@, 25)" "compare with: " diff-with))

(map! :n :<leader>jh [:desc "history of this file"]
      (λ []
        (let [root (repo-root)
              rel (and root (relative 0 root))]
          (when rel
            (pick-revision (string.format "ancestors(@, 200) & files(\"%s\")"
                                          rel)
                           "history of this file: " diff-with)))))

;; fnlfmt: skip
(local
  blame
  {:on false
   :ns (vim.api.nvim_create_namespace :jj_blame)
   :lines {}})

(local annotate-template (.. "commit.change_id().shortest(4) ++ \"\\t\" ++ "
                             "commit.author().name() ++ \"\\t\" ++ "
                             "commit.author().timestamp().ago() ++ \"\\t\" ++ "
                             "commit.description().first_line() ++ \"\\t\" ++ "
                             "line_number ++ \"\\n\""))

(local annotation-pattern "^(.-)\t(.-)\t(.-)\t(.-)\t(%d+)$")

(λ load-blame [buf]
  (let [root (repo-root buf)
        rel (and root (relative buf root))]
    (when (and blame.on rel (= (. vim.bo buf :buftype) ""))
      (jj [:file :annotate :-r "@" :-T annotate-template rel] root
          (λ [res]
            (when (= res.code 0)
              (let [lines {}]
                (each [line (: (or res.stdout "") :gmatch "[^\n]+")]
                  (let [(id author ago desc n) (line:match annotation-pattern)]
                    (when id
                      (tset lines (tonumber n)
                            {: id
                             :text (table.concat [id author ago desc] " · ")}))))
                (tset blame.lines buf lines))))))))

(λ show-blame []
  (let [buf (vim.api.nvim_get_current_buf)]
    (vim.api.nvim_buf_clear_namespace buf blame.ns 0 -1)
    (let [info (and blame.on (not (. vim.bo buf :modified)) (. blame.lines buf)
                    (. blame.lines buf (vim.fn.line ".")))]
      (when info
        (vim.api.nvim_buf_set_extmark buf blame.ns (- (vim.fn.line ".") 1) 0
                                      {:virt_text [[(.. "  " info.text)
                                                    :JjBlame]]
                                       :virt_text_pos :eol})))))

(local blame-group (vim.api.nvim_create_augroup :jj_blame {}))

(map! :n :<leader>jb [:desc "toggle inline blame"]
      (λ []
        (set blame.on (not blame.on))
        (vim.api.nvim_clear_autocmds {:group blame-group})
        (if blame.on
            (do
              (vim.api.nvim_create_autocmd [:BufEnter
                                            :BufWritePost
                                            :FocusGained]
                                           {:group blame-group
                                            :callback (λ [ev]
                                                        (load-blame ev.buf)
                                                        nil)})
              (vim.api.nvim_create_autocmd [:CursorHold
                                            :CursorMoved
                                            :BufEnter
                                            :TextChanged]
                                           {:group blame-group
                                            :callback (λ []
                                                        (show-blame)
                                                        nil)})
              (load-blame (vim.api.nvim_get_current_buf)))
            (vim.api.nvim_buf_clear_namespace 0 blame.ns 0 -1))
        (vim.notify (.. "jj blame " (if blame.on :on :off)))))

(map! :n :<leader>jB [:desc "show the change this line is from"]
      (λ []
        (let [lines (. blame.lines (vim.api.nvim_get_current_buf))
              info (and lines (. lines (vim.fn.line ".")))]
          (if info
              (vim.cmd (.. "J show " info.id))
              (vim.notify "turn blame on first (<leader>jb) and save the file"
                          vim.log.levels.WARN)))))

(local conflict-ns (vim.api.nvim_create_namespace :jj_conflict))

(λ mark-conflicts [buf]
  (vim.api.nvim_buf_clear_namespace buf conflict-ns 0 -1)
  (each [i line (ipairs (vim.api.nvim_buf_get_lines buf 0 -1 false))]
    (when (or (line:match "^<<<<<<<") (line:match "^>>>>>>>")
              (line:match "^%%%%%%%%") (line:match "^%+%+%+%+%+%+%+"))
      (vim.api.nvim_buf_set_extmark buf conflict-ns (- i 1) 0
                                    {:line_hl_group :JjConflict}))))

(vim.api.nvim_create_autocmd [:BufReadPost :BufWritePost :TextChanged]
                             {:callback (λ [ev]
                                          (when (= (. vim.bo ev.buf :buftype)
                                                   "")
                                            (mark-conflicts ev.buf))
                                          nil)})
