;;; agent-reaper.fnl: closes the Rex panes, tabs and scratch sessions agents leave behind
;; One pass when asked ("Reap All Agents"). modules/hjem/rex.nix builds it into
;; ~/.claude/bin/agent-reaper.lua.
;;   rex do ~/.claude/bin/agent-reaper.lua --all            acts
;;   rex do ~/.claude/bin/agent-reaper.lua --all dry=true   only logs what it would do
;;
;; Conventions (an agent only has to pick the right label):
;;   label "agent-NAME"            disposable: closed once its program has exited
;;   label "agent-keep-NAME"       never touched
;;   session "PARENT-scratch-HEX"  throwaway: destroyed when PARENT is gone or nothing in it runs
;; A block's own label decides; a tab's label only applies to blocks without an agent label of their own.
;; Everything else (the user's own panes, the Tools mirror tab, other sessions) is left alone.

;; fnlfmt: skip
(λ disposable? [?label]
  (and (= (type ?label) :string) (= (?label:sub 1 6) :agent-) (not= (?label:sub 1 11) :agent-keep-)))

(λ keep? [?label]
  (and (= (type ?label) :string) (= (?label:sub 1 11) :agent-keep-)))

(λ scratch-parent [?label]
  (if (= (type ?label) :string) (?label:match "^(.+)%-scratch%-%x+$")))

;; fnlfmt: skip
(λ last-exit [block-id]
  "The exit status of an ended terminal block; nil while it runs, and for other blocks."
  (let [(ok proc) (pcall rex.block.call :com.superlogical.terminal :process {:block_id block-id})]
    (if (and ok (= (type proc) :table) (= proc.child nil) (= (type proc.last_exit) :table))
        (or proc.last_exit.exit_code -1))))

(λ act! [dry? what why f]
  (rex.log (.. (if dry? "[dry run] would " "") what ": " why))
  (when (not dry?)
    (let [(_ err) (f)]
      (when (not= err nil)
        (rex.log :warn (.. what " failed: " (tostring err)))))))

(λ window-labels [session-id]
  "Window id -> label (a tab opened with `rex window new LABEL` labels the window, not the block)."
  (let [view (rex.session.view {:session_id session-id})]
    (collect [_ w (ipairs (or (and view view.windows) []))]
      (values w.window_id w.label))))

;; fnlfmt: skip
(λ reap-blocks! [dry? session ?scratch]
  "Close the finished disposable blocks of `session`; returns how many are still running."
  (let [id session.session_id
        listing (rex.session.list_blocks {:session_id id})
        tab-labels (window-labels id)]
    (var running 0)
    (each [_ b (ipairs (or (and listing listing.blocks) []))]
      (let [code (and b.block_id (last-exit b.block_id))
            ;; the shell every session starts with never exits, so it doesn't count as running
            default-shell? (and (not= ?scratch nil) (= b.label :shell))
            tab-label (. tab-labels b.window_id)
            mine? (or (not= ?scratch nil) (disposable? b.label) (disposable? tab-label))
            kept? (or (keep? b.label) (and (not (disposable? b.label)) (keep? tab-label)))]
        (when (and (= code nil) (not default-shell?))
          (set running (+ running 1)))

        (when (and (not= code nil) mine? (not kept?))
          (act!
            dry?
            (.. "close block " b.block_id)
            (.. "exited " code)
            (λ [] (rex.call :block.close {:session_id id :block_id b.block_id}))))))
    running))

;; fnlfmt: skip
(λ destroy-scratch! [dry? session scratch by-label running]
  (let [id session.session_id
        why
          (if (= (. by-label scratch) nil)
              (.. "parent session '" scratch "' is gone")
              (= running 0)
              "nothing in it is still running")]
    (when why
      (act!
        dry?
        (.. "destroy scratch session " id " (" session.label ")")
        why
        (λ []
          ;; destroying a session needs it attached first (else 409 conflict)
          (let [(_ err) (rex.call :session.attach {:session_id id})]
            (if (not= err nil)
                (values nil err)
                (rex.call :session.destroy {:session_id id}))))))))

(λ reap! [dry?]
  (rex.log (.. :agent-reaper (if dry? " (dry run: omit dry=true to act)" "")))
  (let [listed (rex.call :session.list)
        sessions (or (and listed listed.sessions) [])
        by-label (collect [_ s (ipairs sessions)] (values s.label s.session_id))]
    (each [_ session (ipairs sessions)]
      (let [scratch (scratch-parent session.label)
            running (reap-blocks! dry? session scratch)]
        (when (not= scratch nil)
          (destroy-scratch! dry? session scratch by-label running))))))

;; loaded as configuration (e.g. by `rex config check`): nothing to do
(when rex.every
  (let [args (or rex.args {})]
    (reap! (or (= args.dry true) (= args.dry :true)))))
