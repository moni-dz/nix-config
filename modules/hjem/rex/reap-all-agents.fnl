;; fnlfmt: skip
(local reaper-script (.. (or (os.getenv :HOME) "") :/.claude/bin/agent-reaper.lua))

;; fnlfmt: skip
(λ server-rex []
  (let [(r err) (rex.exec [:sh :-c "ps -o comm= -p \"$PPID\""])]
    (if (= r nil)
        (values nil err)
        (let [path (r.stdout:gsub "%s+$" "")]
          (if (or (not r.ok) (= path ""))
              (values nil (.. "could not find the server executable: " r.stderr))
              path)))))

;; fnlfmt: skip
(λ start-reaper []
  (let [(bin err) (server-rex)]
    (when (= bin nil) (error err 0))
    (let [dir (bin:match "^(.*)/[^/]*$")
          (h serr)
          (rex.exec.start
            [bin :do reaper-script :--all]
            {:detach true
             :env {:PATH (.. dir ":" (or (os.getenv :PATH) "/usr/bin:/bin"))}})]
      (when (= h nil) (error serr 0)))))

;; fnlfmt: skip
(rex.action {:name     :agent_reaper_live
             :title    "Reap All Agents"
             :category :Agents
             :run      start-reaper})
