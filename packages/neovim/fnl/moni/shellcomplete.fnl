;; fnlfmt: skip
(λ fish-words [line root]
  (if (= (vim.fn.executable :fish) 1)
      (let [res (: (vim.system [:fish :-c "complete -C $argv" line] {:text true :cwd root}) :wait 1500)]
        (if (and (= res.code 0) (not= res.stdout ""))
            (let [seen {}
                  words []]
              (each [candidate (res.stdout:gmatch "[^\n]+")]
                (let [word (candidate:match "^[^\t]*")]
                  (when (not (. seen word))
                    (tset seen word true)
                    (table.insert words word))))
              words)))))

(λ words [line root]
  "Completion candidates for the shell command line `line`, run from `root`."
  (or (fish-words line root) (vim.fn.getcompletion line :shellcmdline)))

;; fnlfmt: skip
(λ path? [word root]
  (or (word:find "/" 1 true) (= (word:sub 1 1) "~") (not= (vim.uv.fs_stat (.. root "/" word)) nil)))

;; fnlfmt: skip
(λ kind [line word root]
  "Classify a candidate: Command, Option, Path or Subcommand."
  (if (not (line:find "%s"))
      :Command
      (= (word:sub 1 1) "-")
      :Option
      (path? word root)
      :Path
      :Subcommand))

{: words : kind}
