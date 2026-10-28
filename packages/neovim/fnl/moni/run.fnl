(local state {:block nil :root nil :last nil})

(λ in-rex? []
  (and vim.env.REX_SESSION vim.env.REX_BLOCK (= (vim.fn.executable :rex) 1)))

;; fnlfmt: skip
(λ rex [args]
  (let [res (: (vim.system (vim.list_extend [:rex] args) {:text true}) :wait 5000)]
    (values (vim.trim (or res.stdout "")) (= res.code 0) (vim.trim (or res.stderr "")))))

;; fnlfmt: skip
(λ alive? [?block]
  (and (not= ?block nil) (let [(_ ok) (rex [:block :inspect ?block])] ok)))

(λ wait-until-ready [block]
  (var last "")
  (var ready false)
  (var n 0)
  (while (and (not ready) (< n 60))
    (set n (+ n 1))
    (vim.wait 100)
    (let [screen (rex [:capture :-b block :--trim])]
      (if (and (not= screen "") (= screen last))
          (set ready true)
          (set last screen)))))

;; fnlfmt: skip
(λ runner-pane []
  (if (alive? state.block)
      state.block
      (let [(out ok err)
            (rex [:split :-b vim.env.REX_BLOCK :--split=below :--ratio :35 :--focus=false])
            block (out:match "block:[%w%-]+")]
        (if (or (not ok) (not block))
            (do
              (vim.notify (.. "could not open a Rex split: " err) vim.log.levels.ERROR)
              nil)
            (do
              (set state.block block)
              (set state.root nil)
              (wait-until-ready block)
              block)))))

(λ send [block text]
  (rex [:send :-b block text])
  (rex [:send-key :-b block :enter]))

;; fnlfmt: skip
(local projects
  [{:markers [:Cargo.toml] :run "cargo run" :build "cargo build" :test "cargo test"}
   {:markers [:go.mod] :run "go run ." :build "go build ./..." :test "go test ./..."}
   {:markers [:package.json] :run "npm start" :build "npm run build" :test "npm test"}
   {:markers [:pyproject.toml :setup.py] :test :pytest}
   {:markers [:justfile :Justfile] :run :just :build "just build" :test "just test"}
   {:markers [:Makefile] :run "make run" :build :make :test "make test"}
   {:markers [:flake.nix] :run "nix run" :build "nix build" :test "nix flake check"}])

(λ project []
  (var best nil)
  (each [_ p (ipairs projects)]
    (let [root (vim.fs.root 0 p.markers)]
      (when (and root (or (not best) (< (length best.root) (length root))))
        (set best (vim.tbl_extend :force p {: root})))))
  best)

;; fnlfmt: skip
(λ cargo-package [root]
  (var in-package false)
  (var found nil)
  (each [_ line (ipairs (vim.fn.readfile (.. root :/Cargo.toml)))]
    (when (not found)
      (if (line:match "^%[package%]")
          (set in-package true)
          (line:match "^%[")
          (set in-package false)
          in-package
          (let [name (line:match "^name%s*=%s*\"([^\"]+)\"")]
            (when name (set found name))))))
  found)

;; fnlfmt: skip
(λ rust-test-under-cursor []
  (let [(ok node) (pcall vim.treesitter.get_node)]
    (var node node)
    (var found nil)
    (while (and ok node (not found))
      (when (= (node:type) :function_item)
        (var prev (node:prev_named_sibling))
        (while (and prev (not found) (= (prev:type) :attribute_item))
          (when (: (vim.treesitter.get_node_text prev 0) :find :test)
            (set found (vim.treesitter.get_node_text (. (node:field :name) 1) 0)))
          (set prev (prev:prev_named_sibling))))
      (set node (node:parent)))
    found))

(λ in-dev-shell? []
  (not= (or vim.env.IN_NIX_SHELL vim.env.DIRENV_DIR) nil))

;; fnlfmt: skip
(local keep-pane-own
  [:^PWD$ :^OLDPWD$ :^SHLVL$ "^_$" :^TERM :^COLORTERM$ :^REX_ :^NVIM :^VIM :^MYVIMRC$])

(λ sh-quote [text]
  (.. "'" (text:gsub "'" "'\\''") "'"))

;; fnlfmt: skip
(λ dev-shell-script [cmds root]
  (let [lines ["#!/bin/sh"]]
    (each [name value (pairs (vim.fn.environ))]
      (let [own (: (vim.iter keep-pane-own) :any (λ [pattern] (not= (name:match pattern) nil)))]
        (when (and (name:match "^[%a_][%w_]*$") (not own))
          (tset lines (+ (length lines) 1) (string.format "export %s=%s" name (sh-quote value))))))
    (tset
      lines
      (+ (length lines) 1)
      (string.format "cd %s && %s" (sh-quote root) (table.concat cmds " && ")))
    (let [path (.. (vim.fn.tempname) :.sh)]
      (vim.fn.writefile lines path)
      path)))

(λ project-root []
  "The root of the project the current buffer is in, else the cwd."
  (or (. (or (project) {}) :root) (vim.fn.getcwd)))

;; fnlfmt: skip
(λ run-command [cmds root]
  "Run `cmds` in `root`: in the Rex runner split, else in a terminal."
  (set state.last {: cmds : root})
  (if (not (in-rex?))
      (do
        (vim.cmd "botright 12new")
        (vim.fn.jobstart [:sh :-c (table.concat cmds " && ")] {:term true :cwd root}))
      (let [block (runner-pane)]
        (when block
          (rex [:send-key :-b block :ctrl+c])
          (rex [:send-key :-b block :ctrl+l])
          (var line (table.concat cmds "; "))

          (when (in-dev-shell?)
            (set line (.. "sh '" (dev-shell-script cmds root) "'")))

          (when (not= state.root root)
            (set line (.. "cd '" root "'; " line))
            (set state.root root))

          (send block line)))))

;; fnlfmt: skip
(λ with-project [action]
  (let [p (project)]
    (if (not p)
        (vim.notify
          (.. "no project here (looked for Cargo.toml, go.mod, "
              "package.json, Makefile, flake.nix, ...)")
          vim.log.levels.WARN)
        (action p))))

;; fnlfmt: skip
(λ simple [kind]
  (λ []
    (with-project
      (λ [p]
        (if (. p kind)
            (run-command [(. p kind)] p.root)
            (vim.notify
              (.. (. p.markers 1) " projects have no " kind " command")
              vim.log.levels.WARN))))))

;; fnlfmt: skip
(λ test-nearest []
  (with-project
    (λ [p]
      (let [name (and (= (. p.markers 1) :Cargo.toml) (rust-test-under-cursor))]
        (run-command [(if name (.. "cargo test " name) p.test)] p.root)))))

;; fnlfmt: skip
(λ find-debugger []
  (:
    (vim.iter [:rust-lldb :lldb :rust-gdb :gdb])
    :find
    (λ [candidate] (= (vim.fn.executable candidate) 1))))

;; fnlfmt: skip
(λ ask-executable [debugger root]
  (vim.ui.input
    {:prompt "debug which executable: " :completion :file}
    (λ [?path]
      (when (and ?path (not= ?path ""))
        (run-command [(.. debugger " " ?path)] root)))))

;; fnlfmt: skip
(λ debug-project [project]
  (let [debugger (find-debugger)]
    (if (not debugger)
        (vim.notify
          (.. "no debugger on PATH " "(rust-lldb, lldb, rust-gdb or gdb)")
          vim.log.levels.WARN)
        (let [name (and (= (. project.markers 1) :Cargo.toml) (cargo-package project.root))]
          (if name
              (run-command ["cargo build" (.. debugger " target/debug/" name)] project.root)
              (ask-executable debugger project.root))))))

(λ debug-binary []
  (with-project debug-project))

;; message formats that carry a file position, most specific first
;; fnlfmt: skip
(local locators
  [{:pattern "^%s*%-%-> (.-):(%d+):(%d+)"
    :read (λ [message file lnum col] {: file : lnum : col :text message})}
   {:pattern "^(%S[^:]*):(%d+):(%d+):%s*(.*)$"
    :read (λ [_message file lnum col text] {: file : lnum : col : text})}
   {:pattern "^(%S[^:]*):(%d+):%s*(.*)$"
    :read (λ [_message file lnum text] {: file : lnum : text})}
   {:pattern "at (/[^:]+):(%d+):(%d+):"
    :read (λ [message file lnum col] {: file : lnum : col :text message})}
   {:pattern "File \"(.-)\", line (%d+)"
    :read (λ [message file lnum] {: file : lnum :text message})}])

(λ locate [line message]
  "The {: file : lnum ?col : text} a line of output points at, or nil."
  (var found nil)
  (each [_ locator (ipairs locators)]
    (let [captures [(line:match locator.pattern)]]
      (when (and (not found) (. captures 1))
        (set found (locator.read message (unpack captures))))))
  found)

(λ quickfix-item [at root]
  (let [path (if (= (at.file:sub 1 1) "/") at.file (.. root "/" at.file))]
    (if (vim.uv.fs_stat path)
        {:filename path
         :lnum (tonumber at.lnum)
         :col (or (tonumber at.col) 1)
         :text at.text})))

;; fnlfmt: skip
(λ parse-errors [text root]
  (let [items []]
    (var message "")
    (each [line (text:gmatch "[^\r\n]+")]
      (let [headline (or (line:match "^%s*(error[%[:].*)") (line:match "^%s*(warning[%[:].*)"))]
        (when headline (set message headline)))
      (let [at (locate line message)
            item (and at (quickfix-item at root))]
        (when item (table.insert items item))))
    items))

;; fnlfmt: skip
(λ errors-to-quickfix []
  (if (not (alive? state.block))
      (vim.notify "no runner pane yet" vim.log.levels.WARN)
      (let [text (rex [:capture :-b state.block :--trim])
            items (parse-errors text (or state.root (vim.fn.getcwd)))]
        (if (= (length items) 0)
            (vim.notify "no file:line positions in the runner's output")
            (let [title (.. "run: " (table.concat (or (and state.last state.last.cmds) []) ", "))]
              (vim.fn.setqflist {} " " {: title : items})
              (vim.cmd "copen 8"))))))

(λ complete-shell [_arg-lead cmdline cursorpos]
  (let [(line) (: (cmdline:sub 1 cursorpos) :gsub "^%s*%S+%s+" "")
        shellcomplete (require :moni.shellcomplete)]
    (shellcomplete.words line (project-root))))

;; fnlfmt: skip
(vim.api.nvim_create_user_command
  :Run
  (λ [opts] (run-command [opts.args] (project-root)))
  {:nargs "+"
   :complete complete-shell
   :desc "run a command in the runner split"})

(λ map [lhs f desc]
  (vim.keymap.set :n lhs f {: desc}))

(map :<leader>rr (simple :run) "run the project")
(map :<leader>rb (simple :build) :build)
(map :<leader>rt test-nearest "test: nearest (or all)")
(map :<leader>rT (simple :test) "test: all")
(map :<leader>rd debug-binary "debug in lldb/gdb")

;; fnlfmt: skip
(map
  :<leader>rl
  (λ []
    (if state.last
        (run-command state.last.cmds state.last.root)
        (vim.notify "nothing has been run yet")))
  "run the last command again")

;; fnlfmt: skip
(map
  :<leader>rc
  (λ []
    (vim.ui.input
      {:prompt "run: "
       :default (if state.last (. state.last.cmds (length state.last.cmds)) "")}
      (λ [?cmd] (when (and ?cmd (not= ?cmd "")) (run-command [?cmd] (project-root)))))
    (set vim.bo.filetype :moni-run))
  "run a command")

(map :<leader>rq errors-to-quickfix "runner errors to quickfix")

;; fnlfmt: skip
(map
  :<leader>ro
  (λ [] (when (alive? state.block) (rex [:block :focus state.block])))
  "focus the runner")

;; fnlfmt: skip
(map
  :<leader>rx
  (λ []
    (when (alive? state.block) (rex [:block :close state.block]))
    (set state.block nil)
    (set state.root nil))
  "close the runner")

{:root project-root :run run-command}
