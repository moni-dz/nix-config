;;; fennel docset for rex's Lua API: hover docs and completion for `rex.*`.
;;
;; taken from https://www.superlogical.com/rex/docs/reference/lua
;; remember to keep this file and types/rex.lua in sync

(fn fun [binding arglist docstring ?fields]
  {: binding
   :metadata {:fls/itemKind :Function
              :fnl/arglist arglist
              :fnl/docstring docstring}
   :fields ?fields})

(fn value [binding docstring]
  {: binding :metadata {:fls/itemKind :Variable :fnl/docstring docstring}})

(fn section [binding docstring fields]
  {: binding
   :metadata {:fls/itemKind :Module :fnl/docstring docstring}
   : fields})

(macro text [...]
  (table.concat [...]))

(macro session-fields [methods]
  (let [out {}
        note (.. "Call a session method with a table of arguments. `session_id` is filled "
                 "in when left out. Returns a result table, or nil and an error message. "
                 "`rex api describe session.<method>` shows the arguments.")]
    (each [name text (pairs methods)]
      (tset out name `(fun ,(.. :rex.session. name) [:?args]
                           ,(.. text "\n\n" note))))
    out))

;; fnlfmt: skip
(local docset
  {:rex
    (section :rex "Rex Fennel API"
      {:bind
         (fun :rex.bind [:key :action-or-fn :?args]
           (text "Bind a key, a `>` sequence, or either prefixed with `mode/`, to an action "
                 "name (with optional args table) or to a function called as (fn [ctx ev]). "
                 "Return true from the function to mark the key handled. Configuration file "
                 "only."))

       :unbind
         (fun :rex.unbind [:key-or-binding :?target]
           (text "Remove the bindings of a key, a whole mode ({:mode name}), or one binding "
                 "from rex.bindings. Returns how many of this file's bindings it removed."))

       :mode
         (fun :rex.mode [:name :?options]
           (text "Declare a mode and set its options. The only option is `exclusive`, "
                 "default false."))

       :bindings
         (fun :rex.bindings [:?filter]
           (text "Return the bindings made so far, in order. `filter` may have `key`, "
                 "`mode` and `action`."))

       :modes
         (fun :rex.modes []
           (text "Return the modes declared or bound so far, each with `name`, `exclusive` "
                 "and `source`."))

       :action
         (fun :rex.action [:spec]
           (text "Define an action. `spec` has name, title and run (called as (fn [ctx "
                 "args])), and optionally description, category, keywords, args and repeats."))

       :invoke
         (fun :rex.invoke [:name :?args]
           (text "Run an action and return its result, or nil and an error message. Calls "
                 "nest up to 16 deep."))

       :client
         (section :rex.client
           "Ask the client to do something."
           {:queue
              (fun :rex.client.queue [:name :?args]
                (text "Ask the client to perform one of its own actions, such as pane.zoom. "
                      "Raises an error when there is no client."))})

       :session
         (section :rex.session
           "Session methods: rex.session.<method>{...}."
           (session-fields
             {:view                   "Return the session's windows, layouts, and blocks."
              :list_blocks            "List every block and whether it is placed or detached."
              :list_clients           "List the clients attached to the session."
              :describe_block         "Return a block's placement and the methods it offers."
              :new_window             "Create a window from a layout."
              :new_split              "Create a split beside an existing block."
              :new_block              "Create a block without placing it in a layout."
              :new_layer              "Create a floating layer in a window."
              :close_window           "Close a window and every block in it."
              :close_layer            "Close a floating layer and every block in it."
              :move_block             "Move a block into a new split beside another."
              :swap_blocks            "Exchange the positions of two blocks."
              :detach_block           "Remove a block from its layout without closing it."
              :float_block            "Move a tiled block into a new floating layer."
              :zoom_block             "Toggle or set whether a block fills its window."
              :resize_pane            "Move the border between two blocks."
              :resize_layer           "Set a floating layer's position and size."
              :raise_layer            "Raise a floating layer in the stacking order."
              :lower_layer            "Lower a floating layer in the stacking order."
              :move_window            "Move a window in the tab order."
              :focus_block            "Focus a block."
              :focus_window           "Focus a window."
              :focus_direction        "Focus the nearest block in a direction."
              :focus_next_pane        "Focus the next pane."
              :focus_previous_pane    "Focus the previous pane."
              :focus_next_window      "Focus the next window."
              :focus_previous_window  "Focus the previous window."
              :set_label              "Set the session's label."
              :set_window_label       "Set a window's label."
              :set_block_label        "Set a block's label."
              :attach                 "Change which session a script hears events from."
              :detach                 "Change which session a script hears events from."
              :help                   "Describe the methods available through the session."}))

       :call
         (fun :rex.call [:method :?args]
           (text "Call any server method by its full name; `rex api list` prints them. "
                 "Returns a result table, or nil and an error message."))

       :block
         (section :rex.block
           "Block methods."
           {:call
              (fun :rex.block.call [:creator :method :?args]
                (text "Call a method on a block. `creator` is the full name of the kind of "
                      "block, such as com.superlogical.terminal. `block_id` defaults to the "
                      "current block."))})

       :layout
         (section :rex.layout
           "Builders for the `layout` table taken by new_window, new_split and new_layer."
           {:block
              (fun :rex.layout.block [:spec]
                (text "One block. `spec` is {:flavor name :options {...}}, for example "
                      "com.superlogical.terminal.shell."))

            :horizontal
              (fun :rex.layout.horizontal [:ratio :a :b]
                "Two layouts split horizontally. `ratio` is the share given to `a`, from 0 to 1.")

            :vertical
              (fun :rex.layout.vertical [:ratio :a :b]
                "Two layouts split vertically. `ratio` is the share given to `a`, from 0 to 1.")})

       :on
         (fun :rex.on [:event :fn]
           "Call (fn [ctx ev]) each time `event` happens. Scripts run with `rex do` only.")

       :wait
         (fun :rex.wait [:event :?filter :?timeout]
           (text "Block until `event` happens and return ev and ctx, or nil and "
                 "\"timeout\". `filter` is a table of fields to match or (fn [ev ctx]). "
                 "Scripts only."))

       :sleep
         (fun :rex.sleep [:seconds]
           "Block for a number of seconds, which may be fractional. Scripts only.")

       :stop
         (fun :rex.stop [:?code]
           "End the script immediately. `rex do` exits with `code`, or 0. Scripts only.")

       :host
         (fun :rex.host [:spec]
           (text "Declare another Rex server. `spec` has label, endpoints (or endpoint), "
                 "and optionally id and icon. Configuration file only."))

       :servers
         (fun :rex.servers []
           (text "Return the labels of the other servers available to the running action or "
                 "script."))

       :server
         (fun :rex.server [:label]
           (text "Return a table with the same session, block and call functions as rex, "
                 "whose calls go to that server. Raises an error for an unknown label."))

       :exec
         (fun :rex.exec [:argv :?opts]
           (text "Run a command, wait for it to exit, and return {:code :signal :stdout "
                 ":stderr :timed_out :ok}. A non-zero exit is still a result: check `ok`. "
                 "Returns nil and a message only when the command never ran. No shell reads "
                 "`argv`.")
           {:start
              (fun :rex.exec.start [:argv :?opts]
                (text "Start a command without waiting and return a handle with wait, write, "
                      "close_stdin, kill, running and pid. Takes rex.exec's options plus a stdin "
                      "of true, stdout and stderr callbacks, and detach."))})

       :after
         (fun :rex.after [:seconds :fn]
           (text "Call (fn [ctx]) once after `seconds`. Returns a timer with cancel and "
                 "active. Scripts only."))

       :every
         (fun :rex.every [:seconds :fn]
           (text "Call (fn [ctx]) every `seconds`, which must be more than zero, until "
                 "cancelled. Returns a timer with cancel and active. Scripts only."))

       :json
         (section :rex.json
           "JSON encoding."
           {:encode
              (fun :rex.json.encode [:value :?opts]
                (text "Turn a Lua value into JSON text, or return nil and an error message. "
                      "`opts.indent` indents by two spaces."))

            :decode
              (fun :rex.json.decode [:text]
                "Turn JSON text into a Lua value, or return nil and an error message.")})

       :log
         (fun :rex.log [:?level :message]
           "Write a log message. `level` is debug, info, warn or error, default info.")

       :args
         (value :rex.args
           "The script's arguments, as a table. Scripts only.")

       :session_id
         (value :rex.session_id
           "The ID of the session the script runs against. Scripts only.")

       :block_id
         (value :rex.block_id
           "The ID of the block the script runs against, or nil. Scripts only.")})})

docset
