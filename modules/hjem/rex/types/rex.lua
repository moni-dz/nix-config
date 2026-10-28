---@meta rex

-- type stubs for rex's Lua API.
--
-- taken from https://www.superlogical.com/rex/docs/reference/lua and .../reference/events
-- check the page again when rex is updated.

---@class rex.Context
---@field session_id? string
---@field block_id? string
---@field client_id? string
---@field origin? string where the call came from, such as "timer"
---@field server? string

---@class rex.KeyEvent
---@field keybinding string the key that was pressed
---@field prefix? string[] for a sequence, the keys before it

---@class rex.Binding
---@field key string the key or sequence, without its mode
---@field mode? string
---@field action? string nil for a function binding
---@field fn? fun(ctx: rex.Context, ev: rex.KeyEvent): boolean?
---@field args? table
---@field source string where it was bound, such as "keys.lua:3"

---@class rex.BindingFilter
---@field key? string
---@field mode? string
---@field action? string

---@class rex.Mode
---@field name string
---@field exclusive boolean
---@field source string

---@class rex.ModeOptions
---@field exclusive? boolean

---@class rex.ActionSpec
---@field name string lowercase letters, digits and underscores
---@field title string shown in the command palette
---@field run fun(ctx: rex.Context, args: table): any
---@field description? string
---@field category? string defaults to "Actions"
---@field keywords? string extra words for search
---@field args? table<string, string>|table argument types, or a JSON Schema
---@field repeats? boolean run again while the key is held

---@class rex.HostSpec
---@field label string
---@field endpoints? string[] tried in order
---@field endpoint? string in place of `endpoints`
---@field id? string a stable ID, so the host keeps its identity when renamed
---@field icon? string

-- Running commands ----------------------------------------------------------

---@class rex.ExecOpts
---@field cwd? string defaults to Rex's own
---@field env? table<string, string|false> `false` removes a variable
---@field clear_env? boolean start from an empty environment
---@field stdin? string written to the command's standard input
---@field timeout? number seconds until SIGTERM, then SIGKILL two seconds later
---@field stdout? boolean `false` discards the stream
---@field stderr? boolean `false` discards the stream
---@field text? boolean `false` keeps `\r\n` in the output on Windows

---@class rex.StartOpts
---@field cwd? string
---@field env? table<string, string|false>
---@field clear_env? boolean
---@field stdin? boolean|string `true` keeps standard input open for `write`
---@field timeout? number
---@field stdout? false|fun(chunk: string) called with each piece of output
---@field stderr? false|fun(chunk: string)
---@field text? boolean
---@field detach? boolean let the command outlive the work that started it

---@class rex.ExecResult
---@field code integer the exit status, or -1 if a signal ended the command
---@field signal? string such as "SIGTERM"
---@field stdout string
---@field stderr string
---@field timed_out boolean
---@field ok boolean true if `code` is 0 and the command didn't time out

---@class rex.Process
---@field pid integer
local Process = {}

---Wait for the command to exit.
---@param seconds? number
---@return rex.ExecResult? result nil on timeout
---@return string? err "timeout"
function Process:wait(seconds) end

---@param data string
---@return boolean? ok
---@return string? err
function Process:write(data) end

function Process:close_stdin() end

---@param signal? string|integer a name such as "SIGINT", or a number; SIGTERM by default
function Process:kill(signal) end

---@return boolean
function Process:running() end

---@class rex.Timer
local Timer = {}

function Timer:cancel() end

---@return boolean active true until cancelled or fired for the last time
function Timer:active() end

-- Calling the server ----------------------------------------------------------

---@alias rex.Call fun(args?: table): table?, string?

---@class rex.SessionApi: table<string, rex.Call>
---@field view rex.Call windows, layouts and blocks of the session
---@field list_blocks rex.Call
---@field list_clients rex.Call
---@field describe_block rex.Call
---@field new_window rex.Call
---@field new_split rex.Call
---@field new_block rex.Call
---@field new_layer rex.Call
---@field close_window rex.Call
---@field close_layer rex.Call
---@field move_block rex.Call
---@field swap_blocks rex.Call
---@field detach_block rex.Call
---@field float_block rex.Call
---@field zoom_block rex.Call
---@field resize_pane rex.Call
---@field resize_layer rex.Call
---@field raise_layer rex.Call
---@field lower_layer rex.Call
---@field move_window rex.Call
---@field focus_block rex.Call
---@field focus_window rex.Call
---@field focus_direction rex.Call
---@field focus_next_pane rex.Call
---@field focus_previous_pane rex.Call
---@field focus_next_window rex.Call
---@field focus_previous_window rex.Call
---@field set_label rex.Call
---@field set_window_label rex.Call
---@field set_block_label rex.Call
---@field attach rex.Call
---@field detach rex.Call
---@field help rex.Call

---@class rex.BlockApi
local BlockApi = {}

---Call a method on a block. `block_id` defaults to the current block.
---@param creator string the full name of the kind of block, such as "com.superlogical.terminal"
---@param method string
---@param args? table
---@return table? result
---@return string? err
function BlockApi.call(creator, method, args) end

---@class rex.ClientApi
local ClientApi = {}

---Ask the client to perform one of its own actions, such as "pane.zoom".
---@param name string
---@param args? table
function ClientApi.queue(name, args) end

---@class rex.Server
---@field session rex.SessionApi
---@field block rex.BlockApi
---@field call fun(method: string, args?: table): table?, string?

-- Layouts -------------------------------------------------------------------

---@class rex.TerminalOptions
---@field command? string[] the program and its arguments; a shell starts without it
---@field shell? string "none" starts `command` directly
---@field cwd? string
---@field initial_input? string typed into the terminal once it starts
---@field exit? { on_completion: boolean } `{ on_completion = false }` keeps it open

---@class rex.BlockSpec
---@field flavor string what to create, such as "com.superlogical.terminal.shell"
---@field options? rex.TerminalOptions

---A layout table, as taken by `new_window`, `new_split` and `new_layer`.
---@class rex.Layout

---@class rex.LayoutApi
local LayoutApi = {}

---@param spec rex.BlockSpec
---@return rex.Layout
function LayoutApi.block(spec) end

---@param ratio number the share given to `a`, from 0 to 1
---@param a rex.Layout
---@param b rex.Layout
---@return rex.Layout
function LayoutApi.horizontal(ratio, a, b) end

---@param ratio number
---@param a rex.Layout
---@param b rex.Layout
---@return rex.Layout
function LayoutApi.vertical(ratio, a, b) end

---@class rex.JsonApi
local JsonApi = {}

---@param value any
---@param opts? { indent?: boolean } `indent` indents by two spaces
---@return string? text
---@return string? err
function JsonApi.encode(value, opts) end

---@param text string
---@return any value `null` becomes nil
---@return string? err
function JsonApi.decode(text) end

-- Events --------------------------------------------------------------------

---A name Rex emits, or any other string: Rex never checks a name against a list.
---@alias rex.Event
---| "window_created"
---| "window_closed"
---| "window_label_changed"
---| "active_window_changed"
---| "block_created"
---| "block_closed"
---| "session_label_changed"
---| "session_view_changed"
---| "client_connected"
---| "client_disconnected"
---| "client_connection_changed"
---| "creator_changed"
---| "block_event"
---| "terminal.child_exited"
---| "terminal.pwd_changed"
---| "terminal.title_changed"
---| "terminal.bell"
---| "terminal.process_changed"
---| "terminal.desktop_notification"
---| "terminal.progress_report"
---| "terminal.clipboard_written"
---| "terminal.program_status_changed"
---| "terminal.program_status_removed"
---| "session_created"
---| "session_destroyed"
---| "client_joined"
---| "client_left"
---| "client_changed"
---| "keymap_changed"
---| "hosts_changed"
---| "tailscale_status_changed"
---| string

---An event as a handler sees it. Each event adds its own fields, such as
---`exit_code` for "terminal.child_exited"; see the events reference.
---@class rex.EventData
---@field type string "block_event" for every terminal event
---@field session_id? string
---@field payload table the event's own fields again, as a table of their own
---@field block_id? string block events only
---@field creator_name? string block events only, such as "com.superlogical.terminal"
---@field name? string block events only, the event's name without the creator
---@field [string] any

---@alias rex.LogLevel "debug"|"info"|"warn"|"error"

-- The rex table ---------------------------------------------------------------

---@class rex
---Script arguments. Scripts run with `rex do` only.
---@field args table
---The session the script runs against. Scripts only.
---@field session_id string
---The block the script runs against, or nil. Scripts only.
---@field block_id? string
---@field session rex.SessionApi
---@field block rex.BlockApi
---@field client rex.ClientApi
---@field layout rex.LayoutApi
---@field json rex.JsonApi
rex = {}

-- Key bindings. Configuration file only.

---Bind a key, a `>`-separated sequence, or either prefixed with `mode/`, to an action.
---@overload fun(key: string, action: string, args?: table)
---@param key string
---@param fn fun(ctx: rex.Context, ev: rex.KeyEvent): boolean? return true to mark the key handled
function rex.bind(key, fn) end

---Remove the bindings of a key, or of a whole mode.
---@overload fun(spec: { mode: string }): integer
---@overload fun(binding: rex.Binding): integer
---@param key string
---@param target? string|function limits the removal to bindings with this target
---@return integer removed the number of this file's bindings removed
function rex.unbind(key, target) end

---@param name string
---@param options? rex.ModeOptions
function rex.mode(name, options) end

---@param filter? rex.BindingFilter
---@return rex.Binding[]
function rex.bindings(filter) end

---@return rex.Mode[]
function rex.modes() end

-- Actions

---Define an action.
---@param spec rex.ActionSpec
function rex.action(spec) end

---Run an action. In the configuration file, only that file's own actions.
---@param name string
---@param args? table
---@return any result
---@return string? err
function rex.invoke(name, args) end

-- Calling Rex

---Call any server method by its full name; `rex api list` prints them.
---@param method string
---@param args? table
---@return table? result
---@return string? err
function rex.call(method, args) end

-- Hosts

---Declare another Rex server. Configuration file only.
---@param spec rex.HostSpec
function rex.host(spec) end

---@return string[] labels
function rex.servers() end

---@param label string raises an error for an unknown label
---@return rex.Server
function rex.server(label) end

-- Events. Scripts only.

---@param event rex.Event
---@param fn fun(ctx: rex.Context, ev: rex.EventData)
function rex.on(event, fn) end

---Block until `event` happens.
---@overload fun(event: rex.Event, timeout: number): rex.EventData?, rex.Context|string
---@param event rex.Event
---@param filter? table|fun(ev: rex.EventData, ctx: rex.Context): boolean
---@param timeout? number
---@return rex.EventData? ev nil on timeout
---@return rex.Context|string ctx "timeout" when ev is nil
function rex.wait(event, filter, timeout) end

-- Timers and scripts. Scripts only.

---@param seconds number
function rex.sleep(seconds) end

---End the script. `rex do` exits with `code`, or 0.
---@param code? integer
function rex.stop(code) end

---@param seconds number may be zero or fractional
---@param fn fun(ctx: rex.Context)
---@return rex.Timer
function rex.after(seconds, fn) end

---@param seconds number more than zero
---@param fn fun(ctx: rex.Context)
---@return rex.Timer
function rex.every(seconds, fn) end

-- Running commands

---Run a command, wait for it to exit, and return its result. A non-zero exit is
---still a result: check `ok`. Returns nil and a message only when the command
---never ran, such as when the program doesn't exist.
---@param argv string[] the program, then each argument; no shell reads it
---@param opts? rex.ExecOpts
---@return rex.ExecResult? result
---@return string? err
function rex.exec(argv, opts) end

---Start a command without waiting for it.
---@param argv string[]
---@param opts? rex.StartOpts
---@return rex.Process? process
---@return string? err
---@diagnostic disable-next-line: inject-field
function rex.exec.start(argv, opts) end

-- Logging

---@overload fun(message: string)
---@param level rex.LogLevel
---@param message string
function rex.log(level, message) end
