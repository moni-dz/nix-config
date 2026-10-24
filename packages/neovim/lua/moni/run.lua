local M = {}

local state = { block = nil, root = nil, last = nil }

local function in_rex()
  return vim.env.REX_SESSION and vim.env.REX_BLOCK and vim.fn.executable("rex") == 1
end

local function rex(args)
  local res = vim.system(vim.list_extend({ "rex" }, args), { text = true }):wait(5000)
  return vim.trim(res.stdout or ""), res.code == 0, vim.trim(res.stderr or "")
end

local function alive(block)
  return block ~= nil and select(2, rex({ "block", "inspect", block }))
end

local function wait_until_ready(block)
  local last = ""
  for _ = 1, 60 do
    vim.wait(100)
    local screen = rex({ "capture", "-b", block, "--trim" })
    if screen ~= "" and screen == last then
      return
    end
    last = screen
  end
end

local function runner_pane()
  if alive(state.block) then
    return state.block
  end
  local out, ok, err = rex({ "split", "-b", vim.env.REX_BLOCK, "--split=below", "--ratio", "35", "--focus=false" })
  local block = out:match("block:[%w%-]+")
  if not ok or not block then
    vim.notify("could not open a Rex split: " .. err, vim.log.levels.ERROR)
    return nil
  end
  state.block, state.root = block, nil
  wait_until_ready(block)
  return block
end

local function send(block, text)
  rex({ "send", "-b", block, text })
  rex({ "send-key", "-b", block, "enter" })
end

local projects = {
  { markers = { "Cargo.toml" }, run = "cargo run", build = "cargo build", test = "cargo test" },
  { markers = { "go.mod" }, run = "go run .", build = "go build ./...", test = "go test ./..." },
  { markers = { "package.json" }, run = "npm start", build = "npm run build", test = "npm test" },
  { markers = { "pyproject.toml", "setup.py" }, test = "pytest" },
  { markers = { "justfile", "Justfile" }, run = "just", build = "just build", test = "just test" },
  { markers = { "Makefile" }, run = "make run", build = "make", test = "make test" },
  { markers = { "flake.nix" }, run = "nix run", build = "nix build", test = "nix flake check" },
}

local function project()
  local best
  for _, p in ipairs(projects) do
    local root = vim.fs.root(0, p.markers)
    if root and (not best or #root > #best.root) then
      best = vim.tbl_extend("force", p, { root = root })
    end
  end
  return best
end

local function cargo_package(root)
  local in_package = false
  for _, line in ipairs(vim.fn.readfile(root .. "/Cargo.toml")) do
    if line:match("^%[package%]") then
      in_package = true
    elseif line:match("^%[") then
      in_package = false
    elseif in_package then
      local name = line:match('^name%s*=%s*"([^"]+)"')
      if name then
        return name
      end
    end
  end
end

local function rust_test_under_cursor()
  local ok, node = pcall(vim.treesitter.get_node)
  while ok and node do
    if node:type() == "function_item" then
      local prev = node:prev_named_sibling()
      while prev and prev:type() == "attribute_item" do
        if vim.treesitter.get_node_text(prev, 0):find("test") then
          return vim.treesitter.get_node_text(node:field("name")[1], 0)
        end
        prev = prev:prev_named_sibling()
      end
    end
    node = node:parent()
  end
end

local function in_dev_shell()
  return (vim.env.IN_NIX_SHELL or vim.env.DIRENV_DIR) ~= nil
end

local keep_pane_own = { "^PWD$", "^OLDPWD$", "^SHLVL$", "^_$", "^TERM", "^COLORTERM$", "^REX_", "^NVIM", "^VIM", "^MYVIMRC$" }

local function sh_quote(text)
  return "'" .. text:gsub("'", "'\\''") .. "'"
end

local function dev_shell_script(cmds, root)
  local lines = { "#!/bin/sh" }
  for name, value in pairs(vim.fn.environ()) do
    local own = vim.iter(keep_pane_own):any(function(pattern)
      return name:match(pattern) ~= nil
    end)
    if name:match("^[%a_][%w_]*$") and not own then
      lines[#lines + 1] = ("export %s=%s"):format(name, sh_quote(value))
    end
  end
  lines[#lines + 1] = ("cd %s && %s"):format(sh_quote(root), table.concat(cmds, " && "))
  local path = vim.fn.tempname() .. ".sh"
  vim.fn.writefile(lines, path)
  return path
end

function M.root()
  return (project() or {}).root or vim.fn.getcwd()
end

function M.run(cmds, root)
  state.last = { cmds = cmds, root = root }
  if not in_rex() then
    vim.cmd("botright 12new")
    vim.fn.jobstart({ "sh", "-c", table.concat(cmds, " && ") }, { term = true, cwd = root })
    return
  end
  local block = runner_pane()
  if not block then
    return
  end
  rex({ "send-key", "-b", block, "ctrl+c" })
  rex({ "send-key", "-b", block, "ctrl+l" })
  local line = table.concat(cmds, "; ")
  if in_dev_shell() then
    line = "sh '" .. dev_shell_script(cmds, root) .. "'"
  end
  if state.root ~= root then
    line = "cd '" .. root .. "'; " .. line
    state.root = root
  end
  send(block, line)
end

local function with_project(action)
  local p = project()
  if not p then
    vim.notify("no project here (looked for Cargo.toml, go.mod, package.json, Makefile, flake.nix, ...)", vim.log.levels.WARN)
    return
  end
  action(p)
end

local function simple(kind)
  return function()
    with_project(function(p)
      if p[kind] then
        M.run({ p[kind] }, p.root)
      else
        vim.notify(p.markers[1] .. " projects have no " .. kind .. " command", vim.log.levels.WARN)
      end
    end)
  end
end

local function test_nearest()
  with_project(function(p)
    local name = p.markers[1] == "Cargo.toml" and rust_test_under_cursor()
    M.run({ name and ("cargo test " .. name) or p.test }, p.root)
  end)
end

local function debug_binary()
  with_project(function(p)
    local debugger = vim.iter({ "rust-lldb", "lldb", "rust-gdb", "gdb" }):find(function(d)
      return vim.fn.executable(d) == 1
    end)
    if not debugger then
      vim.notify("no debugger on PATH (rust-lldb, lldb, rust-gdb or gdb)", vim.log.levels.WARN)
      return
    end
    local name = p.markers[1] == "Cargo.toml" and cargo_package(p.root)
    if name then
      M.run({ "cargo build", debugger .. " target/debug/" .. name }, p.root)
      return
    end
    vim.ui.input({ prompt = "debug which executable: ", completion = "file" }, function(path)
      if path and path ~= "" then
        M.run({ debugger .. " " .. path }, p.root)
      end
    end)
  end)
end

local function parse_errors(text, root)
  local items, message = {}, ""
  local function add(file, lnum, col, msg)
    local path = file:sub(1, 1) == "/" and file or (root .. "/" .. file)
    if vim.uv.fs_stat(path) then
      items[#items + 1] = { filename = path, lnum = tonumber(lnum), col = tonumber(col) or 1, text = msg }
    end
  end
  for line in text:gmatch("[^\r\n]+") do
    local head = line:match("^%s*(error[%[:].*)") or line:match("^%s*(warning[%[:].*)")
    if head then
      message = head
    end
    local file, lnum, col = line:match("^%s*%-%-> (.-):(%d+):(%d+)")
    if file then
      add(file, lnum, col, message)
    else
      local f, l, c, msg = line:match("^(%S[^:]*):(%d+):(%d+):%s*(.*)$")
      if not f then
        f, l, msg = line:match("^(%S[^:]*):(%d+):%s*(.*)$")
      end
      if f then
        add(f, l, c, msg)
      else
        f, l, c = line:match("at (/[^:]+):(%d+):(%d+):")
        if not f then
          f, l = line:match('File "(.-)", line (%d+)')
        end
        if f then
          add(f, l, c, message)
        end
      end
    end
  end
  return items
end

local function errors_to_quickfix()
  if not alive(state.block) then
    vim.notify("no runner pane yet", vim.log.levels.WARN)
    return
  end
  local text = rex({ "capture", "-b", state.block, "--trim" })
  local items = parse_errors(text, state.root or vim.fn.getcwd())
  if #items == 0 then
    vim.notify("no file:line positions in the runner's output")
    return
  end
  local title = "run: " .. table.concat(state.last and state.last.cmds or {}, ", ")
  vim.fn.setqflist({}, " ", { title = title, items = items })
  vim.cmd("copen 8")
end

local function complete_shell(_, cmdline, cursorpos)
  local line = cmdline:sub(1, cursorpos):gsub("^%s*%S+%s+", "")
  return require("moni.shellcomplete").words(line, M.root())
end

vim.api.nvim_create_user_command("Run", function(opts)
  M.run({ opts.args }, M.root())
end, { nargs = "+", complete = complete_shell, desc = "run a command in the runner split" })

local function map(lhs, fn, desc)
  vim.keymap.set("n", lhs, fn, { desc = desc })
end
map("<leader>rr", simple("run"), "run the project")
map("<leader>rb", simple("build"), "build")
map("<leader>rt", test_nearest, "test: nearest (or all)")
map("<leader>rT", simple("test"), "test: all")
map("<leader>rd", debug_binary, "debug in lldb/gdb")
map("<leader>rl", function()
  if state.last then
    M.run(state.last.cmds, state.last.root)
  else
    vim.notify("nothing has been run yet")
  end
end, "run the last command again")
map("<leader>rc", function()
  vim.ui.input({ prompt = "run: ", default = state.last and state.last.cmds[#state.last.cmds] or "" }, function(cmd)
    if cmd and cmd ~= "" then
      M.run({ cmd }, M.root())
    end
  end)
  vim.bo.filetype = "moni-run"
end, "run a command")
map("<leader>rq", errors_to_quickfix, "runner errors to quickfix")
map("<leader>ro", function()
  if alive(state.block) then
    rex({ "block", "focus", state.block })
  end
end, "focus the runner")
map("<leader>rx", function()
  if alive(state.block) then
    rex({ "block", "close", state.block })
  end
  state.block, state.root = nil, nil
end, "close the runner")

return M
