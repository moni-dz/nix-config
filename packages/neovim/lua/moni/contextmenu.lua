local menu = require("moni.menu")

local M = {}

local function lsp_attached(buf)
  return #vim.lsp.get_clients({ bufnr = buf }) > 0
end

local function press(lhs)
  local map = vim.fn.maparg(lhs, "n", false, true)
  if map.callback then
    map.callback()
  elseif map.rhs and map.rhs ~= "" then
    vim.cmd((map.rhs:gsub("^<Cmd>", ""):gsub("<CR>$", "")))
  end
end

local function copy(text, what)
  vim.fn.setreg("+", text)
  vim.notify("copied " .. what .. "\n" .. text)
end

local function in_hunk(buf, line)
  local ok, gitsigns = pcall(require, "gitsigns")
  if not ok then
    return false
  end
  for _, hunk in ipairs(gitsigns.get_hunks(buf) or {}) do
    local from, count = hunk.added.start, math.max(hunk.added.count, 1)
    if line >= from and line < from + count then
      return true
    end
  end
  return false
end

local function editor_entries(buf, visual)
  local entries = {}
  local function add(label, hint, run)
    entries[#entries + 1] = { label = label, hint = hint, run = run }
  end
  local function separator()
    entries[#entries + 1] = { separator = true }
  end
  local line = vim.fn.line(".")

  if lsp_attached(buf) then
    add("Go to Definition", "gd", vim.lsp.buf.definition)
    add("Go to Type Definition", "gy", vim.lsp.buf.type_definition)
    add("Find References", "grr", vim.lsp.buf.references)
    add("Go to Implementation", "gri", vim.lsp.buf.implementation)
    add("Show Documentation", "K", vim.lsp.buf.hover)
    separator()
    add("Rename Symbol", "grn", vim.lsp.buf.rename)
    add("Code Action", "Space a", vim.lsp.buf.code_action)
    add(visual and "Format Selection" or "Format Document", "Space f", function()
      if visual then
        vim.lsp.buf.format({ range = { start = vim.fn.getpos("'<"), ["end"] = vim.fn.getpos("'>") } })
      else
        vim.lsp.buf.format()
      end
    end)
    separator()
  end

  if visual then
    add("Cut", "d", function()
      vim.cmd("normal! d")
    end)
    add("Copy", "y", function()
      vim.cmd("normal! y")
    end)
  else
    add("Copy Line", "yy", function()
      vim.cmd("normal! yy")
    end)
  end
  add("Paste", "p", function()
    vim.cmd("normal! p")
  end)
  add("Select All", "ggVG", function()
    vim.cmd("normal! ggVG")
  end)
  add("Toggle Comment", visual and "gc" or "gcc", function()
    vim.cmd(visual and "normal gc" or "normal gcc")
  end)
  separator()

  if #vim.diagnostic.get(buf, { lnum = line - 1 }) > 0 then
    add("Show Line Diagnostics", "Space e", vim.diagnostic.open_float)
    add("Next Error", "]e", function()
      vim.diagnostic.jump({ count = 1, severity = vim.diagnostic.severity.ERROR })
    end)
    separator()
  end

  local repo = vim.fs.root(buf, ".jj")
  if repo then
    add("Toggle Blame", "Space jb", function()
      press("<leader>jb")
    end)
    add("Compare with Parent", "Space jD", function()
      press("<leader>jD")
    end)
    add("File History", "Space jh", function()
      press("<leader>jh")
    end)
  end
  if in_hunk(buf, line) then
    local gitsigns = require("gitsigns")
    add("Preview Hunk", "Space hp", gitsigns.preview_hunk)
    add("Reset Hunk", "Space hr", gitsigns.reset_hunk)
  end
  if repo or in_hunk(buf, line) then
    separator()
  end

  if vim.fs.root(buf, { "Cargo.toml", "go.mod", "package.json", "Makefile", "justfile", "flake.nix", "pyproject.toml" }) then
    add("Run Project", "Space rr", function()
      press("<leader>rr")
    end)
    add("Test Nearest", "Space rt", function()
      press("<leader>rt")
    end)
    separator()
  end

  local target = vim.fn.expand("<cfile>")
  if target:match("^https?://") then
    add("Open Link", "gx", function()
      vim.ui.open(target)
    end)
  end
  if vim.bo[buf].filetype == "markdown" then
    add("Markdown Preview", "Space mp", function()
      press("<leader>mp")
    end)
  end

  local name = vim.api.nvim_buf_get_name(buf)
  if name ~= "" then
    add("Copy Path", "", function()
      copy(vim.fn.fnamemodify(name, ":."), "the path")
    end)
    add("Reveal in File Tree", "Space T", function()
      vim.cmd("NvimTreeFindFile")
    end)
  end
  return entries
end

local function tree_entries(has_node)
  local api = require("nvim-tree.api")
  local entries = {}
  local function add(label, hint, run)
    entries[#entries + 1] = { label = label, hint = hint, run = run }
  end
  local function separator()
    entries[#entries + 1] = { separator = true }
  end
  if has_node then
    add("Open", "Enter", api.node.open.edit)
    add("Open in Split", "C-x", api.node.open.horizontal)
    add("Open in Vertical Split", "C-v", api.node.open.vertical)
    add("Open with Default App", "", api.node.run.system)
    separator()
  end
  add("New File or Folder…", "a", api.fs.create)
  if has_node then
    add("Rename…", "r", api.fs.rename)
    add("Delete", "d", api.fs.remove)
    separator()
    add("Cut", "x", api.fs.cut)
    add("Copy", "c", api.fs.copy.node)
  end
  add("Paste", "p", api.fs.paste)
  if has_node then
    separator()
    add("Copy Absolute Path", "gy", api.fs.copy.absolute_path)
    add("Copy Relative Path", "", api.fs.copy.relative_path)
  end
  separator()
  add("Refresh", "R", api.tree.reload)
  add("Collapse All", "W", api.tree.collapse_all)
  return entries
end

local function below(pos)
  return { pos.screenrow, math.max(pos.screencol - 1, 0) }
end

local function right_click()
  local pos = vim.fn.getmousepos()
  local win = pos.winid
  if win == 0 or vim.api.nvim_win_get_config(win).relative ~= "" then
    return
  end
  local buf = vim.api.nvim_win_get_buf(win)
  local mode = vim.fn.mode()
  local visual = mode:match("^[vV\22]") ~= nil

  local keep = false
  if visual and win == vim.api.nvim_get_current_win() then
    local a, b = vim.fn.line("v"), vim.fn.line(".")
    keep = pos.line >= math.min(a, b) and pos.line <= math.max(a, b)
  end
  if not keep then
    if visual then
      vim.cmd("normal! \27")
    elseif mode == "i" then
      vim.cmd("stopinsert")
    end
    visual = false
    vim.api.nvim_set_current_win(win)
    if pos.line >= 1 and pos.line <= vim.api.nvim_buf_line_count(buf) then
      vim.api.nvim_win_set_cursor(win, { pos.line, math.max(pos.column - 1, 0) })
    end
  end

  local entries
  if vim.bo[buf].filetype == "NvimTree" then
    local ok, node = pcall(require("nvim-tree.api").tree.get_node_under_cursor)
    entries = tree_entries(ok and node ~= nil and pos.line >= 1 and pos.line <= vim.api.nvim_buf_line_count(buf))
  elseif vim.bo[buf].buftype == "" then
    entries = editor_entries(buf, visual)
  else
    return
  end
  menu.open(entries, below(pos), {
    before = visual and function()
      vim.cmd("normal! gv")
    end or nil,
  })
end

function M.at_cursor()
  local buf = vim.api.nvim_get_current_buf()
  local visual = vim.fn.mode():match("^[vV\22]") ~= nil
  local at = { vim.fn.screenrow(), vim.fn.screencol() - 1 }
  local entries
  if vim.bo[buf].filetype == "NvimTree" then
    entries = tree_entries(true)
  elseif vim.bo[buf].buftype == "" then
    entries = editor_entries(buf, visual)
  else
    return
  end
  if visual then
    vim.cmd("normal! \27")
  end
  menu.open(entries, at, {
    before = visual and function()
      vim.cmd("normal! gv")
    end or nil,
  })
end

function M.tab(buf)
  local pos = vim.fn.getmousepos()
  local function add(label, run)
    return { label = label, run = run }
  end
  menu.open({
    add("Close", function()
      Snacks.bufdelete(buf)
    end),
    add("Close Others", function()
      for _, info in ipairs(vim.fn.getbufinfo({ buflisted = 1 })) do
        if info.bufnr ~= buf then
          Snacks.bufdelete(info.bufnr)
        end
      end
    end),
    { separator = true },
    add("Copy Path", function()
      copy(vim.fn.fnamemodify(vim.api.nvim_buf_get_name(buf), ":."), "the path")
    end),
    add("Reveal in File Tree", function()
      vim.cmd.buffer(buf)
      vim.cmd("NvimTreeFindFile")
    end),
  }, below(pos))
end

vim.keymap.set({ "n", "v", "i" }, "<RightMouse>", right_click, { desc = "context menu" })
vim.keymap.set({ "n", "x" }, "<leader>.", M.at_cursor, { desc = "context menu at the cursor" })

return M
