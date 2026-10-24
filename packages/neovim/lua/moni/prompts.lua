local function select(items, opts, on_choice)
  opts = opts or {}
  if #items == 0 then
    on_choice(nil, nil)
    return
  end
  local format = opts.format_item or tostring
  local lines, width = {}, 0
  for i, item in ipairs(items) do
    local text = tostring(format(item)):gsub("\n.*", "")
    lines[i] = string.format(" %s  %s ", i <= 9 and tostring(i) or " ", text)
    width = math.max(width, vim.fn.strdisplaywidth(lines[i]))
  end
  local title = vim.trim((opts.prompt or ""):gsub("%s*:%s*$", ""))
  width = math.min(math.max(width + 1, vim.fn.strdisplaywidth(title) + 4, 24), math.floor(vim.o.columns * 0.7))

  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  vim.bo[buf].bufhidden = "wipe"
  for i = 1, math.min(#items, 9) do
    vim.api.nvim_buf_set_extmark(buf, vim.api.nvim_create_namespace("moni_select"), i - 1, 1, { end_col = 2, hl_group = "Comment" })
  end
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "cursor",
    row = 1,
    col = 0,
    width = width,
    height = math.min(#lines, 10),
    style = "minimal",
    border = "rounded",
    title = title ~= "" and (" " .. title .. " ") or nil,
    title_pos = "left",
    zindex = 60,
  })
  vim.wo[win].wrap = false
  vim.wo[win].cursorline = true
  vim.wo[win].winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder,FloatTitle:FloatTitle,CursorLine:PmenuSel"

  local done = false
  local function finish(index)
    if done then
      return
    end
    done = true
    if vim.api.nvim_win_is_valid(win) then
      vim.api.nvim_win_close(win, true)
    end
    vim.schedule(function()
      on_choice(index and items[index] or nil, index)
    end)
  end
  local function move(delta)
    local row = vim.api.nvim_win_get_cursor(win)[1]
    vim.api.nvim_win_set_cursor(win, { (row - 1 + delta) % #items + 1, 0 })
  end
  local function map(keys, fn)
    for _, lhs in ipairs(type(keys) == "table" and keys or { keys }) do
      vim.keymap.set("n", lhs, fn, { buffer = buf, nowait = true })
    end
  end
  local function accept()
    finish(vim.api.nvim_win_get_cursor(win)[1])
  end
  map({ "<CR>", "<Tab>" }, accept)
  map({ "<Esc>", "q", "<C-c>" }, function()
    finish(nil)
  end)
  map({ "j", "<Down>", "<C-n>" }, function()
    move(1)
  end)
  map({ "k", "<Up>", "<C-p>", "<S-Tab>" }, function()
    move(-1)
  end)
  for n = 1, math.min(#items, 9) do
    map(tostring(n), function()
      finish(n)
    end)
  end
  map("<LeftMouse>", function()
    local pos = vim.fn.getmousepos()
    finish(pos.winid == win and pos.line >= 1 and pos.line <= #items and pos.line or nil)
  end)
  vim.api.nvim_create_autocmd({ "WinLeave", "BufLeave" }, {
    buffer = buf,
    once = true,
    callback = function()
      finish(nil)
    end,
  })
end

vim.ui.select = select

vim.ui.input = function(opts, on_confirm)
  opts = opts or {}
  local default = opts.default or ""
  local title = vim.trim((opts.prompt or ""):gsub("%s*:%s*$", ""))
  local width = math.min(math.max(32, vim.fn.strdisplaywidth(default) + 8, vim.fn.strdisplaywidth(title) + 4), math.floor(vim.o.columns * 0.7))

  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, { default })
  vim.bo[buf].bufhidden = "wipe"
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "cursor",
    row = 1,
    col = 0,
    width = width,
    height = 1,
    style = "minimal",
    border = "rounded",
    title = title ~= "" and (" " .. title .. " ") or nil,
    title_pos = "left",
    zindex = 60,
  })
  vim.wo[win].winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder,FloatTitle:FloatTitle"

  local done = false
  local function finish(confirm)
    if done then
      return
    end
    done = true
    local text = vim.api.nvim_buf_get_lines(buf, 0, 1, false)[1]
    vim.cmd.stopinsert()
    if vim.api.nvim_win_is_valid(win) then
      vim.api.nvim_win_close(win, true)
    end
    vim.schedule(function()
      on_confirm(confirm and text or nil)
    end)
  end
  for _, mode in ipairs({ "i", "n" }) do
    vim.keymap.set(mode, "<CR>", function()
      finish(true)
    end, { buffer = buf })
    vim.keymap.set(mode, "<Esc>", function()
      finish(false)
    end, { buffer = buf })
    vim.keymap.set(mode, "<C-c>", function()
      finish(false)
    end, { buffer = buf })
  end
  vim.api.nvim_create_autocmd("WinLeave", {
    buffer = buf,
    once = true,
    callback = function()
      finish(false)
    end,
  })
  vim.cmd("startinsert!")
end
