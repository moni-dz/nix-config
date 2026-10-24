local M = {}

local ns = vim.api.nvim_create_namespace("moni_menu")

function M.open(entries, at, opts)
  opts = opts or {}
  local rows = {}
  for _, entry in ipairs(entries) do
    if not (entry.separator and (#rows == 0 or rows[#rows].separator)) then
      rows[#rows + 1] = entry
    end
  end
  while #rows > 0 and rows[#rows].separator do
    rows[#rows] = nil
  end
  if #rows == 0 then
    return
  end

  local width = 0
  for _, row in ipairs(rows) do
    if not row.separator then
      local w = vim.fn.strdisplaywidth(row.label) + (row.hint and (vim.fn.strdisplaywidth(row.hint) + 3) or 0)
      width = math.max(width, w)
    end
  end
  width = width + 2
  local lines = {}
  for i, row in ipairs(rows) do
    if row.separator then
      lines[i] = string.rep("─", width)
    else
      local gap = row.hint and (width - 2 - vim.fn.strdisplaywidth(row.label) - vim.fn.strdisplaywidth(row.hint)) or 0
      lines[i] = " " .. row.label .. (row.hint and (string.rep(" ", gap) .. row.hint) or "") .. " "
    end
  end

  local buf = vim.api.nvim_create_buf(false, true)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.bo[buf].modifiable = false
  vim.bo[buf].bufhidden = "wipe"
  for i, row in ipairs(rows) do
    if row.separator then
      vim.api.nvim_buf_set_extmark(buf, ns, i - 1, 0, { end_col = #lines[i], hl_group = "MoniMenuSeparator" })
    elseif row.hint then
      local start = #lines[i] - 1 - #row.hint
      vim.api.nvim_buf_set_extmark(buf, ns, i - 1, start, { end_col = start + #row.hint, hl_group = "MoniMenuHint" })
    end
  end

  local height = #rows
  local row, col = at[1], at[2]
  local pointer = row - 1
  local room_below = vim.o.lines - 1 - row - 2
  local room_above = pointer - 2
  if height > room_below and room_above > room_below then
    height = math.max(1, math.min(height, room_above))
    row = pointer - height - 2
  else
    height = math.max(1, math.min(height, room_below))
  end
  if col + width + 2 > vim.o.columns then
    col = math.max(0, vim.o.columns - width - 2)
  end

  local origin = vim.api.nvim_get_current_win()
  local win = vim.api.nvim_open_win(buf, true, {
    relative = "editor",
    row = row,
    col = col,
    width = width,
    height = height,
    style = "minimal",
    border = "rounded",
    zindex = 70,
  })
  vim.wo[win].cursorline = true
  vim.wo[win].wrap = false
  vim.wo[win].winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder,CursorLine:PmenuSel"

  local function selectable(i)
    return rows[i] and not rows[i].separator
  end
  local function first_selectable(from, step)
    local i = from
    while i >= 1 and i <= #rows do
      if selectable(i) then
        return i
      end
      i = i + step
    end
  end
  vim.api.nvim_win_set_cursor(win, { first_selectable(1, 1), 0 })

  local done = false
  local function close()
    if done then
      return
    end
    done = true
    if vim.api.nvim_win_is_valid(win) then
      vim.api.nvim_win_close(win, true)
    end
    if vim.api.nvim_win_is_valid(origin) then
      vim.api.nvim_set_current_win(origin)
    end
  end
  local function run(i)
    if not selectable(i) then
      return
    end
    close()
    vim.schedule(function()
      if opts.before then
        opts.before()
      end
      rows[i].run()
    end)
  end
  local function move(step)
    local i = vim.api.nvim_win_get_cursor(win)[1]
    local target = first_selectable(i + step, step) or first_selectable(step > 0 and 1 or #rows, step)
    if target then
      vim.api.nvim_win_set_cursor(win, { target, 0 })
    end
  end

  local function map(keys, fn)
    for _, lhs in ipairs(type(keys) == "table" and keys or { keys }) do
      vim.keymap.set("n", lhs, fn, { buffer = buf, nowait = true })
    end
  end
  map({ "<CR>", "<Tab>" }, function()
    run(vim.api.nvim_win_get_cursor(win)[1])
  end)
  map({ "j", "<Down>", "<C-n>" }, function()
    move(1)
  end)
  map({ "k", "<Up>", "<C-p>", "<S-Tab>" }, function()
    move(-1)
  end)
  map({ "<Esc>", "q", "<C-c>", "<RightMouse>" }, close)
  map("<LeftMouse>", function()
    local pos = vim.fn.getmousepos()
    if pos.winid == win then
      run(pos.line)
    else
      close()
    end
  end)
  local function wheel(step)
    return function()
      local top = vim.fn.getwininfo(win)[1].topline
      local target = math.max(1, math.min(top + step, #rows - height + 1))
      vim.api.nvim_win_call(win, function()
        vim.fn.winrestview({ topline = target })
      end)
      local line = vim.api.nvim_win_get_cursor(win)[1]
      if line < target or line >= target + height then
        vim.api.nvim_win_set_cursor(win, { math.max(target, math.min(line, target + height - 1)), 0 })
      end
    end
  end
  map({ "<ScrollWheelDown>", "<S-ScrollWheelDown>" }, wheel(3))
  map({ "<ScrollWheelUp>", "<S-ScrollWheelUp>" }, wheel(-3))
  map({ "<ScrollWheelLeft>", "<ScrollWheelRight>", "<S-ScrollWheelLeft>", "<S-ScrollWheelRight>", "zl", "zh", "zL", "zH", "<Left>", "<Right>", "h", "l" }, function() end)
  map("<MouseMove>", function()
    local pos = vim.fn.getmousepos()
    if pos.winid == win and selectable(pos.line) then
      vim.api.nvim_win_set_cursor(win, { pos.line, 0 })
    end
  end)
  vim.api.nvim_create_autocmd("WinLeave", { buffer = buf, once = true, callback = close })
end

return M
