vim.o.mousemoveevent = true
local hover = { timer = vim.uv.new_timer(), win = nil, key = nil }

local function hover_close()
  if hover.win and vim.api.nvim_win_is_valid(hover.win) then
    vim.api.nvim_win_close(hover.win, true)
  end
  hover.win, hover.key = nil, nil
end

local function symbol_under_mouse()
  local pos = vim.fn.getmousepos()
  if pos.winid == 0 or pos.line == 0 or vim.api.nvim_win_get_config(pos.winid).relative ~= "" then
    return nil, pos
  end
  local buf = vim.api.nvim_win_get_buf(pos.winid)
  if vim.bo[buf].buftype ~= "" then
    return nil, pos
  end
  local text = vim.api.nvim_buf_get_lines(buf, pos.line - 1, pos.line, false)[1] or ""
  local word, start = unpack(vim.fn.matchstrpos(text, [[\k*\%]] .. pos.column .. [[c\k\+]]))
  if word == "" then
    return nil, pos
  end
  return pos.winid .. ":" .. pos.line .. ":" .. start, pos, buf, text
end

local function hover_show()
  local key, pos, buf, text = symbol_under_mouse()
  if not key or key == hover.key then
    return
  end
  local diagnostics = vim.tbl_map(function(d)
    return d.message
  end, vim.diagnostic.get(buf, { lnum = pos.line - 1 }))

  local function open(lines, title)
    if #lines == 0 or select(1, symbol_under_mouse()) ~= key then
      return
    end
    hover_close()
    local _, win = vim.lsp.util.open_floating_preview(lines, "markdown", {
      focus = false,
      close_events = {},
      max_width = 80,
      max_height = 20,
      border = "rounded",
      title = " " .. title .. " ",
      title_pos = "left",
    })
    vim.api.nvim_win_set_config(win, { relative = "mouse", row = 1, col = 0 })
    hover.win, hover.key = win, key
  end

  local client = vim.lsp.get_clients({ bufnr = buf, method = "textDocument/hover" })[1]
  if not client then
    return open(diagnostics, "diagnostics")
  end
  local params = {
    textDocument = { uri = vim.uri_from_bufnr(buf) },
    position = {
      line = pos.line - 1,
      character = vim.str_utfindex(text, client.offset_encoding, pos.column - 1, false),
    },
  }
  client:request("textDocument/hover", params, function(_, result)
    local lines = {}
    if result and result.contents then
      lines = vim.lsp.util.convert_input_to_markdown_lines(result.contents)
      lines = vim.split(table.concat(lines, "\n"), "\n", { trimempty = true })
    end
    if #diagnostics > 0 then
      table.insert(lines, 1, "---")
      for i = #diagnostics, 1, -1 do
        table.insert(lines, 1, diagnostics[i])
      end
    end
    open(lines, client.name)
  end, buf)
end

vim.keymap.set({ "n", "v" }, "<MouseMove>", function()
  hover.timer:stop()
  local pos = vim.fn.getmousepos()
  if hover.win and pos.winid == hover.win then
    return
  end
  if hover.win and select(1, symbol_under_mouse()) ~= hover.key then
    hover_close()
  end
  hover.timer:start(500, 0, vim.schedule_wrap(hover_show))
end, { desc = "hover tooltip" })

vim.api.nvim_create_autocmd({ "InsertEnter", "BufLeave", "CursorMoved" }, { callback = hover_close })
