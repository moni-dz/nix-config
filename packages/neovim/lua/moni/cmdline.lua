local cmdline = require("vim._core.ui2.cmdline")

local api, fn = vim.api, vim.fn

vim.api.nvim_set_hl(0, "MoniCmdlinePrompt", { link = "MoniMenuHint", default = true })

local ns = api.nvim_create_namespace("moni.cmdline")
local buf, win
local text, prefix = "", 0

local function ensure_buf()
  if not (buf and api.nvim_buf_is_valid(buf)) then
    buf = api.nvim_create_buf(false, true)
    vim.bo[buf].bufhidden = "hide"
  end
end

local function place()
  local width = math.min(math.max(48, math.floor(vim.o.columns * 0.5)), vim.o.columns - 4)
  api.nvim_win_set_config(win, { width = width })
  local height = math.min(api.nvim_win_text_height(win, {}).all, math.max(1, vim.o.lines - 6))
  api.nvim_win_set_config(win, {
    relative = "editor",
    row = math.max(1, math.floor(vim.o.lines * 0.2)),
    col = math.floor((vim.o.columns - width - 2) / 2),
    width = width,
    height = height,
  })
end

local function highlight(lines, hl_id)
  api.nvim_buf_clear_namespace(buf, ns, 0, -1)
  local last = #lines - 1
  if prefix > 0 then
    api.nvim_buf_set_extmark(buf, ns, last, 0, {
      end_col = prefix,
      hl_group = hl_id and hl_id > 0 and hl_id or "MoniCmdlinePrompt",
    })
  end
  if lines[1]:sub(1, 1) == ":" and last == 0 then
    local ok, parser = pcall(vim.treesitter.get_string_parser, text, "vim")
    if ok then
      parser:parse(true)
      parser:for_each_tree(function(tstree, tree)
        local query = tstree and vim.treesitter.query.get(tree:lang(), "highlights")
        if query then
          for capture, node in query:iter_captures(tstree:root(), text) do
            local _, start_col, _, end_col = node:range()
            if query.captures[capture]:sub(1, 1) ~= "_" then
              api.nvim_buf_set_extmark(buf, ns, 0, prefix + start_col, {
                end_col = prefix + end_col,
                hl_group = ("@%s.%s"):format(query.captures[capture], query.lang),
              })
            end
          end
        end
      end)
    end
  end
end

local function redraw()
  api.nvim__redraw({ flush = true, cursor = true, win = win })
end

function cmdline.cmdline_show(content, pos, firstc, prompt, indent, level, hl_id)
  ensure_buf()
  local lines = {}
  for line in (("%s%s%s"):format(firstc, prompt, (" "):rep(indent)) .. "\n"):gmatch("(.-)\n") do
    lines[#lines + 1] = fn.strtrans(line)
  end
  text = ""
  for _, chunk in ipairs(content) do
    text = text .. chunk[2]
  end
  prefix = #lines[#lines]
  lines[#lines] = lines[#lines] .. fn.strtrans(text) .. " "
  api.nvim_buf_set_lines(buf, 0, -1, false, lines)

  if not (win and api.nvim_win_is_valid(win)) then
    win = api.nvim_open_win(buf, false, {
      relative = "editor",
      row = 1,
      col = 0,
      width = 48,
      height = 1,
      style = "minimal",
      border = "rounded",
      zindex = 250,
      focusable = false,
    })
    vim.wo[win].wrap = true
    vim.wo[win].winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder"
  end
  highlight(lines, hl_id)
  place()
  cmdline.level = level
  cmdline.cmdline_pos(pos)
end

function cmdline.cmdline_pos(pos)
  if not (win and api.nvim_win_is_valid(win)) then
    return
  end
  local last = api.nvim_buf_line_count(buf)
  api.nvim_win_set_cursor(win, { last, prefix + #fn.strtrans(text:sub(1, pos)) })
  redraw()
end

function cmdline.cmdline_special_char(c, shift)
  if win and api.nvim_win_is_valid(win) then
    api.nvim_win_call(win, function()
      api.nvim_put({ c }, shift and "" or "c", false, false)
    end)
  end
end

function cmdline.cmdline_hide(level)
  if level > (fn.getcmdwintype() == "" and 1 or 2) then
    return
  end
  cmdline.level = 0
  if win and api.nvim_win_is_valid(win) then
    api.nvim_win_close(win, true)
  end
  win = nil
end

function cmdline.cmdline_block_show() end
function cmdline.cmdline_block_append() end
function cmdline.cmdline_block_hide() end

local M = {}

function M.position()
  if win and api.nvim_win_is_valid(win) then
    local box = api.nvim_win_get_config(win)
    return { box.row + box.height + 1, box.col + 1 }
  end
  return { vim.o.lines - 1, 0 }
end

return M
