local kinds = require("blink.cmp.types").CompletionItemKind
for _, name in ipairs({ "Command", "Subcommand", "Option", "Path" }) do
  if not vim.tbl_contains(kinds, name) then
    kinds[#kinds + 1] = name
    kinds[name] = #kinds
  end
end

local source = {}

function source.new()
  return setmetatable({}, { __index = source })
end

local function start_of_command(line)
  if vim.fn.mode() == "c" then
    local run = line:match("^%s*Run%s+()")
    return run and run - 1
  end
  return 0
end

function source.enabled()
  if vim.fn.mode() == "c" then
    return vim.fn.getcmdtype() == ":" and start_of_command(vim.fn.getcmdline()) ~= nil
  end
  return vim.bo.filetype == "moni-run"
end

function source.get_trigger_characters()
  return { " ", "/", "-" }
end

function source.get_completions(_, ctx, callback)
  local offset = start_of_command(ctx.line)
  local before = ctx.line:sub(offset + 1, ctx.cursor[2])
  local token = #before - #(before:match("%S*$"))
  local root = require("moni.run").root()
  local row = ctx.cursor[1] - 1
  local items = {}
  for _, word in ipairs(require("moni.shellcomplete").words(before, root)) do
    items[#items + 1] = {
      label = word,
      kind = kinds[require("moni.shellcomplete").kind(before, word, root)],
      textEdit = {
        newText = word,
        range = {
          start = { line = row, character = offset + token },
          ["end"] = { line = row, character = ctx.cursor[2] },
        },
      },
    }
  end
  callback({ items = items, is_incomplete_forward = true, is_incomplete_backward = true })
  return function() end
end

return source
