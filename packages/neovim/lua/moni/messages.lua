require("vim._core.ui2").enable({})

local messages = require("vim._core.ui2.messages")
local original = messages.msg_show

local prompts = { confirm = true, confirm_sub = true, return_prompt = true, wildlist = true }
local dropped = { search_cmd = true, search_count = true, empty = true }
local levels = {
  emsg = vim.log.levels.ERROR,
  echoerr = vim.log.levels.ERROR,
  lua_error = vim.log.levels.ERROR,
  rpc_error = vim.log.levels.ERROR,
  wmsg = vim.log.levels.WARN,
}

messages.msg_show = function(kind, content, ...)
  if prompts[kind] then
    return original(kind, content, ...)
  end
  if dropped[kind] then
    return
  end
  local text = {}
  for _, chunk in ipairs(content) do
    text[#text + 1] = chunk[2]
  end
  local message = vim.trim(table.concat(text):gsub("\r", ""))
  if message ~= "" then
    vim.schedule(function()
      vim.notify(message, levels[kind] or vim.log.levels.INFO)
    end)
  end
end
