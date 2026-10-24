local M = {}

local function fish_words(line, root)
  if vim.fn.executable("fish") ~= 1 then
    return nil
  end
  local res = vim.system({ "fish", "-c", "complete -C $argv", line }, { text = true, cwd = root }):wait(1500)
  if res.code ~= 0 or res.stdout == "" then
    return nil
  end
  local seen, words = {}, {}
  for candidate in res.stdout:gmatch("[^\n]+") do
    local word = candidate:match("^[^\t]*")
    if not seen[word] then
      seen[word] = true
      words[#words + 1] = word
    end
  end
  return words
end

function M.words(line, root)
  return fish_words(line, root) or vim.fn.getcompletion(line, "shellcmdline")
end

local function is_path(word, root)
  if word:find("/", 1, true) or word:sub(1, 1) == "~" then
    return true
  end
  return vim.uv.fs_stat(root .. "/" .. word) ~= nil
end

function M.kind(line, word, root)
  if not line:find("%s") then
    return "Command"
  elseif word:sub(1, 1) == "-" then
    return "Option"
  elseif is_path(word, root) then
    return "Path"
  end
  return "Subcommand"
end

return M
