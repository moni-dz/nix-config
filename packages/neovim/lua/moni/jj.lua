require("jj").setup({})

local function jj(args, root, on_done)
  vim.system(vim.list_extend({ "jj" }, args), { text = true, cwd = root }, vim.schedule_wrap(on_done))
end

local function repo_root(buf)
  return vim.fs.root(buf or 0, ".jj")
end

local function relative(buf, root)
  return vim.fs.relpath(root, vim.api.nvim_buf_get_name(buf))
end

local function runner(cmd)
  local root = repo_root()
  if root then
    require("moni.run").run({ cmd }, root)
  end
end

local function notify_result(label)
  return function(res)
    local out = vim.trim((res.stdout or "") .. (res.stderr or ""))
    vim.notify(label .. (res.code == 0 and "" or " failed") .. (out ~= "" and ("\n" .. out) or ""),
      res.code == 0 and vim.log.levels.INFO or vim.log.levels.ERROR)
    vim.cmd.checktime()
  end
end

vim.keymap.set("n", "<leader>jl", "<Cmd>J log<CR>", { desc = "jj log" })
vim.keymap.set("n", "<leader>js", "<Cmd>J status<CR>", { desc = "jj status" })
vim.keymap.set("n", "<leader>jd", "<Cmd>J describe<CR>", { desc = "jj describe" })
vim.keymap.set("n", "<leader>jn", "<Cmd>J new<CR>", { desc = "jj new" })
vim.keymap.set("n", "<leader>jf", "<Cmd>J diff<CR>", { desc = "jj diff of this file" })

vim.keymap.set("n", "<leader>ju", function()
  local root = repo_root()
  if root then
    jj({ "undo" }, root, notify_result("jj undo"))
  end
end, { desc = "jj undo" })
vim.keymap.set("n", "<leader>jq", function()
  local root = repo_root()
  if root then
    jj({ "squash", "--use-destination-message" }, root, notify_result("squashed into the parent"))
  end
end, { desc = "squash this change into its parent" })
vim.keymap.set("n", "<leader>jQ", function()
  runner("jj squash -i")
end, { desc = "squash, choosing hunks" })
vim.keymap.set("n", "<leader>jS", function()
  runner("jj split")
end, { desc = "split this change, choosing hunks" })
vim.keymap.set("n", "<leader>jP", function()
  runner("jj git push")
end, { desc = "jj git push" })
vim.keymap.set("n", "<leader>jF", function()
  runner("jj git fetch")
end, { desc = "jj git fetch" })

local function pick_revision(revset, prompt, on_choice)
  local root = repo_root()
  if not root then
    return
  end
  local template = [[change_id.shortest(4) ++ "\t" ++ author.name() ++ "\t" ++ committer.timestamp().ago() ++ "\t" ++ description.first_line() ++ "\n"]]
  jj({ "log", "--no-graph", "-r", revset, "-T", template }, root, function(res)
    local items = {}
    for line in (res.stdout or ""):gmatch("[^\n]+") do
      local id, author, ago, desc = line:match("^(.-)\t(.-)\t(.-)\t(.*)$")
      if id and id ~= "" then
        items[#items + 1] = { id = id, author = author, ago = ago, desc = desc ~= "" and desc or "(no description)" }
      end
    end
    vim.ui.select(items, {
      prompt = prompt,
      format_item = function(i)
        return string.format("%s  %s  %s  %s", i.id, i.ago, i.author, i.desc)
      end,
    }, function(choice)
      if choice then
        on_choice(choice.id)
      end
    end)
  end)
end

local function diff_with(rev)
  local buf = vim.api.nvim_get_current_buf()
  local root = repo_root(buf)
  local rel = root and relative(buf, root)
  if not rel then
    vim.notify("this file is not in a jj repo", vim.log.levels.WARN)
    return
  end
  jj({ "file", "show", "-r", rev, rel }, root, function(res)
    if res.code ~= 0 then
      vim.notify(vim.trim(res.stderr or ""), vim.log.levels.WARN)
      return
    end
    local ft = vim.bo[buf].filetype
    vim.cmd("diffthis")
    vim.cmd("leftabove vnew")
    local old = vim.api.nvim_get_current_buf()
    local lines = vim.split(res.stdout, "\n", { plain = true })
    if lines[#lines] == "" then
      table.remove(lines)
    end
    vim.api.nvim_buf_set_lines(old, 0, -1, false, lines)
    vim.bo[old].buftype, vim.bo[old].bufhidden, vim.bo[old].swapfile = "nofile", "wipe", false
    vim.bo[old].filetype, vim.bo[old].modifiable = ft, false
    vim.api.nvim_buf_set_name(old, "jj:" .. rev .. ":" .. rel)
    vim.cmd("diffthis")
    vim.keymap.set("n", "q", "<Cmd>diffoff!<Bar>close<CR>", { buffer = old, desc = "close the comparison" })
  end)
end

vim.keymap.set("n", "<leader>jD", function()
  diff_with("@-")
end, { desc = "compare this file with the parent" })
vim.keymap.set("n", "<leader>jc", function()
  pick_revision("ancestors(@, 25)", "compare with: ", diff_with)
end, { desc = "compare this file with a revision" })
vim.keymap.set("n", "<leader>jh", function()
  local root = repo_root()
  local rel = root and relative(0, root)
  if rel then
    pick_revision(string.format('ancestors(@, 200) & files("%s")', rel), "history of this file: ", diff_with)
  end
end, { desc = "history of this file" })

local blame = { on = false, ns = vim.api.nvim_create_namespace("jj_blame"), lines = {} }
local annotate_template = [[commit.change_id().shortest(4) ++ "\t" ++ commit.author().name() ++ "\t" ++ commit.author().timestamp().ago() ++ "\t" ++ commit.description().first_line() ++ "\t" ++ line_number ++ "\n"]]

local function load_blame(buf)
  local root = repo_root(buf)
  local rel = root and relative(buf, root)
  if not (blame.on and rel and vim.bo[buf].buftype == "") then
    return
  end
  jj({ "file", "annotate", "-r", "@", "-T", annotate_template, rel }, root, function(res)
    if res.code ~= 0 then
      return
    end
    local lines = {}
    for line in (res.stdout or ""):gmatch("[^\n]+") do
      local id, author, ago, desc, n = line:match("^(.-)\t(.-)\t(.-)\t(.-)\t(%d+)$")
      if id then
        lines[tonumber(n)] = { id = id, text = table.concat({ id, author, ago, desc }, " · ") }
      end
    end
    blame.lines[buf] = lines
  end)
end

local function show_blame()
  local buf = vim.api.nvim_get_current_buf()
  vim.api.nvim_buf_clear_namespace(buf, blame.ns, 0, -1)
  local info = blame.on and not vim.bo[buf].modified and blame.lines[buf] and blame.lines[buf][vim.fn.line(".")]
  if info then
    vim.api.nvim_buf_set_extmark(buf, blame.ns, vim.fn.line(".") - 1, 0, {
      virt_text = { { "  " .. info.text, "JjBlame" } },
      virt_text_pos = "eol",
    })
  end
end

local blame_group = vim.api.nvim_create_augroup("jj_blame", {})
vim.keymap.set("n", "<leader>jb", function()
  blame.on = not blame.on
  vim.api.nvim_clear_autocmds({ group = blame_group })
  if blame.on then
    vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "FocusGained" }, {
      group = blame_group,
      callback = function(ev)
        load_blame(ev.buf)
      end,
    })
    vim.api.nvim_create_autocmd({ "CursorHold", "CursorMoved", "BufEnter", "TextChanged" }, {
      group = blame_group,
      callback = show_blame,
    })
    load_blame(vim.api.nvim_get_current_buf())
  else
    vim.api.nvim_buf_clear_namespace(0, blame.ns, 0, -1)
  end
  vim.notify("jj blame " .. (blame.on and "on" or "off"))
end, { desc = "toggle inline blame" })

vim.keymap.set("n", "<leader>jB", function()
  local info = blame.lines[vim.api.nvim_get_current_buf()] and blame.lines[vim.api.nvim_get_current_buf()][vim.fn.line(".")]
  if info then
    vim.cmd("J show " .. info.id)
  else
    vim.notify("turn blame on first (<leader>jb) and save the file", vim.log.levels.WARN)
  end
end, { desc = "show the change this line is from" })

local conflict_ns = vim.api.nvim_create_namespace("jj_conflict")
local function mark_conflicts(buf)
  vim.api.nvim_buf_clear_namespace(buf, conflict_ns, 0, -1)
  for i, line in ipairs(vim.api.nvim_buf_get_lines(buf, 0, -1, false)) do
    if line:match("^<<<<<<<") or line:match("^>>>>>>>") or line:match("^%%%%%%%%") or line:match("^%+%+%+%+%+%+%+") then
      vim.api.nvim_buf_set_extmark(buf, conflict_ns, i - 1, 0, { line_hl_group = "JjConflict" })
    end
  end
end
vim.api.nvim_create_autocmd({ "BufReadPost", "BufWritePost", "TextChanged" }, {
  callback = function(ev)
    if vim.bo[ev.buf].buftype == "" then
      mark_conflicts(ev.buf)
    end
  end,
})
