vim.api.nvim_create_autocmd("FileType", {
  callback = function(ev)
    pcall(vim.treesitter.start, ev.buf)
  end,
})

local flash_ns = vim.api.nvim_create_namespace("flash")
local function flash_changed(buf)
  local last = vim.api.nvim_buf_line_count(buf)
  local first = math.min(vim.api.nvim_buf_get_mark(buf, "[")[1], last)
  local final = math.min(vim.api.nvim_buf_get_mark(buf, "]")[1], last)
  if first < 1 then
    return
  end
  vim.hl.range(buf, flash_ns, "IncSearch", { first - 1, 0 }, { final - 1, -1 }, {
    regtype = "V",
    timeout = 150,
  })
end

vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.hl.hl_op({ higroup = "IncSearch", timeout = 150 })
  end,
})

for key, cmd in pairs({ u = "u", ["<C-r>"] = "\18" }) do
  vim.keymap.set("n", key, function()
    vim.cmd.normal({ vim.v.count1 .. cmd, bang = true })
    flash_changed(0)
  end, { desc = key == "u" and "undo, flashed" or "redo, flashed" })
end

vim.cmd.packadd("nvim.undotree")
vim.cmd.packadd("nohlsearch")
vim.keymap.set("n", "<leader>u", "<Cmd>Undotree<CR>", { desc = "undo history" })

require("mini.pairs").setup()
require("mini.surround").setup({
  mappings = {
    add = "gza",
    delete = "gzd",
    find = "gzf",
    find_left = "gzF",
    highlight = "gzh",
    replace = "gzr",
    update_n_lines = "gzn",
  },
})

local hipatterns = require("mini.hipatterns")
hipatterns.setup({
  highlighters = {
    hex_color = hipatterns.gen_highlighter.hex_color({ style = "inline", inline_text = "■" }),
  },
})
