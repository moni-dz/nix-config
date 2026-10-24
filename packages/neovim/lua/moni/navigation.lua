require("smart-splits-backend-rex").setup({})
require("smart-splits").setup({ mux = { backend = "smart-splits-backend-rex" } })
local ss = require("smart-splits")
vim.keymap.set("n", "<C-h>", ss.move_cursor_left, { desc = "focus left" })
vim.keymap.set("n", "<C-j>", ss.move_cursor_down, { desc = "focus down" })
vim.keymap.set("n", "<C-k>", ss.move_cursor_up, { desc = "focus up" })
vim.keymap.set("n", "<C-l>", ss.move_cursor_right, { desc = "focus right" })
vim.keymap.set("n", "<A-h>", ss.resize_left, { desc = "resize left" })
vim.keymap.set("n", "<A-j>", ss.resize_down, { desc = "resize down" })
vim.keymap.set("n", "<A-k>", ss.resize_up, { desc = "resize up" })
vim.keymap.set("n", "<A-l>", ss.resize_right, { desc = "resize right" })

require("pulse").setup({
  navigators = { files = { icons = true } },
})
vim.keymap.set("n", "<leader>ff", "<Cmd>Pulse files<CR>", { desc = "find file" })
vim.keymap.set("n", "<leader>fg", "<Cmd>Pulse live_grep<CR>", { desc = "grep" })
vim.keymap.set("n", "<leader>fb", "<Cmd>Pulse buffers<CR>", { desc = "buffers" })
vim.keymap.set("n", "<leader>fs", "<Cmd>Pulse symbols<CR>", { desc = "symbols" })
vim.keymap.set("n", "<leader>fd", "<Cmd>Pulse diagnostics<CR>", { desc = "diagnostics" })
vim.keymap.set("n", "<leader>p", "<Cmd>Pulse<CR>", { desc = "pulse" })

require("nvim-tree").setup({
  hijack_cursor = true,
  view = { width = 30 },
  diagnostics = {
    enable = true,
    show_on_dirs = true,
    icons = { hint = "•", info = "●", warning = "▲", error = "■" },
  },
  renderer = {
    icons = {
      show = {
        file = false,
        folder = false,
        folder_arrow = true,
        git = false,
        modified = true,
        diagnostics = true,
        bookmarks = false,
      },
      glyphs = {
        modified = "●",
        folder = { arrow_closed = "►", arrow_open = "▼" },
      },
    },
  },
  filters = { dotfiles = false, git_ignored = true },
  on_attach = function(buf)
    local api = require("nvim-tree.api")
    api.map.on_attach.default(buf)
    local map = function(lhs, rhs, desc)
      vim.keymap.set("n", lhs, rhs, { buffer = buf, nowait = true, desc = "tree: " .. desc })
    end
    map("<LeftRelease>", api.node.open.edit, "open or toggle")
  end,
})

vim.api.nvim_create_autocmd("QuitPre", {
  callback = function()
    if vim.bo.filetype == "NvimTree" or vim.bo.modified then
      return
    end
    local tree, others = {}, 0
    for _, win in ipairs(vim.api.nvim_list_wins()) do
      if vim.bo[vim.api.nvim_win_get_buf(win)].filetype == "NvimTree" then
        table.insert(tree, win)
      elseif vim.api.nvim_win_get_config(win).relative == "" then
        others = others + 1
      end
    end
    if others == 1 then
      for _, win in ipairs(tree) do
        vim.api.nvim_win_close(win, true)
      end
    end
  end,
})
vim.keymap.set("n", "<leader>t", "<Cmd>NvimTreeToggle<CR>", { desc = "file tree" })
vim.keymap.set("n", "<leader>T", "<Cmd>NvimTreeFindFile<CR>", { desc = "reveal current file in tree" })
