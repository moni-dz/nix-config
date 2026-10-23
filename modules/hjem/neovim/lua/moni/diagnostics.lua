local severity = vim.diagnostic.severity

vim.diagnostic.config({
  signs = {
    text = {
      [severity.ERROR] = "■",
      [severity.WARN] = "▲",
      [severity.INFO] = "●",
      [severity.HINT] = "•",
    },
  },
  virtual_text = { prefix = "■", spacing = 2, current_line = false },
  virtual_lines = { current_line = true },
  severity_sort = true,
  float = { border = "rounded", source = true, header = "" },
})

vim.keymap.set("n", "<leader>e", vim.diagnostic.open_float, { desc = "diagnostics at cursor" })
vim.keymap.set("n", "]e", function()
  vim.diagnostic.jump({ count = 1, severity = severity.ERROR })
end, { desc = "next error" })
vim.keymap.set("n", "[e", function()
  vim.diagnostic.jump({ count = -1, severity = severity.ERROR })
end, { desc = "previous error" })
vim.keymap.set({ "n", "x" }, "<leader>a", vim.lsp.buf.code_action, { desc = "code action" })
vim.keymap.set("n", "<leader>xq", function()
  vim.diagnostic.setqflist({ open = true })
end, { desc = "all diagnostics to quickfix" })
vim.keymap.set("n", "<leader>xl", function()
  vim.diagnostic.setloclist({ open = true })
end, { desc = "this buffer's diagnostics to the location list" })
vim.keymap.set("n", "<leader>xx", function()
  vim.diagnostic.enable(not vim.diagnostic.is_enabled())
end, { desc = "toggle diagnostics" })
