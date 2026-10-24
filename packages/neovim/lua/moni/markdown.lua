if vim.env.TERM_PROGRAM == "rex" then
  vim.env.GHOSTTY_RESOURCES_DIR = vim.env.GHOSTTY_RESOURCES_DIR or "rex"
end
require("vellum").setup({ max_width = 100 })
vim.keymap.set("n", "<leader>mp", "<Cmd>Vellum<CR>", { desc = "markdown preview" })
vim.keymap.set("n", "<leader>mz", function()
  require("vellum").zoom()
end, { desc = "zoom image under cursor" })
