return {
  { -- Autoformat
    'stevearc/conform.nvim',
    event = { 'BufWritePre' },
    cmd = { 'ConformInfo' },
    keys = {
      {
        '<leader>f',
        function()
          require('conform').format { async = true, lsp_format = 'fallback' }
        end,
        mode = '',
        desc = '[F]ormat buffer',
      },
    },
    opts = {
      notify_on_error = false,
      format_on_save = function(bufnr)
        local disable_filetypes = { c = true, cpp = true, nix = true }

        if disable_filetypes[vim.bo[bufnr].filetype] then
          return
        end

        return {
          timeout_ms = 500,
          lsp_format = 'fallback',
        }
      end,

      format_after_save = function(bufnr)
        local disable_filetypes = { c = true, cpp = true, nix = true }

        if disable_filetypes[vim.bo[bufnr].filetype] then
          return
        end

        return { lsp_format = 'fallback' }
      end,

      formatters_by_ft = {
        lua = { 'stylua' },
        -- Conform can also run multiple formatters sequentially
        -- python = { "isort", "black" },
        --
        -- You can use 'stop_after_first' to run the first available formatter from the list
        -- javascript = { "prettierd", "prettier", stop_after_first = true },
        nix = { 'nixfmt' },
      },
    },
  },
}
-- vim: ts=2 sts=2 sw=2 et
