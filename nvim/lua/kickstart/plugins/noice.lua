return {
  {
    'folke/noice.nvim',
    event = 'VeryLazy',
    opts = {},

    dependencies = {
      'MunifTanjim/nui.nvim',
      'folke/snacks.nvim',
    },

    config = function()
      require('noice').setup {
        cmdline = {
          format = {
            cmdline = { icon = '→' },
            search_up = { icon = 'S↑' },
            search_down = { icon = 'S↓' },
            filter = { icon = '≈' },
            lua = { icon = 'λ' },
            help = { icon = '?' },
          },
        },

        views = {
          cmdline_popup = {
            border = {
              style = 'none',
              padding = { 1, 2 },
            },
            filter_options = {},
            win_options = {
              winhighlight = 'NormalFloat:NormalFloat,FloatBorder:FloatBorder',
            },
          },
        },

        messages = { enabled = false },

        format = {
          level = {
            icons = {
              error = '‼',
              warn = '●',
              info = '○',
            },
          },
        },

        lsp = {
          override = {
            ['vim.lsp.util.convert_input_to_markdown_lines'] = true,
            ['vim.lsp.util.stylize_markdown'] = true,
            ['cmp.entry.get_documentation'] = true,
          },
        },

        presets = {
          bottom_search = false, -- use a classic bottom cmdline for search
          command_palette = true, -- position the cmdline and popupmenu together
          long_message_to_split = true, -- long messages will be sent to a split
          inc_rename = false, -- enables an input dialog for inc-rename.nvim
          lsp_doc_border = true, -- add a border to hover docs and signature help
        },

        popupmenu = {
          kind_icons = false,
        },
      }
    end,
  },
}
