return {
  'ramojus/mellifluous.nvim',
  priority = 1000,

  init = function()
    vim.cmd 'colorscheme mellifluous'
  end,

  opts = {
    highlight_overrides = {
      dark = function(hl, _)
        hl.set('DiagnosticUnderlineError', { undercurl = true, sp = hl.get('DiagnosticError').fg })
        hl.set('DiagnosticUnderlineWarn', { undercurl = true, sp = hl.get('DiagnosticWarn').fg })
        hl.set('DiagnosticUnderlineInfo', { undercurl = true, sp = hl.get('DiagnosticInfo').fg })
        hl.set('DiagnosticUnderlineHint', { undercurl = true, sp = hl.get('DiagnosticHint').fg })
        hl.set('DiagnosticUnnecessary', { undercurl = true })
      end,

      light = function(hl, _)
        hl.set('DiagnosticUnderlineError', { undercurl = true, sp = hl.get('DiagnosticError').fg })
        hl.set('DiagnosticUnderlineWarn', { undercurl = true, sp = hl.get('DiagnosticWarn').fg })
        hl.set('DiagnosticUnderlineInfo', { undercurl = true, sp = hl.get('DiagnosticInfo').fg })
        hl.set('DiagnosticUnderlineHint', { undercurl = true, sp = hl.get('DiagnosticHint').fg })
        hl.set('DiagnosticUnnecessary', { undercurl = true })
      end,
    },
    flat_background = {
      line_numbers = true,
      floating_windows = false,
      file_tree = false,
      cursor_line_number = false,
    },
  },
}
