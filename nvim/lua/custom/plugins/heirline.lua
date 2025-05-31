return {
  'rebelot/heirline.nvim',

  init = function()
    local status_ok, heirline = pcall(require, 'heirline')
    if not status_ok then
      return
    end

    local conditions = require 'heirline.conditions'
    local utils = require 'heirline.utils'

    local function setup_colors()
      return {
        bright_bg = utils.get_highlight('Folded').bg,
        red = utils.get_highlight('DiagnosticError').fg,
        gray = utils.get_highlight('NonText').fg,
        lightblack = utils.get_highlight('Cursorline').bg,
        string = utils.get_highlight('String').fg,
        orange = utils.get_highlight('Keyword').fg,
        purple = utils.get_highlight('Constant').fg,
        blue = utils.get_highlight('Function').fg,
        white = utils.get_highlight('StatusLine').fg,
        black = utils.get_highlight('Statusline').bg,

        diag_warn = utils.get_highlight('DiagnosticWarn').fg,
        diag_error = utils.get_highlight('DiagnosticError').fg,
        diag_hint = utils.get_highlight('DiagnosticHint').fg,
        diag_info = utils.get_highlight('DiagnosticInfo').fg,
        git_del = utils.get_highlight('diffDelete').fg,
        git_add = utils.get_highlight('diffAdd').fg,
        git_change = utils.get_highlight('diffChange').fg,
      }
    end

    heirline.load_colors(setup_colors())

    local mode = {
      -- get vim current mode, this information will be required by the provider
      -- and the highlight functions, so we compute it only once per component
      -- evaluation and store it as a component attribute
      init = function(self)
        self.mode = vim.fn.mode(1) -- :h mode()

        -- execute this only once, this is required if you want the ViMode
        -- component to be updated on operator pending mode
        if not self.once then
          vim.api.nvim_create_autocmd('ModeChanged', { command = 'redrawstatus' })
          self.once = true
        end
      end,
      -- Now we define some dictionaries to map the output of mode() to the
      -- corresponding string and color. We can put these into `static` to compute
      -- them at initialisation time.
      static = {
        mode_names = { -- change the strings if you like it vvvvverbose!
          n = 'NOR',
          no = 'NOR',
          nov = 'NOr',
          noV = 'NOv',
          ['no\22'] = 'NO?',
          niI = 'NOi',
          niR = 'Nir',
          niV = 'Niv',
          nt = 'NOt',
          v = 'VIS',
          vs = 'VIs',
          V = 'VIl',
          Vs = 'Vls',
          ['\22'] = 'VIb',
          ['\22s'] = 'VIb',
          s = 'SEL',
          S = 'SEl',
          ['\19'] = 'SEb',
          i = 'INS',
          ic = 'Ic',
          ix = 'Ix',
          R = 'REP',
          Rc = 'REc',
          Rx = 'REx',
          Rv = 'REv',
          Rvc = 'Rvc',
          Rvx = 'Rvx',
          c = 'CMD',
          cv = 'Ex ',
          r = '...',
          rm = 'M ',
          ['r?'] = '? ',
          ['!'] = '! ',
          t = 'TER',
        },
        mode_colors = {
          n = 'grey',
          i = 'blue',
          v = 'orange',
          V = 'orange',
          ['\22'] = 'orange',
          c = 'string',
          s = 'purple',
          S = 'purple',
          ['\19'] = 'purple',
          R = 'blue',
          r = 'blue',
          ['!'] = 'red',
          t = 'red',
        },
      },
      -- We can now access the value of mode() that, by now, would have been
      -- computed by `init()` and use it to index our strings dictionary.
      -- note how `static` fields become just regular attributes once the
      -- component is instantiated.
      -- To be extra meticulous, we can also add some vim statusline syntax to
      -- control the padding and make sure our string is always at least 2
      -- characters long. Plus a nice Icon.
      provider = function(self)
        return ' %2(' .. self.mode_names[self.mode] .. '%) '
        -- return " "
      end,
      -- Same goes for the highlight. Now the foreground will change according to the current mode.
      hl = function(self)
        local mode = self.mode:sub(1, 1) -- get only the first mode character
        return { bg = self.mode_colors[mode], fg = 'black', bold = false }
        --     return { bg = "black", bold = false }
      end,
      -- Re-evaluate the component only on ModeChanged event!
      -- This is not required in any way, but it's there, and it's a small
      -- performance improvement.
      update = 'ModeChanged',
    }

    local file = (function()
      local name_block = {
        -- let's first set up some attributes needed by this component and it's children
        init = function(self)
          self.filename = vim.api.nvim_buf_get_name(0)
        end,
      }
      -- We can now define some children separately and add them later

      local icon = {
        provider = function(self)
          return self.icon and (self.icon .. ' ')
        end,
        hl = function(self)
          return { fg = self.icon_color }
        end,
      }

      local name = {
        init = function(self)
          self.lfilename = vim.fn.fnamemodify(self.filename, ':.')
          if self.lfilename == '' then
            self.lfilename = '[No Name]'
          end
        end,
        hl = { fg = 'white' },

        {
          flexible = 6,
          {
            provider = function(self)
              return self.lfilename
            end,
          },
          {
            provider = function(self)
              return vim.fn.pathshorten(self.lfilename)
            end,
          },
        },
      }

      local flags = {
        {
          provider = function()
            if vim.bo.modified then
              return ' [+]'
            end
          end,
          -- provider = function() if vim.bo.modified then return "  " end end,
          hl = { fg = 'orange' },
        },
        {
          provider = function()
            if (not vim.bo.modifiable) or vim.bo.readonly then
              return '[-]'
            end
          end,
          -- provider = function() if (not vim.bo.modifiable) or vim.bo.readonly then return "  " end end,
          hl = { fg = 'red' },
        },
      }

      -- Now, let's say that we want the filename color to change if the buffer is
      -- modified. Of course, we could do that directly using the FileName.hl field,
      -- but we'll see how easy it is to alter existing components using a "modifier"
      -- component

      local name_modifier = {
        hl = function()
          if vim.bo.modified then
            -- use `force` because we need to override the child's hl foreground
            -- return { fg = "orange", bold = false, force=true }
            return { fg = 'white', force = true }
          end
        end,
      }

      -- let's add the children to our FileNameBlock component
      name_block = utils.insert(
        name_block,
        icon,
        utils.insert(name_modifier, name), -- a new table where FileName is a child of FileNameModifier
        unpack(flags), -- A small optimisation, since their parent does nothing
        { provider = '%<' } -- this means that the statusline is cut here when there's not enough space
      )

      local type = {
        provider = function()
          return string.upper(vim.bo.filetype)
        end,
        hl = { fg = utils.get_highlight('Type').fg, bold = true },
      }

      local size = {
        provider = function()
          -- stackoverflow, compute human readable file size
          local suffix = { 'b', 'k', 'M', 'G', 'T', 'P', 'E' }
          local fsize = vim.fn.getfsize(vim.api.nvim_buf_get_name(0))
          fsize = (fsize < 0 and 0) or fsize
          if fsize < 1024 then
            return fsize .. suffix[1]
          end
          local i = math.floor((math.log(fsize) / math.log(1024)))
          return string.format('%.2g%s', fsize / math.pow(1024, i), suffix[i + 1])
        end,
      }

      local last_modified = {
        -- did you know? Vim is full of functions!
        provider = function()
          local ftime = vim.fn.getftime(vim.api.nvim_buf_get_name(0))
          return (ftime > 0) and os.date('%c', ftime)
        end,
      }

      local ruler = {
        -- %l = current line number
        -- %L = number of lines in the buffer
        -- %c = column number
        -- %P = percentage through file of displayed window
        -- provider = "%10(%2cc %P %3Ll%)",
        provider = '%5(%l:%c%)',
      }

      local meta = {
        {
          provider = '%Ll ',
        },
        size,
      }

      return {
        name_block = name_block,
        type = type,
        size = size,
        last_modified = last_modified,
        ruler = ruler,
        meta = meta,
      }
    end)()

    local git = (function()
      local git = {
        condition = conditions.is_git_repo,

        init = function(self)
          self.status_dict = vim.b.gitsigns_status_dict
          self.has_changes = self.status_dict.added ~= 0 or self.status_dict.removed ~= 0 or self.status_dict.changed ~= 0
        end,

        hl = { fg = 'orange' },
        -- hl = { fg = "white" },

        { -- git branch name
          provider = function(self)
            return self.status_dict.head
          end,
          hl = { bold = false },
        },
        -- You could handle delimiters, icons and counts similar to Diagnostics
        {
          condition = function(self)
            return self.has_changes
          end,
          provider = '(',
          hl = { fg = 'grey' },
        },
        {
          provider = function(self)
            local count = self.status_dict.added or 0
            return count > 0 and ('+' .. count)
          end,
          hl = { fg = 'grey' },
        },
        {
          provider = function(self)
            local count = self.status_dict.removed or 0
            return count > 0 and ('-' .. count)
          end,
          hl = { fg = 'grey' },
        },
        {
          provider = function(self)
            local count = self.status_dict.changed or 0
            return count > 0 and ('~' .. count)
          end,
          hl = { fg = 'grey' },
        },
        {
          condition = function(self)
            return self.has_changes
          end,
          provider = ')',
          hl = { fg = 'grey' },
        },
        {
          provider = ' ',
        },
      }
      return git
    end)()

    local lsp = (function()
      local server_name = {
        condition = conditions.lsp_attached,
        update = { 'LspAttach', 'LspDetach' },

        provider = function()
          local names = {}
          for _, server in pairs(vim.lsp.get_clients()) do
            table.insert(names, server.name)
          end
          return '[' .. table.concat(names, ' ') .. '] '
        end,
        -- hl = { fg = "", bold = false },
      }

      -- local navic = {
      --     condition = require("nvim-navic").is_available,
      --     provider = require("nvim-navic").get_location,
      -- }

      local diagnostics = {

        condition = conditions.has_diagnostics,

        static = {
          -- error_icon = vim.fn.sign_getdefined("DiagnosticSignError")[1].text:gsub("%s+", ""),
          -- warn_icon = vim.fn.sign_getdefined("DiagnosticSignWarn")[1].text:gsub("%s+", ""),
          -- info_icon = vim.fn.sign_getdefined("DiagnosticSignInfo")[1].text:gsub("%s+", ""),
          -- hint_icon = vim.fn.sign_getdefined("DiagnosticSignHint")[1].text:gsub("%s+", ""),
        },

        init = function(self)
          self.errors = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
          self.warnings = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
          self.hints = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.HINT })
          self.info = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.INFO })
        end,

        -- update = { "LspRequest", "BufEnter" },
        {
          provider = '●',
        },
        {
          provider = '[',
        },
        {
          provider = function(self)
            if self.errors > 0 then
              return ' ' .. self.errors
            end
          end,
          hl = { fg = 'diag_error' },
        },
        {
          provider = function(self)
            if self.warnings > 0 then
              return ' ' .. self.warnings
            end
          end,
          hl = { fg = 'diag_warn' },
        },
        {
          provider = function(self)
            if self.info > 0 then
              return ' ' .. self.info
            end
          end,
          hl = { fg = 'diag_info' },
        },
        {
          provider = function(self)
            if self.hints > 0 then
              return ' ' .. self.hints
            end
          end,
          hl = { fg = 'diag_hint' },
        },
        {
          provider = ' ]',
        },
      }

      return {
        server_name = server_name,
        -- navic = navic,
        diagnostics = diagnostics,
      }
    end)()

    local align = { provider = '%=' }
    local space = { provider = ' ' }

    file.meta = utils.surround({ '(', ')' }, nil, file.meta)

    git = { flexible = 3, git, { provider = '' } }
    lsp.server_name = { flexible = 4, lsp.server_name, { provider = '' } }
    file.meta = { flexible = 5, file.meta, { provider = '' } }
    file.ruler = { flexible = 7, file.ruler, { provider = '' } }
    mode = { flexible = 8, mode, { provider = '' } }

    local default_statusline = {
      condition = function()
        return conditions.is_active()
      end,

      mode,
      space,
      file.name_block,
      space,
      space,
      lsp.diagnostics,
      lsp.server_name --[[ lsp.navic, (waiting for winbar) ]],
      align,
      file.ruler,
      space,
      space,
      git,
      file.meta,
      space,
    }

    local inactive_statusline = {
      condition = function()
        return not conditions.is_active()
      end,

      mode,
      space,
      file.name_block,
      space,
      space,
      lsp.diagnostics,
      lsp.server_name --[[ lsp.navic, (waiting for winbar) ]],
      align,
      file.ruler,
      space,
      space,
      git,
      file.meta,
      space,
    }

    local file_explorer_statusline = {
      condition = function()
        return conditions.buffer_matches {
          filetype = { 'NvimTree', 'neo%-tree' },
        }
      end,

      hl = utils.get_highlight('Normal').bg,
      { provider = ' Files %=' },
    }

    local empty_statusline = {
      condition = function()
        return conditions.buffer_matches {
          buftype = { 'nofile' },
        }
      end,
      { provider = '' },
    }

    local terminal_statusline = {
      condition = function()
        return conditions.buffer_matches {
          buftype = { 'terminal' },
        }
      end,
      { provider = '' },
    }

    local statuslines

    if vim.opt.laststatus == 3 then
      statuslines = {
        hl = 'StatusLine',
        fallthrough = false,
        default_statusline,
      }
    else
      statuslines = {
        hl = function()
          if conditions.is_active() then
            return 'StatusLine'
          else
            return 'StatusLineNC'
          end
        end,

        fallthrough = false,

        file_explorer_statusline,
        terminal_statusline,
        empty_statusline,
        default_statusline,
        inactive_statusline,
      }
    end

    heirline.setup { statusline = statuslines }

    local group = vim.api.nvim_create_augroup('Heirline', { clear = true })
    vim.api.nvim_create_autocmd('ColorScheme', {
      callback = function()
        require('heirline').reset_highlights()
        require('heirline').load_colors(setup_colors())
      end,
      group = group,
    })
  end,
}
