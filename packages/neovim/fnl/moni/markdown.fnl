(import-macros {: map!} :laurel.macros)

(when (= vim.env.TERM_PROGRAM :rex)
  (set vim.env.GHOSTTY_RESOURCES_DIR (or vim.env.GHOSTTY_RESOURCES_DIR :rex)))

(local vellum (require :vellum))
(vellum.setup {:max_width 100})

(map! :n :<leader>mp [:desc "markdown preview"] :<Cmd>Vellum<CR>)
(map! :n :<leader>mz [:desc "zoom image under cursor"] vellum.zoom)
