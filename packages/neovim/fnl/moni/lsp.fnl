(import-macros {: map!} :laurel.macros)

(local {: format!} (require :moni.format))

;; fnlfmt: skip
(vim.lsp.config :nixd
  {:cmd [:nixd]
   :filetypes [:nix]
   :root_markers [:flake.nix :.git]
   :settings {:nixd {:formatting {:command [:nixfmt]}}}})

;; fnlfmt: skip
(vim.lsp.config :tinymist
  {:cmd [:tinymist]
   :filetypes [:typst]
   :root_markers [:.git]})

;; fnlfmt: skip
(vim.lsp.config :clangd
  {:cmd [:clangd :--background-index :--clang-tidy]
   :filetypes [:c :cpp :objc :objcpp]
   :root_markers [:compile_commands.json :.clangd :.git]})

;; fnlfmt: skip
(vim.lsp.config :rust_analyzer
  {:cmd [:rust-analyzer]
   :filetypes [:rust]
   :root_markers [:Cargo.toml :rust-project.json :.git]})

;; fnlfmt: skip
(vim.lsp.config :emmylua_ls
  {:cmd [:emmylua_ls]
   :filetypes [:lua]
   :root_markers [:.luarc.json :.luarc.jsonc :.stylua.toml :.git]
   :settings
     {:Lua
        {:runtime {:version :LuaJIT}
         :diagnostics {:globals [:vim :Snacks]}
         :workspace {:checkThirdParty false :library [vim.env.VIMRUNTIME]}
         :telemetry {:enable false}}}})

;; fennel-ls only sees macros under its project root: link nvim-laurel's into it
(λ uses-laurel? [root]
  (let [path (vim.fs.joinpath root :flsproject.fnl)]
    (and (vim.uv.fs_stat path)
         (: (table.concat (vim.fn.readfile path) "\n") :find :.laurel 1 true))))

(λ link-laurel! [?root]
  (let [[macro-file] (vim.api.nvim_get_runtime_file :fnl/laurel/macros.fnl
                                                    false)
        link (vim.fs.joinpath (or ?root "") :.laurel)
        existing (vim.uv.fs_lstat link)]
    ;; never replace anything that isn't our link
    (when (and macro-file ?root (uses-laurel? ?root)
               (or (= existing nil) (= existing.type :link)))
      (let [target (vim.fs.dirname (vim.fs.dirname macro-file))]
        (when (not= (vim.uv.fs_readlink link) target)
          (vim.uv.fs_unlink link)
          (vim.uv.fs_symlink target link))))))

(vim.lsp.config :fennel_ls
                {:cmd [:fennel-ls]
                 :filetypes [:fennel]
                 :root_markers [:flsproject.fnl :.git]
                 ;; linking must not block startup
                 :before_init (λ [_params config]
                                (pcall link-laurel! config.root_dir))})

(vim.lsp.config :taplo
                {:cmd [:taplo :lsp :stdio]
                 :filetypes [:toml]
                 :root_markers [:.taplo.toml :taplo.toml :.git]
                 :before_init (λ [params]
                                (set params.workspaceFolders nil))})

;; fnlfmt: skip
(vim.lsp.config :jsonls
  {:cmd [:vscode-json-language-server :--stdio]
   :filetypes [:json :jsonc]
   :root_markers [:.git]})

;; fnlfmt: skip
(vim.lsp.config :bashls
  {:cmd [:bash-language-server :start]
   :filetypes [:sh :bash]
   :root_markers [:.git]})

;; fnlfmt: skip
(vim.lsp.config :svelte
  {:cmd [:svelteserver :--stdio]
   :filetypes [:svelte]
   :root_markers [:svelte.config.js :svelte.config.ts :package.json :.git]})

(vim.lsp.enable [:nixd
                 :tinymist
                 :clangd
                 :rust_analyzer
                 :emmylua_ls
                 :fennel_ls
                 :taplo
                 :jsonls
                 :bashls
                 :svelte])

(map! :n :<leader>f [:desc :format] format!)
(map! :n :<leader>H [:desc "toggle inlay hints"]
      (λ []
        (let [enabled (vim.lsp.inlay_hint.is_enabled {:bufnr 0})]
          (vim.lsp.inlay_hint.enable (not enabled) {:bufnr 0}))))

(map! :n :gd [:desc "go to definition"] vim.lsp.buf.definition)
(map! :n :gy [:desc "go to type definition"] vim.lsp.buf.type_definition)
