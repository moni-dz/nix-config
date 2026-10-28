(import-macros {: map!} :laurel.macros)
(import-macros {: setup! : on!} :moni.macros)

(setup! :smart-splits-backend-rex {})

;; fnlfmt: skip
(setup! :smart-splits
  {:mux {:backend :smart-splits-backend-rex}})

(local ss (require :smart-splits))
(map! :n :<C-h> [:desc "focus left"] ss.move_cursor_left)
(map! :n :<C-j> [:desc "focus down"] ss.move_cursor_down)
(map! :n :<C-k> [:desc "focus up"] ss.move_cursor_up)
(map! :n :<C-l> [:desc "focus right"] ss.move_cursor_right)
(map! :n :<A-h> [:desc "resize left"] ss.resize_left)
(map! :n :<A-j> [:desc "resize down"] ss.resize_down)
(map! :n :<A-k> [:desc "resize up"] ss.resize_up)
(map! :n :<A-l> [:desc "resize right"] ss.resize_right)

;; fnlfmt: skip
(setup! :pulse
  {:navigators {:files {:icons true}}})

(map! :n :<leader>ff [:desc "find file"] "<Cmd>Pulse files<CR>")
(map! :n :<leader>fg [:desc :grep] "<Cmd>Pulse live_grep<CR>")
(map! :n :<leader>fb [:desc :buffers] "<Cmd>Pulse buffers<CR>")
(map! :n :<leader>fs [:desc :symbols] "<Cmd>Pulse symbols<CR>")
(map! :n :<leader>fd [:desc :diagnostics] "<Cmd>Pulse diagnostics<CR>")
(map! :n :<leader>p [:desc :pulse] :<Cmd>Pulse<CR>)

(λ on-tree-attach [buf]
  (let [api (require :nvim-tree.api)]
    (api.map.on_attach.default buf)
    (vim.keymap.set :n :<LeftRelease> api.node.open.edit
                    {:buffer buf :nowait true :desc "tree: open or toggle"})))

;; fnlfmt: skip
(setup! :nvim-tree
  {:hijack_cursor true
   :view {:width 30}
   :diagnostics
     {:enable true
      :show_on_dirs true
      :icons {:hint "•" :info "●" :warning "▲" :error "■"}}

   :renderer
     {:icons
        {:show
           {:file false
            :folder false
            :folder_arrow true
            :git false
            :modified true
            :diagnostics true
            :bookmarks false}
         :glyphs {:modified "●" :folder {:arrow_closed "►" :arrow_open "▼"}}}}

   :filters {:dotfiles false :git_ignored true}
   :on_attach on-tree-attach})

(on! :QuitPre [] (when (not (or (= vim.bo.filetype :NvimTree) vim.bo.modified))
                   (let [tree []]
                     (var others 0)
                     (each [_ win (ipairs (vim.api.nvim_list_wins))]
                       (if (= (. vim.bo (vim.api.nvim_win_get_buf win)
                                 :filetype)
                              :NvimTree)
                           (table.insert tree win)
                           (= (. (vim.api.nvim_win_get_config win) :relative)
                              "")
                           (set others (+ others 1))))
                     (when (= others 1)
                       (each [_ win (ipairs tree)]
                         (vim.api.nvim_win_close win true))))))

(map! :n :<leader>t [:desc "file tree"] :<Cmd>NvimTreeToggle<CR>)
(map! :n :<leader>T [:desc "reveal current file in tree"]
      :<Cmd>NvimTreeFindFile<CR>)
