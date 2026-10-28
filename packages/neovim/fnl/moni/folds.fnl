(import-macros {: set! : map!} :laurel.macros)

(set! :foldcolumn :0)
(set! :foldlevel 99)
(set! :foldlevelstart 99)
(set! :foldenable true)
(set! :fillchars "fold: ,foldsep: ,eob: ")

(λ _G.fold_click []
  (let [pos (vim.fn.getmousepos)
        cmd (if (= (vim.fn.foldclosed pos.line) -1) :foldclose :foldopen)]
    (vim.api.nvim_win_call pos.winid #(pcall vim.cmd (.. pos.line cmd)))))

(λ _G.fold_mark []
  (let [lnum vim.v.lnum]
    (if (not= vim.v.virtnum 0)
        " "
        (= (vim.fn.foldclosed lnum) lnum)
        "%@v:lua.fold_click@%#FoldMark#►%T"
        (< (vim.fn.foldlevel (- lnum 1)) (vim.fn.foldlevel lnum))
        "%@v:lua.fold_click@%#FoldMark#▼%T"
        " ")))

(set! :statuscolumn "%s%=%l %{%v:lua.fold_mark()%} ")

(let [ufo (require :ufo)]
  (ufo.setup {:provider_selector (λ [] [:treesitter :indent])})
  (map! :n :zR [:desc "open all folds"] ufo.openAllFolds)
  (map! :n :zM [:desc "close all folds"] ufo.closeAllFolds))
