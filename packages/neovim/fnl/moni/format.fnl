;; fnlfmt: skip
(λ fnlfmt [text]
  "`text` formatted by fnlfmt, or nil and why that failed."
  (if (= (vim.fn.executable :fnlfmt) 0)
      (values nil "fnlfmt is not on PATH")
      (let [result (: (vim.system [:fnlfmt "-"] {:stdin text :text true}) :wait 5000)]
        (if (= result.code 0)
            result.stdout
            (let [first-line (. (vim.split (vim.trim result.stderr) "\n") 1)
                  (message) (first-line:gsub "^.-/bin/lua: " "")]
              (values nil message))))))

(λ join [lines]
  (.. (table.concat lines "\n") "\n"))

(λ added-above [lines new-lines row]
  "How many lines the change added above line `row`."
  (let [hunks (vim.diff (join lines) (join new-lines) {:result_type :indices})]
    (accumulate [added 0 _ [start count-a _ count-b] (ipairs hunks)]
      (let [last-line (if (= count-a 0) start (+ start count-a -1))]
        (if (< last-line row) (+ added (- count-b count-a)) added)))))

(λ replace-lines! [lines new-lines]
  "Replace the current buffer's lines, keeping the cursor on the same code."
  (let [view (vim.fn.winsaveview)
        shift (added-above lines new-lines view.lnum)]
    (vim.api.nvim_buf_set_lines 0 0 -1 false new-lines)
    (set view.lnum (math.max 1 (+ view.lnum shift)))
    (set view.topline (math.max 1 (+ view.topline shift)))
    (vim.fn.winrestview view)))

(λ format-fennel! []
  (let [lines (vim.api.nvim_buf_get_lines 0 0 -1 false)
        (formatted err) (fnlfmt (join lines))]
    (if (not formatted)
        (vim.notify (.. "fnlfmt: " err) vim.log.levels.ERROR)
        (let [new-lines (vim.split (formatted:gsub "\n$" "") "\n" {:plain true})]
          (when (not (vim.deep_equal new-lines lines))
            (replace-lines! lines new-lines))))))

(λ format! []
  "Format the current buffer: fnlfmt for Fennel, else the language servers."
  (if (= vim.bo.filetype :fennel) (format-fennel!) (vim.lsp.buf.format)))

;; fnlfmt: skip
(vim.api.nvim_create_autocmd
  :FileType
  {:pattern :fennel
   :callback (λ [] (set vim.bo.formatprg "fnlfmt -") nil)})

{: format!}
