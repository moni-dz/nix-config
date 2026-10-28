(import-macros {: lazy} :moni.macros)

(local kinds (. (require :blink.cmp.types) :CompletionItemKind))

(each [_ name (ipairs [:Command :Subcommand :Option :Path])]
  (when (not (vim.tbl_contains kinds name))
    (tset kinds (+ (length kinds) 1) name)
    (tset kinds name (length kinds))))

(local source {})

(λ source.new []
  (setmetatable {} {:__index source}))

(λ start-of-command [line]
  (if (= (vim.fn.mode) :c)
      (let [run (line:match "^%s*Run%s+()")] (and run (- run 1)))
      0))

;; fnlfmt: skip
(λ source.enabled []
  (if (= (vim.fn.mode) :c)
      (and (= (vim.fn.getcmdtype) ":") (not= (start-of-command (vim.fn.getcmdline)) nil))
      (= vim.bo.filetype :moni-run)))

(λ source.get_trigger_characters []
  [" " "/" "-"])

;; fnlfmt: skip
(λ source.get_completions [_self ctx callback]
  (let [shellcomplete (require :moni.shellcomplete)
        offset (start-of-command ctx.line)
        col (. ctx.cursor 2)
        before (ctx.line:sub (+ offset 1) col)
        token (- (length before) (length (before:match "%S*$")))
        root (lazy :moni.run :root)
        row (- (. ctx.cursor 1) 1)
        items []]
    (each [_ word (ipairs (shellcomplete.words before root))]
      (tset
        items
        (+ (length items) 1)
        {:label word
         :kind (. kinds (shellcomplete.kind before word root))
         :textEdit
           {:newText word
            :range
              {:start {:line row :character (+ offset token)}
               :end {:line row :character col}}}}))

    (callback {: items :is_incomplete_forward true :is_incomplete_backward true})
    (λ [] nil)))

source
