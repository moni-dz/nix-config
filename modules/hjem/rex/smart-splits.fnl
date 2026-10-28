(local directions {:h :left :j :down :k :up :l :right})

(λ vim-name? [name]
  (not= (name:match :^n?vim) nil))

;; fnlfmt: skip
(λ vim-pane? [ctx]
  (let [process (rex.block.call :com.superlogical.terminal :process {:block_id ctx.block_id})
        foreground (and process process.foreground)]
    (if foreground
        (or (vim-name? (or foreground.name ""))
            (vim-name? (: (or foreground.argv0 "") :match "[^/]*$")))
        false)))

;; fnlfmt: skip
(each [key direction (pairs directions)]
  (rex.bind (.. :ctrl+ key) (λ [ctx]
                              (if (vim-pane? ctx)
                                  false
                                  (do (rex.session.focus_direction {: direction}) true)))))

;; fnlfmt: skip
(rex.action {:name :smart_splits_resize
             :title "Smart Splits: Resize Pane"
             :run (λ [ctx args]
                    (if (vim-pane? ctx)
                        (rex.client.queue :pane.send_key {:key args.key})
                        (rex.client.queue :pane.resize {:direction args.direction :amount 3})))})

;; fnlfmt: skip
(each [key direction (pairs directions)]
  (rex.bind (.. :alt+ key) 
            :smart_splits_resize
            {:key (.. :alt+ key) : direction}))
