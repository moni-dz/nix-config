(let [ui2 (require :vim._core.ui2)]
  (ui2.enable {}))

(local messages (require :vim._core.ui2.messages))
(local original messages.msg_show)

;; fnlfmt: skip
(local
  prompts
  {:confirm true
   :confirm_sub true
   :return_prompt true
   :wildlist true})

(local dropped {:search_cmd true :search_count true :empty true})

;; fnlfmt: skip
(local
  levels
  {:emsg vim.log.levels.ERROR
   :echoerr vim.log.levels.ERROR
   :lua_error vim.log.levels.ERROR
   :rpc_error vim.log.levels.ERROR
   :wmsg vim.log.levels.WARN})

(set messages.msg_show
     (λ [kind content ...]
       (if (. prompts kind)
           (original kind content ...)
           (not (. dropped kind))
           (let [text (icollect [_ chunk (ipairs content)]
                        (. chunk 2))
                 message (vim.trim (: (table.concat text) :gsub "\r" ""))
                 level (or (. levels kind) vim.log.levels.INFO)]
             (when (not= message "")
               (vim.schedule (partial vim.notify message level)))))))
