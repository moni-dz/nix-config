(local fennel (require :fennel))

(λ walk [node file problems]
  (if (fennel.sym? node)
      nil
      (fennel.list? node)
      (do
        (when (= (tostring (. node 1)) :fn)
          (table.insert problems (string.format "%s:%d: fn form, use λ" file (or node.line 0))))
        (each [_ child (ipairs node)]
          (walk child file problems)))
      (= (type node) :table)
      (each [key value (pairs node)]
        (walk key file problems)
        (walk value file problems))))

(λ check-file [file problems]
  (let [text (with-open [handle (assert (io.open file))] (handle:read :*a))
        next-form (fennel.parser (fennel.stringStream text) file)]
    (var done false)
    (while (not done)
      (let [(ok node) (next-form)]
        (if ok
            (walk node file problems)
            (set done true))))))

(let [problems []]
  (each [_ file (ipairs arg)]
    (check-file file problems))
  (each [_ problem (ipairs problems)]
    (io.stderr:write problem "\n"))
  (os.exit (if (= (length problems) 0) 0 1)))
