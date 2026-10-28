;; fnlfmt: skip
{:compiler-options {:correlate true :compilerEnv _G :allowedGlobals false :error-pinpoint ["|>>" "<<|"]}
 :fnl-dir :fnl
 :macro-path (table.concat [:./fnl/?.fnlm
                            :./fnl/?/init.fnlm
                            :./fnl/?.fnl
                            :./fnl/?/init-macros.fnl
                            :./fnl/?/init.fnl] ";")
 :max-rollbacks 5}
