(λ to-number [?hex]
  (if ?hex (tonumber (?hex:gsub "^#" "") 16)))

;; fnlfmt: skip
(λ palette [name]
  "The mellifluous palette color `name` as a number, or nil."
  (let [(ok hex) (pcall #(. ((. (require :mellifluous.colors) :get_colors)) name :hex))]
    (if ok (to-number hex))))

;; fnlfmt: skip
(λ background-hex [config]
  (let [colorset (require (.. :mellifluous.colors.colorsets. config.colorset))]
    (. (if config.is_bg_dark (colorset.get_bg_dark) (colorset.get_bg_light)) :hex)))

;; fnlfmt: skip
(λ theme-bg []
  "The background color of the active mellifluous variant as a number, or nil."
  (let [(ok hex) (pcall #(background-hex (. (require :mellifluous.config) :config)))]
    (if ok (to-number hex))))

(λ mix [fg bg alpha]
  "Blend the 0xRRGGBB color `fg` over `bg`, `alpha` being fg's share (0 to 1)."
  (λ channel [shift]
    (let [f (bit.band (bit.rshift fg shift) 255)
          b (bit.band (bit.rshift bg shift) 255)]
      (math.floor (+ b (* (- f b) alpha) 0.5))))
  (+ (* (channel 16) 65536) (* (channel 8) 256) (channel 0)))

{: palette :theme_bg theme-bg : mix}
