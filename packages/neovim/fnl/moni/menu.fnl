(local {: bo! : wo! : float! : nmap!} (require :moni.lib))

(local ns (vim.api.nvim_create_namespace :moni_menu))

;; fnlfmt: skip
(local sideways-keys [:<ScrollWheelLeft> :<ScrollWheelRight>
                      :<S-ScrollWheelLeft> :<S-ScrollWheelRight>
                      :zl :zh :zL :zH :<Left> :<Right> :h :l])

(λ clamp [low x high]
  (math.max low (math.min x high)))

(λ trim-separators [entries]
  "`entries` without leading, trailing or doubled separators."
  (let [rows []]
    (each [_ entry (ipairs entries)]
      (let [prev (. rows (length rows))]
        (when (not (and entry.separator (or (= prev nil) prev.separator)))
          (tset rows (+ (length rows) 1) entry))))
    (while (and (< 0 (length rows)) (. rows (length rows) :separator))
      (tset rows (length rows) nil))
    rows))

(λ menu-width [rows]
  (var width 0)
  (each [_ row (ipairs rows)]
    (when (not row.separator)
      (let [hint-width (if row.hint
                           (+ (vim.fn.strdisplaywidth row.hint) 3)
                           0)
            row-width (+ (vim.fn.strdisplaywidth row.label) hint-width)]
        (set width (math.max width row-width)))))
  (+ width 2))

(λ render-line [row width]
  (if row.separator
      (string.rep "─" width)
      (let [gap (if row.hint
                    (- width 2 (vim.fn.strdisplaywidth row.label)
                       (vim.fn.strdisplaywidth row.hint))
                    0)
            hint (if row.hint
                     (.. (string.rep " " gap) row.hint)
                     "")]
        (.. " " row.label hint " "))))

(λ highlight-rows! [buf rows lines]
  (each [i row (ipairs rows)]
    (let [line (. lines i)]
      (if row.separator
          (vim.api.nvim_buf_set_extmark buf ns (- i 1) 0
                                        {:end_col (length line)
                                         :hl_group :MoniMenuSeparator})
          row.hint
          (let [start (- (length line) 1 (length row.hint))]
            (vim.api.nvim_buf_set_extmark buf ns (- i 1) start
                                          {:end_col (+ start (length row.hint))
                                           :hl_group :MoniMenuHint}))))))

(λ place [rows width at]
  "Row, column and height for a menu of `rows` at `at`: below the pointer if it fits, else above."
  (let [[row col] at
        pointer (- row 1)
        room-below (- vim.o.lines 1 row 2)
        room-above (- pointer 2)
        above? (and (< room-below (length rows)) (< room-below room-above))
        height (math.max 1
                         (math.min (length rows)
                                   (if above? room-above room-below)))]
    (values (if above? (- pointer height 2) row)
            (if (< vim.o.columns (+ col width 2))
                (math.max 0 (- vim.o.columns width 2))
                col) height)))

(λ selectable? [rows i]
  (and (. rows i) (not (. rows i :separator))))

(λ first-selectable [rows from step]
  (var i from)
  (var found nil)
  (while (and (not found) (<= 1 i) (<= i (length rows)))
    (if (selectable? rows i)
        (set found i)
        (set i (+ i step))))
  found)

(λ scroll! [win rows height step]
  "Scroll the menu by `step` lines, keeping the cursor inside the view."
  (let [top (. (vim.fn.getwininfo win) 1 :topline)
        target (clamp 1 (+ top step) (+ (- (length rows) height) 1))]
    (vim.api.nvim_win_call win #(vim.fn.winrestview {:topline target}))
    (let [line (. (vim.api.nvim_win_get_cursor win) 1)
          bottom (- (+ target height) 1)]
      (when (or (< line target) (< bottom line))
        (vim.api.nvim_win_set_cursor win [(clamp target line bottom) 0])))))

(λ attach! [{: buf : win : origin : rows : height :before ?before}]
  "Bind the keys that drive the menu, and close it when it loses focus."
  (var done false)
  (λ close []
    (when (not done)
      (set done true)
      (when (vim.api.nvim_win_is_valid win)
        (vim.api.nvim_win_close win true))
      (when (vim.api.nvim_win_is_valid origin)
        (vim.api.nvim_set_current_win origin))))
  (λ run [i]
    (when (selectable? rows i)
      (close)
      (vim.schedule (λ []
                      (when ?before
                        (?before))
                      ((. rows i :run))))))
  (λ move [step]
    (let [i (. (vim.api.nvim_win_get_cursor win) 1)
          wrapped (first-selectable rows (if (< 0 step) 1 (length rows)) step)
          target (or (first-selectable rows (+ i step) step) wrapped)]
      (when target
        (vim.api.nvim_win_set_cursor win [target 0]))))
  (nmap! buf [:<CR> :<Tab>]
         (λ []
           (run (. (vim.api.nvim_win_get_cursor win) 1))))
  (nmap! buf [:j :<Down> :<C-n>] (λ [] (move 1)))
  (nmap! buf [:k :<Up> :<C-p> :<S-Tab>] (λ [] (move (- 1))))
  (nmap! buf [:<Esc> :q :<C-c> :<RightMouse>] close)
  (nmap! buf :<LeftMouse> (λ []
                            (let [pos (vim.fn.getmousepos)]
                              (if (= pos.winid win)
                                  (run pos.line)
                                  (close)))))
  (nmap! buf [:<ScrollWheelDown> :<S-ScrollWheelDown>]
         (λ [] (scroll! win rows height 3)))
  (nmap! buf [:<ScrollWheelUp> :<S-ScrollWheelUp>]
         (λ [] (scroll! win rows height (- 3))))
  (nmap! buf sideways-keys (λ []))
  (nmap! buf :<MouseMove>
         (λ []
           (let [pos (vim.fn.getmousepos)]
             (when (and (= pos.winid win) (selectable? rows pos.line))
               (vim.api.nvim_win_set_cursor win [pos.line 0])))))
  (vim.api.nvim_create_autocmd :WinLeave
                               {:buffer buf
                                :once true
                                :callback (λ []
                                            (close)
                                            nil)}))

(λ open [entries at ?opts]
  "Open a floating menu of `entries` at `at`. An entry is {: label : run ?hint} or {:separator true}."
  (let [opts (or ?opts {})
        rows (trim-separators entries)]
    (when (< 0 (length rows))
      (let [width (menu-width rows)
            lines (icollect [_ row (ipairs rows)]
                    (render-line row width))
            buf (vim.api.nvim_create_buf false true)
            (row col height) (place rows width at)
            origin (vim.api.nvim_get_current_win)]
        (vim.api.nvim_buf_set_lines buf 0 -1 false lines)
        (bo! buf :modifiable false :bufhidden :wipe)
        (highlight-rows! buf rows lines)
        (let [win (float! buf true
                          {:relative :editor
                           : row
                           : col
                           : width
                           : height
                           :zindex 70}
                          (.. "Normal:NormalFloat,FloatBorder:FloatBorder,"
                              "CursorLine:PmenuSel"))]
          (wo! win :cursorline true :wrap false)
          (vim.api.nvim_win_set_cursor win [(first-selectable rows 1 1) 0])
          (attach! {: buf : win : origin : rows : height :before opts.before}))))))

{: open}
