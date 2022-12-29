;; -*- lexical-binding: t -*-

;; Disable tooltips
(tooltip-mode -1)

;; Early frame modifications
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(font . "Iosevka FT Light-12"))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

(setq-default use-dialog-box nil
              custom-file (if (memq system-type '(gnu/linux darwin)) "/dev/null" "NUL")
              frame-inhibit-implied-resize t
              ring-bell-function 'ignore
              visible-bell nil
              uniquify-buffer-name-style 'post-forward-angle-brackets
              inhibit-startup-screen t
              inhibit-startup-message t
              inhibit-startup-echo-area-message t
              initial-scratch-message nil
              package-enable-at-startup nil
              vc-handled-backends nil
              fringes-outside-margins t
	      use-package-always-ensure nil
	      use-package-always-defer t)

(defconst p!-font-lock-keywords
  '(("(\\(p!\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode p!-font-lock-keywords)

(defmacro p! (name &rest args)
  "`use-package' that is shorter and less cumbersome."
  (declare (indent defun))
  `(use-package ,name
     ,@args))

(p! no-littering
  :demand t
  :config
  (require 'no-littering))

(p! exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :demand t
  :config
  (exec-path-from-shell-initialize))

(setq-default flycheck-emacs-lisp-load-path load-path)

;; native compilation
(setq comp-speed 3
      comp-async-report-warnings-errors nil)

;; Temporarily save the file-name-handler-alist
(defvar temp-file-name-handler-alist file-name-handler-alist)

;; Ignore .Xresources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Avoid any GC pauses at init.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      preferred-gc-threshold 16777216)

(setq-default file-name-handler-alist nil
              create-lockfiles nil
              bidi-display-reordering nil
              cursor-in-non-selected-windows nil
              highlight-nonselected-windows nil
              frame-inhibit-implied-resize t
              inhibit-compacting-font-caches nil)

;; and then reset it to 16MiB after with the file-name-handler-alist
(defun optimization/normalize ()
	(setq-default gc-cons-threshold preferred-gc-threshold
		            gc-cons-percentage 0.1)
  (garbage-collect))

(add-hook 'emacs-startup-hook #'optimization/normalize)

(unless (or (daemonp) noninteractive)
  (let ((temp-file-name-handler-alist file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    (defun reset-file-handler-alist-h ()
      (setq file-name-handler-alist
            (delete-dups (append file-name-handler-alist
                                 temp-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'reset-file-handler-alist-h)))

;; ESC cancels anything
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;; Fundamental Mode is the simplest mode
(setq initial-major-mode 'fundamental-mode)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use the Garbage Collector Magic Hack
(p! gcmh
  :hook
  ((emacs-startup . gcmh-mode)
   (focus-out . gcmh-idle-garbage-collect))
  :custom
  (gcmh-verbose nil)
  (gcmh-idle-delay 6)
  (gcmh-high-cons-threshold 16777216))

;; Show only errors, not warnings
(setq-default warning-minimum-level :warning)

;; UTF-8 encoding
(setq-default buffer-file-coding-system 'utf-8
	      save-buffer-coding-system 'utf-8)

(prefer-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Scrolling
(setq-default scroll-step 1
	      scroll-conservatively 10000
	      auto-window-vscroll nil)

;; Editor defaults
(setq-default auto-save-default nil
	      require-final-newline t
	      indent-tabs-mode nil
	      tab-width 2)

(add-hook 'emacs-startup-hook #'global-visual-line-mode)

(p! paren
  :hook
  (prog-mode . show-paren-mode))

(p! hl-line
  :hook
  ((prog-mode org-mode text-mode conf-mode) . hl-line-mode))

(p! autorevert
  :hook
  (emacs-startup . global-auto-revert-mode))

(p! super-save
  :hook
  (emacs-startup . super-save-mode)
  :custom
  (super-save-auto-save-when-idle t))

(p! smartparens
  :config
  (require 'smartparens-config)
  :hook
  (prog-mode . smartparens-mode))

(p! hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   `(("TODO" warning bold)
     ("FIXME" error bold)
     ("HACK" font-lock-constant-face bold)
     ("REVIEW" font-lock-keyword-face bold)
     ("NOTE" success bold)
     ("DEPRECATED" font-lock-doc-face bold)
     ("BUG" error bold)
     ("XXX" font-lock-constant-face bold))))

(p! highlight-indent-guides
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character))

(defun kill-other-buffers ()
  "Kill all other buffers"
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(global-set-key (kbd "C-c C-k") #'kill-other-buffers)
(global-set-key (kbd "C-c k") #'kill-buffer-and-window)

(p! evil
  :hook
  (emacs-startup . evil-mode))

;; Fonts
(defconst fonts/variable-pitch
  "Sarasa Gothic J"
  "Default variable width font")

(defun fonts/set-fonts ()
  (set-face-attribute 'fixed-pitch-serif nil :family fonts/variable-pitch)
  (set-face-attribute 'variable-pitch nil :family fonts/variable-pitch)
  (set-face-attribute 'font-lock-type-face nil :weight 'semi-bold)
  (set-face-attribute 'button nil :weight 'semi-bold)
  (set-face-attribute 'link nil :weight 'semi-bold))

(defun fonts/enable-ligatures ()
  "Define general ligatures and load them with HarfBuzz."
  (let ((alist  '((?!  . "\\(?:!\\(?:==\\|[!=]\\)\\)")                                      ; (regexp-opt '("!!" "!=" "!=="))
                  (?#  . "\\(?:#\\(?:###?\\|_(\\|[#(:=?[_{]\\)\\)")                         ; (regexp-opt '("##" "###" "####" "#(" "#:" "#=" "#?" "#[" "#_" "#_(" "#{"))
                  (?$  . "\\(?:\\$>>?\\)")                                                  ; (regexp-opt '("$>" "$>>"))
                  (?%  . "\\(?:%%%?\\)")                                                    ; (regexp-opt '("%%" "%%%"))
                  (?&  . "\\(?:&&&?\\)")                                                    ; (regexp-opt '("&&" "&&&"))
                  (?*  . "\\(?:\\*\\(?:\\*[*/]\\|[)*/>]\\)?\\)")                            ; (regexp-opt '("*" "**" "***" "**/" "*/" "*>" "*)"))
                  (?+  . "\\(?:\\+\\(?:\\+\\+\\|[+:>]\\)?\\)")                              ; (regexp-opt '("+" "++" "+++" "+>" "+:"))
                  (?-  . "\\(?:-\\(?:-\\(?:->\\|[>-]\\)\\|<[<-]\\|>[>-]\\|[:<>|}~-]\\)\\)") ; (regexp-opt '("--" "---" "-->" "--->" "->-" "-<" "-<-" "-<<" "->" "->>" "-}" "-~" "-:" "-|"))
                  (?.  . "\\(?:\\.\\(?:\\.[.<]\\|[.=>-]\\)\\)")                             ; (regexp-opt '(".-" ".." "..." "..<" ".=" ".>"))
                  (?/  . "\\(?:/\\(?:\\*\\*\\|//\\|==\\|[*/=>]\\)\\)")                      ; (regexp-opt '("/*" "/**" "//" "///" "/=" "/==" "/>"))
                  (?:  . "\\(?::\\(?:::\\|[+:<=>]\\)?\\)")                                  ; (regexp-opt '(":" "::" ":::" ":=" ":<" ":=" ":>" ":+"))
                  (?\; . ";;")                                                              ; (regexp-opt '(";;"))
                  (?0  . "0\\(?:\\(x[a-fA-F0-9]\\).?\\)") ; Tries to match the x in 0xDEADBEEF
                  ;; (?x . "x") ; Also tries to match the x in 0xDEADBEEF
                  ;; (regexp-opt '("<!--" "<$" "<$>" "<*" "<*>" "<**>" "<+" "<+>" "<-" "<--" "<---" "<->" "<-->" "<--->" "</" "</>" "<<" "<<-" "<<<" "<<=" "<=" "<=<" "<==" "<=>" "<===>" "<>" "<|" "<|>" "<~" "<~~" "<." "<.>" "<..>"))
                  (?<  . "\\(?:<\\(?:!--\\|\\$>\\|\\*\\(?:\\*?>\\)\\|\\+>\\|-\\(?:-\\(?:->\\|[>-]\\)\\|[>-]\\)\\|\\.\\(?:\\.?>\\)\\|/>\\|<[<=-]\\|=\\(?:==>\\|[<=>]\\)\\||>\\|~~\\|[$*+./<=>|~-]\\)\\)")
                  (?=  . "\\(?:=\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)\\)")               ; (regexp-opt '("=/=" "=:=" "=<<" "==" "===" "==>" "=>" "=>>"))
                  (?>  . "\\(?:>\\(?:->\\|=>\\|>[=>-]\\|[:=>-]\\)\\)")                      ; (regexp-opt '(">-" ">->" ">:" ">=" ">=>" ">>" ">>-" ">>=" ">>>"))
                  (??  . "\\(?:\\?[.:=?]\\)")                                               ; (regexp-opt '("??" "?." "?:" "?="))
                  (?\[ . "\\(?:\\[\\(?:|]\\|[]|]\\)\\)")                                    ; (regexp-opt '("[]" "[|]" "[|"))
                  (?\\ . "\\(?:\\\\\\\\[\\n]?\\)")                                          ; (regexp-opt '("\\\\" "\\\\\\" "\\\\n"))
                  (?^  . "\\(?:\\^==?\\)")                                                  ; (regexp-opt '("^=" "^=="))
                  (?w  . "\\(?:wwww?\\)")                                                   ; (regexp-opt '("www" "wwww"))
                  (?{  . "\\(?:{\\(?:|\\(?:|}\\|[|}]\\)\\|[|-]\\)\\)")                      ; (regexp-opt '("{-" "{|" "{||" "{|}" "{||}"))
                  (?|  . "\\(?:|\\(?:->\\|=>\\||=\\|[]=>|}-]\\)\\)")                        ; (regexp-opt '("|=" "|>" "||" "||=" "|->" "|=>" "|]" "|}" "|-"))
                  (?_  . "\\(?:_\\(?:|?_\\)\\)")                                            ; (regexp-opt '("_|_" "__"))
                  (?\( . "\\(?:(\\*\\)")                                                    ; (regexp-opt '("(*"))
                  (?~  . "\\(?:~\\(?:~>\\|[=>@~-]\\)\\)"))))                                  ; (regexp-opt '("~-" "~=" "~>" "~@" "~~" "~~>"))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring])))))

(add-hook 'emacs-startup-hook #'fonts/set-fonts)
(add-hook 'after-change-major-mode-hook #'fonts/enable-ligatures)

;; Pretty stuff
(p! hide-mode-line
  :hook
  ((comint-mode helpful-mode help-mode) . hide-mode-line-mode))

(p! selectrum
  :hook
  (after-init . selectrum-mode))

(p! prescient
  :hook
  (after-init . prescient-persist-mode))

(p! selectrum-prescient
  :hook
  (after-init . selectrum-prescient-mode))

(p! helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key))
  :config
  (custom-theme-set-faces
   'user
   '(helpful-heading ((t (:inherit variable-pitch))))))

(p! solaire-mode
  :hook
  ((change-major-mode . turn-on-solaire-mode)
   (after-revert . turn-on-solaire-mode)
   (ediff-prepare-buffer . solaire-mode))
  :custom
  (solaire-mode-auto-swap-bg nil)
  :config
  (solaire-global-mode +1))

(p! doom-themes
  :after solaire-mode
  :hook
  (emacs-startup . (lambda () (load-theme 'doom-material-dark t)))
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

(p! simple-modeline
  :hook
  (emacs-startup . simple-modeline-mode))

(p! olivetti
  :custom
  (olivetti-body-width 140))

(p! display-line-numbers
  :hook
  ((prog-mode org-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-width 3)
  (display-line-numbers-widen t))

(defun interface/toggle-zen-mode ()
  "Toggle a distraction-free environment for writing."
  (interactive)

  (defface regular-face
  	'((nil :family "Iosevka FT Light" :height 120))
    "Regular face")

  (defface zen-mode-face
  	'((nil :family "Iosevka FT Light" :height 150))
    "Zen mode face")

  (cond ((bound-and-true-p olivetti-mode)
         (olivetti-mode -1)
         (display-line-numbers-mode +1)
         (hide-mode-line-mode -1)
         (buffer-face-set 'regular-face))
        (t
         (olivetti-mode +1)
         (display-line-numbers-mode -1)
         (hide-mode-line-mode +1)
         (buffer-face-set 'zen-mode-face))))

(global-set-key (kbd "C-x z") 'interface/toggle-zen-mode)

(p! which-key
  :hook
  (emacs-startup . which-key-mode))

;; Syntax checking, completions and LSP
(p! flycheck
  :hook
  (prog-mode . flycheck-mode)
  ;; Shut up the most annoying flycheck lint.
  :custom
  (flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(p! flycheck-popup-tip
  :after flycheck
  :hook
  (flycheck-mode . flycheck-popup-tip-mode))

(p! flycheck-posframe
  :after flycheck
  :hook
  (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults))

(p! company
  :hook
  (prog-mode . company-mode)
  :custom
  (company-idle-delay 2)
  (company-tooltip-limit 14)
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  (company-backends '(company-capf))
  (company-format-margin-function nil))

;; Language support

;;;; Emacs Lisp
;; ELisp/l looks odd
(add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "Emacs Lisp")))

(p! highlight-quoted
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

(p! highlight-defined
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(p! elisp-def
  :hook
  ((emacs-lisp-mode ielm-mode) . elisp-def-mode))

(p! lisp-butt-mode
  :hook
  (emacs-lisp-mode . lisp-butt-mode))

(p! aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;;;; Nix
(p! lsp-nix
  :after (lsp-mode)
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(p! nix-mode
  :mode "\\.nix\\'"
  :hook
  (nix-mode . lsp-deferred))

;;;; R
(p! ess
  :hook
  (ess-mode . lsp-deferred))
