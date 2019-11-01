;; Things to try:
;; company-quickhelp
;; hydra


;; Code navigation:
;; M-? xref-find-references
;; M-. xref-find-definitions
;; M-, xref-pop-marker-stack

;; Environment

(setq custom-file
      (expand-file-name (locate-user-emacs-file "custom-settings.el")))
(when (file-exists-p custom-file)
  (load custom-file t))

;; Formatting
;;
(setq-default tab-width 2
              indent-tabs-mode nil       ; always indent with spaces
              tab-stop-list (number-sequence 2 60 2)
              visible-bell t
              show-trailing-whitespace t ; show extra whitespace
              indicate-empty-lines t
              require-final-newline t)   ; ensure last line is a return

;; Display
;;
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

(dolist (mode '(column-number-mode))
  (when (fboundp mode) (funcall mode 1)))

(setq inhibit-startup-screen t)

(add-to-list 'default-frame-alist '(fullscreen . fullboth))
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
;; (setq line-move-visual nil)

(setq linum-format "%4d | ")
(global-linum-mode)

;; Misc
;;
(setq load-prefer-newer t)    ; Please don't load outdated byte code
(fset 'yes-or-no-p 'y-or-n-p) ; short answers
(setq make-backup-files nil)  ; no backups

;; Bootstrap use-package
;;

;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(if (and (version<= emacs-version "26.2") (string-equal system-type "darwin"))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-always-pin "melpa-stable"
      use-package-verbose t
      use-package-compute-statistics nil)

(eval-when-compile
  (require 'use-package))

;; Packages
;;
(use-package auto-package-update
  :disabled
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables '("MANPATH" "PATH" "exec-path" "GO111MODULE" "GOPATH" "GOBIN" "LANG" "LC_CTYPE"))
  (exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-initialize))

(use-package diminish)
(use-package bind-key)

;; TODO: make sure this is still working
(use-package server
  :disabled
  :init (server-mode)
  :diminish server-buffer-clients)

(use-package eldoc :diminish eldoc-mode)

(use-package magit
  :bind   ("C-x g" . magit-status))

(use-package dap-mode
  :pin "melpa"
  :config
  (dap-mode t)
  (dap-ui-mode t)
  ;; enables mouse hover support
  (dap-tooltip-mode t)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode t))

;;;

;; (defun my/window-visible (b-name)
;;   "Return whether B-NAME is visible."
;;   (-> (-compose 'buffer-name 'window-buffer)
;;       (-map (window-list))
;;       (-contains? b-name)))

;; (defun my/show-debug-windows (session)
;;   "Show debug windows."
;;   (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
;;     (save-excursion
;;       ;; display locals
;;       (unless (my/window-visible dap-ui--locals-buffer)
;;         (dap-ui-locals))
;;       ;; display sessions
;;       (unless (my/window-visible dap-ui--sessions-buffer)
;;         (dap-ui-sessions)))))

;; (add-hook 'dap-stopped-hook 'my/show-debug-windows)

;; (defun my/hide-debug-windows (session)
;;   "Hide debug windows when all debug sessions are dead."
;;   (unless (-filter 'dap--session-running (dap--get-sessions))
;;     (and (get-buffer dap-ui--sessions-buffer)
;;          (kill-buffer dap-ui--sessions-buffer))
;;     (and (get-buffer dap-ui--locals-buffer)
;;          (kill-buffer dap-ui--locals-buffer))))

;; (add-hook 'dap-terminated-hook 'my/hide-debug-windows)

;;;

;; https://ebzzry.io/en/emacs-pairs/
(use-package smartparens
  :bind (:map smartparens-mode-map
              ("C-M-a" . sp-beginning-of-sexp)
              ("C-M-e" . sp-end-of-sexp)

              ("C-<down>" . sp-down-sexp)
              ("C-<up>"   . sp-up-sexp)
              ("M-<down>" . sp-backward-down-sexp)
              ("M-<up>"   . sp-backward-up-sexp)

              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)

              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)

              ("C-S-f" . sp-forward-symbol)
              ("C-S-b" . sp-backward-symbol)

              ("C-<right>" . sp-forward-slurp-sexp)
              ("M-<right>" . sp-forward-barf-sexp)
              ("C-<left>"  . sp-backward-slurp-sexp)
              ("M-<left>"  . sp-backward-barf-sexp)

              ("C-M-t" . sp-transpose-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-k"   . sp-kill-hybrid-sexp)
              ("M-k"   . sp-backward-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("C-M-d" . delete-sexp)

              ("M-<backspace>" . backward-kill-word)
              ("C-<backspace>" . sp-backward-kill-word)
              ([remap sp-backward-kill-word] . backward-kill-word)

              ;; For some reason this breaks pasting into emacs:
              ;; ("M-[" . sp-backward-unwrap-sexp)
              ;; ("M-]" . sp-unwrap-sexp)
              ;; --

              ("C-x C-t" . sp-transpose-hybrid-sexp)

              ("C-c ("  . wrap-with-parens)
              ("C-c ["  . wrap-with-brackets)
              ("C-c {"  . wrap-with-braces)
              ("C-c '"  . wrap-with-single-quotes)
              ("C-c \"" . wrap-with-double-quotes)
              ("C-c _"  . wrap-with-underscores)
              ("C-c `"  . wrap-with-back-quotes)
              )

  :hook ((prog-mode . smartparens-mode)
         (prog-mode . show-smartparens-mode)
         (emacs-lisp-mode . smartparens-strict-mode))
  :config
  ;;   (defmacro def-pairs (pairs)
  ;;     "Define functions for pairing. PAIRS is an alist of (NAME . STRING)
  ;; conses, where NAME is the function name that will be created and
  ;; STRING is a single-character string that marks the opening character.

  ;;   (def-pairs ((paren . \"(\")
  ;;               (bracket . \"[\"))

  ;; defines the functions WRAP-WITH-PAREN and WRAP-WITH-BRACKET,
  ;; respectively."
  ;;     `(progn
  ;;        ,@(loop for (key . val) in pairs
  ;;                collect
  ;;                `(defun ,(read (concat
  ;;                                "wrap-with-"
  ;;                                (prin1-to-string key)
  ;;                                "s"))
  ;;                     (&optional arg)
  ;;                   (interactive "p")
  ;;                   (sp-wrap-with-pair ,val)))))

  ;;   (def-pairs ((paren . "(")
  ;;               (bracket . "[")
  ;;               (brace . "{")
  ;;               (single-quote . "'")
  ;;               (double-quote . "\"")
  ;;               (back-quote . "`")))
  (require 'smartparens-config)
  :diminish smartparens-mode)

(use-package hl-line
  :config (global-hl-line-mode t))
(use-package hl-todo
  :config (global-hl-todo-mode t))
(use-package highlight-numbers
  :hook ((prog-mode . highlight-numbers-mode)))

(use-package highlight-symbol
  :custom
  (highlight-symbol-on-navigation-p t) ;Highlight immediately after navigation
  (highlight-symbol-idle-delay 0.1) ; Highlight quickly
  :config
  ;;Enable symbol navigation using M-n and M-p
  (dolist (hook '(highlight-symbol-nav-mode highlight-symbol-mode))
    (add-hook 'prog-mode-hook hook))
  :diminish highlight-symbol-mode)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package projectile
  :custom
  (projectile-completion-system 'helm)
  (projectile-git-ignored-command nil) ; also find ignored files
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  :diminish projectile-mode)

(use-package helm
  :config
  (require 'helm-config)
  (helm-mode 1)
  :bind ([remap execute-extended-command] . helm-M-x)
  :diminish helm-mode)
(use-package helm-ag
  :custom
  (helm-ag-insert-at-point 'symbol))
(use-package helm-projectile
  :config (helm-projectile-on)
  :bind   (
           ("C-c h" . helm-projectile) ;; also C-c p f
           ([remap switch-to-buffer] . helm-mini)
           ([remap find-file] . helm-find-files)
           ([remap projectile-switch-project] . helm-projectile-switch-project)
           ([remap projectile-ag] . helm-projectile-ag)
           ([remap projectile-commander] . helm-imenu)
           ))
(use-package helm-xref
  :pin "melpa"
  :config
  ;; Use helm-xref as the default xref show function.
  (setq-default xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("C-c r"   . lsp-rename)
              ("C-c C-r" . lsp-find-references)
              ;; ("C-c C-j" . lsp-find-definitions)
              ;; ("C-c i"   . lsp-find-implementation)
              )
  :diminish lsp-mode
  :hook ((go-mode . lsp-deferred)
         (enh-ruby-mode . lsp-deferred)
         (lsp-after-open . disable-flymake-mode))
  :custom
  (lsp-auto-guess-root t)
  (lsp-signature-render-all t)
  (lsp-eldoc-render-all t)
  (lsp-prefer-flymake nil)
  :commands lsp-deferred
  :config
  (defun disable-flymake-mode ()
    (flymake-mode -1)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-enable nil)
  (lsp-ui-imenu-enable nil)
  (lsp-ui-doc-enable nil)
  (lsp-ui-flycheck-enable t)
  :config
  (require 'lsp-ui-flycheck)
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))
  )

(use-package company-lsp
  :bind (("M-RET" . company-complete))
  :commands company-lsp
  :diminish company-mode)

;; For some reason this is not working. It seems related to not being able to find the current workspace
;; (use-package helm-lsp
;;   :commands helm-lsp-workspace-symbol
;;   :pin "melpa"
;;   :bind ([remap xref-find-apropos] . helm-lsp-workspace-symbol))

(use-package ag)
(use-package yaml-mode)
(use-package yasnippet :diminish yas-minor-mode)
(use-package flycheck)
(use-package flycheck-color-mode-line
  :config
  (set-face-attribute 'flycheck-color-mode-line-error-face nil :background "red2")
  :hook ((flycheck-mode . flycheck-color-mode-line-mode)))

(use-package recentf
  :custom
  (recentf-max-menu-items 25)
  :init (recentf-mode t))

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

(use-package sh-script
  :custom
  (sh-basic-offset 2)
  :mode
  ("zshrc*" . shell-script-mode)
  ("zshenv*" . shell-script-mode)
  ("zprofile*" . shell-script-mode))

(use-package enh-ruby-mode
  :after (lsp-mode dap-mode)
  :defines     (end-ruby-deep-indent-paren end-ruby-deep-arglist)
  :interpreter "ruby"
  :mode        (("\\.rb$" . enh-ruby-mode)
                ("Rakefile" . enh-ruby-mode)
                ("\\.rake" . enh-ruby-mode)
                ("\\.gemspec" . enh-ruby-mode)
                ("Gemfile" . enh-ruby-mode)
                ("Guardfile" . enh-ruby-mode)
                ("Vagrantfile" . enh-ruby-mode)
                ("\\.ru" . enh-ruby-mode))
  :custom
  (enh-ruby-bounce-deep-indent t)
  (enh-ruby-deep-indent-paren t)
  (enh-ruby-hanging-brace-deep-indent-level 1)
  (enh-ruby-hanging-brace-indent-level 2)
  (enh-ruby-hanging-indent-level 2)
  (enh-ruby-hanging-paren-deep-indent-level 0)
  (enh-ruby-hanging-paren-indent-level 2)
  (enh-ruby-indent-level 2)
  (enh-ruby-add-encoding-comment-on-save nil)

  :config
  ;; When you've hit the breakpoint, hit C-x C-q to enable inf-ruby
  (use-package inf-ruby :config (inf-ruby-switch-setup))
  (use-package bundler)
  (use-package rspec-mode)
  (require 'align)
  (add-to-list 'align-rules-list
               '(ruby-comma-delimiter
                 (regexp . ",\\(\\s-*\\)[^# \t\n]")
                 (repeat . t)
                 (modes  . '(enh-ruby-mode))))

  (add-to-list 'align-rules-list
               '(ruby-hash-literal
                 (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
                 (group 2 3)
                 (repeat . t)
                 (modes  . '(enh-ruby-mode))))

  (add-to-list 'align-rules-list
               '(ruby-hash-literal2
                 (regexp . "[a-z0-9]:\\(\\s-*\\)[^# \t\n]")
                 (repeat . t)
                 (modes  . '(enh-ruby-mode))))

  (add-to-list 'align-rules-list
               '(ruby-assignment-literal
                 (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
                 (repeat . t)
                 (modes  . '(enh-ruby-mode))))
  (add-to-list 'align-rules-list
               '(ruby-xmpfilter-mark
                 (regexp . "\\(\\s-*\\)# => [^#\t\n]")
                 (repeat . nil)
                 (modes  . '(enh-ruby-mode))))
  )

(use-package go-mode
  :hook ((before-save . go-mode-before-save-fn))
  :after (lsp-mode dap-mode)
  :config
  (defun go-mode-before-save-fn ()
    (if (equal major-mode 'go-mode)
        (format-buffer)))
  (require 'dap-go)
  (dap-go-setup))

(use-package gotest
  :bind (:map go-mode-map
              ("C-c , v" . go-test-current-file)
              ("C-c , s" . go-test-current-test)
              ("C-c , a" . go-test-current-project)
              ("C-c , b" . go-test-current-benchmark)
              ("C-c x" . go-run)))

;; Functions
;;
(defun format-buffer()
  "format buffer"
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (progn (ignore-errors (lsp-organize-imports))
             (lsp-format-buffer))
    (cleanup-buffer)))

(defun cleanup-buffer()
  "indent and clean buffer"
  (interactive)
  (delete-trailing-whitespace)
  (unless (derived-mode-p 'makefile-mode)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;; Keybindings
;;
(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-c n") 'format-buffer)

;; Window management
;;
(global-set-key (kbd "<up>") 'shrink-window)
(global-set-key (kbd "<down>") 'enlarge-window)
(global-set-key (kbd "<left>") 'shrink-window-horizontally)
(global-set-key (kbd "<right>") 'enlarge-window-horizontally)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
