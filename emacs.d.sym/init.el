;; Environment

(setenv "LANG" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

(setq load-prefer-newer t)


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
(dolist (mode '(tool-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; (dolist (mode '(column-number-mode))
;;   (when (fboundp mode) (funcall mode 1)))

(setq inhibit-startup-screen t)

;; Misc
;;
(setq load-prefer-newer t)    ; Please don't load outdated byte code
(fset 'yes-or-no-p 'y-or-n-p) ; short answers
(setq make-backup-files nil)

;; Bootstrap use-package
;;

;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(if (version<= emacs-version "26.2")
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
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa-stable")

(eval-when-compile
  (require 'use-package))

;; Packages
;;
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package exec-path-from-shell
  :config
  (dolist (var '("GO111MODULE"
                 "GOPATH"
                 "GOBIN"
                 ))
    (add-to-list 'exec-path-from-shell-variables var))
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))
(getenv "PATH")

(use-package diminish)

(use-package server
  :defer t
  :init (server-mode)
  :diminish server-buffer-clients)

(use-package magit
  :bind   ("C-x g" . magit-status))

(use-package smartparens
  :config   (smartparens-global-strict-mode)
  :diminish smartparens-mode)

(use-package enh-ruby-mode
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
  :init        (progn
                 (use-package rspec-mode)
                 (use-package inf-ruby :config (inf-ruby-switch-setup)) ;; When you've hit the breakpoint, hit C-x C-q to enable inf-ruby
                 (use-package bundler)
                 (setq enh-ruby-bounce-deep-indent t
                       enh-ruby-deep-indent-paren t
                       enh-ruby-hanging-brace-deep-indent-level 1
                       enh-ruby-hanging-brace-indent-level 2
                       enh-ruby-hanging-indent-level 2
                       enh-ruby-hanging-paren-deep-indent-level 0
                       enh-ruby-hanging-paren-indent-level 2
                       enh-ruby-indent-level 2
                       enh-ruby-add-encoding-comment-on-save nil))
  :config      (progn
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
                 ))

(use-package go-mode)

(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

(use-package highlight-numbers          ; Fontify number literals
  :defer t
  :init (highlight-numbers-mode))

(use-package projectile
  :config (projectile-mode)
  :diminish projectile-mode)

(use-package helm-ag)
(use-package helm-projectile
  :config (helm-projectile-on)
  :bind   (
           ("C-c h" . helm-projectile)
           ([remap switch-to-buffer] . helm-mini)
           ([remap find-file] . helm-find-files)
           ("C-c p p" . helm-projectile-switch-project)
           ("C-c p a" . helm-projectile-ag)))
(use-package helm
  :bind ([remap execute-extended-command] . helm-M-x)
  :diminish helm-mode)

(use-package lsp-mode
  :diminish lsp-mode
  :hook (
         (go-mode . lsp)
         (enh-ruby-mode . lsp))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(add-to-list 'company-lsp-filter-candidates '(gopls . nil))
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol :pin "melpa")
(use-package lsp-treemacs :commands lsp-treemacs-errors-list :pin "melpa")
(use-package dap-mode)

(use-package ag)
(use-package yaml-mode)
(use-package yasnippet)

(use-package recentf
  :init (progn (setq recentf-max-menu-items 25)
               (recentf-mode t)))

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode))

;; Functions
;;
(defun cleanup-buffer()
  "indent and clean buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

;; Keybindings
;;
(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Window management
;;
(progn
  (global-set-key (kbd "<up>") 'shrink-window)
  (global-set-key (kbd "<down>") 'enlarge-window)
  (global-set-key (kbd "<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "<right>") 'enlarge-window-horizontally))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-ag helm-projectile bundler inf-ruby rspec-mode enh-ruby-mode dap-mode lsp-treemacs helm helm-lsp company-lsp lsp-ui projectile which-key abbrev yasnippet yaml-mode go-mode exec-path-from-shell use-package-ensure-system-package ag sql-indent spinner lsp-mode use-package smartparens magit diminish auto-package-update))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
