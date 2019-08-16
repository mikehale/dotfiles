;; Environment

(setenv "LANG" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

(add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)

;; Misc
;;
(setq load-prefer-newer t)    ; Please don't load outdated byte code
(fset 'yes-or-no-p 'y-or-n-p) ; short answers
(setq make-backup-files nil)

;; Formatting
;;
(progn
  (setq-default tab-width 2
                indent-tabs-mode nil       ; always indent with spaces
                tab-stop-list (number-sequence 2 60 2)
                visible-bell t
                show-trailing-whitespace t ; show extra whitespace
                indicate-empty-lines t
                require-final-newline t))  ; ensure last line is a return

;; Display
;;
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

(dolist (mode '(column-number-mode))
  (when (fboundp mode) (funcall mode 1)))


;; Window management
;;
(progn
  (global-set-key (kbd "<up>") 'shrink-window)
  (global-set-key (kbd "<down>") 'enlarge-window)
  (global-set-key (kbd "<left>") 'shrink-window-horizontally)
  (global-set-key (kbd "<right>") 'enlarge-window-horizontally))

;; Elpa
;;
(require 'package)
(dolist (s '(("melpa-stable" . "http://stable.melpa.org/packages/")
             ("melpa" . "http://melpa.org/packages/")
             ("marmalade" . "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives s t))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Bootstrap `use-package'
;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;; (require 'diminish)
(require 'bind-key)
(setq use-package-verbose t)

;; Packages
;;

;; (use-package auto-package-update
;;    :ensure t
;;    :config
;;    (setq auto-package-update-delete-old-versions t
;;          auto-package-update-interval 1)
;;    (auto-package-update-maybe))

(use-package server
  :defer t
  :init (server-mode)
  :diminish server-buffer-clients
                                        ; export ALTERNATE_EDITOR=
                                        ; export EDITOR=emacsclient -t
  )

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;; (use-package hydandata-light-theme
;;   :ensure t)

(use-package compile
  :config (setq compilation-scroll-output t))

(use-package ag :ensure t)

(use-package magit
  :ensure t
  :bind   ("C-x g" . magit-status))

(use-package smartparens
  :ensure t
  :init   (progn
            (smartparens-global-mode)
            (show-smartparens-global-mode))
  :diminish smartparens-mode)

(use-package projectile
  :ensure t
  :init   (progn
            (projectile-global-mode))
  :diminish projectile-mode)

(use-package helm
  :ensure  t
  :defines (helm-M-x-fuzzy-match helm-semantic-fuzzy-match helm-imenu-fuzzy-match)
  :init    (progn
             (helm-mode t)
             (require 'helm-config)
             (setq helm-M-x-fuzzy-match t
                   helm-recentf-fuzzy-match t
                   helm-semantic-fuzzy-match t
                   helm-imenu-fuzzy-match t)
             (use-package semantic
               :init   (semantic-mode 1))
             (use-package helm-projectile
               :ensure t
               ;; :pin    melpa-stable
               :bind   (
                        ("C-c h" . helm-projectile)
                        ;; ("C-c h i" . helm-semantic-or-imenu)
                        ([remap switch-to-buffer] . helm-mini)
                        ([remap find-file] . helm-find-files)
                        ([remap projectile-switch-project] . helm-projectile-switch-project)
                        ("C-c p a" . helm-projectile-ag)
                        ;; ([remap projectile-find-other-file] . helm-projectile-ag)
                        ))
             (use-package helm-ag :ensure t))

  :bind   (([remap execute-extended-command] . helm-M-x))
  :diminish helm-mode)

(use-package sh-mode
  :config (progn(
                 (sh-set-shell "zsh")
                 (setq sh-basic-offset 2
                       sh-indentation 2)))
  :interpreter "zsh"
  :mode        (
                ("\\.zsh$" . sh-mode)
                ("zshrc" . sh-mode)
                ("zprofile" . sh-mode)))

(use-package autorevert                 ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config (setq auto-revert-verbose nil ; Shut up, please!
                global-auto-revert-non-file-buffers nil))

(use-package recentf
  :init (progn (setq recentf-max-menu-items 25)
               (recentf-mode t)))

(use-package global-auto-revert-mode
  :init (global-auto-revert-mode 1))

(use-package abbrev
  :diminish abbrev-mode)

(use-package flycheck
  :ensure t
  :init   (progn
            (global-flycheck-mode)
            (use-package flycheck-pos-tip
              :disabled t
              :ensure   t
              :config   (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
            (use-package flycheck-color-mode-line
              :ensure t
              :init   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))
  :config (progn
            (setq flycheck-highlighting-mode 'symbols)
            (set-face-attribute 'flycheck-error nil
                                :background "#660000"
                                :foreground nil)
            (set-face-attribute 'flycheck-warning nil
                                :background "#775500"
                                :foreground nil)
            (set-face-attribute 'flycheck-color-mode-line-error-face nil
                                :background "#660000"
                                :foreground nil)
            (set-face-attribute 'flycheck-color-mode-line-warning-face nil
                                :background "#775500"
                                :foreground nil)))

(use-package enh-ruby-mode
  :ensure      t
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
                 (use-package rspec-mode :ensure t)
                 (use-package inf-ruby
                   :ensure t
                   :config (inf-ruby-switch-setup)) ;; When you've hit the breakpoint, hit C-x C-q to enable inf-ruby
                 (use-package bundler :ensure t)
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


(use-package expand-region              ; Expand region by semantic units
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package hl-line                    ; Highlight the current line
  :init (global-hl-line-mode 1))

(use-package highlight-numbers          ; Fontify number literals
  :ensure t
  :defer t
  :init (highlight-numbers-mode))

(use-package web-mode
  :ensure t
  :mode   (("\\.erb$" . web-mode))
  :config (setq web-mode-code-indent-offset 2))

(use-package elixir-mode :ensure t)
(use-package go-mode :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Keybindings
;;
(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Functions
;;
(defun cleanup-buffer()
  "indent and clean buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/org/inbox.org")))
 '(package-selected-packages
   (quote
    (auto-package-update flycheck go-mode markdown-mode sql-indent elixir-mode web-mode use-package smartparens smart-mode-line rspec-mode obsidian-theme magit hydandata-light-theme highlight-numbers helm-projectile helm-ag flycheck-color-mode-line expand-region exec-path-from-shell enh-ruby-mode bundler ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
