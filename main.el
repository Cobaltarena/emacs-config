(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "No ssl found"))
  (add-to-list 'package-archives (cons "melpa"
  (concat proto "://melpa.org/packages/")) t)
  )
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(mapc 'load-file (directory-files "~/.emacs.d/emacs-config/lisp/" t "cb-custom.el$"))

(setq global-bindings
      (expand-file-name "emacs-config/global-bindings.org" user-emacs-directory))
(org-babel-load-file global-bindings)

(setq inhibit-startup-screen t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; hide tool bar (GUI only)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; hide scroll bar (GUI only)
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ; hide menu bar

(setq debug-on-error t ; show stack trace on config error
      vc-follow-symlinks t) ; always follow symlink

(add-hook 'display-line-numbers-mode-hook
	  (lambda ()
	    (set-face-attribute 'line-number nil
				:weight 'normal)
	    (set-face-attribute 'line-number-current-line nil
				:foreground (face-attribute 'cursor :background)
				:weight 'bold
				:slant 'normal)
            )          )

(add-hook 'c-mode-hook 'lsp)
  (add-hook 'cpp-mode-hook 'lsp)

  (with-eval-after-load 'lsp-mode
    (yas-global-mode)
    (require 'dap-cpptools)
)

(use-package aggressive-indent
  :straight (aggressive-indent :type git :host github
                               :repo "Malabarba/aggressive-indent-mode")
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (global-set-key (kbd "C-x a") 'aggressive-indent-mode)
  )

(use-package clang-format
    :straight (clang-format :type git :host github
                                 :repo "sonatard/clang-format")
  )

(setq company-config
      (expand-file-name "emacs-config/company-init.org" user-emacs-directory))
(org-babel-load-file company-config)

(use-package counsel
  :straight (swiper :type git :host github
                                 :repo "abo-abo/swiper")
  :config
  (counsel-mode 1)
  )

(use-package counsel-projectile
  :straight (counsel-projectile :type git :host github
                                :repo "ericdanan/counsel-projectile")
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (counsel-projectile-mode +1)
  )

(use-package dap-mode
      :straight (dap-mode :type git :host github
                                    :repo "emacs-lsp/dap-mode")
:custom
(lsp-enable-dap-auto-configure nil)
:config
(dap-ui-mode 1)
)
(global-unset-key (kbd "C-d"))
(add-hook 'c++-mode-hook
          (lambda()
            (local-unset-key (kbd "C-d"))))
(let ((bindings '(
                  ("C-a" . dap-ui-show-many-windows)
                  ("C-e" . dap-ui-hide-many-windows)
                  ("a" . dap-breakpoint-add)
                  ("d" . dap-breakpoint-delete)
                  ("c" . dap-breakpoint-delete-all)
                  ("n" . dap-next)
                  ("C-c" . dap-continue)
                  ("s" . dap-step-in)
                  ("r" . dap-step-out)
                  ("e" . dap-eval)
                  ("C-g" . dap-debug)
                  ("C-r" . dap-debug-restart)
                  ("g" . dap-debug-edit-template)
                  )))
  (dolist (binding bindings)
    (global-set-key (kbd (concat "C-d " (car binding))) (cdr binding))))

(setq dired-config
      (expand-file-name "emacs-config/dired-init.org" user-emacs-directory))
(org-babel-load-file dired-config)

(use-package doom-modeline
  :straight (doom-modeline :type git :host github
                                    :repo "seagle0128/doom-modeline")
  :init
  (doom-modeline-mode 1)
  :hook
  (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-minor-modes t)
  ;; (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-before-update-env-hook nil)
  (setq doom-modeline-after-update-env-hook nil)
  )

(use-package eterm-256color
  :straight (eterm-256color :type git :host github
                                    :repo "dieggsy/eterm-256color")
  :hook
  (term-mode . eterm-256color-mode)
  (term-mode . eterm-256color-mode)
  (vterm-mode . eterm-256color-mode)
  (eshell-mode . eterm-256color-mode)
  )

(use-package button-lock
  :defer t
  :straight (button-lock :type git :host github
				:repo "rolandwalker/button-lock"))
(use-package fixmee
  :defer t
  :straight (fixmee :type git :host github
				:repo "rolandwalker/fixmee")

  :config
  (global-fixmee-mode 1)
  )


;; fixmee-mode next/prev rebind and view list
(global-set-key [f6] 'fixmee-goto-next-by-position)
(global-set-key [f5] 'fixmee-goto-previous-by-position)
(global-set-key [f4] 'fixmee-view-listing)

(use-package gnus
  :defer t
  :straight (gnus :type git :host github
                              :repo "espenhw/gnus")
  :config
  (setq gnus-select-method '(nntp "news.epita.fr"))
  )

(use-package helm-gtags
  :straight (helm-gtags :type git :host github
                                :repo "emacsorphanage/helm-gtags")
  :config
  (helm-gtags-mode +1)
  (global-set-key (kbd "C-c r") 'helm-gtags-find-rtag)
  (global-set-key (kbd "C-c C-r") 'helm-gtags-find-tag-other-window)
  )

(use-package highlight-defined
  :straight (highlight-defined :type git :host github
                               :repo "Fanael/highlight-defined")
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
  )

(setq ivy-config
      (expand-file-name "emacs-config/ivy-init.org" user-emacs-directory))
(org-babel-load-file ivy-config)

(use-package keycast
  :defer t
  :straight (keycast :type git :host github
                     :repo "tarsius/keycast")
  :config
  ;; found on https://github.com/tarsius/keycast/issues/7#issuecomment-627604064
  ;; since I had the same issue with enabling keycast
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update t)
      (remove-hook 'pre-command-hook 'keycast--update)))
  (add-to-list 'global-mode-string '(mode-line-keycast ""))
  )

(use-package magit
  :straight (magit :type git :host github
                   :repo "magit/magit")
  :config
  (global-set-key (kbd "C-c C-g") 'magit)
  )

(use-package magit-todos
  :straight (magit-todos :type git :host github
                   :repo "alphapapa/magit-todos")
  :config
  (magit-todos-mode t)
  )

(use-package markdown-mode
  :straight (markdown-mode :type git :host github
                   :repo "jrblevin/markdown-mode")

  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  )

(use-package modern-sh
  :straight (modern-sh :type git :host github
                       :repo "damon-kwok/modern-sh")
  :config
  (add-hook 'sh-mode-hook 'modern-sh-mode)
  )

(setq org-config
      (expand-file-name "emacs-config/org-init.org" user-emacs-directory))
(org-babel-load-file org-config)

(use-package python-mode
:straight (python-mode :type git :host github
                     :repo "russell/python-mode")

  :hook (python-mode . lsp-deferred)
  :config
(progn
  (setq dap-python-executable "python3"
        dap-python-debugger 'debugpy
        aggressive-indent-mode nil)
  (require 'dap-python))
)

(use-package sphinx-doc
  :ensure t
  :hook (python-mode . sphinx-doc-mode)
  :config
  )

(use-package python-pytest
  :custom
  (python-pytest-confirm t))

(use-package rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(use-package smooth-scrolling
  :straight (smooth-scrolling :type git :host github
                              :repo "aspiers/smooth-scrolling")

  :config
  (smooth-scrolling-mode t)
  )

(use-package zerodark-theme
  :straight (zerodark-theme :type git :host github
                     :repo "NicolasPetton/zerodark-theme")
  :init
  (setq zerodark-enlarge-headings nil
        zerodark-alternate-mode-line-and-minibuffer t)
  :config
  (load-theme 'zerodark t)
  )

(use-package treemacs
:defer t
:config
(global-set-key [f12] 'treemacs)
(global-set-key (kbd "C-c i") 'treemacs-add-project-to-workspace)
(unbind-key "s" treemacs-mode-map)
(bind-key "s" #'treemacs-find-file treemacs-mode-map)
)

(use-package which-key
  :defer t
  :straight (which-key :type git :host github
                       :repo "justbur/emacs-which-key")

  :config
  (which-key-mode)
  (setq which-key-idle-delay 10000
        which-key-idle-secondary-delay 0.05
        which-key-popup-type 'minibuffer
        which-key-show-early-on-C-h t
        which-key-show-major-mode t)
  (global-set-key (kbd "C-x w") 'which-key-show-top-level)
  )

(setq bg "#222222")
(set-background-color bg)
(set-face-attribute 'cursor nil :background "#DD7538")

(set-face-foreground 'font-lock-string-face "light green")
(set-face-foreground 'font-lock-comment-face "green")
(set-face-foreground 'font-lock-comment-delimiter-face "green")

(set-face-attribute 'default nil
                    :family "MesloLGS NF"
                    :slant 'normal
                    :weight 'normal
                    :height 140
                    :width 'semi-condensed
                    )

(set-face-attribute 'fringe nil :background bg)
(setq-default left-fringe-width 5)

(global-hl-line-mode t)
(set-face-attribute 'hl-line nil
                    :background "#580818")

(global-linum-mode) ; show line numbers
(set-face-attribute 'line-number nil :background bg)
(set-face-attribute 'line-number-current-line nil :background bg)
(set-face-attribute 'linum nil :background bg)

(global-display-fill-column-indicator-mode 1)
(setq-default fill-column 80)
(set-face-attribute 'fill-column-indicator nil :foreground "#55342b")
(set-face-attribute 'fill-column-indicator nil :background "#55342b")

(global-whitespace-mode t)
(setq whitespace-display-mappings
      '(
        (newline-mark 10
                      [5321 10])
        (tab-mark 9
                  [9655 9]
                  [92 9])
        )
      )
(setq whitespace-style
      '(
        face ; show ...
        tabs tab-mark ; the tabulations,
        newline-mark
        newline
        trailing
        )
      )
  (set-face-attribute 'whitespace-newline nil :foreground "#A68064")
  (set-face-attribute 'whitespace-space nil :foreground "#A68064")
  (set-face-attribute 'whitespace-space nil :background bg)

(setq backup-directory-alist '(("." . "~/local/emacs_tf"))
      backup-by-copying t)

(setq c-basic-offset 4 ; spaces of indentation
      c-default-style "bsd" ; sort of fits the coding style
      fill-column 80) ; 80 columns rule

(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

(setq-default cursor-type 'hollow)
(setq-default cursor-type 'box)

(show-paren-mode 1)
(setq show-paren-style 'expression)
(set-face-attribute 'show-paren-match nil
              :foreground "#00FFFF")
