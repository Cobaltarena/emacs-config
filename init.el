(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "No ssl found"))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t
               )
  )

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/init.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-color-icon t)
 '(company-box-icons-alist 'company-box-icons-all-the-icons)
 '(company-idle-delay 0.2)
 '(company-minimum-prefix-length 1)
 '(company-tooltip-idle-delay 0.2)
 '(company-tooltip-minimum-width 20)
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(irony-eldoc company-c-headers company-irony irony all-the-icons-gnus company-box company-try-hard slime-company minimap yaml-mode bison-mode cmake-mode markdown-mode gh-md highlight-doxygen counsel-projectile highlight-defined counsel-world-clock all-the-icons-ivy-rich all-the-icons counsel ivy projectile which-key dired-k aggressive-indent modern-sh darcula-theme clang-format selectrum-prescient selectrum magit pdf-tools smooth-scrolling rust-mode))
 '(tooltip-delay 0.4))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview-common ((t (:background "#44475a" :foreground "#0189cc"))))
 '(company-tooltip ((t (:background "#332211"))))
 '(company-tooltip-common ((t (:weight bold)))))


;; --------------------------------------------------------
;; <-------------------- HOOKS --------------------------->
;; --------------------------------------------------------


(add-hook 'display-line-numbers-mode-hook
	  (lambda ()
	    (set-face-attribute 'line-number nil
				:weight 'normal)
	    (set-face-attribute 'line-number-current-line nil
				:foreground (face-attribute 'cursor :background)
				:weight 'bold
				:slant 'normal)
            )
          )



;; ########################################################


;; --------------------------------------------------------
;; <---------------- USE-PACKAGE ------------------------->
;; --------------------------------------------------------


(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (global-set-key (kbd "C-x a") 'aggressive-indent-mode)
  )

(use-package clang-format
  :config
  (global-set-key (kbd "C-c C-f") 'clang-format-buffer)
  )

;; ---------------------- COMPANY -------------------------


(use-package company
  :config
  (company-mode +1)
  (global-company-mode +1)
  (setq company-selection-wrap-around +1
        company-tooltip-minimum-width 60
        company-tooltip-maximum-width 60)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  :custom-face
  (company-tooltip (
                    (t (:background "#332211")))
                   )
  )
(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony)
  )

(use-package irony
  :ensure t
  :config
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )


(use-package irony-eldoc
  :ensure t
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc)
  )

;; --------------------------------------------------------


(use-package cmake-mode
  :config
  (setq cmake-mode +1)
  )


(use-package counsel
  :config
  (counsel-mode 1)
  )


(use-package counsel-projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (counsel-projectile-mode +1)
  )


(use-package dracula-theme
  :init
  (setq dracula-enlarge-headings nil
        dracula-alternate-mode-line-and-minibuffer t)
  :config
  (load-theme 'dracula t)
  )


;; newsgroup EPITA
(use-package gnus
  :config
  (setq gnus-select-method '(nntp "news.epita.fr"))
  )


(use-package highlight-defined
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
  )

;; ---------------------- IVY -----------------------------

(use-package all-the-icons-ivy
  :init
  (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  )


(use-package ivy
  :config
  (ivy-mode 1)
  ;; ??? TODO
  (setq
   enable-recursive-minibuffers t
   ivy-display-style 'fancy
   ivy-initial-inputs-alist nil
   ivy-use-virtual-buffers t
   ivy-wrap t)
  ;;rebind buffer list switch
  (global-set-key (kbd "<C-tab>") 'ivy-switch-buffer)
  )


(use-package ivy-rich
  :config
  ;; fancier ivy
  (ivy-rich-mode 1)
  )


(use-package swiper
  :config
  (global-unset-key (kbd "C-s"))
  (global-set-key (kbd "C-s") 'swiper)

  )

;; --------------------------------------------------------

(use-package magit
  :config
  (global-set-key (kbd "C-c C-g") 'magit)
  )


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  )


;; minimap
(use-package minimap
  :init
  (setq minimap-update-delay 0
        minimap-window-location 'right)
  :config
  (minimap-mode)
  (global-set-key (kbd "C-c m") 'minimap-mode)
  )


(use-package modern-sh
  :config
  (add-hook 'sh-mode-hook 'modern-sh-mode)
  )


;; Projectile config
(use-package projectile
  :config
  (projectile-mode +1)
  )


(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  )

;; -------------------- Eval after load -------------------
(eval-after-load 'company
  '(add-to-list 'company-backend 'company-irony))


;; ########################################################


;; --------------------------------------------------------
;; <---------------------- BINDINGS ---------------------->
;; --------------------------------------------------------


;;rebind compile
(global-set-key (kbd "C-x C-p") 'counsel-compile)


;;rebind next error and previous error to make them easier to access
(global-unset-key [f3])
(global-set-key [f3] 'next-error)


(global-unset-key [f2])
(global-set-key [f2] 'previous-error)


;; Rebind for qwerty layout
(global-set-key (kbd "C-q") 'move-beginning-of-line)
(global-set-key (kbd "M-z") 'kill-ring-save)
(global-set-key (kbd "C-z") 'kill-region)


;; ########################################################


;; --------------------------------------------------------
;; <---------------------- FACES ------------------------->
;; --------------------------------------------------------

;; maximum size of line config
(global-display-fill-column-indicator-mode 1)
(setq-default fill-column 80)
(set-face-attribute 'fill-column-indicator nil :foreground "#55342b")
(set-face-attribute 'fill-column-indicator nil :background "#55342b")

;; font config
(set-face-attribute 'default nil
		    :family "JetBrains Mono"
		    :foundry "outline"
		    :slant 'normal
		    :weight 'normal
		    :height 130
		    :width 'semi-condensed
                    )
(global-hl-line-mode t)
(set-face-attribute 'hl-line nil
                    :background "#580818")


;;modify comment colors
(set-face-foreground 'font-lock-string-face "light green")
(set-face-foreground 'font-lock-comment-face "green")
(set-face-foreground 'font-lock-comment-delimiter-face "green")


;; background config
(setq bg "#222222")
(set-background-color bg)
(set-face-attribute 'cursor nil :background "#DD7538")


;; fringe
(set-face-attribute 'fringe nil :background bg)
(setq-default left-fringe-width 5)


;; linum stuff
(global-linum-mode) ; show line numbers
(set-face-attribute 'line-number nil :background bg)
(set-face-attribute 'line-number-current-line nil :background bg)
(set-face-attribute 'linum nil :background bg)


;; Whitespace config
(global-whitespace-mode t)
(setq whitespace-display-mappings
      '(
        (spaces 32 [183] [46])
        (space-mark 32 [183] [46])
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


;; ########################################################



;; disable shift selection with Shift + arrow
(setq shift-select-mode nil)


;; EPITA DEFAULT CONFIG ING1
;; General Emacs configuration
(setq debug-on-error t ; show stack trace on config error
      vc-follow-symlinks t) ; always follow symlink

; Basic interface configuration
(tool-bar-mode -1) ; hide tool bar (GUI only)
(scroll-bar-mode -1) ; hide scroll bar (GUI only)
(menu-bar-mode -1) ; hide menu bar

(column-number-mode) ; show column number in the modeline



;; Disable tabulations (repeated to ensure compatibility with any major mode)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; Basic C configuration
(setq c-basic-offset 4 ; spaces of indentation
      c-default-style "bsd" ; sort of fits the coding style
      fill-column 80) ; 80 columns rule


;;modify backup files directory
(setq backup-directory-alist '(("." . "~/local/emacs_tf"))
      backup-by-copying t)


;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line

(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

