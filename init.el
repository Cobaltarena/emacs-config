(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
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
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(cmake-mode markdown-mode gh-md highlight-doxygen counsel-projectile highlight-defined counsel-world-clock all-the-icons-ivy-rich all-the-icons counsel ivy projectile which-key dired-k aggressive-indent modern-sh darcula-theme clang-format selectrum-prescient selectrum magit pdf-tools smooth-scrolling rust-mode))

 )


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'display-line-numbers-mode-hook
	  (lambda ()
	    (set-face-attribute 'line-number nil
				:weight 'normal)
	    (set-face-attribute 'line-number-current-line nil
				:foreground (face-attribute 'cursor :background)
				:weight 'bold
				:slant 'normal)))


(use-package highlight-defined
  :config
  (add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
  )

(use-package dracula-theme
  :init
  (setq dracula-enlarge-headings nil)
  (setq dracula-alternate-mode-line-and-minibuffer t)
  :config
  (load-theme 'dracula t)
  )

(set-face-attribute 'default nil
		    :family "JetBrains Mono"
		    :foundry "outline"
		    :slant 'normal
		    :weight 'normal
		    :height 130
		    :width 'semi-condensed)

;;CMake
(require 'cmake-mode)



;; Magit
(use-package magit
  :config
  (global-set-key (kbd "C-c C-g") 'magit)
  )

;; Projectile config
(use-package projectile
  :config
  (projectile-mode +1)
  )

;; Counsel config
(use-package counsel
 :config
 (counsel-mode 1)
 )



(use-package counsel-projectile
 :init
 (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
 :config
 (counsel-projectile-mode +1))

;; Ivy config
(use-package all-the-icons-ivy
  :init
  (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  )

(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  )

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)
  )

(use-package all-the-icons-ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1)
  )


(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  )

(use-package modern-sh
  :config
  (add-hook 'sh-mode-hook 'modern-sh-mode)
  )

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  )

;; markdown-config
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  )

(setq shift-select-mode nil)


;;rebind buffer list switch
(global-set-key (kbd "<C-tab>") 'ivy-switch-buffer)

;;rebind compile
(global-set-key (kbd "C-x C-a") 'counsel-compile)

;;rebind next error on f2
(global-unset-key [f3])
(global-set-key [f3] 'next-error)

(global-unset-key [f2])
(global-set-key [f2] 'previous-error)

;; EPITA DEFAULT CONFIG ING1
;; General Emacs configuration
(setq debug-on-error t ; show stack trace on config error
      vc-follow-symlinks t) ; always follow symlink

; Basic interface configuration
(tool-bar-mode -1) ; hide tool bar (GUI only)
(scroll-bar-mode -1) ; hide scroll bar (GUI only)
(menu-bar-mode -1) ; hide menu bar
(global-linum-mode) ; show line numbers
(column-number-mode) ; show column number in the modeline


(global-display-fill-column-indicator-mode 1)
(setq-default fill-column 80)
(set-face-attribute 'fill-column-indicator nil :foreground "#55342b")
(set-face-attribute 'fill-column-indicator nil :background "#55342b")

(global-hl-line-mode t)
(set-face-attribute 'hl-line nil :background "#580818")

;; Disable tabulations (repeated to ensure compatibility with any major mode)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; Basic C configuration
(setq c-basic-offset 4 ; spaces of indentation
      c-default-style "bsd" ; sort of fits the coding style
      fill-column 80) ; 80 columns rule




;;modify backup files directory

(setq backup-directory-alist '(("." . "~/local/emacs_tf")))
(setq backup-by-copying t)

;;modify comment colors

(set-face-foreground 'font-lock-string-face "light green")
(set-face-foreground 'font-lock-comment-face "green")
(set-face-foreground 'font-lock-comment-delimiter-face "green")

;; modify background colors

(setq bg "#222222")
(set-background-color bg)
(set-face-attribute 'cursor nil :background "#DD7538")
(set-face-attribute 'fringe nil :background bg)
(set-face-attribute 'line-number nil :background bg)
(set-face-attribute 'line-number-current-line nil :background bg)
(set-face-attribute 'linum nil :background bg)

;;modify fringe
(setq-default left-fringe-width 5)


;; Rebind for qwerty layout
(global-set-key (kbd "C-q") 'move-beginning-of-line)
(global-set-key (kbd "M-z") 'kill-ring-save)
(global-set-key (kbd "C-z") 'kill-region)

;; Redefine TAB
(define-key input-decode-map [?\C-i] [C-i])

;; Rebind arrows.
(global-set-key (kbd "<C-i>") 'previous-line)
(global-set-key (kbd "C-j") 'left-char)
(global-set-key (kbd "C-l") 'right-char)
(global-set-key (kbd "C-k") 'next-line)

(global-set-key (kbd "C-S-i") 'backward-paragraph)
(global-set-key (kbd "C-S-j") 'left-word)
(global-set-key (kbd "C-S-l") 'right-word)
(global-set-key (kbd "C-S-k") 'forward-paragraph)

;; Remove new line keybind and rebind it as insert new char
(defun insert-space ()
  (interactive)
  (insert-char 32))

(global-unset-key (kbd "C-o"))
(global-set-key (kbd "C-o") 'insert-space)
;; Set C-u as backspace

(global-unset-key (kbd "C-u"))
(define-key key-translation-map (kbd "C-u") (kbd "DEL"))
;; Set C-p as ENTER
(global-unset-key (kbd "C-p"))
(define-key key-translation-map (kbd "C-p") (kbd "RET"))

;; newsgroup EPITA
(use-package gnus
  :config
  (setq gnus-select-method '(nntp "news.epita.fr"))
  )

;; clang-format
(require 'clang-format)
(global-set-key (kbd "C-c C-f") 'clang-format-buffer)



;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line

(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line


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


(setq whitespace-style '(
                         face ; show ...
                         tabs tab-mark ; the tabulations,
                         newline-mark
                         newline
                         spaces
                         trailing
                         )
      )


(set-face-attribute 'whitespace-newline nil :foreground "#A68064")
(set-face-attribute 'whitespace-space nil :foreground "#A68064")
(set-face-attribute 'whitespace-space nil :background bg)
