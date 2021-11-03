(use-package company
  :defer t
  :straight (company :type git :host github :repo "company-mode/company-mode")
  :config
  (progn
  (setq company-selection-wrap-around +1
        company-tooltip-minimum-width 60
        company-tooltip-maximum-width 60)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "<right>") #'company-complete-selection)
  (global-set-key (kbd "C-c c") 'company-complete)
  )
  :custom-face
  (company-tooltip (
                     (t (:background "#332211")))
                   )
  )

(use-package company-irony
  :defer t
  :config
  (progn
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony-c-headers))
  ))

(use-package irony
  :defer t
  :config
  (progn
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  ))

(use-package irony-eldoc
  :defer t
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc)
  )

(use-package company-prescient
  :after company
  :config
  (setq company-prescient-mode 1)
  )

(use-package company-jedi
  :defer t
  :straight (company-jedi :type git :host github :repo "emacsorphanage/company-jedi")
  :config
  (progn
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'jedi-mode)
  ))

(cb/add-hooks (list #'c++-mode-hook
                    #'c-mode-hook
                    #'cider-mode-hook
                    #'cider-repl-mode-hook
                    #'lisp-mode-hook
                    #'emacs-lisp-mode-hook
                    #'org-mode-hook
                    #'python-mode-hook
                    #'rust-mode-hook))
