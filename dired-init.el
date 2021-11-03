(use-package all-the-icons-dired
  :straight (all-the-icons-dired :type git :host github
                                 :repo "jtbm37/all-the-icons-dired")
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

(use-package diredful
  :defer t
  :straight (diredful :type git :host github
                                 :repo "thamer/diredful")
  :config
  (diredful-mode 1)
  )

(use-package dired-git-info
  :defer t
  :config
  ;; uncomment to stop hiding details such as ownership, permissions or size
  ;; when toggling dired-git
  (setq dgi-auto-hide-details-p nil)
  )
(with-eval-after-load 'dired
  (define-key dired-mode-map ")" 'dired-git-info-mode)
  )

(use-package dired-quick-sort
  :straight (dired-quick-sort :type git :host github
                                 :repo "xuhdev/dired-quick-sort")
  :config
  (dired-quick-sort-setup)
  )
