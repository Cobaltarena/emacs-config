(use-package ivy
  :config
  (ivy-mode 1)
  ;; ??? TODO
  (setq
   enable-recursive-minibuffers t
   ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create
   ivy-display-style 'fancy
   ivy-initial-inputs-alist nil
   ivy-use-virtual-buffers t
   ivy-wrap t)
  (global-unset-key (kbd "C-x b"))
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
  )

(use-package all-the-icons-ivy
  :init
  (add-hook 'after-init-hook 'all-the-icons-ivy-setup)
  )

(use-package ivy-rich
  :config
  ;; fancier ivy
  (ivy-rich-mode 1)
  )

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1)
  )

(use-package swiper
  :config
  (global-unset-key (kbd "C-s"))
  (global-set-key (kbd "C-s") 'swiper)
  (global-unset-key (kbd "C-r"))
  (global-set-key (kbd "C-r") 'swiper-backward)
  )
