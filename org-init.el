(use-package org
  :straight (org :type git :host github :repo "bzg/org-mode")
  :config
  (setq org-src-tab-acts-natively t)
  (global-set-key (kbd "C-c C-c") 'org-edit-special)

  )
  (setq org-html-validation-link nil)

(use-package org-bullets
 :straight (org-bullets :type git :host github :repo "sabof/org-bullets")
 :hook (org-mode . org-bullets-mode)
 :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(with-eval-after-load 'org
    ;; This is needed as of Org 9.2
    (require 'org-tempo)

    (add-to-list 'org-structure-template-alist '("csrc" . "src c"))
    (add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("http" . "src http"))
    (add-to-list 'org-structure-template-alist '("java" . "src java"))
    (add-to-list 'org-structure-template-alist '("json" . "src json"))
    (add-to-list 'org-structure-template-alist '("latex" . "src latex"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
    (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))

)

;; (use-package org-appear
;;   :straight (org-appear :type git :host github :repo "awth13/org-appear")
;;   :hook (org-mode . org-appear-mode)
;;   :config
;;   (setq org-hide-emphasis-markers t)
;;   )

(use-package org-bars
  :straight (org-bars :type git :host github :repo "tonyaldon/org-bars")
  :config
  (org-bars-mode)
  )

;;  (use-package org-roam
;;    :ensure t
;;    :init
;;    (setq org-roam-v2-ack t)
;; ;;   :hook
;; ;;   (after-init . org-roam-mode)
;;   :config
;;   (org-roam-db-autosync-mode)
;;   (setq org-roam-completion-everywhere t
;;         org-roam-completion-system 'default
;;         org-roam-directory "~/Notes/")
;;    :bind (:map org-roam-mode-map
;;               (("C-c n l"   . org-roam)
;;                ("C-c n f"   . org-roam-find-file)
;;                ("C-c n g"   . org-roam-graph))
;;               :map org-mode-map
;;               (("C-c n i" . org-roam-insert))
;;               (("C-c n I" . org-roam-insert-immediate))))

;; (use-package deft
;;   :commands (deft)
;;   :config
;;   (setq deft-directory "~/Notes/Roam"
;;                  deft-recursive t
;;                  deft-extensions '("md" "org"))
;;    )

(with-eval-after-load 'org
    ;; This is needed as of Org 9.2
    (require 'org-tempo)

    (add-to-list 'org-structure-template-alist '("csrc" . "src c"))
    (add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("http" . "src http"))
    (add-to-list 'org-structure-template-alist '("java" . "src java"))
    (add-to-list 'org-structure-template-alist '("json" . "src json"))
    (add-to-list 'org-structure-template-alist '("latex" . "src latex"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
    (add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))

)
