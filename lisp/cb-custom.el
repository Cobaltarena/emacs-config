(defun cb/pop-shell ()
  (interactive)
  (setq shell-name (read-string "Shell Name: " nil))
  (vterm-other-window)
  (rename-buffer (concat "<SH> " shell-name)))

(defun cb/reload-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el")
  )

(defun cb/add-hooks (hook-list)
  "perform add-hook on every elements of the list"
  (mapcar (lambda (hook-list)
            (funcall 'add-hook hook-list 'company-mode) hook-list)
          hook-list))

(defun cb/template ()
  "paste the template arg located in the file arg"
  (interactive)
  (progn
    (insert-file-contents (read-file-name "Template file: " "~/.emacs.d/emacs-config/template/")))
  )

(provide 'cb-custom)
