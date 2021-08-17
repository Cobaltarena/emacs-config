(defun cb/reload-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el")
  )

(defun cb/add-hooks (hook-list)
  "perform add-hook on every elements of the list"
  (mapcar (lambda (hook-list)
            (funcall 'add-hook hook-list 'company-mode) hook-list)
          hook-list))

(provide 'cb-custom)
