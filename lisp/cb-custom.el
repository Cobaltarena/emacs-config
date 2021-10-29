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

(defcustom cb/notes-directory "~/quick_notes"
  "custom directory in which quick notes should be saved"
  :type 'string)

(defun cb/quick-notes ()
  (interactive)
  (unless (file-directory-p cb/note-directory)
    (make-directory cb/note-directory))
  (setq file-name (read-string "FileName: " nil))
  (setq path (concat (file-name-as-directory cb/note-directory) file-name))
  (make-empty-file path)
  (switch-to-buffer path)
  )

(provide 'cb-custom)
