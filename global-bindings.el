(global-set-key (kbd "C-c C-f") 'clang-format-buffer)

(global-set-key (kbd "C-c d") 'comment-or-uncomment-region)

(global-set-key (kbd "C-x C-p") 'counsel-compile)

(global-set-key (kbd "C-c m") 'cb/reload-config)

(global-set-key (kbd "C-c C-d") 'docker)

(global-unset-key [f3])
(global-set-key [f3] 'next-error)

(global-unset-key [f2])
(global-set-key [f2] 'previous-error)

(global-unset-key (kbd "C-x m"))
(global-set-key (kbd "C-x m") 'indent-region)

(global-set-key (kbd "C-x j") 'ivy-magit-todos)

(global-set-key (kbd "C-q") 'move-beginning-of-line)
(global-set-key (kbd "M-z") 'kill-ring-save)
(global-set-key (kbd "C-z") 'kill-region)

(global-set-key (kbd "C-x C-m") 'helm-man-woman)

(global-set-key (kbd "C-c C-g") 'magit)

(global-set-key (kbd "C-x p") 'package-install)

(global-set-key (kbd "C-c <C-left>") 'windmove-left)
(global-set-key (kbd "C-c <C-right>") 'windmove-right)
(global-set-key (kbd "C-c <C-up>") 'windmove-up)
(global-set-key (kbd "C-c <C-down>") 'windmove-down)

(global-set-key (kbd "C-x C-h") 'cb/pop-shell)
