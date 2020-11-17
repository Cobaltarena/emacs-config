;;; highlight-doxygen-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "highlight-doxygen" "highlight-doxygen.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from highlight-doxygen.el

(autoload 'highlight-doxygen-mode "highlight-doxygen" "\
Minor mode that highlights Doxygen comments.

If called interactively, enable Highlight-Doxygen mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(put 'highlight-doxygen-global-mode 'globalized-minor-mode t)

(defvar highlight-doxygen-global-mode nil "\
Non-nil if Highlight-Doxygen-Global mode is enabled.
See the `highlight-doxygen-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `highlight-doxygen-global-mode'.")

(custom-autoload 'highlight-doxygen-global-mode "highlight-doxygen" nil)

(autoload 'highlight-doxygen-global-mode "highlight-doxygen" "\
Toggle Highlight-Doxygen mode in all buffers.
With prefix ARG, enable Highlight-Doxygen-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Highlight-Doxygen mode is enabled in all buffers where
`(lambda nil (when (apply #'derived-mode-p highlight-doxygen-modes) (highlight-doxygen-mode 1)))' would do it.
See `highlight-doxygen-mode' for more information on Highlight-Doxygen mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "highlight-doxygen" '("highlight-doxygen-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highlight-doxygen-autoloads.el ends here
