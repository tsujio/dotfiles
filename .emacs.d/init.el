;; Load elisps in the directory in order
(let ((elisp-dir (expand-file-name "elisp" user-emacs-directory)))
  (let ((elisps (directory-files elisp-dir nil ".*el$")))
    (dolist (el (sort elisps 'string<))
      (load-file (concat (file-name-as-directory elisp-dir) el)))))
