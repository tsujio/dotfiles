;;;; Change backup and auto-save dir
(let ((backup-dir (expand-file-name "backup" user-emacs-directory)))
  ;; Make directory if not exists
  (if (not (file-exists-p backup-dir))
      (make-directory backup-dir))

  ;; Set backup dir to backup-dir
  (setq backup-directory-alist
        (cons (cons "\\.*$" (expand-file-name backup-dir))
              backup-directory-alist))

  ;; Set auto-save dir to backup-dir
  (setq auto-save-file-name-transforms
        `((".*" ,(expand-file-name backup-dir) t)))

  ;; Delete old backups
  (dolist (file (directory-files backup-dir))
    (let ((path (concat backup-dir file)))
      (if (and (file-regular-p path)
	       (time-less-p (nth 5 (file-attributes path))
			    (time-subtract (current-time) (days-to-time 31))))
	  (progn (delete-file path)
		 (message "deleted: %s" path))
	))))
