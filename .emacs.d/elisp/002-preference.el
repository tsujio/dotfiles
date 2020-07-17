;; Color
(set-face-background 'default "black")
(set-face-foreground 'default "gray")

;; Frame size
(set-frame-size (selected-frame) 90 40)

;; Frame potision
(let ((geometry (car (car (display-monitor-attributes-list)))))
  (let ((center-x (/ (nth 3 geometry) 2))
        (center-y (/ (nth 4 geometry) 2)))
    (let ((x (- center-x (/ (frame-outer-width) 2)))
          (y (- center-y (/ (frame-outer-height) 2))))
      (set-frame-position (selected-frame) x y))))

;; Line number
(global-linum-mode)

;; Column number
(column-number-mode t)

;; Display file name on title bar
(setq frame-title-format (format "Emacs@%s : %%f" (system-name)))

;; Suppress beep
(setq ring-bell-function 'ignore)

;; Font setting
;; (you should install Ricty font at first)
(add-to-list 'default-frame-alist '(font . "Ricty Diminished"))

;; Set default encoding to UTF-8 and unix style line encoding
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Auto reload buffer
(global-auto-revert-mode 1)

;; Highlight corresponding parenthesis
(show-paren-mode 1)

;; Display line coding
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; Hide tool bar
(tool-bar-mode 0)

;;;; Visualization settings of spaces and newlines
(global-whitespace-mode 1)

;; Define space character
(setq whitespace-space-regexp "\x3000+")

;; Change newline color
(set-face-foreground 'whitespace-newline "gray60")

;; Exclude half-width space and newline
(dolist (d '((space-mark ?\ ) (newline-mark ?\n)))
  (setq whitespace-display-mappings
        (cl-delete-if
         '(lambda (e) (and (eq (car d) (car e))
                           (eq (cadr d) (cadr e))))
         whitespace-display-mappings)))

;; Add full-width space and newline
(dolist (e '((space-mark   ?\x3000 [?\u25A1])
             (newline-mark ?\n     [?\u21B5 ?\n] [?$ ?\n])))
  (add-to-list 'whitespace-display-mappings e))

;; Exclude items which are not expected to be emphasized
(dolist (d '(face lines space-before-tab
                  indentation empty space-after-tab tab-mark))
  (setq whitespace-style (delq d whitespace-style)))

;;; Prevent Extraneous Tabs
(setq-default indent-tabs-mode nil)
