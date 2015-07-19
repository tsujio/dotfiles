;;;; package
(when (require 'package nil t)
  ;; Add package-archives
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
	       '("marmalade" .
		 "http://marmalade-repo.org/packages/"))

  ;; Setup elisp directory
  (let ((target-dir "~/.emacs.d/elisp"))
    ;; Make directory if not exists
    (if (not (file-exists-p target-dir))
	(make-directory target-dir))
    ;; Add to load-path
    (add-to-list 'load-path target-dir)
    ;; Set as install directory
    (setq package-user-dir target-dir))

  ;; Initialize
  (package-initialize)

  ;; My Packages
  (setq my-packages
	'(undo-tree
	  tabbar
	  auto-complete
	  ac-math
	  flymake-cursor
	  flymake-python-pyflakes
	  flymake-ruby
	  ruby-electric
	  yasnippet
	  web-mode
	  scala-mode2
	  init-loader
      yaml-mode
	  ))

  ;; Install packages
  (when (require 'cl nil t)
    (let ((not-installed-list
	   (remove-if (lambda (x) (package-installed-p x))
		      my-packages)))
      (when not-installed-list
	(package-refresh-contents)
	(dolist (x not-installed-list)
	  (package-install x)
	  (message "installed %s." x))))))

;;;; Preference
;; Window
(setq default-frame-alist
      (append (list '(foreground-color . "gray")
                    '(background-color . "black")
                    '(border-color . "black")
                    '(mouse-color . "white")
                    '(width . 120)
                    '(height . 40)
                    '(top . 100)
                    '(left . 200))
              default-frame-alist))

;; Display line number
(require 'linum)
(global-linum-mode)

;; Display column number
(column-number-mode t)

;; Display file name on title bar
(setq frame-title-format (format "Emacs@%s : %%f" (system-name)))

;; Suppress beep
;(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Set default encoding to UTF-8
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

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
        (delete-if
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

;;;; undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;;;; Tabbar
(require 'tabbar)
(tabbar-mode)

;; Not use mouse wheel on tabs
(tabbar-mwheel-mode nil)

;; Not group tabs
(setq tabbar-buffer-groups-function nil)

;; Disable a button on the left side
(dolist (btn '(tabbar-buffer-home-button
	       tabbar-scroll-left-button
	       tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
		 (cons "" nil))))

;; Interval between tabs
(setq tabbar-separator '(0.8))

;; Preference
(set-face-attribute
 'tabbar-default nil
 :family (face-attribute 'default :family)
 :background (face-attribute 'mode-line-inactive :background)
 :height 0.9)
(set-face-attribute
 'tabbar-unselected nil
 :background (face-attribute 'mode-line-inactive :background)
 :foreground (face-attribute 'mode-line-inactive :foreground)
 :box nil)
(set-face-attribute
 'tabbar-selected nil
 :background (face-attribute 'mode-line :background)
 :foreground (face-attribute 'mode-line :foreground)
 :box nil)

;;;; Buffer functions and preferences
;; Window Resizer
;;   - http://d.hatena.ne.jp/mooz/20100119/p1
;;   - http://d.hatena.ne.jp/khiker/20100119/window_resize
(defun window-resizer ()
  "Control window size and position."
  (interactive)
  (let ((window-obj (selected-window))
        (current-width (window-width))
        (current-height (window-height))
        (dx (if (= (nth 0 (window-edges)) 0) 1
              -1))
        (dy (if (= (nth 1 (window-edges)) 0) 1
              -1))
        action c)
    (catch 'end-flag
      (while t
        (setq action
              (read-key-sequence-vector (format "size[%dx%d]"
                                                (window-width)
                                                (window-height))))
        (setq c (aref action 0))
        (cond ((equal c 'right)
               (enlarge-window-horizontally dx))
              ((equal c 'left)
               (shrink-window-horizontally dx))
              ((equal c 'down)
               (enlarge-window dy))
              ((equal c 'up)
               (shrink-window dy))
              ;; otherwise
              (t
               (let ((last-command-char (aref action 0))
                     (command (key-binding action)))
                 (when command
                   (call-interactively command)))
               (message "Quit")
               (throw 'end-flag t)))))))

(global-set-key "\C-c\C-r" 'window-resizer)

;; Move window by Ctrl + cursor key
(global-set-key (kbd "C-<left>")  'windmove-left)
(global-set-key (kbd "C-<down>")  'windmove-down)
(global-set-key (kbd "C-<up>")    'windmove-up)
(global-set-key (kbd "C-<right>") 'windmove-right)

;; Auto reload of buffer
(global-auto-revert-mode 1)

;; Scroll by mouse wheel
(defun scroll-down-with-lines ()
  "" (interactive) (scroll-down 1))
(defun scroll-up-with-lines ()
   "" (interactive) (scroll-up 1))
(global-set-key [wheel-up] 'scroll-down-with-lines)
(global-set-key [wheel-down] 'scroll-up-with-lines)

;; Scroll by single line
(setq scroll-step 1)

;; Hi-lock mode
(global-hi-lock-mode 1)
(setq hi-lock-file-patterns-policy t)

;; Toggle truncate line
(defun toggle-truncate-lines ()
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))

(global-set-key "\C-c\C-l" 'toggle-truncate-lines)

;;;; Default indent setting
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;;;; ac-math
(require 'ac-math)

(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
        (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                ac-sources))
  )

(add-hook 'latex-mode-hook 'ac-latex-mode-setup)

;;;; flymake
(when (require 'flymake nil t)
  (global-set-key "\M-p" 'flymake-goto-prev-error)
  (global-set-key "\M-n" 'flymake-goto-next-error))

;;;; flymake-python-pyflakes
;;
;; - Installation on Windows
;;   - Download and install setup tools (https://pypi.python.org/pypi/setuptools/)
;;   - Add "C:\Python27\Scripts" to Path
;;   - Install pip (>easy_install pip)
;;   - Install pyflakes (>pip install --upgrade pyflakes)
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)

;;;; flymake-ruby
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;;;; indent setting for ruby-mode
(setq ruby-deep-indent-paren-style nil)

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;;;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;;;; Set backup and auto-save dir to ~/.emacs.d/backup
(let ((backup-dir "~/.emacs.d/backup"))
  ;; Make directory if not exists
  (if (not (file-exists-p backup-dir))
      (make-directory backup-dir))

  ;; Set backup dir to backup-dir
  (setq backup-directory-alist
	(cons (cons "\\.*$" (expand-file-name backup-dir))
	      backup-directory-alist))

  ;; Set auto-save dir to backup-dir
  (setq auto-save-file-name-transforms
	`((".*" ,(expand-file-name backup-dir) t))))

;;;; Delete old backups
(defun delete-old-backups ()
  (setq backup-dir "~/.emacs.d/backup/")
  (setq backups (directory-files backup-dir))
  (while backups
    (setq file (concat backup-dir (car backups)))
    (if (and (file-regular-p file)
	     (time-less-p (nth 5 (file-attributes file))
			  (time-subtract (current-time) (days-to-time 31))))
	(progn (delete-file file)
	       (message "deleted: %s" file))
      )
    (setq backups (cdr backups))))

(delete-old-backups)

;;;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-menu-map t)

;;;; Insert encoding pragma
(defun insert-encoding-pragma ()
  "Insert encoding pragma for each programming language"
  (interactive)
  (let* ((extension (insert-encoding-get-file-extension (buffer-name)))
         (comment-char (insert-encoding-get-comment-char extension))
	 (pragma (format "%s -*- coding: %S -*-" comment-char (my-short-buffer-file-coding-system))))
    (progn (beginning-of-line)
    (progn (beginning-of-line)
           (insert-string pragma)))))

(defun my-short-buffer-file-coding-system (&optional default-coding)
  (let ((coding-str (format "%S" buffer-file-coding-system)))
    (cond ((string-match "shift-jis" coding-str) 'shift_jis)
	  ((string-match "euc-jp" coding-str) 'euc-jp)
	  ((string-match "utf-8" coding-str) 'utf-8)
	  (t (or default-coding 'utf-8)))))

(defun insert-encoding-get-comment-char (extension)
  (let ((sharp-langs '("sh" "pl" "t" "pm" "rb" "py"))
        (slash-langs '("c" "h" "cpp"))
        (semicolon-langs '("gosh" "el" "scm" "lisp")))
    (cond ((member extension sharp-langs) "#")
          ((member extension slash-langs) "//")
          ((member extension semicolon-langs) ";;")
          (t ""))))

(defun insert-encoding-get-file-extension (filename)
  (if (string-match "\\.\\([a-zA-Z0-9]+\\)$" filename)
      (substring filename (match-beginning 1) (match-end 1))))

;;;; flymake Tex
(defun flymake-get-tex-args (file-name)
    (list "latex" (list "-file-line-error-style" file-name)))

;;;; web-mode
;; Set target exts
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.php5?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$"     . web-mode))

;; Add my snippets dir
(defun add-web-mode-snippets ()
  (let ((my-snippet-dir "~/.emacs.d/snippets"))
    (if (not (find my-snippet-dir yas-snippet-dirs))
      (add-to-list 'yas-snippet-dirs my-snippet-dir))))

(add-web-mode-snippets)

;; Set indent offsets
(setq web-mode-markup-indent-offset 2) ;; html indent
(setq web-mode-css-indent-offset 2)    ;; css indent
(setq web-mode-code-indent-offset 2)   ;; script indent(js,php,etc..

;; Indent with spaces
(add-hook 'web-mode-hook
          (function
           (lambda ()
             (setq indent-tabs-mode nil)
             )))

;;;; js-mode
(setq js-indent-level 2)
(add-hook 'js-mode-hook
          (function
           (lambda ()
             (setq indent-tabs-mode nil)
             )))

;;;; cperl-mode
(defalias 'perl-mode 'cperl-mode)
(let ((indent-level 2))
  (setq cperl-indent-level indent-level)
  (setq cperl-close-paren-offset (- indent-level)))
(setq cperl-indent-parens-as-block t)
(setq cperl-indent-subs-specially nil)
(add-to-list 'auto-mode-alist '("\\.cgi$" . cperl-mode))

;;;; init-loader
(require 'init-loader)
(setq init-loader-show-log-after-init nil)
(init-loader-load "~/.emacs.d/inits")

