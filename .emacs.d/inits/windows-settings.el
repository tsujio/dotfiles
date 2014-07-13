;;;; IME settings
(setq default-input-method "W32-IME")
(setq-default w32-ime-mode-line-state-indicator "[--]")
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
(w32-ime-initialize)

;;;; Change cursor color on IME ON/OFF
(setq ime-activate-cursor-color "#00a000")
(setq ime-inactivate-cursor-color "#000000")
(set-cursor-color ime-inactivate-cursor-color)

(add-hook 'w32-ime-on-hook
          (function (lambda ()
                      (set-cursor-color ime-activate-cursor-color))))
(add-hook 'w32-ime-off-hook
          (function (lambda ()
                      (set-cursor-color ime-inactivate-cursor-color)))) 

;;;; Enable isearch by Japanese
(defun w32-isearch-update ()
  (interactive)
  (isearch-update))
(define-key isearch-mode-map [compend] 'w32-isearch-update)
(define-key isearch-mode-map [kanji] 'isearch-toggle-input-method)
