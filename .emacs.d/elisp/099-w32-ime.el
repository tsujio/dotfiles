;; Windows IME
(setq default-input-method "W32-IME")
(setq-default w32-ime-mode-line-state-indicator "[--]")
(setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
(w32-ime-initialize)

;; Change cursor color when on/off ime
(add-hook 'w32-ime-on-hook
          (function (lambda ()
                      (set-cursor-color "#00a000"))))
(add-hook 'w32-ime-off-hook
          (function (lambda ()
                      (set-cursor-color "#ffffff"))))

;; Enable isearch by Japanese
(define-key isearch-mode-map [compend]
  (function (lambda () ((interactive) (isearch-update)))))
(define-key isearch-mode-map [kanji] 'isearch-toggle-input-method)
