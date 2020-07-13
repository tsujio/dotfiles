;; Set target exts
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))

;; Set indent offsets
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; Indent with spaces
(add-hook 'web-mode-hook
          (function
           (lambda ()
             (setq indent-tabs-mode nil)
             )))
