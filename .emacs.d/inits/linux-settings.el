;;;; IME settings
(require 'ibus)
(add-hook 'after-init-hook 'ibus-mode-on)

;;;; Move buffer by Ctrl + Tab
(global-set-key
 [(control tab)] 'tabbar-forward)
(global-set-key
 [(control shift iso-lefttab)] 'tabbar-backward)
