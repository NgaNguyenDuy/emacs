

(auto-load-mode '("\\.zsh$" "\\.set_env$") 'sh-mode)


(add-hook 'post-command-hook 'set-cursor-according-to-mode)
(add-hook 'after-save-hook 'make-executable-on-save)


;; (add-hook 'css-mode-hook 'rainbow-mode)
;; (add-hook 'elisp-mode-hook 'rainbow-mode)
(provide 'e:hooks)
