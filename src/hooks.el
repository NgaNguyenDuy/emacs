

(add-hook 'post-command-hook 'set-cursor-according-to-mode)
(add-hook 'after-save-hook 'make-executable-on-save)
(provide 'e:hooks)
