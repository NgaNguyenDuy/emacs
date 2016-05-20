

(require 'smartparens)

(sp-local-pair 'markdown-mode "**" "**")
(sp-local-pair 'markdown-mode "_" "_" :unless '(sp-point-after-word-p))



(provide 'smartparens-markdown)
