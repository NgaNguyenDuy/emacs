



(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
;; (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)


(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-(") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-)") 'sp-end-of-sexp)

(provide 'smartparens-keybinding)
