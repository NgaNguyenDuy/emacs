

(require 'smartparens)


(defun sp-elixir-post-handler (id action context)
  "Handler for elixir do-end inserts"
  (when (equal action 'insert)
    (save-excursion
      (insert "e")
      (newline)
      (indent-according-to-mode))
    (delete-char 1))
  )


(sp-local-pair 'elixir-mode "do" "end"
               :when '(("RET"))
               :unless '(sp-in-comment-p sp-in-string-p)
               :actions '(insert)
               :post-handlers '(sp-elixir-post-handler))




(sp-local-pair 'elixir-mode "fn" "end"
               :post-handlers '(:add "| "))


(provide 'smartparens-elixir)
