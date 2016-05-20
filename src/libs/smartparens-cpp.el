
(require 'smartparens)

(defun sp-cpp-cmt-mul (id action context)
  "Handler for comment multy line."
  (when (equal action 'insert)
    (insert "* ")
    (save-excursion
      (newline)
      (indent-according-to-mode)
      )
    (indent-according-to-mode)
    (next-line 1)
    (indent-according-to-mode)
    (previous-line 1)
    ))


(sp-local-pair 'c++-mode "{"   nil :post-handlers '(("||\n[i]" "RET")))
(sp-local-pair 'c++-mode "/*" "*/" :post-handlers '(("| " "SPC")
                                                    (sp-cpp-cmt-mul "RET")))

(provide 'smartparens-cpp)
