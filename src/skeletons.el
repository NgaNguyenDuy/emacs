;;; Adds hook to find-files-hook

(auto-insert-mode)

(define-skeleton erl-head
  "My erlang header"
  nil
  "%%\n"
  "%% Writen on " (format-time-string "%a, %D")
  "\n%%"
  "\n"
  "-module(" (file-name-sans-extension
               (file-name-nondirectory (buffer-file-name)))
  ").\n"
  "-export(["
  > _
  "]).")

(define-auto-insert "\\.erl\\'" 'erl-head)


(define-skeleton c-header
  "My C/C++ header"
  "Short description: "
  "/**\n * "
  "\n * "
  "Written on " (format-time-string "%a, %D.")
  "\n * "
  (file-name-nondirectory (buffer-file-name))
  " -- " str \n
  "*/" > \n \n
  "#include <stdio.h>" \n
  "#include \""
  (file-name-sans-extension
    (file-name-nondirectory (buffer-file-name)))
  ".h\"" \n \n
  "int main() {" \n
  > _ \n
  "}" > \n)

(define-auto-insert "\\.\\([Cc]\\|cc\\)\\'" 'c-header)


(provide 'e:skeletons)
