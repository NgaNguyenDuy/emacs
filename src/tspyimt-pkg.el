;;
;; List all my packages to be installed when start emacs
;;
(require 'cl)

;; Use default package with emacs 24
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/"))
  ;;(add-to-list 'package-archives
  ;;       '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize))

;; Fetch the list of available packages
(when (not package-archive-contents)
  (package-refresh-contents))

;; load all local packages
(let ((local-package-dir (~get-cfg "local-pkgs/")))
  (dolist (package-dir (directory-files local-package-dir))
    ;; Don't know why it not working
    ;; (add-to-list 'load-path (~get-cfg local-package-dir package-dir))
    (add-to-list 'load-path (concat local-package-dir package-dir))
    ))


(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
;; Show loading activity in *Message* if loaded packages take longer than 0.1s
(setq use-package-verbose t)

(provide 'e:list-pkg)
