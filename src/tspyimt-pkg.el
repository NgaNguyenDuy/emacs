;;
;; List all my packages to be installed when start emacs
;;
(require 'cl)

;; Use default package with emacs 24
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  ;;(add-to-list 'package-archives
	;;       '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize))

;; Define required packages must be installed
(defvar required-packages
  '(
    use-package
    )
  "List packages must be installed")


;; Method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))


; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'e:list-pkg)
