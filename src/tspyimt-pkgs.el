;;; package -- Summary

;;; Commentary:

;;; Code:

;; Alchemist: Tooling interaction with Elixir
;; http://alchemist.readthedocs.org/en/latest/
;;
;; (use-package alchemist
;;   :ensure t
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control systems: Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :commands (magit-status)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save and restore current editing point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package saveplace
  :ensure t
  :init (progn
          (setq-default save-place t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better M-x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; best remove since we can use helm-m-x
;; (use-package smex
;;   :ensure t
;;   :init (progn
;;           (smex-initialize)
;;           ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow delimiters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :config (progn
            ;; Enable in emacs lisp buffer
            (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
            ;; Enables rainbow-delimiters-mode in Clojure buffers.
            (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
            ;; enables rainbow-delimiters-mode in other Lisp mode buffers.
            (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
            )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :ensure t
  :init (progn
          (auto-load-mode '("\\.html?\\'" "\\.ejs\\'" "\\.js[x]?\\'") 'web-mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq load-path
      (cons  "~/.bin/otp_7.3/lib/erlang/lib/tools-2.8.3/emacs"
              load-path))
(setq-default erlang-root-dir "~/.bin/otp_7.3/lib/erlang")
(setq exec-path (cons "~/.bin/erlang/bin" exec-path))
(setq-default erlang-man-root-dir "~/.bin/otp_7.3/lib/erlang/man")
(setq exec-path (cons "~/.bin/erlang/bin/erl" exec-path))
(require 'erlang-start)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Racket development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package racket-mode
  :ensure t
  :commands racket-mode
  :mode "\\.rkt\\'"
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :config (progn
            (auto-load-mode '("\\.py$") 'python-mode)

            ;; Workaround: virtualenvwrapper.el needs to be loaded explicitly
            (progn
              (unless (package-installed-p 'virtualenvwrapper)
                (package-install 'virtualenvwrapper))
              (load-file (get-library-full-path "virtualenvwrapper")))

            (use-package virtualenvwrapper
              :config (progn
                        (venv-initialize-interactive-shells)
                        (venv-initialize-eshell)
                        (setq venv-location (or
                                              (getenv "WORKON_HOME")
                                              (concat
                                                (getenv "HOME")
                                                "/L/Python/virtual-envs/"
                                                )
                                              ))))))


;; https://github.com/jorgenschaefer/elpy
;; http://elpy.readthedocs.io/en/latest/index.html
;; Config with (elpy-config)
;;
;; Setup workflow:
;; * Open a file in the project
;; * Run (pyvenv-workon) and choose the appropriate virtual env
;; * Run (elpy-config) and install necessary dependencies
;;
;; Beginning to work:
;; * Run (pyvenv-workon)
;; * Have fun
;; 
;; # Either of these
;; pip install rope
;; pip install jedi
;; # flake8 for code checks
;; pip install flake8
;; # importmagic for automatic imports
;; pip install importmagic
;; # and autopep8 for automatic PEP8 formatting
;; pip install autopep8
;; # and yapf for code formatting
;; pip install yapf

(add-to-list 'package-archives
  '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(use-package elpy
  :ensure elpy
  :init (progn
          (elpy-enable)

          ;; (defun my/python-mode-hook ()
          ;;   "Customized python mode"
          ;;   (setq python-indent-offset 4)
          ;;   )

          ;;(add-hook 'python-mode-hook 'my/python-mode-hook)
	  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :config (progn
            (add-hook 'js-mode-hook 'js2-minor-mode)
            ;; (~auto-load-mode '("\\.js\\'") 'js2-mode)
            ;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))

            ;; (add-hook 'html-mode-hook '~auto-reload-firefox-after-save-hook)
            ;; (add-hook 'css-mode-hook '~auto-reload-firefox-after-save-hook)
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editorconfig
;; https://github.com/editorconfig/editorconfig-emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package editorconfig
  :ensure t
  :config
  (progn
    (editorconfig-mode 1)))

(provide 'e:tspyimt-pkgs)
;;; tspyimt-pkgs ends here
