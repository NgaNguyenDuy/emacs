;;
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
;; Smart completion framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-config
  :ensure helm
  :diminish helm-mode
  :commands (helm-find-files helm-buffers-list)
  :init (use-package helm
          :config (progn
                    (helm-autoresize-mode 1)
                    ))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better M-x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smex
  :ensure t
  :init (progn
          (smex-initialize)))



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
(setq erlang-root-dir "~/.bin/otp_7.3/lib/erlang")
(setq exec-path (cons "~/.bin/erlang/bin" exec-path))
(setq erlang-man-root-dir "~/.bin/otp_7.3/lib/erlang/man")
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
