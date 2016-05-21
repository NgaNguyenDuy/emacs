;; Numbering lines
;; (use-package linum
;;   :init (progn
;;           (global-linum-mode 1)
;;           (setq linum-format "%d ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control systems: Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure magit
  :commands (magit-status)
  )

;;
;; Alchemist: Tooling interaction with Elixir
;; http://alchemist.readthedocs.org/en/latest/
;;
(use-package alchemist
  :ensure alchemist
  :config (progn
            ;; (global-company-mode 1)
            (add-hook 'elixir-mode-hook 'company-mode)
            ))

;;
;; Load color theme
;;
(use-package color-theme
  :ensure color-theme)

;;
;; s.el: String processing
;; https://github.com/magnars/s.el
(use-package s
  :ensure s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Save and restore current editing point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package saveplace
  :ensure saveplace
  :init (progn
          (setq-default save-place t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-pairing brackets
;; https://github.com/Fuco1/smartparens/wiki/Permissions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartparens-config
  :ensure smartparens
  :config (progn
            (smartparens-global-mode)
            (require 'smartparens-elixir
                     (load-f "libs/smartparens-elixir"))
            (require 'smartparens-markdown
                     (load-f "libs/smartparens-markdown"))
            (require 'smartparens-cpp
                     (load-f "libs/smartparens-cpp"))
            (require 'smartparens-keybinding
                     (load-f "libs/smartparens-keybinding"))
            ;; ;; C and c++ should use RET to open block
            ;; (sp-local-pair 'c++-mode "{" nil :post-handlers '(:add my-open-block-without-ret))
            ;; (sp-local-pair 'c-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
            ;; ;; Should autopair with <> in string
            ;; (sp-local-pair 'emacs-lisp-mode "<" ">" :when '(sp-in-string-p))
            ;; ;; Custom smartparens in exlixr mode
            ;; ;; (sp-local-pair 'elixir-mode "do" "end" :post-handlers '(:add my-open-block-without-ret))
            ;; (sp-local-pair 'elixir-mode "fn" "end" :post-handlers '(:add "| "))
            ))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :ensure multiple-cursors)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :ensure expand-region
  :commands er/expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smoother scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smooth-scrolling
  :ensure smooth-scrolling)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart completion framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-config
  :ensure helm
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
  :ensure smex
  :init (progn
          (smex-initialize)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer navigation with pattern matching and replacing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-swoop
  :ensure helm-swoop)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow delimiters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure rainbow-delimiters
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
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure markdown-mode
  :commands markdown-mode
  :init (progn
          (auto-load-mode '("\\.md$" "\\.markdown$" "\\.text$") 'markdown-mode))
  :config (progn
            (use-package markdown-mode+
              :ensure markdown-mode+)

            (add-hook 'markdown-mode-hook 'auto-fill-mode)
            (custom-set-faces
             ;; ;; Your init file should contain only one such instance.
             ;; ;; If there is more than one, they won't work right.
             ;; '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7 :background "#ABCDEF"))))
             ;; '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.5 :background "green"))))
             ;; '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.3))))
             )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popup window manager
;; https://github.com/m2ym/popwin-el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package popwin
  :ensure popwin
  :config (progn
            (popwin-mode 1)
            (global-set-key (kbd "C-x p") popwin:keymap)
            
            ;; M-x dired-jump-other-window
            (push '(dired-mode :position bottom) popwin:special-display-config)

            (push '("magit" :regexp t :height 0.3) popwin:special-display-config)
            (push '("alchemist" :regexp t :height 20 :stick t) popwin:special-display-config)
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uniquify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq 
 uniquify-buffer-name-style 'reverse
 uniquify-separator ":"
 uniquify-ignore-buffers-re "^\\*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package web-mode
  :ensure web-mode
  :init (progn
          (auto-load-mode '("\\.html?\\'" "\\.ejs\\'" "\\.js[x]?\\'") 'web-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Docker file mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dockerfile-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart tab mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(use-package smart-tabs-mode
;;   :ensure smart-tabs-mode)

;; Todo later
;; Ref: http://ergoemacs.org/emacs/whitespace-mode.html
;; (use-package whitespace
;;   :commands whitespace-mode)
;; (setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
;; (setq whitespace-display-mappings
;;       ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
;;       '(
;;         (space-mark 32 [32] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
;;         (newline-mark 10 [8617 10]) ; 10 LINE FEED
;;         (tab-mark 9 [187 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
;;         ))


(provide 'e:load-pkg)
