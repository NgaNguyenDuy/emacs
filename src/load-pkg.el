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
  :commands (magit-status))

;;
;; Alchemist: Tooling interaction with Elixir
;; http://alchemist.readthedocs.org/en/latest/
;;
(use-package alchemist
  :ensure alchemist
  :config (progn
            (global-company-mode 1)
            ;; (add-hook 'elixir-mode-hook 'company-mode)
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
            ;; C and c++ should use RET to open block
            (sp-local-pair 'c++-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
            (sp-local-pair 'c-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
            ;; Should autopair with <> in string
            (sp-local-pair 'emacs-lisp-mode "<" ">" :when '(sp-in-string-p))
            ;; Custom smartparens in exlixr mode
            (sp-local-pair 'elixir-mode "do" "end" :post-handlers '(:add my-open-block-without-ret))
            (sp-local-pair 'elixir-mode "fn" "end" :post-handlers '(:add "| "))
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
                    (helm-autoresize-mode 1)))
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
;; (use-package helm-swoop
;;   :ensure helm-swoop)
(add-to-list 'load-path "~/src/emacs/src/local-pkgs/helm-swoop")
(require 'helm-swoop)


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
