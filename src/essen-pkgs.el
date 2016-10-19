;;; essen-pkgs.el --- Load essential packages        -*- lexical-binding: t; -*-

;; Copyright © 2016  Duy Nga Nguyen

;; Author: Duy Nga Nguyen <tspyimt@local>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String processing - https://github.com/magnars/s.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package s
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 'color)
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote file processing with Tramp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tramp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package multiple-cursors
  :ensure t
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expand region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package expand-region
  :ensure t
  :commands er/expand-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Live function signature at echo area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :config (progn
            (add-hook 'emacs-lisp-mode-hook        'turn-on-eldoc-mode)
            (add-hook 'lisp-interaction-mode-hook  'turn-on-eldoc-mode)
            (add-hook 'ielm-mode-hook              'turn-on-eldoc-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Popup window manager
;; https://github.com/m2ym/popwin-el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package popwin
  :ensure t
  :config (progn
            (popwin-mode 1)
            (global-set-key (kbd "C-x p") popwin:keymap)
            
            ;; M-x dired-jump-other-window
            (push '(dired-mode :position bottom) popwin:special-display-config)

            (push '("magit" :regexp t :height 0.3)
            popwin:special-display-config)
            ;; (push '("alchemist" :regexp t :height 20 :stick t)
            ;; popwin:special-display-config) 
            (push '(
                    "*Alchemist-IEx*" :position bottom)
                  popwin:special-display-config)
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load color theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package color-theme
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smoother scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smooth-scrolling
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Uniquify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq 
  uniquify-buffer-name-style 'reverse
  uniquify-separator ":"
  uniquify-ignore-buffers-re "^\\*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer navigation with pattern matching and replacing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-swoop
  :ensure t
  :commands swoop
  :init (use-package helm-swoop
          :ensure t)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL shortener
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq goo-api-key "AIzaSyAj7PaE0JaOq2O8jsEPj1zP_iAi7E47mmI")
(use-package url-shortener
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gist mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package gist
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
  :ensure t
  :commands yaml-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto complete backend system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :diminish company-mode
  :config (progn
            (global-company-mode 1)

            (bind-key "C-/" 'company-complete company-mode-map)

            (use-package pos-tip
              :ensure t)

            (use-package company-quickhelp
              :ensure t
              :config (progn
                        (company-quickhelp-mode 1)

                        ;; Do not trigger automatically
                        (setq company-quickhelp-delay nil)
                        (bind-key "M-h" 'company-quickhelp-manual-begin
                                  company-active-map)))

            (use-package helm-company
              :ensure t
              :config (progn
                        (bind-key "M-RET" 'helm-company company-mode-map)
                        (bind-key "M-RET" 'helm-company company-active-map)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (progn
          (auto-load-mode '("\\.md$" "\\.markdown$" "\\.text$")
            'markdown-mode)) 
  :config (progn
            (add-hook 'markdown-mode-hook 'auto-fill-mode)
            (custom-set-faces
              ;; Your init file should contain only one such instance.
              ;; If there is more than one, they won't work right.
              '(markdown-header-face-1 ((t (:inherit markdown-header-face
              :height 1.7 :background "#ABCDEF")))) 
              '(markdown-header-face-2 ((t (:inherit markdown-header-face
              :height 1.5 :background "green")))) 
              '(markdown-header-face-3 ((t (:inherit markdown-header-face
              :height 1.3)))) 
              )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-pairing brackets
;; https://github.com/Fuco1/smartparens/wiki/Permissions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smartparens-config
  :ensure smartparens
  :config (progn
            (eval-after-load 'smartparens
              '(progn
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
            ))

;; Todo later
;; Ref: http://ergoemacs.org/emacs/whitespace-mode.html
;; Documentation: https://www.emacswiki.org/emacs/WhiteSpace
(use-package whitespace
  :ensure t
  :commands whitespace-mode
  :config
  (progn
    (setq whitespace-style
      (quote
        (face space-mark tab-mark newline-mark lines-tail)
        ))
    (setq whitespace-line-column 78)
    (setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal.try (insert-char
      ;; 182) to see it
      '(
         (space-mark 32 [183] [46]) ;; 32 SPACE, 183 MIDDLE DOT 「·」, 46
         ;; FULL STOP「.」
         (newline-mark 10 [172 10]) ;; 10 LINE FEED, 9166 is ⏎, 8629 is ↵,
         ;; 172 is ¬
         (tab-mark 9 [187 9] [92 9]) ; 9 TAB, 9655 is ▷ WHITE RIGHT-POINTING
         ;; TRIANGLE 「▷」, 187 is »
         ))
    ))

;;
;; Neotree might break tab completion in minibuffer
;;
;; https://github.com/jaypei/emacs-neotree
;; https://www.emacswiki.org/emacs/NeoTree
(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-theme 'icons)
    ;; (setq neo-theme 'arrow)

    ;; (setq projectile-switch-project-action
    ;;   'neotree-projectile-action)


    ;; bind Space, Enter, and Tab in evil mode to open the file or directory
    ;; (neotree-enter)
    (add-hook 'neotree-mode-hook
      (lambda ()
        (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
        (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-enter)
        (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
        (evil-define-key 'normal neotree-mode-map (kbd "RET")
          'neotree-enter)))

    ;; If you use popwin, when NeoTree is open and successively a temporary
    ;; buffer is opened with popwin, a new window with the NeoTree buffer is
    ;; displayed side by side next to the first one (#50). This code will help
    ;; you

    ;; (when neo-persist-show
    ;;   (add-hook 'popwin:before-popup-hook
    ;;     (lambda () (setq neo-persist-show nil)))
    ;;   (add-hook 'popwin:after-popup-hook
    ;;     (lambda () (setq neo-persist-show t))))
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart tab mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smart-tabs-mode
  :ensure t
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ansible
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ansible
  :ensure t
  :config (progn
            (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ido
  :ensure t
  :config (progn
            (ido-mode 1)
            (ido-everywhere 1)
            (use-package flx-ido
              :config (progn
                        (flx-ido-mode 1)

                        ;; disable ido faces to see flx highlights.
                        (setq ido-use-faces nil)))
            (use-package ido-vertical-mode
              :ensure t
              :config (ido-vertical-mode 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find file using fuzzy matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fiplr
  :ensure t
  :config (progn
            (add-to-list 'fiplr-root-markers "README.md")
            (add-to-list 'fiplr-root-markers "README.adoc")
            (add-to-list 'fiplr-root-markers "README.txt")
            (add-to-list 'fiplr-root-markers "README")
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package evil
  :ensure t
  :config (progn

            ;; https://github.com/timcharper/evil-surround
            (use-package evil-surround
              :ensure t
              :config (progn
                        (global-evil-surround-mode 1)))

            (evil-mode 1)

            (bind-key [mouse-2] nil evil-normal-state-map)
            (bind-key [mouse-2] nil evil-visual-state-map)
            (bind-key [mouse-2] nil evil-insert-state-map)
            
            (setq evil-emacs-state-cursor 'bar)
            (setq-default cursor-type 'hbar)

            (define-key evil-insert-state-map "\C-y" 'yank)
            ;; (define-key evil-insert-state-map "\C-o" '~open-line)
            (define-key evil-insert-state-map "\C-w" 'kill-region)


            (bind-spacemacs-like-keys)

            ;; Make visual select by v or V and then hit * to search forward,
            ;; # to search backward
            (use-package evil-visualstar
              :ensure t
              :init (progn
                      (global-evil-visualstar-mode)
                      ;; (setq evil-visualstar/persistent nil)
                      ))
            
            ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Docker file mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dockerfile-mode
  :ensure t
  :commands dockerfile-mode
  :mode "Dockerfile\\'")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto make header2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To have Emacs update file headers automatically whenever you save a
;; file, put this in your init file (~/.emacs):
;;
;;   (autoload 'auto-update-file-header "header2")
;;   (add-hook 'write-file-hooks 'auto-update-file-header)
;;
;; To have Emacs add a file header whenever you create a new file in
;; some mode, put this in your init file (~/.emacs):
;;
;;   (autoload 'auto-make-header "header2")
;;   (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
;;   (add-hook 'c-mode-common-hook   'auto-make-header)
(autoload 'auto-make-header "header2")
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)

(provide 'e:essen-pkgs)
;;; essen-pkgs.el ends here
