;;
;; Copyright (C) 2016 Nga Nguyen ([@nganguyenduy](https://github.com/nganguyenduy/))
;;
;; This project is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This project is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.
;; 


;;
;; Buffer
;;
(bind-key "<f2>" 'save-buffer)
(bind-key "M-<f2>" 'rename-file-and-buffer)
;; (bind-key "<f3>" 'helm-find-files)
(bind-key "M-<f3>" 'fiplr-find-file)
(bind-key "C-<f4>" 'xah-close-current-buffer)
(bind-key "M-<f4>" '~delete-window)
(bind-key "C-S-<f4>" 'xah-open-last-closed)
(bind-key "<f8>" 'helm-buffers-list)
(bind-key "C-<f8>" 'xah-open-recently-closed)
(bind-key "C-<escape>" '~switch-to-last-buffer)
(bind-key "C-c C-n" '~new-empty-buffer)
(bind-key "C-<f1>" '~switch-to-scratch)


;;
;; Magit
;;
(bind-key "C-c g" 'magit-status)

;;
;; Helm keymapping
;;
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)
(bind-key (kbd "s-s") 'helm-swoop)
(bind-key (kbd "s-S") 'helm-swoop-back-to-last-point)
(bind-key (kbd "C-c M-i") 'helm-multi-swoop)
(bind-key (kbd "C-x M-i") 'helm-multi-swoop-all)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "s-\\") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-x c") 'helm-lisp-completion-at-point)
(global-set-key (kbd "C-c h M-:") 'helm-eval-expression-with-eldoc)
;;
;; window binding
;; 
(bind-key "<M-S-left>" 'windmove-left)
(bind-key "<M-S-right>" 'windmove-right)
(bind-key "<M-S-up>" 'windmove-up)
(bind-key "<M-S-down>" 'windmove-down)
(bind-key "C-c s" 'make-window-sticky)


;;
;; company mode
;;
(global-set-key (kbd "C-M-i") 'company-complete)
;; C-n, C-p to select next/previous completeion text
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
;; C-s search completion text
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)
;; TAB to complete text
(define-key company-active-map (kbd "C-i") 'company-complete-selection)
;; complete candicate
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)


;; Multiple cursors
(bind-key "s-+" 'mc/edit-lines)
(bind-key "s-#" 'mc/mark-next-like-this)
(bind-key "s-!" 'mc/mark-previous-like-this)
(bind-key "C-c C-." 'mc/mark-all-like-this)
;; Mouse binding
(bind-key "C-S-<mouse-1>" 'mc/add-cursor-on-click)

;;
;; Mode
;;
(bind-key "C-<menu> C-f" 'auto-fill-mode)
(bind-key "C-<menu> C-e" 'toggle-evil-local)

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (hs-minor-mode t)
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
    (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)))


;; Erlang mode
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;; Css mode
(add-hook 'css-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c m") 'xah-css-compact-css-region)))


;; Others
(bind-key "s-=" 'er/expand-region)
(bind-key "M-/" 'hippie-expand)
;; (bind-key "M-x" 'smex)
(bind-key "s--" 'comment-or-uncomment-region)
(bind-key "M-s-w" 'whitespace-mode)
(bind-key "s-w" 'delete-trailing-whitespace)
(bind-key "s-u" 'unicode-input-mode)


;;
;; neotree
;;
;; (bind-key "<f7>" '~neotree)
(bind-key "<f7>" 'neotree-toggle)


(provide 'e:keybindings)
