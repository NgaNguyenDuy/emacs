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
(bind-key "C-<f2>" 'buffer-menu)
(bind-key "<f3>" 'helm-find-files)
(bind-key "C-<f4>" 'kill-buffer)
(bind-key "<f8>" 'helm-buffers-list)
(bind-key "<f12>" 'helm-M-x)





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

;;
;; window navigate
;;
(bind-key "<M-S-left>" 'windmove-left)
(bind-key "<M-S-right>" 'windmove-right)
(bind-key "<M-S-up>" 'windmove-up)
(bind-key "<M-S-down>" 'windmove-down)


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
;; Programming Mode
;;
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
            (define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)))


;; Others
(bind-key "s-=" 'er/expand-region)
(bind-key "M-/" 'hippie-expand)
(bind-key "M-x" 'smex)


(provide 'e:keybindings)
