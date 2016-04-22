;; Seti - A theme inspired by Seti Atom Theme
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
(defun color-theme-seti ()
  "Color theme Seti - Dark theme ispired by Atom Seti."
  (interactive)
  (color-theme-install
   '(color-theme-seti
     ((background-color . "#000000") ;#0C1021
      (background-mode . dark)
      (foreground-color . "#F8F8F8")
      (mouse-color . "sienna1"))

     ;; Basic 
     (default ((t (:background "#151718" :foreground "#D4D7D6"))))
     ;; (cursor ((t (:background "#CCCCCC" :foreground "#151718"))))
     (minibuffer-prompt ((t (:foreground "#4F99D3" :weight bold))))
     (region ((t (:background "#434546"))))
     (error ((t (:foreground "#CE4045" :weight bold :underline (:color "#CE4045" :style line)))))

     (isearch ((t (:background "#151718" :foreground "#D4D7D6" :box (:line-width 1 :color "#4F99D3") :weight bold))))
     (lazy-highlight ((t (:background "#151718" :foreground "#858D8A" :box (:line-width 1 :color "#4F99D3")))))
     (mode-line ((t (:foreground "#D4D7D6" :background "#0D1011" :underline))))
     (mode-line-buffer-id ((t (:weight bold :foreground "#DCCD69"))))
     (mode-line-emphasis ((t (:weight bold))))
     (mode-line-highlight ((t (:box (:line-width 3 :color "#4F99D3")))))
     (mode-line-inactive ((t (:weight light :foreground "#D4D7D6" :background "#1E2326"))))
     (secondary-selection ((t (:background "#1E2326"))))
     (trailing-whitespace ((t (:background "#0D1011"))))
     (match ((t (:weight bold :foreground "#151718" :background "#8BE03C"))))
     (next-error ((t (:inherit (region)))))
     (query-replace ((t (:inherit (isearch)))))
     
     ;; Font lock
     (font-lock-builtin-face ((t (:foreground "#A074C4"))))
     (font-lock-comment-face ((t (:italic t :foreground "#41535B"))))
     (font-lock-comment-delimiter-face ((t (:inherit (font-lock-comment-face)))))
     (font-lock-constant-face ((t (:foreground "#CE4045"))))
     (font-lock-doc-face ((t (:foreground "#55B5DB"))))
     (font-lock-function-name-face ((t (:foreground "#55B5DB"))))
     (font-lock-keyword-face ((t (:foreground "#9FCA56"))))
     (font-lock-negation-char-face ((t nil)))
     (font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
     (font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
     (font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
     (font-lock-string-face ((t (:foreground "#55B5DB"))))
     (font-lock-type-face ((t (:foreground "#DCCD69"))))
     (font-lock-variable-name-face ((t (:foreground "#55B5DB"))))
     (font-lock-warning-face ((t (:weight bold :inherit (error)))))

     ;; Dired
     (dired-directory ((t (:foreground "#D4D7D6" :weight extrabold))))
     (dired-header ((t (:foreground "white"  :background "#55B5DB" :weight bold))))
     (dired-ignored ((t (:foreground "#41535B"))))
     (dired-flagged ((t (:foreground "#CE4045" :weight bold))))
     (dired-marked ((t (:background "#55B5DB" :foreground "white" :weight normal))))
     (dired-perm-write ((t (:foreground "#DCCD69" :weight ultra-bold))))
     (dired-symlink ((t (:foreground "#75E5F4" :weight normal))))
     (dired-warning ((t (:inherit (font-lock-warning-face)))))
     
     ;; Company mode
     (company-tooltip ((t (:inherit default :background "black" :foreground "lightgrey"))))
     (company-tooltip-common-selection ((t (:foreground "#CA74F3" :background "white" :bold t))))
     (company-tooltip-selection ((t (:foreground "black" :background "white"))))
     (company-preview-common ((t (:foreground "lightgrey" :background nil :underline t))))
     (company-scrollbar-fg ((t (:background "white"))))
     (company-scrollbar-bg ((t (:background "#BABAB9"))))
     

     ;; paren match
     ;;(set-face-foreground 'show-paren-match (face-foreground 'default))
     ;;(set-face-attribute 'show-paren-match nil :weight 'bold)
     (show-paren-match ((t (:foreground "#858D8A" :underline (:color "#4F99D3" :style line)))))
     (show-paren-mismatch ((t (:foreground "#858D8A" :underline (:color "#CE4045" :style line)))))


     ;; (default ((t (:height 150 :family "Menlo"))))

     ;; Lines
     (linum ((t (:foreground "#2F3C42"  :weight light :height 0.9))))
     (fringe ((t (:background "#0D1011" :foreground "#2F3C42"))))
     (left-margin ((t (nil))))
     (hl-line ((t (:background "#101112"))))

     )))


;; (let ((blue "#55B5DB")
;;       (green "#9FCA56")
;;       (yellow "#DCCD69")
;;       (red "#CE4045")
;;       (purple "#A074C4")
;;       (background "#151718")
;;       (background-2 "#1E2326")      
;;       (background-3 "#0D1011")
;;       (background-4 "#101112")
;;       (text "#D4D7D6")
;;       (text-2 "#858D8A")
;;       (text-3 "#41535B")
;;       (text-4 "#2F3C42")
;;       (text-highlight "#FFFFFF")
;;       (text-region "#434546")
;;       (text-dired "#A0A0A0")
;;       (input-text "#CCCCCC")
;;       (light-blue "#75E5F4")
;;       (dark-blue "#4F99D3")
;;       (intense-green "#8BE03C"))
;;   )  

