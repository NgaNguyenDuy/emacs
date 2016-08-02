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

(require 'cl-lib)
(require 'color)
(require 'cl)

(dolist (theme-file (directory-files (~get-cfg "themes/")))
  (when (s-ends-with? ".el" theme-file)
    (load-file (~get-cfg "themes/" theme-file))))

(color-theme-seti)

;; (color-theme-custom)



;; (load-theme 'afternoon t)
;;
;; Cycle themes
;;
;; (setq my-color-themes (list 'color-theme-billw 'color-theme-jsc-dark 
;;                             'color-theme-sitaramv-solaris 'color-theme-resolve
;;                             'color-theme-classic 'color-theme-jonadabian-slate
;;                             'color-theme-kingsajz 'color-theme-shaman
;;                             'color-theme-subtle-blue 'color-theme-snowish
;;                             'color-theme-sitaramv-nt 'color-theme-wheat))

;; (defun my-theme-set-default () ; Set the first row
;;   (interactive)
;;   (setq theme-current my-color-themes)
;;   (funcall (car-theme)))

;; (defun car-theme () ;figure out if we need car or caar
;;   (interactive)
;;   (cond
;;    ((consp (car theme-current))
;;     (caar theme-current))
;;    (t
;;     (car theme-current))))

;; (defun my-describe-theme () ; Show the current theme
;;   (interactive)
;;   (message "%s" (car-theme)))


;; (defun my-shell () 
;;   (interactive)
;;   (let ((color-theme-is-global nil))
;;     (color-theme-gnome)
;;     (switch-to-buffer-other-window "*test*")
;;     (eshell)))


;; ; Set the next theme (fixed by Chris Webber - tanks)
;; (defun my-theme-cycle ()		
;;   (interactive)
;;   (setq theme-current (cdr theme-current))
;;   (if (null theme-current)
;;       (setq theme-current my-color-themes)
;;     (funcall (car-theme)))
;;   (message "%S" (car-theme)))

;; (setq theme-current my-color-themes)
;; (setq color-theme-is-global nil) ; Initialization
;; (my-theme-set-default)

;; (global-set-key [f12] 'my-theme-cycle)


;; Using system font
(setq font-use-system-font t)

;; Hide scroll bar
(scroll-bar-mode -1)

;; Hide tool bar
(tool-bar-mode -1)

;; Turn off welcome message
(setq inhibit-startup-message t)

;; Highlight the editing line
(global-hl-line-mode 1)

;; Display the size of the buffer
(size-indication-mode 1)

;;; Set frame title
(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))



;; Custom variables
(custom-set-variables
 ;; '(column-number-mode t)
 ;; '(display-battery-mode t)
 ;; '(display-time-mode t)
 ;; '(size-indication-mode t)
 ;; '(face-font-family-alternatives (quote (("Monaco" "Consolas" "Monospace")
 ;;                                         ("Monaco" "Consolas" "CMU Typewriter Text" "fixed")
 ;;                                         ("Geneva" "Sans Serif" "helv" "helvetica" "arial" "fixed")
 ;;                                         ("helv" "helvetica" "arial" "fixed"))))
 ;; '(safe-local-variable-values (quote ((Syntax . ANSI-Common-Lisp) (Base . 10) (encoding . utf-8))))
 ;; '(show-paren-mode t)
 ;; '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 )


;;
;; Custom interface with company tooltip
;;

;; (set-face-attribute 'company-tooltip nil
;;                     :foreground "lightgrey" :background "black")
;; (set-face-attribute 'company-tooltip-common nil
;;                     :foreground "#8DA6CE" :background "black")
;; (set-face-attribute 'company-tooltip-common-selection nil
;;                     :foreground "#CA74F3" :background "white" :bold t)
;; (set-face-attribute 'company-tooltip-selection nil
;;                     :foreground "black" :background "white")
;; (set-face-attribute 'company-preview-common nil
;;                     :background nil :foreground "lightgrey" :underline t)
;; (set-face-attribute 'company-scrollbar-fg nil
;;                     :background "white")
;; (set-face-attribute 'company-scrollbar-bg nil
;;                     :background "#BABAB9")

;; (set-face-attribute 'helm-selection nil 
;;                     :background "#101112"
;;                     :foreground nil)





;; Custom fonts
(custom-set-faces
 '(default ((t (:inherit nil
                         :stipple nil
                         :inverse-video nil
                         :box nil
                         :strike-through nil
                         :overline nil
                         :underline nil
                         :slant normal
                         :weight normal
                         :height 140
                         :width normal
                         :foundry "unknown"
                         :family "Inconsolata"))))

 '(rst-level-1-face ((t (:embolden t))) t))

;; (set-frame-font "ReemKufi:pixelsize=18")

;;
;; Mode line setup
;;
(setq-default
 mode-line-format
 '(
   ;; Sticked mode line
   (:eval
    (let* ((window (get-buffer-window (current-buffer)))
           (sticky? (window-dedicated-p window)))
      (cond (sticky?
             (propertize "  ⚡" 'face 'mode-line-sticky-face))
            (t ""))))
   ;; Position, including warning for 78 columns
   (:propertize "%4l :" face mode-line-position-face)
   (:eval (propertize "%3c " 'face
                      (if (> (current-column) 78)
                          'mode-line-78col-face
                        'mode-line-position-face)))
   ;; emacsclient [default -- keep?]
   mode-line-client
   "  "
   ;; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "      ")))
   "    "
   ;; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ;; narrow [default -- keep?]
   " %n "
   ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "  %["
   (:propertize mode-name
                face mode-line-mode-face)
   "%] "
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   "    "
   ;; nyan-mode uses nyan cat as an alternative to %p
   ;; (:eval (when nyan-mode (list (nyan-create))))
   ))



;;
;; Rainbow delimiters
;;
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))

;;
;; Change height mini-buffer
;;
(with-current-buffer (get-buffer " *Echo Area 0*")
  (setq-local face-remapping-alist '((default (:height 0.9) variable-pitch))))



(provide 'e:env)
