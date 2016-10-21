;;; my-modeline.el --- Customized my mode line 
;; 
;; Filename: my-modeline.el
;; Description: 
;; Author: Duy Nga Nguyen
;; Maintainer: 
;; Created: Thu Oct 20 10:29:35 2016 (+0700)
;; Version: 
;; Package-Requires: ()
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; Extra mode line faces

(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-78col-face)

(defface custom-modeline-modified-face
  '((((class color))
      :foreground "#c82829" :background "#ffffff" :inherit (mode-line-face))
     (t (:weight bold)))
  "Custom modeline modefied"
  )

(defface custom-modeline-filename-face
  '((((class color))
      :foreground "#CA74F3" :inherit (mode-line-face) :height 145)
     (t (:weight bold)))
  "Custom modeline filename"
  )

(defface custom-modeline-folder-face
  '((((class color))
      :foreground "gray60" :inherit (mode-line-face))
     )
  "Custom modeline folder name"
  )

(defface custom-modeline-position-face
  '((((class color))
      :inherit (mode-line-face) :height 100 :family "Menlo")
     )
  "Custom modeline position"
  )

(defface custom-modeline-mode-face
  '((((class color))
      :foreground "gray80" :inherit (mode-line-face))
     )
  "Custom modeline mode"
  )

(defface custom-modeline-minormode-face
  '((((class color))
      :foreground "gray40" :height 110 :inherit (custom-modeline-mode-face))
     )
  "Custom modeline minor mode"
  )

(defface custom-modeline-process-face
  '((((class color))
      :foreground "#718c00" :inherit (custom-modeline-face))
     )
  "Custom modeline process mode"
  )

(defface custom-modeline-78col-face
  '((((class color))
      :foreground "#c82829" :inherit (custom-modeline-position-face))
     )
  "Custom modeline process mode"
  )

(defface custom-modeline-readonly-face
  '((((class color))
      :foreground "red" :inherit (mode-line-face)
      :box (:line-width 2 :color "red")
      ))
  "Custom modeline readonly files"
  )


;; Generate modeline format
(setq-default
  mode-line-format
  '(
     ;; Sticked mode line
     (:eval
       (let* ((window (get-buffer-window (current-buffer)))
               (sticky? (window-dedicated-p window)))
         (cond (sticky?
                 ;; (propertize "  ⚡" 'face 'mode-line-sticky-face)
                 (concat "  "
                   (propertize (all-the-icons-octicon "check")
                     'face `(:height 1.2 :foreground "#8BE03C" :family ,(all-the-icons-octicon-family))
                     'display '(raise 0.0)
                     )
                   )
                 )
           (t ""))))
     ;; Position, including warning for 78 columns
     (:propertize "%4l :" face custom-modeline-position-face)
     (:eval (propertize "%3c " 'face
              (if (> (current-column) 78)
                'custom-modeline-78col-face
                'custom-modeline-position-face)))
     ;; emacsclient [default -- keep?]
     mode-line-client
     "  "
     ;; read-only or modified status
     (:eval
       (cond (buffer-read-only
               (propertize " RO " 'face 'custom-modeline-readonly-face))
         ((buffer-modified-p)
           (propertize " ** " 'face 'custom-modeline-modified-face))
         (t "      ")))
     "    "
     ;; directory and buffer/file name
     (:propertize
       (:eval (shorten-directory default-directory 30))
       face mode-line-folder-face)
     (:propertize "%b"
       face custom-modeline-filename-face)
     ;; narrow [default -- keep?]
     " %n "
     ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
     (vc-mode vc-mode)
     "  %["
     (:propertize mode-name
       face custom-modeline-mode-face)
     "%] "
     (:eval (propertize (format-mode-line minor-mode-alist)
              'face 'mode-line-minor-mode-face))
     (:propertize mode-line-process
       face custom-modeline-process-face)
     (global-mode-string global-mode-string)
     "    "
     ;; nyan-mode uses nyan cat as an alternative to %p
     ;; (:eval (when nyan-mode (list (nyan-create))))
     ))


(provide 'e:my-modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; my-modeline.el ends here
