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


;; Display paren mode
(show-paren-mode 1)

;; Show column number
(column-number-mode 1)

;; Expand tabs to spaces
(setq-default indent-tabs-mode nil)

;; fill-column
(setq-default fill-column 78)
(set-fill-column 78)

;; Backspace and Del delete selection
(delete-selection-mode 1)

;; Set the default tab width
(setq default-tab-width 4)

;; Set tab width
(setq tab-width 4)

;; Setup backup file
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)


;;
;; Helm-swoop config
;;
;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-horizontally)
;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)
;; If you prefer fuzzy matching
(setq helm-swoop-use-fuzzy-match t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; scroll one line at a time (less "jumpy" than defaults)
;; 3 lines at a time normally, 5 lines at a time with shift
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5)))
;; don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
;; scroll window under mouse
(setq mouse-wheel-follow-mouse 't)
;; keyboard scroll one line at a time
(setq scroll-step 1)

(provide 'e:settings)
