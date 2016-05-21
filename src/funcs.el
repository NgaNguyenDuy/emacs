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


;; Change cursor color according to mode; inspired by
;; http://emacs-fu.blogspot.com/2009/12/changing-cursor-color-and-shape.html
;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see the docs for set-cursor-type
(defun set-cursor-according-to-mode ()
  "Change cursor color and type according to some minor modes."
  (let ((read-only-color "#CE4045")
        (read-only-cursor-type 'hbar)
        (overwrite-cursor-type 'box)
        (normal-color "yellow")
        (normal-cursor-type 'bar))

    (cond
     (buffer-read-only
      (set-cursor-color read-only-color)
      (setq cursor-type read-only-cursor-type))
     (overwrite-mode
      (set-cursor-color overwrite-color)
      (setq cursor-type overwrite-cursor-type))
     (t 
      (set-cursor-color normal-color)
      (setq cursor-type normal-cursor-type))))
    )
  



;;
;; Post-handler function for few define smartparens
;;
(defun my-open-block-without-ret (id action context)
  (when (eq action 'insert)
    (newline)
    (newline)
    (indent-according-to-mode)
    (previous-line)
    (indent-according-to-mode)))


;;
;; filter annoying messages
;;
(defvar message-filter-regexp-list '("^Starting new Ispell process \\[.+\\] \\.\\.\\.$"
                                     "^Ispell process killed$")
  "filter formatted message string to remove noisy messages")
(defadvice message (around message-filter-by-regexp activate)
  (if (not (ad-get-arg 0))
      ad-do-it
    (let ((formatted-string (apply 'format (ad-get-args 0))))
      (if (and (stringp formatted-string)
               (some (lambda (re) (string-match re formatted-string)) message-filter-regexp-list))
          (save-excursion
            (set-buffer "*Messages*")
            (goto-char (point-max))
            (insert formatted-string "\n"))
        (progn
          (ad-set-args 0 `("%s" ,formatted-string))
          ad-do-it)))))


;;
;; Helper function for display buffer-name in modeline
;;
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie Expand flexible
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun try-expand-flexible-abbrev (old)
  "Try to complete word using flexible matching.

Flexible matching works by taking the search string and then
interspersing it with a regexp for any character. So, if you try
to do a flexible match for `foo' it will match the word
`findOtherOtter' but also `fixTheBoringOrange' and
`ifthisisboringstopreadingnow'.

The argument OLD has to be nil the first call of this function, and t
for subsequent calls (for further possible completions of the same
string).  It returns t if a new completion is found, nil otherwise."
  (if (not old)
      (progn
        (he-init-string (he-lisp-symbol-beg) (point))
        (if (not (he-string-member he-search-string he-tried-table))
            (setq he-tried-table (cons he-search-string he-tried-table)))
        (setq he-expand-list
              (and (not (equal he-search-string ""))
                   (he-flexible-abbrev-collect he-search-string)))))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (if old (he-reset-string))
        ())
    (progn
      (he-substitute-string (car he-expand-list))
      (setq he-expand-list (cdr he-expand-list))
      t)))

(defun he-flexible-abbrev-collect (str)
  "Find and collect all words that flex-matches STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (let ((collection nil)
        (regexp (he-flexible-abbrev-create-regexp str)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp regexp nil t)
        ;; Is there a better or quicker way than using
        ;; `thing-at-point' here?
        (setq collection (cons (thing-at-point 'symbol) collection))))
    (reverse collection)))

(defun he-flexible-abbrev-create-regexp (str)
  "Generate regexp for flexible matching of STR.
See docstring for `try-expand-flexible-abbrev' for information
about what flexible matching means in this context."
  (let ((constituent "[A-za-z0-9_]*"))
    (concat (mapconcat (lambda (x) (concat constituent (list x))) str "")
            constituent)))

(setq hippie-expand-try-functions-list
      (cons 'try-expand-flexible-abbrev hippie-expand-try-functions-list))



;;
;; Rename current file interactive
;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file;;
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;
;; Make script executable on saving
;;
(defun make-executable-on-save ()
  "Make script executable on saving."
  (interactive)
  (if (not (= (shell-command (concat "test -x " (buffer-file-name))) 0))
      (progn 
        ;; This puts message in *Message* twice, but minibuffer
        ;; output looks better.
        (message (concat "Wrote " (buffer-file-name)))
        (save-excursion
          (goto-char (point-min))
          (if (looking-at "^#!")
              (if (= (shell-command  
                      (concat "chmod u+x " (buffer-file-name)))
                     0)
                  (message (concat 
                            "Wrote and made executable " 
                            (buffer-file-name)))))))
    ;; This puts message in *Message* twice, but minibuffer output
    ;; looks better.
    (message (concat "Wrote " (buffer-file-name)))))


;;
;; 
;;
(defun auto-load-mode (filetypes mode)
  "Autoload mode for filetype regex or a list of filetypes."
  (if (stringp filetypes)
      (add-to-list 'auto-mode-alist (cons filetypes mode))
    (dolist (filetype filetypes)
      (add-to-list 'auto-mode-alist (cons filetype mode)))))


;;
;; Window functional helper
;; Thank to cmpitg: https://github.com/cmpitg/emacs-cmpitg
;;
(defun ~one-window ()
  "Delete all other non-dedicate window."
  (interactive)
  (mapcar #'(lambda (window)
              (unless (window-dedicated-p window)
                (delete-window window))
              (message "Have at least one window is sticky."))
          (cdr (window-list)))
  )

(defun ~delete-window ()
  "Delete current window if it's not sticky/dedicated. Use prefix arg 
(`C-u') to force deletion if it is."
  (interactive)
  (or (and (not current-prefix-arg)
           (window-dedicated-p (selected-window))
           (message "Window '%s' is sticky/dedicated, should you want to delete, re-invoke the command with C-u prefix."
                    (current-buffer)))
      (delete-window (selected-window))))

(defun make-window-sticky ()
  "Make the current window always display this buffer."
  (interactive)
  (let* ((window (get-buffer-window (current-buffer)))
         (dedicated? (window-dedicated-p window)))
    (if (not dedicated?)
        (progn
          (face-remap-add-relative 'mode-line-sticky-face
                                   '(:foreground "#8BE03C"))
          (message "Window '%s' is sticky now" (current-buffer))
          )
      (progn
        (face-remap-add-relative 'mode-line '(:background "#0D1011"))
        (message "window '%s' is normal" (current-buffer))
        ))
    (set-window-dedicated-p window (not dedicated?))))


;;
;; Popup notification
;; http://emacs-fu.blogspot.ae/2009/11/showing-pop-ups.html
;;
(defun djcb-popup (title msg)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context."
  (interactive)
  (if (eq window-system 'x)
      (shell-command (concat "notify-send "
                             " '" title "' '" msg "'"))
    ;; text only version
    (message (concat title ": " msg))))

;; (djcb-popup "Wanderlust" "You have new mail!")

;;
;; Xah functions utilities
;; http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html
;;
(defvar xah-recently-closed-buffers nil "alist of recently closed buffers.
Each element is (buffer name, file path). The max number to track is 
controlled by the variable 'xah-recently-closed-buffers-max'.")

(defvar xah-recently-closed-buffers-max 40 "The maximum length for
'xah-recently-closed-buffers'.")


(defun xah-close-current-buffer ()
  "Close the current buffer.

Similar to 'kill-buffer', with the following addition:

• Prompt user to save if the buffer has been modified even if the buffer is 
not associated with a file.
• Make sure the buffer shown after closing is a user buffer.
• If the buffer is editing a source file in an org-mode file, prompt the user 
to save before closing.
• If the buffer is a file, add the path to the list `xah-recently-closed
-buffers'.
• If it is the minibuffer, exit the minibuffer

A emacs buffer is one who's name starts with *.
Else it is a user buffer."
  (interactive)
  (let (ξemacs-buff-p
        (ξorg-p (string-match "^*Org Src" (buffer-name))))

    (setq ξemacs-buff-p (if (string-match "^*" (buffer-name)) t nil))

    (if (string= major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit) ; if the buffer is minibuffer
      (progn
        ;; offer to save buffers that are non-empty and modified, even for
        ;; non-file visiting buffer. (because kill-buffer does not offer
        ;; to save buffers that are not associated with files)
        (when (and (buffer-modified-p)
                   (not ξemacs-buff-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        (when (and (buffer-modified-p)
                   ξorg-p)
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))

        ;; save to a list of closed buffer
        (when (not (equal buffer-file-name nil))
          (setq xah-recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) xah-recently-closed-buffers))
          (when (> (length xah-recently-closed-buffers) xah-recently-closed-buffers-max)
            (setq xah-recently-closed-buffers (butlast xah-recently-closed-buffers 1))))

        ;; close
        (kill-buffer (current-buffer))

        ;; if emacs buffer, switch to a user buffer
        (when (string-match "^*" (buffer-name))
          (next-buffer)
          (let ((i 0))
            (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
              (setq i (1+ i)) (next-buffer))))))))

(defun xah-open-last-closed ()
  "Open the last closed file."
  (interactive)
  (find-file (cdr (pop xah-recently-closed-buffers))))

(defun xah-open-recently-closed ()
  "Open recently closed file."
  (interactive)
  (find-file (ido-completing-read
              "Open:" (mapcar (lambda (f)
                                (cdr f)) xah-recently-closed-buffers))))


(provide 'e:funcs)
