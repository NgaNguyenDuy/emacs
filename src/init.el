;; Start emacs as daemon
;; (server-start)

;;
;; Global variable
;;
(defvar *cfg-dir* (file-name-directory (or (buffer-file-name) load-file-name))
  "Return config dirertory.")

;;
;; Config helpers
;;
(defun ~get-cfg (&rest paths)
  "Returns path to a config file or directory."
  (apply 'concat *cfg-dir* paths))

(defun get-library-full-path (library-name)
  "Return the full path to a library."
  (save-excursion
    (find-library library-name)
    (let ((file-path (or
                       (expand-file-name buffer-file-name)
                       "")))
      (kill-buffer)
      file-path))
  )

;; Load config function
(defun load-f (&rest p)
  "Return full file path."
  (apply 'concat *cfg-dir* p))


(require 'e:unicode-input (load-f "libs/unicode-input.el"))
(require 'e:funcs (load-f "funcs.el"))
;; (require 'e:skeletons (load-f "skeletons.el")) ;; disabled custom
;; auto-insert-header
(require 'e:pkg-manager (load-f "config-pkg.el"))
(require 'e:essen-pkgs (load-f "essen-pkgs.el"))
(require 'e:tspyimt-pkgs (load-f "tspyimt-pkgs.el"))
(require 'e:hooks (load-f "hooks.el"))
(require 'e:settings (load-f "settings.el"))
(require 'e:env (load-f "env.el"))
(require 'e:keybindings (load-f "keybindings.el"))
