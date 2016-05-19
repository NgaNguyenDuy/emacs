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

;; Load config function
(defun load-f (&rest p)
  "Return full file path."
  (apply 'concat *cfg-dir* p))


(require 'e:funcs (load-f "funcs.el"))
(require 'e:list-pkg (load-f "tspyimt-pkg.el"))
(require 'e:load-pkg (load-f "load-pkg.el"))
(require 'e:hooks (load-f "hooks.el"))
(require 'e:settings (load-f "settings.el"))
(require 'e:env (load-f "env.el"))
(require 'e:keybindings (load-f "keybindings.el"))
