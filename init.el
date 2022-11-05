;;; init.el --- emacs start up initialization file
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "enikar")

;; start emacs server early
(server-start)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/")
      abbrev-file-name (expand-file-name "~/.emacs.d/abbrev_defs")
      save-abbrevs 'silently
      recentf-max-saved-itmes 30
      use-package-enable-imenu-support t)

(push (file-name-as-directory "~/.emacs.d/themes/") custom-theme-load-path)
(push "~/.emacs.d/elisp/perso" load-path)

(defvar-local my/bookmarks-file-name
  (expand-file-name "~/.emacs.d/bookmarks"))

(defun my/recentf-exclude (f)
  "Predicate to exlude filename from the recent file name list"
    ;; (or (string-equal f my/bookmarks-file-name)
    ;;     (string-equal (file-name-directory f) persp-save-dir)))
  (string-equal f my/bookmarks-file-name))

(setq recentf-exclude  `(,#'my/recentf-exclude))

(let ((file-name-handler-alist nil))
  (load custom-file)
  (require 'general-interface)
  (require 'programming)
  (require 'personal-commands)
  (require 'epilogue)
  (recentf-mode)
  (quietly-read-abbrev-file))

;;; init.el ends here
