;;; init.el --- emacs start up initialization file
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "enikar")

;; start emacs server early
(server-start)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(add-to-list 'custom-theme-load-path (file-name-as-directory "~/.emacs.d/themes/") )
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/")
      abbrev-file-name (expand-file-name "~/.emacs.d/abbrev_defs")
      save-abbrevs 'silently
      confirm-kill-processes nil
      native-comp-async-report-warnings-errors 'silent
      initial-scratch-message nil
      ring-bell-function 'ignore)

(add-to-list 'load-path "~/.emacs.d/elisp/perso")

(defvar-local my/bookmarks-file-name
  (expand-file-name "~/.emacs.d/bookmarks"))

(defun my/recentf-exclude (f)
  "Predicate to exlude filename from the recent file name list"
    (or (string-equal f my/bookmarks-file-name)
        (string-equal (file-name-directory f) persp-save-dir)))

(setq recentf-exclude  `(,#'my/recentf-exclude))

(let ((file-name-handler-alist nil))
  (load custom-file)
  (load "shadow-settings")
  (load "general-interface")
  (load "programming")
  (load "epilogue")
  (load "personal-bindings")
  (recentf-mode)
  (quietly-read-abbrev-file))


;;; init.el ends here
