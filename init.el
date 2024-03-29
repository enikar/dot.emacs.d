;;; init.el --- emacs start up initialization file -*- lexical-binding: t -*-
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "enikar")

;; start emacs server early
(server-start)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(defvar-local my/emacs-var-dir
  (expand-file-name "var" user-emacs-directory))

(defun my/put-this-in-var (name)
  (expand-file-name name my/emacs-var-dir))

(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/")
      abbrev-file-name (my/put-this-in-var "abbrev_defs")
      save-abbrevs 'silently
      bookmark-default-file (my/put-this-in-var "bookmarks")
      recentf-save-file (my/put-this-in-var "recentf")
      recentf-max-saved-itmes 30
      save-place-file (my/put-this-in-var "saveplace"))

(push (file-name-as-directory "~/.emacs.d/themes/") custom-theme-load-path)
(push "~/.emacs.d/elisp/perso" load-path)

(defun my/recentf-exclude (f)
  "Predicate to exlude filename from the recent file name list"
    ;; (or (string-equal f bookmark-default-file)
    ;;     (string-equal (file-name-directory f) persp-save-dir)))
  (string-equal f bookmark-default-file))

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
