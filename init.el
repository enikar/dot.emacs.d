;;; init.el --- emacs start up initialization file -*- lexical-binding: t -*-
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "enikar")


;; start emacs server early
(server-start)

(push (file-name-as-directory "~/.emacs.d/themes/") custom-theme-load-path)
(load-theme 'tsdh-modified t)

;;;; Packages stuff
(setq package-user-dir "~/.emacs.d/elpa")
(setq load-prefer-newer t)
(setq package-archives
    '(("melpa" . "https://melpa.org/packages/")
      ("elpa"   . "https://elpa.gnu.org/packages/")
      ("elpa-devel" . "https://elpa.gnu.org/devel/")
      ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(setq package-archive-priorities
      '(("melpa" . 3)
        ("elpa" . 2)
        ("nongnu" . 1)
        ("elpa-devel" . 1)))
;; package-pinned-packages: alist of packages which dont't follow package-archive-priorities
;; none for the moment.

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(defvar-local my/emacs-var-dir
  (expand-file-name "var" user-emacs-directory))

(defun my/put-this-in-var (name)
  (expand-file-name name my/emacs-var-dir))

(setq custom-file (make-temp-file "emacs-custom-")
      abbrev-file-name (my/put-this-in-var "abbrev_defs")
      save-abbrevs 'silently
      bookmark-default-file (my/put-this-in-var "bookmarks")
      recentf-save-file (my/put-this-in-var "recentf")
      recentf-max-saved-items 30
      save-place-file (my/put-this-in-var "saveplace"))


(push "~/.emacs.d/elisp/perso" load-path)

(defun my/recentf-exclude (f)
  "Predicate to exlude filename from the recent file name list"
    ;; (or (string-equal f bookmark-default-file)
    ;;     (string-equal (file-name-directory f) persp-save-dir)))
  (string-equal f bookmark-default-file))

(setq recentf-exclude  `(,#'my/recentf-exclude))

(let ((file-name-handler-alist nil))
  (require 'general-interface)
  (require 'programming)
  (require 'personal-commands)
  (require 'epilogue)
  (recentf-mode)
  (quietly-read-abbrev-file))

;;; init.el ends here
