;;; init.el --- emacs start up initialization file
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "enikar")

(add-to-list 'custom-theme-load-path (file-name-as-directory "~/.emacs.d/themes/") )
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/"))
(setq abbrev-file-name (expand-file-name "~/.emacs.d/abbrev_defs"))
(setq save-abbrevs 'silently)
(add-to-list 'load-path "~/.emacs.d/elisp/perso")

(let ((file-name-handler-alist nil))
  (load custom-file)
  (load "general-interface")
  (load "programming")
  (load "epilogue")
  (load "personal-bindings")
  (quietly-read-abbrev-file))

;;;; better dired mode
(autoload 'dired-omit-mode "dired-x")
(add-hook 'dired-load-hook
          (lambda() (load "dired-x")))
(add-hook 'dired-mode-hook
          (lambda () (dired-omit-mode 1)))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defvar-local my/bookmarks-file-name
  (expand-file-name "~/.emacs.d/bookmarks"))

(defun my/recentf-exclude (f)
  "Predicate to exlude filename from the recent file name list"
    (or (string-equal f my/bookmarks-file-name)
        (string-equal (file-name-directory f) persp-save-dir)))

(setq recentf-exclude  `(,#'my/recentf-exclude))
(recentf-mode)

;; Peut poser un problème lorsqu'on édite un fichier
;; qui est destiné à être une liste de fichier pour tar
;; option -T de tar.
(setq require-final-newline t)
(setq-default indicate-empty-lines t)

(defalias 'man-mode 'Man-mode)
(setq comint-scroll-show-maximum-output t)
(setq comint-scroll-to-bottom-on-input t)

(setq scroll-step 1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; diminish some minor modes
(diminish 'auto-revert-mode "ARev")
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)

;; finally start emacs server
(server-start)

(defun my/set-personnal-font ()
  "Restore my favorite font setting."
  (interactive)
  (set-frame-font "-PfEd-Inconsolata-normal-normal-normal-*-24-*-*-*-m-0-iso10646-1"))

;; perhaps I should setq disabled-command-function to nil
;; thus there were no longer disabled commands.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;;; init.el ends here
