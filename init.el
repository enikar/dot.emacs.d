(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "karine")

(add-to-list 'custom-theme-load-path (file-name-as-directory "~/.emacs.d/themes/") )
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/"))
(load custom-file)

;; (setq recentf-exclude
;;       (mapcar #'expand-file-name
;;               (cons "~/.emacs.d/bookmarks"
;;                     (mapcar
;;                      (lambda(f) (concat "~/.emacs.d/persp-conf/" f))
;;                      '("persp-auto-save" "haskell" "cl" "C" "default")))))
(defvar my/bookmarks-file-name
  (expand-file-name "~/.emacs.d/bookmarks"))
(defvar my/persp-confs-dir
  (expand-file-name "~/.emacs.d/persp-confs/"))

(defun my/recentf-exclude (f)
  "Predicate to exlude filename from recent file name list"
  (progn
    ;; (message "From my/recentf-exclude file name: %s" f) ;; to debug
    (or (equal f my/bookmarks-file-name)
        (equal (file-name-directory f) my/persp-confs-dir))))

(setq recentf-exclude  `(,#'my/recentf-exclude))

(load "~/.emacs.d/load-packages")
(load "~/.emacs.d/bindings")

;; Peut poser un problème lorsqu'on édite un fichier
;; qui est destiné à être une liste de fichier pour tar
;; option -T de tar.
(setq require-final-newline t)
(setq-default indicate-empty-lines t)
(setq abbrev-file-name (expand-file-name "~/.emacs.d/abbrev_defs"))
(setq save-abbrevs 'silently)
(quietly-read-abbrev-file)

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

;; perhaps I should setq disabled-command-function to nil
;; thus there were no longer disabled commands.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
