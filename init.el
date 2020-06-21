(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "karine")

(setq-default indicate-empty-lines t)


(add-to-list 'custom-theme-load-path (file-name-as-directory "~/.emacs.d/themes/") )
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/"))
(load custom-file)

(load "~/.emacs.d/load-packages")
(load "~/.emacs.d/programming-modes")
(load "~/.emacs.d/bindings")

(recentf-mode)
(setq recentf-exclude
      (mapcar #'expand-file-name
              '("~/.emacs.d/bookmarks"
                "~/.emacs.d/persp-confs/persp-auto-save"
                "~/.emacs.d/persp-confs/haskell"
                "~/.emacs.d/persp-confs/cl")))

;; Peut poser un problème lorsqu'on édite un fichier
;; qui est destiné à être une liste de fichier pour tar
;; option -T de tar.
(setq require-final-newline t)
(setq abbrev-file-name (expand-file-name "~/.emacs.d/abbrev_defs"))
(setq save-abbrevs 'silently)
(quietly-read-abbrev-file)
(save-place-mode 1)

(defalias 'man-mode 'Man-mode)
(setq comint-scroll-show-maximum-output t)
(setq comint-scroll-to-bottom-on-input t)

(setq scroll-step 1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; finally start emacs server
(server-start)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
