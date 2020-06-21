(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "karine")
(setq gc-cons-threshold 10000000)

;; (setq initial-frame-alist
;;       '((top . 1) (left . 1) (width . 80) (height . 35)))
;;         ;(foreground-color . "black")
;;         ;(background-color . "snow2"))


(setq default-frame-alist
      '((top . 0)
        (left . 0)
        (width . 158)
        (height . 40)
        (menu-bar-lines 0)
        (tool-bar-lines 0)
        (vertical-scroll-bars)
        (font . "-PfEd-Inconsolata-normal-normal-normal-*-24-*-*-*-m-0-iso10646-1")
        ))

(setq-default indicate-empty-lines t)

(menu-bar-mode 0)


(add-to-list 'custom-theme-load-path (file-name-as-directory "~/.emacs.d/themes/") )
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/"))
(load custom-file)

(load "~/.emacs.d/load-packages")
(load "~/.emacs.d/programming-modes")
(load  "~/.emacs.d/bindings")

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

(defalias 'man-mode 'Man-mode)
(setq comint-scroll-show-maximum-output t)
(setq comint-scroll-to-bottom-on-input t)
;(setq line-number-mode t)

(setq scroll-step 1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; finally start emacs server
(server-start)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; try to load evil-eyebrowse in load-package
;; (load "~/.emacs.d/evil-eyebrowse")
