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

;; (setq package-user-dir "~/.emacs.d/elpa")
;; (require 'package)

;; (setq package-archives
;;     '(("melpa" . "https://melpa.org/packages/")
;;       ("gnu" . "https://elpa.gnu.org/packages/")
;;       ("org" . "https://orgmode.org/elpa/")))

(setq load-prefer-newer t)
;;(package-initialize)

(push (file-name-as-directory "~/.emacs.d/themes/") custom-theme-load-path)
(setq custom-file (expand-file-name "custom.el" "~/.emacs.d/"))
(load custom-file)

(load "~/.emacs.d/load-packages")
(load "~/.emacs.d/programming-modes")
(load  "~/.emacs.d/bindings")

(recentf-mode)
;; (setq recentf-exclude `(,(expand-file-name "~/.emacs.d/bookmarks")
;;                         ,(expand-file-name "~/.emacs.d/persp-confs/persp-auto-save")
;;                         ,(expand-file-name "~/.emacs.d/persp-confs/haskell")
;;                         ,(expand-file-name "~/.emacs.d/persp-confs/cl")))
(setq recentf-exclude (mapcar #'expand-file-name
                             '("~/.emacs.d/bookmarks"
                               "~/.emacs.d/persp-confs/persp-auto-save"
                               "~/.emacs.d/persp-confs/haskell"
                               "~/.emacs.d/persp-confs/cl"
                               )))

(setq transient-mark-mode 't)
;; Peut poser un problème lorsqu'on édite un fichier
;; qui est destiné à être une liste de fichier pour tar
;; option -T de tar.
(setq require-final-newline t)
;; (setq display-time-24hr-format t)
;;(display-time)  ;avoir l'heure dans la barre de mode
;; (setq european-calendar-style t)
(autoload 'gid "id-utils" "id-utils" t nil)
(setq abbrev-file-name (expand-file-name "~/.emacs.d/abbrev_defs"))
(setq save-abbrevs 'silently)
(quietly-read-abbrev-file)

(defalias 'man-mode 'Man-mode)
;; (setq ange-ftp-default-user "anonymous")
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

(load "~/.emacs.d/evil-eyebrowse")
