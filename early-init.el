(setq package-user-dir "~/.emacs.d/elpa")
(setq package-archives
    '(("melpa" . "https://melpa.org/packages/")
      ("gnu"   . "https://elpa.gnu.org/packages/")
      ("org"   . "https://orgmode.org/elpa/")))

(setq load-prefer-newer t)

;; Increases Garbage Collection During Startup
(defvar startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          #'(lambda()
              (setq gc-cons-threshold startup/gc-cons-threshold)))

;; (setq initial-frame-alist
;;       '((top . 1) (left . 1) (width . 80) (height . 35)))
;;         ;(foreground-color . "black")
;;         ;(background-color . "snow2"))


(advice-add #'x-apply-session-resources :override #'ignore)
(setq default-frame-alist
      '((top . 0)
        (left . 0)
        (width . 158)
        (height . 40)
        (menu-bar-lines 0)
        (tool-bar-lines 0)
        (vertical-scroll-bars)
        (font . "-PfEd-Inconsolata-normal-normal-normal-*-24-*-*-*-m-0-iso10646-1")))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
