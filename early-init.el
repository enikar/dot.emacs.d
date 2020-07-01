(setq package-user-dir "~/.emacs.d/elpa")
(setq package-archives
    '(("melpa" . "https://melpa.org/packages/")
      ("gnu"   . "https://elpa.gnu.org/packages/")
      ("org"   . "https://orgmode.org/elpa/")))

(setq load-prefer-newer t
      gc-cons-threshold 800000000
      gc-cons-percentage 0.6)

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

(menu-bar-mode 0)

