;; -*- lexical-binding: t -*-

;;;; startup hooks
;; Increases Garbage Collection During Startup
;;(defvar startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          #'(lambda()
              (setq gc-cons-threshold (expt 2 23))))

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "*** Emacs loaded in %.2f with %d garbage collections."
                       (float-time
                        (time-subtract after-init-time before-init-time))
                       gcs-done)))


;;;; Settings for Xorg. Ignore settings from .Xresources and .Xdefault
(advice-add #'x-apply-session-resources :override #'ignore)
(setq default-frame-alist
      '((top . 0)
        (left . 0)
        (width . 158)
        (height . 39)
        (menu-bar-lines 0)
        (tool-bar-lines 0)
        (vertical-scroll-bars)
        ;; (font . "-PfEd-Inconsolata-normal-normal-normal-*-24-*-*-*-m-0-iso10646-1")))
        (font . "Inconsolata 18")))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;;; Dissociate (TAB, C-i), (ESC, C-[) and (RET, C-m).

;; We win 3 key chords when using a graphical display.
;; We can bind them with `(define-key [C-i] #'command-to-be-bound)' or
;; `(define-key (kbd "<C-i>") #'command-to-be-bound)' for example.
;; Notice [C-i] and (Kbd "<C-i>") identify the same key code while
;; (kbd "C-i") returns a literal TAB character.

;; (add-hook
;;  'after-make-frame-functions
;;  (defun setup-blah-keys (frame)
;;    (with-selected-frame frame
;;      (when (display-graphic-p)
;;        (define-key input-decode-map (kbd "C-i") [C-i])
;;        (define-key input-decode-map (kbd "C-[") [C-lsb])
;;        (define-key input-decode-map (kbd "C-m") [C-m])))))
