;;; personal-commands.el --- commands I use sometimes -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:
(require 'general-interface)

;; TODO: look to visual-line-mode
(defun toogle-hscroll-mode ()
  "Toggle buffer local truncate-lines's value."
  (interactive)
  (if truncate-lines
      (set-variable 'truncate-lines nil)
    (set-variable 'truncate-lines t)))

;;(prefix-c-xw "s" #'toggle-hscroll-mode)
(leader-ala-vim "th" #'toggle-hscroll-mode)

(defvar-local tab8-old-tab-width 0)
(defun tab8 ()
  "set tab-width to 8"
  (interactive)
  (if (= 0 tab8-old-tab-width)
      (save-window-excursion
          (progn
            (setq tab8-old-tab-width tab-width)
            (setq tab-width 8)
            (recenter)))))

(defun tab8-undo ()
  "revert tab-width to its previous value"
  (interactive)
  (if (or (not (boundp 'tab8-old-tab-width)) (null tab8-old-tab-width))
      (message "No information for tab8-undo")
       (save-window-excursion
           (progn
             (setq tab-width tab8-old-tab-width)
             (setq tab8-old-tab-width 0)
             (recenter)))))

(defun toggle-indent-tabs-mode ()
  "toggle indent-tabs-mode"
  (interactive)
  (if indent-tabs-mode
      (message "Now indent-tabs-mode is off")
      (message "Now indent-tabs-mode is on"))
  (setq indent-tabs-mode (not indent-tabs-mode)))

(leader-ala-vim "tg" #'toggle-indent-tabs-mode)

(defun tabify-buffer ()
  "tabify a buffer"
  (interactive)
  (save-window-excursion
    (save-excursion
      (tabify (point-min) (point-max)))))

(defun untabify-buffer ()
  "untabify a buffer"
  (interactive)
  (save-window-excursion
    (save-excursion
      (untabify (point-min) (point-max)))))


(defun trim-buffer ()
  "Remove trailing white spaces at the end of lines for a complete buffer."
  (interactive)
  (save-window-excursion
    (save-mark-and-excursion
      (goto-char (point-min))
      (while (re-search-forward "[\t ]+$" nil t)
        (replace-match "" t t)))))


(defun trim-region ()
  "Remove trailing white spaces at the end of lines for a region  between
   point and mark."
  (interactive)
  (save-restriction
    (save-window-excursion
      (save-mark-and-excursion
        (narrow-to-region (mark) (point))
        (trim-buffer)
        (widen)))))

(prefix-c-xw
  "T" #'trim-buffer
  "t" #'trim-region)

(defun no-break-to-space-in-buffer ()
  "Convert NON-BREAKING SPACE to simple SPACE in a whole buffer."
  (interactive)
  (save-window-excursion
    (save-mark-and-excursion
      (goto-char (point-min))
      (while (re-search-forward (rx ?\240) nil t)
        (replace-match " " t t)))))

(defun no-break-to-space-in-region ()
  "Convert NON-BREAKING SPACE to simple SPACE in a region."
  (interactive)
  (save-restriction
    (save-window-excursion
      (save-mark-and-excursion
        (narrow-to-region (mark) (point))
        (no-break-to-space-in-buffer)
        (widen)))))

(prefix-c-xw
 "n" #'no-break-to-space-in-region
 "N" #'no-break-to-space-in-buffer)

;; A trick to toggle between two background color
(defvar-local my/current-background
  '(default ((t (:background "gray15" :foreground "white smoke")))))
;; (defvar-local my/alternative-background
;;   '(default ((t (:background "gray17" :foreground "white smoke")))))

(defun my/toggle-background ()
  (interactive)
  (if (equal my/current-background '(default ((t (:background "gray15" :foreground "white smoke")))))
      (custom-set-faces (setq my/current-background '(default ((t (:background "gray17" :foreground "white smoke"))))))
    (custom-set-faces (setq my/current-background '(default ((t (:background "gray15" :foreground "white smoke"))))))))

(leader-ala-vim "tb" #'my/toggle-background)

(defun my/set-personnal-font ()
  "Restore my favorite font setting."
  (interactive)
  ;;(set-frame-font "-PfEd-Inconsolata-normal-normal-normal-*-24-*-*-*-m-0-iso10646-1")
  (set-frame-font "Inconsolata 18"))

;; Two functions borrow from Mickey Petersen:
;; https://www.masteringemacs.org/article/searching-buffers-occur-mode
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq mode major-mode)
          (push buf buffer-mode-matches))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(general-def "M-s M-m" #'multi-occur-in-this-mode)

;; From https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/
(defun font-available-p (font-name)
  "Check if a font is available"
  (find-font (font-spec :name font-name)))
;; Alternative defintion:
;; (defun font-available-p (font-name)
;;   (member font-name (font-family-list)))

(defvar my/favorite-fonts
  '("Inconsolata 18"
    "Go Mono Medium 15"
    "Liberation Mono Medium 15"
    "Fira Code Medium 16"
    "DejaVu Sans Mono Medium 16"
    "Hack Medium 15"
    "Menlo Medium 15"
    "Anonymous Pro Medium 17"
    "Source Code Pro Medium 15"))

(defun choose-default-font (font)
  "Choose a font interactively using the minibuffer among those
   in `my/favorite-fonts'."
  (interactive
   (let ((candidate
          (completing-read
           "Default frame font: "
           my/favorite-fonts)))
     `(,candidate)))
  (set-frame-font font t))

(leader-ala-vim "tF" #'choose-default-font)

(provide 'personal-commands)
;;; personal-commands.el ends here
