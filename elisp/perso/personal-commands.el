;;; personal-commands.el --- commands I use sometimes -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:
(require 'general-interface)

(defun my-hscroll-mode ()
  "Set buffer local truncate-lines's value to t."
  (interactive)
  (set-variable 'truncate-lines t))

(prefix-c-xw "s" #'my-hscroll-mode)

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

(prefix-c-xt "g" #'toggle-indent-tabs-mode)

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

(prefix-c-xt "b" #'trim-buffer)

(defun no-break-to-space ()
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
        (no-break-to-space)
        (widen)))))

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

(defun my/set-personnal-font ()
  "Restore my favorite font setting."
  (interactive)
  (set-frame-font "-PfEd-Inconsolata-normal-normal-normal-*-24-*-*-*-m-0-iso10646-1"))

(provide 'personal-commands)
;;; personal-commands.el ends here
