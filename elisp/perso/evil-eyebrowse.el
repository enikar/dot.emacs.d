;; evil-eyebrowse --- Ease the use of eyebrowse in evil -*- lexical-binding: t; -*-

;; Author: enikar <enikar at chezlefab.net>

;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil-eyebrowse is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil-eyebrowse is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; These are just shortcuts. `Z' is used as prefix key.
;; We use eyebrowse's slot as a vim's tab.
;;
;; | Key binding | Description                 |
;; |:-----------:|-----------------------------|
;; | `ZC'        | Create new slot             |
;; | `ZQ'        | Close current slot          |
;; | `ZN'        | Goto next slot              |
;; | `ZP'        | Goto previous slot          |
;; | `ZA'        | Switch other slot           |
;; | `Z0'        | Switch to slot 10           |
;; | `Z1'        | Switch to slot 1            |
;; | `Z2'        | Switch to slot 2            |
;; | `Z3'        | Switch to slot 3            |
;; | `Z4'        | Switch to slot 4            |
;; | `Z5'        | Switch to slot 5            |
;; | `Z6'        | Switch to slot 6            |
;; | `Z7'        | Switch to slot 7            |
;; | `Z8'        | Switch to slot 8            |
;; | `Z9'        | Switch to slot 9            |
;; | `Z"'        | Choose a slot interactively |
;;
;;
;; In addition, when possible, `gt'and `gT' work as in vim, i.e.
;; go to next and previous slot respectively.

;;; Code:

(require 'evil)
(require 'eyebrowse)
(require 'dash)
(require 'general)

;; I just copy/paste code from eyebrowse and adapt
;; the method in eyebrowse--format-slot and eyebrowse--read-slot.
;; NOTE: This function needs improvement. When a slot
;; contains two or more windows, nothing is listed (just a number).
(defun evil-eb--format-slot (slot)
  (let* ((n (number-to-string (car slot)))
         (current-slot (eyebrowse--get 'current-slot))
         (p (cadr slot))
         (bname (if (= current-slot (car slot))
                    (buffer-name)
                    (cadr (assoc 'buffer p)))))
    (concat n ": " bname)))


;; In next function the only difference with the original (eyebrowse--read-slot)
;; is the call to `evil-eb--format-slot'
;; This is ugly because i violate the privacy of the function eyebrowse--get
(defun evil-eb--read-slot ()
  "Read in a window config SLOT to switch to.
A formatted list of window configs is presented as candidates.
If no match was found, the user input is interpreted as a new
slot to switch to."
  (let* ((candidates (--map (cons (evil-eb--format-slot it)
                                  (car it))
                            (eyebrowse--get 'window-configs)))
         (candidate (completing-read "Enter slot: " candidates))
         (choice (cdr (assoc candidate candidates))))
    (or choice (eyebrowse--string-to-number candidate)
        (user-error "Invalid slot number"))))

(defun evil-eb--switch-to-window-config (c)
  (interactive (list (if (numberp current-prefix-arg)
                           current-prefix-arg
                           (evil-eb--read-slot))))
  (eyebrowse-switch-to-window-config c))

;; TODO: functions to open a new file in new window config (Z E)
;; and to swith to a buffer in a new window config (Z B)
;; Finaly, I built a keymap. It's simpler.
(defvar evil-eb-map
  (let ((prefix-map (make-sparse-keymap)))
    (define-key prefix-map (kbd "\"") #'evil-eb--switch-to-window-config)
    (define-key prefix-map (kbd "0") #'eyebrowse-switch-to-window-config-0)
    (define-key prefix-map (kbd "1") #'eyebrowse-switch-to-window-config-1)
    (define-key prefix-map (kbd "2") #'eyebrowse-switch-to-window-config-2)
    (define-key prefix-map (kbd "3") #'eyebrowse-switch-to-window-config-3)
    (define-key prefix-map (kbd "4") #'eyebrowse-switch-to-window-config-4)
    (define-key prefix-map (kbd "5") #'eyebrowse-switch-to-window-config-5)
    (define-key prefix-map (kbd "6") #'eyebrowse-switch-to-window-config-6)
    (define-key prefix-map (kbd "7") #'eyebrowse-switch-to-window-config-7)
    (define-key prefix-map (kbd "8") #'eyebrowse-switch-to-window-config-8)
    (define-key prefix-map (kbd "9") #'eyebrowse-switch-to-window-config-9)
    (define-key prefix-map (kbd "C") #'eyebrowse-create-window-config)
    (define-key prefix-map (kbd "P") #'eyebrowse-prev-window-config)
    (define-key prefix-map (kbd "N") #'eyebrowse-next-window-config)
    (define-key prefix-map (kbd "Q") #'eyebrowse-close-window-config)
    (define-key prefix-map (kbd "A") #'eyebrowse-last-window-config)
    prefix-map)
  "Inital keymap for `evil-eyebrowse'.")

(defun evil-eb-update-map (map)
  "Function to define all bindings for `map'"
  (define-key map (kbd "Z") evil-eb-map))


(defun evil-eb-add-vim-compat (map)
  "Add gT and gt to switch to previous and next
   slot respectively."
  (define-key map "gT" #'eyebrowse-prev-window-config)
  (define-key map "gt" #'eyebrowse-next-window-config))

(evil-eb-update-map evil-normal-state-map)
(evil-eb-add-vim-compat evil-normal-state-map)

;; Modify key bindings for specific modes
(with-eval-after-load 'flycheck
  (evil-eb-update-map flycheck-error-list-mode-map))

(declare-function 'dired-do-compress "dired-aux")
(with-eval-after-load 'dired
  (progn
    (general-def dired-mode-map "V" #'dired-do-compress)
    ;;(unbind-key "Z" dired-mode-map)
    (general-unbind dired-mode-map "Z")
    (evil-eb-update-map dired-mode-map)))

(with-eval-after-load 'man
  (if (boundp 'Man-mode-map)
      (evil-eb-update-map Man-mode-map)))


(defun evil-eb-ibuffer-mode-bindings ()
  (if (boundp 'ibuffer-mode-map)
      (evil-eb-update-map ibuffer-mode-map)))

(add-hook 'ibuffer-mode-hook #'evil-eb-ibuffer-mode-bindings)

(defun evil-eb-help-mode-bindings ()
  (if (boundp 'help-mode-map)
      (evil-eb-update-map help-mode-map)))

(add-hook 'help-mode-hook #'evil-eb-help-mode-bindings)


(defun evil-eb-info-mode-bindings ()
  (if (boundp 'Info-mode-map)
      (evil-eb-update-map Info-mode-map)))

(add-hook 'Info-mode-hook #'evil-eb-info-mode-bindings)

;; Add some shortcut to the packages-menu-mode (list-packages)
;; in his map (package-menu-mode-map). We use package-menu-mode-hook.
(defun evil-eb-package-menu-bindings ()
  (if (boundp 'package-menu-mode-map)
      (progn
        (evil-eb-update-map package-menu-mode-map)
        (evil-eb-add-vim-compat package-menu-mode-map)
        (define-key package-menu-mode-map (kbd "$") #'end-of-line)
        (define-key package-menu-mode-map (kbd "^") #'beginning-of-line))))

(add-hook 'package-menu-mode-hook #'evil-eb-package-menu-bindings)


;; Define a special binding for Customization buffers. It can also be usefull
;; for special mode where emacs state is more handy.
(defvar evil-eb-map2
  (let ((prefix-map (make-sparse-keymap)))
    (define-key prefix-map (kbd "M-\"") #'evil-eb--switch-to-window-config)
    (define-key prefix-map (kbd "0") #'eyebrowse-switch-to-window-config-0)
    (define-key prefix-map (kbd "1") #'eyebrowse-switch-to-window-config-1)
    (define-key prefix-map (kbd "2") #'eyebrowse-switch-to-window-config-2)
    (define-key prefix-map (kbd "3") #'eyebrowse-switch-to-window-config-3)
    (define-key prefix-map (kbd "4") #'eyebrowse-switch-to-window-config-4)
    (define-key prefix-map (kbd "5") #'eyebrowse-switch-to-window-config-5)
    (define-key prefix-map (kbd "6") #'eyebrowse-switch-to-window-config-6)
    (define-key prefix-map (kbd "7") #'eyebrowse-switch-to-window-config-7)
    (define-key prefix-map (kbd "8") #'eyebrowse-switch-to-window-config-8)
    (define-key prefix-map (kbd "9") #'eyebrowse-switch-to-window-config-9)
    (define-key prefix-map (kbd "M-c") #'eyebrowse-create-window-config)
    (define-key prefix-map (kbd "M-p") #'eyebrowse-prev-window-config)
    (define-key prefix-map (kbd "M-n") #'eyebrowse-next-window-config)
    (define-key prefix-map (kbd "M-q") #'eyebrowse-close-window-config)
    (define-key prefix-map (kbd "M-a") #'eyebrowse-last-window-config)
    (define-key prefix-map (kbd "Z")   #'zap-to-char) ; keep a binding to zap-to-char
    prefix-map)
  "Inital keymap for `Eyebrowse' for special buffers that need emacs state.")

(defun my/eyebrowse-customization-buffer-bindings ()
  (if (boundp 'custom-mode-map)
      (define-key custom-mode-map (kbd "M-z") evil-eb-map2)))

(add-hook 'Custom-mode-hook #'my/eyebrowse-customization-buffer-bindings)

(provide 'evil-eyebrowse)

;;; evil-eyebrowse.el ends here
