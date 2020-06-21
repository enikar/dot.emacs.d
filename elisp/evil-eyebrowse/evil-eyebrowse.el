;; evil-eyebrowse --- Ease the use of eyebrowse in evil

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

;; I just copy/paste code from eyebrowse and adapt
;; the method in eyebrowse--format-slot and eyebrowse--read-slot.
;; NOTE: This function needs improvement. When a slot
;; contains two or more windows, nothing is listed (just an number).
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

(defun evil-eb--next-window-config (c)
  (interactive "P")
  (eyebrowse-next-window-config c))

(defun evil-eb--previous-window-config (c)
  (interactive "P")
  (eyebrowse-prev-window-config c))

(defun evil-eb--switch-to-window-config (c)
  (interactive (list (if (numberp current-prefix-arg)
                           current-prefix-arg
                           (evil-eb--read-slot))))
  (eyebrowse-switch-to-window-config c))

;; TODO: functions to open a new file in new window config (Z E)
;; and to swith to a buffer in a new window config (Z B)

(defun evil-eb--make-conses (prefix)
  (let ((assocs
         `(("N" . ,#'evil-eb--next-window-config) 
           ("P" . ,#'evil-eb--previous-window-config)
           ("A" . ,#'eyebrowse-last-window-config)
           ("\"" . ,#'evil-eb--switch-to-window-config)
           ("C" . ,#'eyebrowse-create-window-config)
           ("Q" . ,#'eyebrowse-close-window-config))))
        (mapcar #'(lambda(x)
                    (cons (kbd (concat prefix " " (car x))) (cdr x)))
                assocs)
        ))
;; try to make a prefix-map like in eyebrowse.el
;; look at variable eyebrowse-mode-map
;;(define-key evil-normal-state-map (kbd "Z N") 'eyebrowse-next-window-config)
(defun evil-eb-make-map (map prefix)
  "Make bindings for others keys than 0 to 9"
  (let ((bindings (evil-eb--make-conses prefix)))
    (mapc #'(lambda(x)
              (define-key map (car x) (cdr x)))
          bindings)))

(defun evil-eb--make-goto (n)
  `(lambda () (interactive (eyebrowse-switch-to-window-config ,n))))

(defun evil-eb-make-goto-map (map prefix)
  "Make bindings for keys 0 to 9"
  (let ((bindings
         (mapcar #'(lambda(n)
                 (cons (kbd (concat prefix " " (number-to-string n)))
                       (evil-eb--make-goto n)))
                 (number-sequence 0 9))))
    (mapc #'(lambda(x) (define-key map (car x) (cdr x)))
          bindings)))

(defun evil-eb-update-map (map)
  "Function to define all bindings for `map'"
  (evil-eb-make-goto-map map "Z")
  (evil-eb-make-map map "Z"))

(defun evil-eb-add-vim-compat (map)
  "Add gT and gt to switch to previous and next
   slot respectively."
  (define-key map "gT" 'eyebrowse-prev-window-config)
  (define-key map "gt" 'eyebrowse-next-window-config))

(evil-eb-update-map evil-normal-state-map)
(evil-eb-add-vim-compat evil-normal-state-map)
(with-eval-after-load 'flycheck
  (evil-eb-update-map flycheck-error-list-mode-map))

;; Add shortcuts for specific mode
(add-hook 'Man-mode-hook
         (function (lambda ()
                     (evil-eb-update-map Man-mode-map))))

(add-hook 'help-mode-hook
        (function
         (lambda ()
           (evil-eb-update-map help-mode-map))))


(add-hook 'Info-mode-hook
        (function
         (lambda ()
           (evil-eb-update-map Info-mode-map)
           )))

;; Add some shortcut to the packages-menu-mode (list-packages)
;; in his map (package-menu-mode-map). We use package-menu-mode-hook.
(add-hook 'package-menu-mode-hook
          (function
           (lambda ()
             (evil-eb-update-map package-menu-mode-map)
             (evil-eb-add-vim-compat package-menu-mode-map)
             (define-key package-menu-mode-map (kbd "$") #'end-of-line)
             (define-key package-menu-mode-map (kbd "^") #'beginning-of-line)
             )))

(provide 'evil-eyebrowse)

;;; evil-eyebrowse.el ends here
