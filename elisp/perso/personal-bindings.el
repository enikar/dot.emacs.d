;;; personal-bindings.el --- set my personnal bindings -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

;; I use evil-matchit instead.
;; (global-set-key "%" 'match-paren)
;; (defun match-paren (arg)
;;   "Go to the matching parenthesis if on parenthesis otherwise insert %."
;;   (interactive "p")
;;   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;         (t (self-insert-command (or arg 1)))))

(global-set-key (kbd "C-c d") #'yas-expand)
(global-set-key (kbd "M-RET")  #'hippie-expand)
(global-set-key (kbd "C-c h") #'hippie-expand)
;;(global-set-key (kbd "C-c e") 'flymake-popup-current-error-menu)
(define-key function-key-map [C-xw] (kbd "C-x w"))
(global-set-key (kbd "C-x w f") #'find-file-at-point)

;; un raccourci pour 'hexl-find-file : je l'utilise parfois souvent...
(global-set-key (kbd "C-x w h") #'hexl-find-file)

; plus quelques commodités (en anglais ça se dit : convenient)
; pour pouvoir insérer un fichier, écrire une région dans un fichier,
; ouvrir un fichier en mode visualistion (view-mode)
(global-set-key (kbd "C-x w i") #'insert-file)
(global-set-key (kbd "C-x w w") #'write-region)
(global-set-key (kbd "C-x w v") #'view-file)
(global-set-key (kbd "C-x w c") #'comment-dwim)
(global-set-key (kbd "C-x w s") #'my-hscroll-mode)

(defun my-hscroll-mode ()
  "Set buffer local truncate-lines's value to t."
  (interactive)
  (set-variable 'truncate-lines t))

(global-set-key (kbd "M-*") #'query-replace-regexp)
;(global-set-key "[n" '(lambda()(interactive)(scroll-up 1)))
;(global-set-key "[p" '(lambda()(interactive)(scroll-down 1)))

; pour font-lock-mode
(global-set-key (kbd "C-x w l") #'font-lock-mode)
(global-set-key (kbd "C-x w b") #'font-lock-fontify-block)
(global-set-key (kbd "C-x w a") #'font-lock-fontify-buffer)
;petit truc pour des listes de pages manuels
(global-set-key (kbd "C-x w m") #'man-follow)

;; pour une raison inconnue, emacs refuse de prendre en compte
;; les fonctions qui suivent lorsqu'il est lancé avec l'interface texte !!
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

;;(define-key function-key-map [C-xt] (kbd "C-x t"))
;;(global-set-key (kbd "C-x t t") 'tab8)
;;(global-set-key (kbd "C-x t u") 'tab8-undo)
(global-set-key (kbd "C-x t G") #'toggle-indent-tabs-mode)
;;(global-set-key (kbd "C-x t b") 'tabify-buffer)
;;(global-set-key (kbd "C-x t n") 'untabify-buffer)
(global-set-key (kbd "C-x t C-p") #'pop-tag-mark)

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

;;(global-set-key (kbd "C-x t t")   #'treemacs)
(global-set-key (kbd "C-x t C-b") #'trim-buffer)
;; (global-set-key (kbd "C-x t C-f") #'fci-mode)
;;(global-set-key (kbd "C-x t F")   #'flycheck-mode)

(with-eval-after-load 'recentf
  (progn
    ;; (global-set-key (kbd "C-x t C-r") #'recentf-open-files)
    (global-set-key (kbd "C-x t C-r") #'consult-recent-file)
    (global-set-key (kbd "C-x t C-e") #'recentf-edit-list)))


(global-set-key [f11] (function next-error))
(global-set-key [f12] (function previous-error))
(global-set-key [f9] (function compile))

;; (global-set-key [f8] (function my/toggle-flycheck-error-list))
;; (global-set-key (kbd "<f5>") (function flycheck-first-error))
;; (global-set-key (kbd "<f6>") (function flycheck-previous-error))
;; (global-set-key (kbd "<f7>") (function flycheck-next-error))

;; Add a shortcut to show only directories in dired
(fset 'dired-only-show-directories
      "*/tk")

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-x C-k D") #'dired-only-show-directories))

(with-eval-after-load 'evil-leader
  (evil-leader/set-key "gg" #'rgrep))

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

(provide 'personal-bindings)
;;; personal-bindings.el ends here
