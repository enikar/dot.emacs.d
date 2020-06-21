;;; programming-modes -- some programming mode settings
;;; Commentary:
;; programming mode settings that aren't in load-packages.el
;;; Code:


;(add-to-list 'load-path "/usr/local/share/emacs/24.4/site-lisp/bigloo")
(autoload 'gp-mode "pari" nil t)
(autoload 'gp-script-mode "pari" nil t)
(autoload 'gp "pari" nil t)
(autoload 'gpman "pari" nil t)

(add-hook 'ruby-mode-hook #'(lambda()
                            (flycheck-mode)
                            (setq tab-width 2
                                  evil-shift-width 2)))

;; un meilleur mode pour Objective Caml: tuareg (ahah! ils sont drôle ces gens là)
;;(setq auto-mode-alist (cons '("\\.ml\\'" . tuareg-mode) auto-mode-alist))
;;(autoload 'tuareg-mode "tuareg" "Major mode for editing OCaml" t)

(setq inferior-lisp-mode-hook
      '(lambda() (local-set-key [C-tab] 'comint-dynamic-complete-filename)))

;(setq inferior-lisp-program "clisp -I")
(setq scheme-program-name "guile")

(defun octave ()
  "Load octave el libraries."
  (interactive)
  (progn
    (load-library "octave")
    (run-octave)))


(add-hook 'shell-mode-hook (function (lambda () (setq tab-width 8))))

(provide 'programming-modes)
;;; programming-modes.el ends here
