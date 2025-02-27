;;; epilogue.el --- Loads feature that needs to load at the end. -*- lexical-binding: t -*-
;;; Commentary:
;; Load persistent-scratch, eyebrowse and set modes to be in initial emacs-state.
;; Finally load desktop.

;;; Code:

(eval-when-compile
  (require 'use-package))
(require 'general-interface)
(require 'programming)

;;;; Load persitent scratch
(defun scratch-buffer-settings ()
  (with-current-buffer "*scratch*"
    (setq lexical-binding t)
    (evil-mode)))

(use-package persistent-scratch
  :init (persistent-scratch-autosave-mode t)
        (ignore-errors (persistent-scratch-restore))

  :hook (after-init . scratch-buffer-settings)

  :custom (persistent-scratch-save-file (my/put-this-in-var "persistent-scratch"))
          (persistent-scratch-what-to-save '(major-mode point narrowing text-properties)))

;;;; Load eyebrowse and evil-eyebrowse
(use-package eyebrowse
  :hook (after-init . eyebrowse-mode)
  :custom (eyebrowse-mode-line-style 'always)
  :general ("C-<right>" #'eyebrowse-next-window-config
            "C-<left>" #'eyebrowse-prev-window-config)
  :config (which-key-add-key-based-replacements
            "C-c C-w" "Eyebrowse"
            "M-z" "Evil eyebrowse")
          (require 'evil-eyebrowse))

;; set some mode to be in emacs state.
(add-hook 'after-init-hook #'my/set-mode-in-emacs-state)

;;;; Load desktop
(desktop-save-mode 1)
(setq desktop-load-locked-desktop 'check-pid
      desktop-globals-to-save
      (append
       '((consult--buffer-history . 20)
         (consult--apropos-history . 20)
         (Info-isearch-initial-history . 20)
         (Info-search-history . 20)
         (Man-topic-history . 20)
         (bookmark-history . 20)
         (buffer-name-history . 20)
         (calc-alg-entry-history . 20)
         (command-history . 20)
         (compile-history . 20)
         (cscope-prompt-minibuffer-history . 20)
         (evil-ex-history . 20)
         (evil-ex-search-history . 20)
         (evil-markers-alist . 20)
         (evil-search-backward-history . 20)
         (evil-search-forward-history . 20)
         (evil-jumps-history . 20)
         (eww-prompt-history . 20)
         (extended-command-history . 20)
         (geiser-doc--history . 20)
         (grep-files-history . 20)
         (grep-regexp-history . 20)
         (hi-lock-face-history . 20)
         (hi-lock-regexp-history . 20)
         (info-lookup-history . 20)
         (kill-ring . 20)
         (kmacro-ring . 20)
         (magit-git-command-history . 20)
         (magit-revision-history . 20)
         (minibuffer-history . 20)
         (occur-collect-regexp-history . 20)
         (query-replace-history . 20)
         (read-char-history . 20)
         (read-expression-history . 20)
         (realgud:byebug-minibuffer-history . 20)
         (realgud:perldb-minibuffer-history . 20)
         (realgud:pry-minibuffer-history . 20)
         (regexp-history . 20)
         (set-variable-value-history . 20)
         (shell-command-history . 20)
         (slime-connect-host-history . 20)
         (slime-connect-port-history . 20)
         (slime-inferior-lisp-program-history . 20)
         (slime-minibuffer-history . 20)
         (slime-repl-shortcut-history . 20)
         (tmm--history . 20)
         (xref--history . 20)
         (xref--read-identifier-history . 20)
         )
       desktop-globals-to-save))
(provide 'epilogue)
;;; epilogue.el ends here
