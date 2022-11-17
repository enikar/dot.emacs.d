;;; epilogue.el --- Load feature that needs to loaded at the end. -*- lexical-binding: t -*-
;;; Commentary:
;; keep persistent-scratch and eyebrowse at the end of the loading.
;; Time-stamp: <2022-11-17 19:45:53 (enikar)>
;;; Code:

(require 'use-package)
(require 'general-interface)
(require 'programming)

(defun scratch-lexical-binding ()
  (with-current-buffer "*scratch*"
    (setq lexical-binding t)))

(use-package persistent-scratch
  :init (persistent-scratch-autosave-mode t)
        (ignore-errors (persistent-scratch-restore))

  :hook (after-init . scratch-lexical-binding)

  :custom (persistent-scratch-save-file (my/put-this-in-var "persistent-scratch"))
          (persistent-scratch-what-to-save '(major-mode point narrowing text-properties)))

(use-package eyebrowse
  :init (eyebrowse-mode t)
  :custom (eyebrowse-mode-line-style 'always)
  :general ("C-<right>" #'eyebrowse-next-window-config
            "C-<left>" #'eyebrowse-prev-window-config)
  :config (which-key-add-key-based-replacements
            "C-c C-w" "Eyebrowse"
            "M-z" "Evil eyebrowse")
          (require 'evil-eyebrowse))

(my/set-mode-in-emacs-state)

(provide 'epilogue)
;;; epilogue.el ends here
