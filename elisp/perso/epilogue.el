;;; epilogue.el --- Load feature that needs to loaded at the end. -*- lexical-binding:t -*-
;;; Commentary:
;; keep persistent-scratch and eyebrowse at the end of the loading.
;;; Code:

(require 'use-package)
(require 'general-interface)
(require 'programming)

(use-package persistent-scratch
  :init (progn
          (persistent-scratch-autosave-mode t)
          (ignore-errors (persistent-scratch-restore)))

  :custom (persistent-scratch-save-file "~/.emacs.d/.cache/persistent-scratch")
          (persistent-scratch-what-to-save '(major-mode point narrowing text-properties)))

(use-package eyebrowse
  :init (eyebrowse-mode t)
  :custom (eyebrowse-mode-line-style 'always)
  :general ("C-<right>" #'eyebrowse-next-window-config
            "C-<left>" #'eyebrowse-prev-window-config)
  :config (require 'evil-eyebrowse))

(my/set-mode-in-emacs-state)

(provide 'epilogue)
;;; epilogue.el ends here
