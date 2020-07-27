;;; epilogue.el --- Load feature that needs to loaded at the end. -*- lexical-binding:t -*-
;;; Commentary: 
;; keep persistent-scratch and eyebrowse at the end of the loading.
;;; Code: 

(eval-when-compile
  (require 'use-package))

(use-package persistent-scratch
  :ensure t
  :custom (persistent-scratch-save-file "~/.emacs.d/.cache/persistent-scratch")
          (persistent-scratch-what-to-save '(major-mode point narrowing text-properties))
  :init
  (progn
    (persistent-scratch-autosave-mode t)
    (ignore-errors (persistent-scratch-restore))))

(use-package eyebrowse
  :ensure t
  :custom (eyebrowse-mode-line-style 'always)
  :init (eyebrowse-mode t)
  :config (require 'evil-eyebrowse))

(provide 'epilogue)
;;; epilogue.el ends here
