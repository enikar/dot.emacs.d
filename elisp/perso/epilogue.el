;;; epilogue.el --- Load feature that needs to loaded at the end. -*- lexical-binding:t -*-
;;; Commentary: 
;; keep persistent-scratch and eyebrowse at the end of the loading.
;;; Code: 

;; (eval-when-compile
;;   (require 'use-package))

;; provoque l'erreur :
;; Eager macro-expansion failure: (wrong-type-argument listp [(first-item . rest-items) (sp-get-list-items)])
(use-package persistent-scratch
  :init (progn
          (persistent-scratch-autosave-mode t)
          (ignore-errors (persistent-scratch-restore)))

  :custom (persistent-scratch-save-file "~/.emacs.d/.cache/persistent-scratch")
          (persistent-scratch-what-to-save '(major-mode point narrowing text-properties)))

(use-package eyebrowse
  :custom (eyebrowse-mode-line-style 'always)
  :init (eyebrowse-mode t)
  :config (require 'evil-eyebrowse))

(provide 'epilogue)
;;; epilogue.el ends here
