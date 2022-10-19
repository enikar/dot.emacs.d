;;; epilogue.el --- Load feature that needs to loaded at the end. -*- lexical-binding:t -*-
;;; Commentary:
;; keep persistent-scratch and eyebrowse at the end of the loading.
;;; Code:

(require 'general-interface)
(require 'programming)

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

;; Finally, bind "C-x t" prefix
(prefix-c-xt
  "e" #'recentf-edit-list
  "i" #'indent-region
  "p" #'pop-tag-mark
  "r" #'consult-recent-file
  "t" #'treemacs
  "C-f" #'fci-mode)

(prefix-c-xt
  :no-autload t ; autoload is done with use-package declaration
  "f" #'flycheck-mode)

;; for which-key
(general-unbind evil-window-map
  "C-h" ; use by which-key
  ;; "gt"  ; bindings to emacs tab functions
  ;; "gT"
  "g" ; remove the prefix is sufficient
  )

(provide 'epilogue)
;;; epilogue.el ends here
