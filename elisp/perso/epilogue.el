;;; epilogue.el --- Load feature that needs to loaded at the end. -*- lexical-binding:t -*-
;;; Commentary:
;; keep persistent-scratch and eyebrowse at the end of the loading.
;;; Code:

(use-package persistent-scratch
  :init (progn
          (persistent-scratch-autosave-mode t)
          (ignore-errors (persistent-scratch-restore)))

  :custom (persistent-scratch-save-file "~/.emacs.d/.cache/persistent-scratch")
          (persistent-scratch-what-to-save '(major-mode point narrowing text-properties)))

;; ;;;; Fix some keystroke for org-mode shadowed by evil: doesn't work!
;; (with-eval-after-load 'org
;;   (add-hook 'org-mode-hook
;;             (lambda()
;;               (evil-define-key '(normal insert visual motion emacs) org-mode-map
;;                 (kbd "TAB") #'org-cycle)
;;                 (kbd "RET") #'org-return)
;;                 (kbd "|")   #'org-force-self-insert))

(use-package evil-collection
  :diminish (evil-collection-unimpaired-mode)
  :init (evil-collection-init '(consult
                                corfu
                                embark
                                flycheck
                                magit
                                vertico
                                help
                                helpful
                                dired
                                package-menu)))

(use-package eyebrowse
  :custom (eyebrowse-mode-line-style 'always)
  :init (eyebrowse-mode t)
  :config (require 'evil-eyebrowse))

(provide 'epilogue)
;;; epilogue.el ends here
