;;; epilogue.el --- Load feature that needs to loaded at the end. -*- lexical-binding: t -*-
;;; Commentary:
;; keep persistent-scratch and eyebrowse at the end of the loading.
;;; Code:

(require 'use-package)
(require 'general-interface)
(require 'programming)
(require 'personal-commands)
(require 'dash)

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

;; (cl-loop
;;  for font in my/favorite-fonts
;;  if (font-available-p font)
;;  do (set-frame-font font t)
;;     (cl-return))

;; More functionnal way. We can also use cl-some, but for simple task
;; dash is more efficient, although it is not built into emacs. In any
;; event cl-some or -some are more efficient than cl-loop.
(let ((font (-some #'font-available-p my/favorite-fonts)))
  (if font
      (set-frame-font font t)))

(provide 'epilogue)
;;; epilogue.el ends here
