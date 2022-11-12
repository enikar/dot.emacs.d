;;; epilogue.el --- Load feature that needs to loaded at the end. -*- lexical-binding:t -*-
;;; Commentary:
;; keep persistent-scratch and eyebrowse at the end of the loading.
;;; Code:

(require 'use-package)
(require 'general-interface)
(require 'programming)
(require 'personal-commands)
(require 'cl-lib)

(use-package persistent-scratch
  :init (persistent-scratch-autosave-mode t)
        (ignore-errors (persistent-scratch-restore))

  :custom (persistent-scratch-save-file (my/put-this-in-var "persistent-scratch"))
          (persistent-scratch-what-to-save '(major-mode point narrowing text-properties)))

(use-package eyebrowse
  :init (eyebrowse-mode t)
  :custom (eyebrowse-mode-line-style 'always)
  :general ("C-<right>" #'eyebrowse-next-window-config
            "C-<left>" #'eyebrowse-prev-window-config)
  :config
  (which-key-add-key-based-replacements
    "C-c C-w" "Eyebrowse"
    "M-z" "Evil eyebrowse")
  (require 'evil-eyebrowse))

(my/set-mode-in-emacs-state)

(cl-loop
 for font in my/favorite-fonts
 if (font-available-p font)
 do (set-frame-font font t)
    (cl-return))

(provide 'epilogue)
;;; epilogue.el ends here
