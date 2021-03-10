;;; general-interface.el --- loads and configures pacakges for general interface. -*- lexical-binding: t; -*-
;;; Commentary:
;;  use-package is used when apropriate, else set load-path
;;  auto-mode-alist, autoloads and hooks using the bare emacs way.
;;; Code:

;; (eval-when-compile
;;   (require 'use-package))

;; To add a :ensure for each use-package
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

(use-package diminish
  :commands (diminish))

(use-package bind-key)

(use-package auto-compile
  :diminish (auto-compile-mode)
  :diminish (auto-compile-on-load-mode)
  :diminish (auto-compile-on-save-mode)
  :init
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode))

(use-package paradox
  :custom (paradox-automatically-star nil)
          (paradox-execute-asynchronously t)
          (paradox-github-token t)
  :commands (paradox-list-packages))

;; general interface
;; leader key ala vim.
(use-package evil-leader
  :custom (evil-leader/leader "_")
  :diminish (evil-leader-mode)
  :init (global-evil-leader-mode)
        (evil-leader/set-key "w" 'whitespace-mode))

(use-package evil
  :after (evil-leader)
  :init (evil-mode 1)
  :custom (evil-ex-search-highlight-all t)
          (evil-ex-search-persistent-highlight nil)
          (evil-ex-visual-char-range t)
          (evil-want-C-u-scroll t)
          (evil-want-Y-yank-to-eol nil)
          (evil-want-fine-undo t)
  :diminish (evil-mode)
  :hook (dired-mode . evil-emacs-state)
  :config
  (progn
    (defun evil-ex-search-next-auto-clear-highlights ()
        (interactive)
        (evil-ex-search-next)
        (run-with-idle-timer 1 nil #'evil-ex-nohighlight))
    (defun evil-ex-search-previous-auto-clear-highlights ()
        (interactive)
        (evil-ex-search-previous)
        (run-with-idle-timer 1 nil #'evil-ex-nohighlight))

    (define-key
        evil-motion-state-map
        (kbd "/")
        #'evil-ex-search-forward)
    (define-key
        evil-motion-state-map
        (kbd "?")
        #'evil-ex-search-backward)
    (define-key
        evil-motion-state-map
        (kbd "n")
        #'evil-ex-search-next-auto-clear-highlights)

    (define-key
        evil-motion-state-map
        (kbd "N")
        #'evil-ex-search-previous-auto-clear-highlights)

    ;;(evil-ex-define-cmd "ls" #'ibuffer)
    ;; settings to use evil-numbers C-a and C-x in vim normal mode
    ;; But C-x is use by emacs, and it is convenient to keep it.
    ;; (define-key evil-normal-state-map (kbd "C-c +") #'evil-numbers/inc-at-pt)
    ;; (define-key evil-visual-state-map (kbd "C-c +") #'evil-numbers/inc-at-pt)
    ;; (define-key evil-normal-state-map (kbd "C-c -") #'evil-numbers/dec-at-pt)
    ;; (define-key evil-visual-state-map (kbd "C-c -") #'evil-numbers/dec-at-pt)
    (evil-leader/set-key "+" 'evil-numbers/inc-at-pt)
    (evil-leader/set-key "-" 'evil-numbers/dec-at-pt)
    (evil-define-key 'normal 'global (kbd "Q") #'evil-fill-and-move)
    (evil-define-key 'normal (current-global-map) (kbd "C-w e") #'find-file-other-window)
    (evil-define-key 'normal (current-global-map) (kbd "C-w b") #'ivy-switch-buffer-other-window)
    (evil-define-key 'normal (current-global-map) (kbd "C-w C-l") #'evil-window-right)
))

(use-package evil-quickscope
  :after evil
  :diminish (evil-quickscope-mode)
  :config (global-evil-quickscope-mode 1))

(use-package evil-lion
  :after (evil)
  :diminish (evil-lion-mode)
  :config (evil-lion-mode))

(use-package evil-surround
  :after (evil)
  :diminish (evil-surround-mode)
  :config (global-evil-surround-mode 1))

(use-package embrace
  :bind ("C-," . embrace-commander)
  :init (evil-leader/set-key "e" 'embrace-commander))

(use-package evil-embrace
  :after  (evil-surround embrace)
  :config (evil-embrace-enable-evil-surround-integration))

(use-package evil-goggles
  :hook (after-init . evil-goggles-mode)
  :diminish (evil-goggles-mode)
  :custom
  (evil-goggles-pulse 'display-graphic-p)
  (evil-goggles-async-duration nil)
  (evil-goggles-blocking-duration nil))

(use-package evil-string-inflection)

(use-package evil-matchit
  :init (global-evil-matchit-mode 1))

(use-package evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines
             evilnc-quick-comment-or-uncomment-to-the-line
             evilnc-copy-and-comment-lines
             evilnc-comment-or-uncomment-paragraphs
             comment-dwim
             evilnc-toggle-invert-comment-line-by-line
             evilnc-comment-operator)
  :init
  (global-set-key (kbd "M-;") #'evilnc-comment-or-uncomment-lines)
  (evil-leader/set-key
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-dwim
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    "."  'evilnc-comment-operator))

(use-package nocomments-mode
  :commands (nocomments-mode)
  :init (evil-leader/set-key
          "C" 'nocomments-mode))

(use-package evil-visualstar
  :custom (evil-visualstar/persistent t)
  :init (global-evil-visualstar-mode t))

(use-package which-key
  :custom (which-key-sort-order 'which-key-key-order-alpha)
  :diminish (which-key-mode)
  :hook (after-init . which-key-mode)
  :init (global-unset-key (kbd "C-h C-h")))

(use-package goto-chg
  :bind (("M-s M-e" . goto-last-change)
         ("M-s M-r" . goto-last-change-reverse)))

(use-package hydra
  :commands (defhydra))

(use-package counsel
  :custom (counsel-find-file-at-point t)
          (counsel-find-file-ignore-regexp "\\.bak$\\|\\.elc$\\|~$"))

(use-package ivy
  :after (persp-mode)
  :diminish (ivy-mode)
  :custom (ivy-mode t)
          (ivy-use-virtual-buffers t)
          (ivy-count-format "(%d/%d) ")
          (ivy-virtual-abbreviate 'abbreviate)
  :config
  (progn
    (defun persp-ivy-ignore-buffers (b)
      "Ignore buffers which are not in the current perspective
when switching buffer with ivy-switch-buffer."
      (when persp-mode
        (let ((persp (get-current-persp)))
          (if persp
              (not (persp-contain-buffer-p b persp))))))

    (add-to-list 'ivy-ignore-buffers #'persp-ivy-ignore-buffers)
    (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode)
    ;; (setq search-default-mode #'char-fold-to-regexp)
    (global-set-key (kbd "C-s") 'swiper)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-explorer)
    (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
    ;;(global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
    (global-set-key (kbd "C-x C-r") 'ivy-resume)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "C-h f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "C-h v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f1> S") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f1> u") 'counsel-unicode-char)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch  . nil))))
))

(use-package ivy-historian
  :diminish (ivy-historian-mode historian-mode)
  :init (ivy-historian-mode))

(use-package ivy-hydra
  :after (ivy hydra))

(use-package avy
  :config (avy-setup-default))

(use-package ivy-avy)

(use-package ivy-explorer
  :diminish (ivy-explorer-mode)
  :after (ivy evil)
  :init (ivy-explorer-mode 1))

(use-package counsel-etags
  :after (ivy)
  :bind (:map evil-normal-state-map
              ("C-]" . #'counsel-etags-find-tag-at-point)))

;; useless beacause the minor is not activated
;; But I setup some bindings with evil-leader
(use-package evil-avy
  :after (avy evil-leader)
  :commands (avy-goto-char
             avy-goto-char
             avy-goto-char-2
             avy-goto-subword-1
             avy-resume
             avy-goto-word-0
             avy-goto-word-1
             avy-goto-line
             evil-avy-mode)
  :init
  (progn
    (global-set-key (kbd "M-s M-s")  'avy-goto-char)
    (evil-leader/set-key "ac" 'avy-goto-char)
    (evil-leader/set-key "aC" 'avy-goto-char-2)
    (evil-leader/set-key "as" 'avy-goto-subword-1)
    (evil-leader/set-key "ar" 'avy-resume)
    (evil-leader/set-key "aw" 'avy-goto-word-0)
    (evil-leader/set-key "aW" 'avy-goto-word-1)
    (evil-leader/set-key "al" 'avy-goto-line)
    (evil-leader/set-key "aa" 'evil-avy-mode)))

(use-package ace-jump-mode
  :after (evil-leader)
  :commands (ace-jump-mode
             ace-jump-char-mode
             ace-jump-line-mode)
  :init
  (progn
    (evil-leader/set-key "jw" 'ace-jump-mode)
    (evil-leader/set-key "jc" 'ace-jump-char-mode)
    (evil-leader/set-key "jl" 'ace-jump-line-mode)
  ))

;; (use-package ace-mc
;;   :after (evil-leader)
;;   :commands (ace-mc-add-multiple-cursors ace-mc-add-single-cursor)
;;   :init
;;   (progn
;;     (evil-leader/set-key "mM" 'ace-mc-add-single-cursor)
;;     (evil-leader/set-key "mm" 'ace-mc-add-multiple-cursors)
;;     ))

(use-package ace-window
  :commands (ace-window)
  :init (evil-leader/set-key "o" 'ace-window)
  )

(use-package ace-link
  :commands (ace-link)
  :init (ace-link-setup-default)
  )

(use-package session
  :hook (after-init . session-initialize))

;; Note: The loading occurs after the init file is loaded.
;; So it's safe to set file-name-handler-alist to nil during
;; the loading of this file. (I set this in the init file).
;; Then, when persp-mode restore the default perspective
;; the file-name-handler-alist has its default value.
(use-package persp-mode
  :hook (after-init . (lambda() (persp-mode 1)))
  :custom (persp-autokill-buffer-on-remove 'kill-weak)
          (persp-nil-name "none")
  :config
  (progn
    (defun persp-ibuffer (arg)
      (interactive "P")
      (with-persp-buffer-list () (ibuffer arg)))

    (evil-ex-define-cmd "ls" #'persp-ibuffer)
    (global-set-key (kbd "C-x C-b") #'persp-ibuffer)))

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))
;; Install an advice when setup doom modeline.
;; Hence, I can redefine the modeline used with
;; paradox.
(use-package doom-modeline
  :custom (doom-modeline-minor-modes t)
          (doom-modeline-persp-name t)
  :config
  (require 'doom-modeline-core)
  (require 'doom-modeline-segments)
  (doom-modeline-def-modeline 'my/info
    '(bar workspace-name window-number modals buffer-info info-nodes buffer-position parrot selection-info)
    '(misc-info buffer-encoding major-mode))
  (defun doom-modeline-set-my/info-modeline ()
    "Set info mode-line."
    (doom-modeline-set-modeline 'my/info))

  (doom-modeline-def-modeline 'my/package
    '(bar workspace-name window-number modals package)
    '(misc-info major-mode process))
  (defun doom-modeline-set-my/package-modeline ()
    "Set package mode-line."
    (doom-modeline-set-modeline 'my/package))

  (doom-modeline-def-modeline 'my/project
    '(bar workspace-name window-number modals buffer-default-directory)
    '(misc-info debug minor-modes input-method major-mode process))
  (defun doom-modeline-set-my/project-modeline ()
      "Set project modeline"
    (doom-modeline-set-modeline 'my/project))

  (defsubst my/replace-hook (hook from to)
    (remove-hook hook from)
    (add-hook hook to))

  (defun my/modeline-advice ()
    (progn
      (my/replace-hook
       'paradox-menu-mode-hook
       #'doom-modeline-set-package-modeline
       #'doom-modeline-set-my/package-modeline)

      (my/replace-hook
       'Info-mode-hook
       #'doom-modeline-set-info-modeline
       #'doom-modeline-set-my/info-modeline)

      (my/replace-hook
       'dired-mode-hook
       #'doom-modeline-set-project-modeline
       #'doom-modeline-set-my/project-modeline)))

  (advice-add #'doom-modeline-mode :after #'my/modeline-advice)
  (doom-modeline-mode))

(use-package evil-anzu
  :after (evil)
  :diminish (anzu-mode)
  :config
  (require 'anzu)
  (global-anzu-mode +1))

(use-package all-the-icons-dired
  :diminish (all-the-icons-dired-mode)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :init (all-the-icons-ibuffer-mode 1))

(use-package emojify
  :diminish (emojify))
  ;:hook (after-init . global-emojify-mode))

(use-package ivy-rich
  :init (ivy-rich-mode 1)
  :config (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package flyspell-correct-ivy
  :defer t)

(use-package company
  :diminish (company-mode)
  :hook (prog-mode . company-mode)
  :init
    (setq company-require-match nil)
    (setq company-tooltip-align-annotations t)
    (setq company-eclim-auto-save nil)
    (setq company-dabbrev-downcase nil)
    (require 'company-statistics)
    (company-statistics-mode))

(use-package company-fuzzy
  :after (company flx)
  :diminish (company-fuzzy-mode)
  :custom (company-fuzzy-sorting-backend 'alphabetic)
  :config (global-company-fuzzy-mode 1))

(use-package iedit
  :bind ("C-;" . iedit-mode)
  :init (evil-leader/set-key ";" 'iedit-mode))

;; multiple-cursors
;; see also https://github.com/fgallina/region-bindings-mode
;; to activate bindings when a region is selected

;; (use-package multiple-cursors
;;   :commands (mc/mark-next-like-this-word
;;              mc/mark-previous-like-this-word
;;              mc/mark-next-like-this
;;              mc/mark-previous-like-this
;;              mc/mark-all-like-this
;;              mc/mark-all-in-region
;;              mc/mark-more-like-this-extended
;;              mc/mark-all-words-like-this
;;              mc/mark-all-symbols-like-this
;;              mc/mark-all-like-this-dwim)
;;   :init
;;   (progn
;;     (evil-leader/set-key
;;       "mw" 'mc/mark-next-like-this-word
;;       "mb" 'mc/mark-previous-like-this-word
;;       "mt" 'mc/mark-next-like-this
;;       "mT" 'mc/mark-previous-like-this
;;       "ma" 'mc/mark-all-like-this
;;       "mr" 'mc/mark-all-in-region
;;       "me" 'mc/mark-more-like-this-extended
;;       "mW" 'mc/mark-all-words-like-this
;;       "mS" 'mc/mark-all-symbols-like-this
;;       "mD" 'mc/mark-all-like-this-dwim)))

;; use `gr` prefix in normal mode to access mc functionalities
(use-package evil-mc
  :diminish (evil-mc-mode)
  :init (global-evil-mc-mode 1))


(use-package undo-tree
  :diminish (undo-tree-mode)
  :commands (undo-tree-visualize)
  :custom (evil-undo-system 'undo-tree)
  :init (evil-leader/set-key "u" 'undo-tree-visualize)
        (global-undo-tree-mode))

(use-package openwith
  :custom (openwith-confirm-invocation t)
          (openwith-associations
            '(("\\.\\(pdf\\|ps\\|djvu\\)\\'" "zathura" (file))
              ("\\.\\(mp3\\|flac\\|ogg\\|aac\\)\\'" "mplayer" (file))
              ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mkv\\|mp4\\|webm\\|ogv\\)\\'" "mplayer" ("-idx" file))
              ("\\.\\(od[sgtbfm]\\|st[icwd]\\|sx[gmdiwc]\\|ot[sgtp]\\|docx?\\|rtf\\|xl[sw]\\|pp[ts]\\)\\'" "libreoffice" nil)))
  :init (openwith-mode t))

;; projects
(use-package projectile
  :diminish (projectile-mode)
  :custom (projectile-completion-system 'ivy)
          (projectile-verbose nil)
  :init (projectile-mode 1)
  :bind-keymap ("C-c P" . projectile-command-map))

;; TODO: configure persp-mode-projectile-bridge

(use-package magit
  :commands (magit))

(use-package libgit)
(use-package magit-libgit)

(use-package treemacs
  :bind ("C-x t t" . treemacs)
  :custom (treemacs-width 40)
          (treemacs-indentation 1)
  :config
  (require 'treemacs-evil)
  (require 'treemacs-persp)
  (require 'treemacs-projectile)
  (require 'treemacs-magit))

(use-package ripgrep
  :commands (ripgrep-regexp)
  :init (evil-leader/set-key "gr" 'ripgrep-regexp))

(use-package ag
  :commands (ag
             ag-files
             ag-regexp
             ag-project
             ag-project-files
             ag-project-regexp)
  :init (setq ag-highlight-search t)
        (evil-leader/set-key "gaa" 'ag)
        (evil-leader/set-key "gaf" 'ag-files)
        (evil-leader/set-key "gar" 'ag-regexp)
        (evil-leader/set-key "gap" 'ag-project)
        (evil-leader/set-key "gaF" 'ag-project-files)
        (evil-leader/set-key "gaR" 'ag-project-regexp))

(use-package pt
  :commands (pt-regexp
             pt-regex-file-pattern
             projectile-pt)
  :init (evil-leader/set-key "gpp" 'pt-regexp)
        (evil-leader/set-key "gpf" 'pt-regexp-file-pattern)
        (evil-leader/set-key "gpP" 'projectile-pt))

(use-package helpful
  :hook (helpful-mode . evil-emacs-state)
  :bind ("C-h k" . helpful-key)
  :commands (helpful-at-point
             helpful-callable
             helpful-variable
             helpful-command)
  :init
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (evil-leader/set-key "hf" 'counsel-describe-function)
  (evil-leader/set-key "hv" 'counsel-describe-variable)
  (evil-leader/set-key "hk" 'helpful-key)
  (evil-leader/set-key "hp" 'helpful-at-point)
  (evil-leader/set-key "hc" 'helpful-command))

(use-package parent-mode
  :commands (parent-mode-list parent-mode-is-derived-p))

(use-package vterm)

(use-package fill-column-indicator
  :bind ("C-x t C-f" . fci-mode)
  :commands (fci-mode))

(provide 'general-interface)
;;; general-interface.el ends here
