;;; load-packages --- loads and configures packages.
;;; Commentary:
;; loads and configures packages with use-package
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t
  :commands (diminish))

(use-package bind-key
  :ensure t)

(use-package auto-compile
  :ensure t
  :init
  (progn
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

;;;; better dired mode
(add-hook 'dired-load-hook
          (lambda()
            (load "dired-x")))
(add-hook 'dired-mode-hook
          (lambda()
            (dired-omit-mode 1)))

;; general interface
;; leader key ala vim.
(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode)
  :config (evil-leader/set-key "c" 'comment-dwim))

(use-package evil
  :ensure t
  :after (evil-leader)
  :init (evil-mode 1)
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
    (evil-define-key '(normal visual) 'global (kbd "C-c +") #'evil-numbers/inc-at-pt)
    (evil-define-key '(normal visual) 'global (kbd "C-c -") #'evil-numbers/dec-at-pt)
    (evil-define-key 'normal 'global (kbd "Q") #'evil-fill-and-move)
    (evil-define-key 'normal (current-global-map) (kbd "C-w e") #'find-file-other-window)
    (evil-define-key 'normal (current-global-map) (kbd "C-w b") #'ivy-switch-buffer-other-window)
    (evil-define-key 'normal (current-global-map) (kbd "C-w C-l") #'evil-window-right)
))

(use-package evil-quickscope
  :ensure t
  :after evil
  :config (global-evil-quickscope-mode 1))

(use-package evil-lion
  :ensure t
  :after (evil)
  :config (evil-lion-mode))

(use-package evil-surround
  :ensure t
  :after (evil)
  :config (global-evil-surround-mode 1))

(use-package embrace
  :ensure t
  :bind ("C-," . embrace-commander))

(use-package evil-embrace
  :ensure t
  :after  (evil-surround embrace)
  :config (evil-embrace-enable-evil-surround-integration))

(use-package evil-goggles
  :ensure t
  :config
  (setq evil-goggles-pulse 'display-graphic-p
        evil-goggles-async-duration nil
        evil-goggles-blocking-duration nil))

(use-package which-key
  :ensure t
  :init (progn
          (global-unset-key (kbd "C-h C-h"))
          (which-key-mode)))

(use-package hydra
  :ensure t)

(use-package ivy
  :ensure t
  :after (persp-mode)
  :init  (ivy-mode 1)
  :diminish (ivy-mode)
  :hook (ivy-ignore-buffers .
              (lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))
  :config
  (progn
    (setq ivy-use-virtual-buffers t
          ivy-count-format "(%d/%d) ")
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

(use-package ivy-hydra
  :ensure t
  :after (ivy hydra))

(use-package ivy-explorer
  :ensure t
  :after (ivy evil)
  :init (ivy-explorer-mode 1))

(use-package counsel-etags
  :ensure t
  :after (ivy)
  :commands (counsel-etags-find-tag-at-point)
  :config
  (progn
    (evil-define-key 'normal 'global (kbd "C-]") #'counsel-etags-find-tag-at-point)
  ))

(use-package avy
  :ensure t
  :config (avy-setup-default))

(use-package evil-avy
  :ensure t
  :after (avy evil-leader)
  :config
  (progn
    (global-set-key (kbd "M-s M-s")  'avy-goto-char)
    (evil-leader/set-key "g" 'avy-goto-char)
    (evil-leader/set-key "G" 'avy-goto-char-2)
    (evil-leader/set-key "h" 'avy-goto-subword-1)
    (evil-leader/set-key "H" 'avy-resume)
    ))

(use-package ace-jump-mode
  :ensure t
  :after (evil-leader)
  ;:commands (ace-jump-mode)
  :config
  (progn
    (evil-leader/set-key "j" 'ace-jump-mode)
  ))

(use-package ace-mc
  :ensure t
  :after (evil-leader)
  ;:commands (ace-mc-add-multiple-cursors ace-mc-add-single-cursor)
  :config
  (progn
    (evil-leader/set-key "m" 'ace-mc-add-single-cursor)
    (evil-leader/set-key "M" 'ace-mc-add-multiple-cursors)
    ))

(use-package ace-window
  :ensure t
  :commands (ace-window)
  :config (evil-leader/set-key "o" 'ace-window)
  )

(use-package ace-link
  :ensure t
  :commands (ace-link-info)
  :init (ace-link-setup-default)
  )


(use-package session
  :ensure t
  :hook (after-init . session-initialize))

(use-package persp-mode
  :ensure t
  :hook (after-init . (lambda() (persp-mode 1)))
  :config
  (progn
    (setq persp-autokill-buffer-on-remove 'kill-weak)
    (defun persp-ibuffer (arg)
      (interactive "P")
      (with-persp-buffer-list () (ibuffer arg)))

    (evil-ex-define-cmd "ls" #'persp-ibuffer)
    (global-set-key (kbd "C-x C-b") #'persp-ibuffer)
    ))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))


(use-package anzu
  :ensure t
  :config (global-anzu-mode +1))

(use-package evil-anzu
  :ensure t
  :after (evil anzu))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1)
  :config (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  )

(use-package persistent-scratch
  :ensure t
  :init
  (progn
    (setq persistent-scratch-save-file "~/.emacs.d/.cache/persistent-scratch")
    (persistent-scratch-autosave-mode t)
    (ignore-errors (persistent-scratch-restore))))

(use-package flyspell-correct-ivy
  :defer t
  :ensure t)

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :init
    (setq company-require-match nil)
    (setq company-tooltip-align-annotations t)
    (setq company-eclim-auto-save nil)
    (setq company-dabbrev-downcase nil))

(use-package company-fuzzy
  :ensure t
  :after (company flx)
  :custom (company-fuzzy-sorting-backend 'flx)
  :config (global-company-fuzzy-mode 1))

(use-package iedit
  :ensure t
  :init (global-set-key (kbd "C-;") 'iedit-mode)
  :config (evil-leader/set-key ";" 'iedit-mode))

;; multiple-cursors
;; see also https://github.com/fgallina/region-bindings-mode
;; to activate bindings when a region is selected
(use-package multiple-cursors
  :ensure t
  :config
  (progn
    (evil-leader/set-key
      "w" 'mc/mark-next-like-this-word
      "b" 'mc/mark-previous-like-this-word
      "t" 'mc/mark-next-like-this
      "T" 'mc/mark-previous-like-this
      "a" 'mc/mark-all-like-this
      "r" 'mc/mark-all-in-region
      "e" 'mc/mark-more-like-this-extended
      "W" 'mc/mark-all-words-like-this
      "S" 'mc/mark-all-symbols-like-this
      "D" 'mc/mark-all-like-this-dwim)))

(use-package undo-tree
  :ensure t
  :commands (undo-tree-visualize)
  :init (evil-leader/set-key "u" 'undo-tree-visualize)) 

(use-package openwith
  :ensure t
  :init (openwith-mode t))

;; projects
(use-package projectile
  :ensure t
  :init (projectile-mode 1)
  :bind-keymap ("C-c P" . projectile-command-map))

(use-package magit
  :ensure t
  :commands (magit))

(use-package treemacs
  :ensure t
  :commands treemacs
  :config
  (setq treemacs-indentation 1
        treemacs-width 40))

(use-package treemacs-evil
  :ensure t
  :defer t
  :after (treemacs evil))

(use-package treemacs-persp
  :ensure t
  :defer t
  :after (treemacs persp-mode))

(use-package treemacs-projectile
  :ensure t
  :defer t
  :after (treemacs projectile))

;; (use-package treemacs-icons-dired
;;   :ensure t
;;   :hook (dired-load . treemacs-icons-dired-mode))

(use-package treemacs-magit
  :ensure t
  :defer t
  :after (treemacs magit))

;; programming
(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode))

(use-package parent-mode
  :ensure t
  :commands (parent-mode-list parent-mode-is-derived-p))

(use-package fill-column-indicator
  :ensure t
  :commands (fci-mode))

(use-package smartparens
  :ensure t
  :hook ((prog-mode lisp-interaction-mode) . smartparens-mode)
  :commands (sp-split-sexp sp-newline sp-up-sexp)
  :config
  (progn
    (sp-local-pair '(lisp-mode emacs-lisp-mode)  "'" nil :actions nil)
    (sp-local-pair '(lisp-mode emacs-lisp-mode)  "`" nil :actions nil)
   )
  )

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode lisp-interaction-mode) . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :ensure t
  :hook ((prog-mode lisp-interaction-mode) . highlight-parentheses-mode)
  :config
  (progn
    (setq hl-paren-delay 0.2
          hl-paren-colors '("SpringGreen3"
                            "IndianRed1"
                            "IndianRed3"
                            "IndianRed4"))))

(use-package highlight-indentation
  :ensure t
  :commands (highlight-indentation-mode))

(use-package highlight-numbers
  :ensure t
  :hook ((prog-mode lisp-interaction-mode) . highlight-numbers-mode))

;;;; syntax checking
(use-package flycheck
  :ensure t
  :commands (flycheck))

(use-package avy-flycheck
  :ensure t
  :after (flycheck)
  :commands (avy-flycheck-goto-error)
  :config (evil-leader/set-key "f" 'avy-flycheck-goto-error))

;;;; language C
(use-package xcscope
  :defer t
  :ensure t)

;;;; haskell
(use-package haskell-mode
  :ensure t
  :mode "\\.l?hs\\'"
  :hook ((haskell-mode . flycheck-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . imenu-add-menubar-index)
         (haskell-mode . yas-minor-mode))
  :load-path "~/.emacs.d/elisp/hasky-cabal"
  :config
  (progn
    (require 'hasky-cabal)
    (setq haskell-process-args-ghci '("-ferror-spans" "-ghci-script ~/dot.ghci")
          haskell-process-log t
          haskell-process-suggest-hoogle-imports t
          haskell-process-suggest-remove-import-lines t
          haskell-process-suggest-restart nil)))

(use-package company-ghci
  :ensure t
  :after (company haskell-mode)
  :init (add-to-list 'company-backends 'company-ghci)
  :hook (haskell-mode . company-mode))

;; (use-package lsp-mode
;;   :ensure t
;;   :hook (haskell-mode . lsp)
;;   :commands lsp)

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; (use-package lsp-haskell
;;   :ensure t
;;   :config
;;     (setq lsp-haskell-process-path-hie "ghcide")
;;     (setq lsp-haskell-process-args-hie '())
;; ;; Comment/uncomment this line to see interactions between lsp client/server.
;; ;;(setq lsp-log-io t)
;;  )

;; I use dante flycheck instead of flycheck-haskell because it is
;; more quick.
(use-package dante
  :defer t
  :ensure t
  :after (haskell-mode)
  :commands 'dante-mode
  :hook ((haskell-mode . dante-mode)
         (dante-mode . (lambda () (flycheck-add-next-checker
                              'haskell-dante
                              '(warning . haskell-hlint)))))
  :config
  (progn
    (setq  dante-load-flags '("-Wall" "+c" "-ignore-dot-ghci"))
    (evil-define-key
        '(normal insert)
        'dante-mode-map
        (kbd "M-.")
        #'xref-find-definitions)
    (evil-define-key
        '(normal insert)
        'dante-mode-map
        (kbd "M-?")
        #'xref-find-references)))

(use-package retrie
  :ensure t
  :commands (retrie)
  :after (haskell-mode))

;;;; yaml (for stack)
(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")
  

(use-package flycheck-yamllint
  :ensure t
  :defer t
  :after (yaml-mode))

(use-package hasky-extensions
  :ensure t
  :commands (hasky-extensions)
  :after (haskell-mode)
  )

(use-package hasky-stack
  :ensure t
  :defer t
  :after (haskell-mode))

;;;; ruby
(use-package inf-ruby
  :ensure t
  :commands (inf-ruby)
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package robe
  :defer t
  :ensure t
  :hook (ruby-mode . robe-mode))

(use-package ruby-end
  :defer t
  :ensure t
  :hook (ruby-mode . ruby-end-mode))

(use-package rubocop
  :defer t
  :ensure t)

(use-package yard-mode
  :defer t
  :ensure t)

(use-package realgud-byebug
  :defer t
  :ensure t)

(use-package evil-ruby-text-objects
  :ensure t
  :defer t
  :after (evil)
  :hook (ruby-mode . evil-ruby-text-objects-mode))

;;;; ocaml
(use-package tuareg
  :defer t
  :mode "\\.ml\\'"
  :ensure t)

(use-package merlin
  :defer t
  :ensure t
  :after (tuareg)
  :hook ((tuareg-mode caml-mode) . merlin-mode)
  :config
  (progn
    (add-to-list 'company-backends 'merlin-company-backend)
    (setq merlin-command 'opam)))

(use-package dune
  :ensure t
  :commands (dune-mode))

(use-package ocp-indent
  :ensure t
  :commands (ocp-indent-line ocp-indent-region ocp-indent-buffer))

(use-package utop
  :ensure t
  :commands (utop))

;;;; common lisp
(use-package slime
  :ensure t
  :commands (slime)
  ;; :hook (slime-mode . (lambda()(unless (slime-connected-p)
  ;;                          (save-excursion (slime)))))
  :config
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--core" "/karine/lang/lisp/sbcl/sbcl.core-for-slime" "--dynamic-space-size" "2000")
         :coding-system utf-8-unix)
          (clisp ("clisp" "-I"))
          (ecl ("ecl")))
        slime-contribs '(slime-fancy)))

(use-package slime-company
  :ensure t
  :after (slime)
  :config
  (slime-setup '(slime-company)))

;;;; scheme
(use-package geiser
  :ensure t
  :mode "\\.scm\\'")

(use-package flycheck-guile
  :ensure t
  :after (geiser))

;;;; python
(use-package python-mode
  :ensure t
  :mode "\\.py\\'")

(use-package elpy
  :ensure t
  :after (python-mode))

(use-package ein
  :ensure t
  :after (python-mode))

(use-package company-jedi
  :ensure t
  :after (python-mode))

(use-package importmagic
  :ensure t
  :after (python-mode))

(use-package python-docstring
  :ensure t
  :after (python-mode))

(use-package anaconda-mode
  :ensure t
  :after (python-mode))

(use-package python-environment
  :ensure t
  :after (python-mode))

;; perl6
;; flycheck-raku is not available in melpa
;; so I git clone the repo, configure the loading
;; when raku-mode is activated
(use-package raku-mode
  :ensure t
  :load-path "~/.emacs.d/elisp/flycheck-raku"
  :hook (raku-mode . (lambda ()
                       (require 'flycheck-raku)
                       (flycheck-mode)
                       )))

;; rust
;; rustic provide all functionnalities
(use-package rustic
  :ensure t
  :defer t)

;; cmake
(use-package cmake-mode
  :ensure t)

;; maxima
(add-to-list 'load-path "~/.emacs.d/elisp/maxima")
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'maxima "maxima" "Maxima interaction" t) 
(add-to-list 'auto-mode-alist '("\\.mac\\'" . maxima-mode))

;; recutils
(add-to-list 'load-path "~/.emacs/elisp/recutils")
(autoload 'rec-mode "rec-mode" "Recutils mode" t)
(add-to-list 'auto-mode-alist '("\\.rec\\'" . rec-mode))

;; gforth
(add-to-list 'load-path "~/.emacs.d/elisp/gforth")
(autoload 'forth-mode "gforth" "Forth mode" t)

;; .desktop files
(add-to-list 'load-path "~/.emacs.d/elisp/freedesktop")
(autoload 'desktop-entry-mode "desktop-entry-mode" "Desktop Entry mode" t)
(add-to-list 'auto-mode-alist
             '("\\.desktop\\(\\.in\\)?$" . desktop-entry-mode))

;; asymptote
(add-to-list 'load-path "~/.emacs.d/elisp/asymptote")
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode))

;; id-utils
(add-to-list 'load-path "~/.emacs.d/elisp/id-utils")
(autoload 'gid "idutils" "run idutils' gid command" t)

;; latex help
(add-to-list 'load-path "~/.emacs.d/elisp/latex-help")
(autoload 'latex-help "latex-help" "Latex help in info" t)
(define-key help-map "\C-l" 'latex-help)

;; keep eyebrowse at the end of this file.
(use-package eyebrowse
  :ensure t
  :load-path ("~/.emacs.d/elisp/evil-eyebrowse")
  :init (eyebrowse-mode t)
  :config (require 'evil-eyebrowse))

(provide 'load-packages)
;;; load-packages.el ends here
