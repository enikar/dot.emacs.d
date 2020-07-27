;;; programming.el --- loads and configure package for programming -*- lexical-binding: t -*-
;;; Commentary:
;; use-package to init and set configuration when appropriate else load-path
;; auto-mode-alist, autoloads and hooks are used.
;;; Code:

(eval-when-compile
  (require 'use-package))

(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode))

(use-package smartparens
  :ensure t
  :diminish (smartparens-mode)
  :hook ((prog-mode) . smartparens-mode)
  :commands (sp-split-sexp sp-newline sp-up-sexp)
  :config
  (progn
    (sp-local-pair '(lisp-mode emacs-lisp-mode)  "'" nil :actions nil)
    (sp-local-pair '(lisp-mode emacs-lisp-mode)  "`" nil :actions nil)))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package highlight-parentheses
  :ensure t
  :diminish (highlight-parentheses-mode)
  :hook ((prog-mode) . highlight-parentheses-mode)
  :custom
    (hl-paren-delay 0.2)
    (hl-paren-colors '("SpringGreen3"
                       "Orange"
                       "IndianRed1"
                       "IndianRed3"
                       "IndianRed4")))

(use-package highlight-indent-guides
  :ensure t
  :diminish (highlight-indent-guides-mode)
  :commands (highlight-indent-guides-mode)
  :init (evil-leader/set-key "i" 'highlight-indent-guides-mode))

(use-package highlight-numbers
  :ensure t
  :hook ((prog-mode) . highlight-numbers-mode))

;;;; syntax checking
;; toggle flycheck window (from spacemacs)
(require 'dash)
(defun my/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

(use-package flycheck
  :ensure t
  :custom (flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
          (flycheck-mode-line-prefix "E|W")
          (flycheck-python-flake8-executable "python3")
          (flycheck-python-mypy-executable "python3")
          (flycheck-python-pycompile-executable "python3")
          (flycheck-python-pylint-executable "python3")
          (flycheck-shellcheck-follow-sources nil)
  :hook (sh-mode . flycheck-mode)
  :bind (("C-x t F" . flycheck-mode)
         ("<f8>" . my/toggle-flycheck-error-list)
         ("<f5>" . flycheck-first-error)
         ("<f6>" . flycheck-next-error)
         ("<f7>" . flycheck-previous-error))
  :commands (flycheck flycheck-mode))

(use-package avy-flycheck
  :ensure t
  :after (flycheck)
  :commands (avy-flycheck-goto-error)
  :init (evil-leader/set-key "af" 'avy-flycheck-goto-error))

(add-hook 'shell-mode-hook (function (lambda () (setq tab-width 8))))

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

;; (use-package company-ghci
;;   :ensure t
;;   :after (company haskell-mode)
;;   :init (add-to-list 'company-backends 'company-ghci)
;;   :hook (haskell-mode . company-mode))

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
;; faster.
(use-package dante
  :ensure t
  :diminish (dante-mode)
  :after (haskell-mode)
  :functions (flycheck-add-next-checker)
  :commands (dante-mode)
  :hook ((haskell-mode . dante-mode)
         (dante-mode . (lambda () (flycheck-add-next-checker
                              'haskell-dante
                              '(warning . haskell-hlint)))))
  :config
  (progn
    (setq  dante-load-flags '("+c" "-Wall" "-ferror-spans" "-fdefer-typed-holes" "-fdefer-type-errors" "-Wwarn=missing-home-modules" "-fno-diagnostics-show-caret" "--make" "-ignore-dot-ghci"))
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

(use-package attrap
  :ensure t
  :commands (attrap-attrap)
  :hook (dante-mode . (lambda() (evil-define-key
                             '(normal insert)
                             'dante-mode-map
                             (kbd "M-!")
                             #'attrap-attrap))))

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
;; TODO: de nouveau essayer realgud-byebug.
;; Il ne faut pas utiliser :realgud:byebug mais M-x realgud:byebug
(add-hook 'ruby-mode-hook
          #'(lambda()
              (flycheck-mode)
              (setq tab-width 2
                    evil-shift-width 2)))

(use-package inf-ruby
  :ensure t
  :custom (inf-ruby-default-implementation "pry")
          (inf-ruby-implementations
           '(("ruby" . "irb --prompt default --noreadline -r irb/completion")
             ("jruby" . "jruby -S irb --prompt default --noreadline -r irb/completion")
             ("rubinius" . "rbx -r irb/completion")
             ("yarv" . "irb1.9 -r irb/completion")
             ("pry" . "pry -f")))

  :commands (inf-ruby)
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package robe
  :defer t
  :ensure t
  :diminish (robe-mode)
  :hook (ruby-mode . robe-mode))

(use-package ruby-end
  :defer t
  :ensure t
  :diminish (ruby-end-mode)
  :hook (ruby-mode . ruby-end-mode))

(use-package rubocop
  :ensure t
  :diminish (rubocop-mode)
  :hook (ruby-mode . rubocop-mode))

(use-package yard-mode
  :ensure t
  :diminish (yard-mode)
  :hook (ruby-mode . yard-mode))

(use-package evil-ruby-text-objects
  :ensure t
  :after (evil)
  :hook (ruby-mode . evil-ruby-text-objects-mode))

;;;; ocaml
(use-package tuareg
  :ensure t
  :mode ("\\.ml\\'" . tuareg-mode)
  :config (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el"))

(use-package merlin
  :ensure t
  :after (tuareg)
  :hook ((tuareg-mode caml-mode) . merlin-mode)
  :config
  (progn
    (add-to-list 'company-backends 'merlin-company-backend)
    (setq merlin-command 'opam)))

(use-package flycheck-ocaml
  :ensure t
  :hook (tuareg-mode . flycheck-mode)
  :init
  (progn
    (setq merlin-error-after-save nil)
    (flycheck-ocaml-setup)))

(use-package dune
  :ensure t
  :commands (dune-mode))

(use-package ocp-indent
  :ensure t
  :hook ((tuareg-mode . ocp-setup-indent)
         (caml-mode . ocp-indent-caml-mode-setup)))

(use-package utop
  :ensure t
  :commands (utop))

;;;; common lisp
(use-package slime
  :ensure t
  :hook (lisp-mode . slime-mode)
  :diminish (slime-autodoc-mode)
  :config
  (require 'slime-company)
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--core" "/enikar/lang/lisp/sbcl/sbcl.core-for-slime" "--dynamic-space-size" "2000")
                :coding-system utf-8-unix)
          (clisp ("clisp" "-I"))
          (ecl ("ecl")))
        slime-contribs '(slime-fancy slime-company)
        slime-company-completion 'fuzzy)
  (slime-setup))

;; (use-package slime-company
;;   :ensure t
;;   :after (slime)
;;   :config
;;   (add-to-list 'slime-contribs 'slime-company)
;;   (slime-setup))

;; scheme
(use-package geiser
  :ensure t
  :defer t
  :custom (geiser-default-implementation 'guile)
          (geiser-guile-manual-lookup-nodes '("Guile Reference" "guile-2.0" "Guile"))
          (geiser-guile-manual-lookup-other-window-p t)
          (geiser-guile-warning-level 'high)
  :defines (geiser-active-implementations)
  :init (setq scheme-program-name "guile")
  :config
  (progn
    (setq geiser-active-implementations '(guile))
    (require 'flycheck-guile)
    (flycheck-mode t)))

;;;; python
;; Pour l'instant python-mode est supprimé en faveur
;; de elpy.
;; (use-package python-mode
;;   :disabled
;;   :custom (python-shell-interpreter "python3")
;;           (pylint-command "pylint3" t)
;;           (pylint-alternate-pylint-command "pylint")
;;           (py-shell-name "ipython")
;;           (py-keep-windows-configuration t)
;;           (py-pdb-path "/usr/lib/python3.8/pdb.py")
;;           (py-split-window-on-execute t)
;;           (py-split-windows-on-execute-function 'split-window-horizontally)
;;           (python-shell-interpreter "python3")
;;   :mode "\\.py\\'")

;; Les règles de la communauté python sont moisis.
;; C'est trop rigide leur système d'indentation à 4
;; espaces uniquement.
;; (add-hook 'python-mode-hook
;;           #'(lambda()
;;               (setq tab-width 2
;;                     evil-shift-width 2)))

(use-package elpy
  :ensure t
  :defer t
  :custom
  (elpy-modules '(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-yasnippet elpy-module-sane-defaults))
  :init (advice-add 'python-mode :before 'elpy-enable))

(use-package company-jedi
  :ensure t
  :hook (python-mode . (lambda() (add-to-list 'company-backends 'company-jedi))))

(use-package importmagic
  :ensure t
  :after (python-mode))

;; (use-package python-docstring
;;   :ensure t
;;   :after (python-mode))

;; Il faudra décider entre anaconda et elpy
;; (use-package anaconda-mode
;;   :ensure t
;;   :after (python-mode))

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
  :mode ("\\.rs\\'" . rustic-mode))

;; cmake
(use-package cmake-mode
  :ensure t
  :mode "CMakeLists\\.txt\\'"
        "\\.cmake\\'")

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

;; LaTeX
(use-package latex-extra
  :ensure t
  :hook (LaTeX-mode .  latex-extra-mode))

(use-package latex-math-preview
  :defer t)

(use-package latex-preview-pane
  :defer t)

(use-package latexdiff
  :defer t)

(use-package company-auctex
  :defer t)

;; (use-package auctex
;;   :ensure t
;;   :mode ("\\.tex\\'" . tex-site)
;;   :config
;;   (TeX-load-hack)
;;   (require 'company-auctex)
;;   (company-auctex-init))

(load "auctex.el")
(add-hook 'LaTeX-mode
          #'(lambda()
              (progn
                (require 'company-auctex)
                (company-auctex-init)
              )))
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

(provide 'programming)
;;; programming.el ends here
