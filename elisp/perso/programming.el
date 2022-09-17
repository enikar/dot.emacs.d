;;; programming.el --- loads and configure package for programming -*- lexical-binding: t -*-
;;; Commentary:
;; use-package to init and set configuration when appropriate else load-path
;; auto-mode-alist, autoloads and hooks are used.
;;; Code:

;; (eval-when-compile
;;   (require 'use-package))

(use-package yasnippet
  :commands (yas-minor-mode)
  :hook ((prog-mode) . yas-minor-mode)
  :config (yas-reload-all))

(use-package consult-yasnippet
  :commands (consult-yasnippet))

(use-package rainbow-delimiters
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package highlight-parentheses
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
  :diminish (highlight-indent-guides-mode)
  :commands (highlight-indent-guides-mode)
  :init (evil-leader/set-key "i" 'highlight-indent-guides-mode))

(use-package highlight-numbers
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
  :custom (flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
          (flycheck-mode-line-prefix "E|W")
          (flycheck-python-flake8-executable "python3")
          (flycheck-python-pycompile-executable "python3")
          (flycheck-shellcheck-follow-sources nil)
  :hook (sh-mode . flycheck-mode)
  :bind (("C-x t F" . flycheck-mode)
         ("<f8>" . my/toggle-flycheck-error-list)
         ("<f5>" . flycheck-first-error)
         ("<f6>" . flycheck-next-error)
         ("<f7>" . flycheck-previous-error))
  :commands (flycheck flycheck-mode))

(use-package avy-flycheck
  :after (flycheck)
  :commands (avy-flycheck-goto-error)
  :init (evil-leader/set-key "af" 'avy-flycheck-goto-error))

(use-package consult-flycheck
  :after (flycheck consult avy avy-flycheck)
  :commands (consult-flycheck)
  :init (evil-leader/set-key "Ff" 'consult-flycheck))

(add-hook 'shell-mode-hook (function (lambda () (setq tab-width 8))))

;;;; language C
(use-package xcscope
  :defer t)

;;;; haskell
(use-package haskell-mode
  :mode "\\.l?hs\\'"
  :hook ((haskell-mode . flycheck-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . imenu-add-menubar-index)
         (haskell-mode . yas-minor-mode))
  :config
  (progn
    (add-hook 'ghci-script-mode-hook (function (lambda() (auto-fill-mode 0))))
    (setq haskell-process-args-ghci '("-ferror-spans" "-ghci-script ~/dot.ghci")
          haskell-process-log t
          haskell-process-suggest-hoogle-imports t
          haskell-process-suggest-remove-import-lines t
          haskell-process-suggest-restart nil)))

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
  :diminish (dante-mode)
  :after (haskell-mode)
  :functions (flycheck-add-next-checker)
  :commands (dante-mode)
  :custom (dante-load-flags '("+c" "-Wall" "-fdiagnostics-color=never" "-ferror-spans" "-fdefer-typed-holes" "-fdefer-type-errors" "-Wwarn=missing-home-modules" "-fno-diagnostics-show-caret" "--make" "-ignore-dot-ghci"))
  :hook ((haskell-mode . dante-mode)
         (dante-mode . (lambda () (flycheck-add-next-checker
                              'haskell-dante
                              '(warning . haskell-hlint)))))
  :config
  (progn
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
  :commands (attrap-attrap)
  :hook (dante-mode . (lambda()
                        (evil-define-key
                          '(normal insert)
                          'dante-mode-map
                          (kbd "M-!")
                          #'attrap-attrap))))

;; (use-package retrie
;;   :commands (retrie)
;;   :after (haskell-mode))

;;;; yaml (for stack)
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package flycheck-yamllint
  :defer t
  :after (yaml-mode))

(use-package hasky-extensions
  :after (haskell-mode)
  :bind (:map haskell-mode-map
              ("C-c l" . #'hasky-extensions)))

(use-package hasky-stack
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
  :custom (inf-ruby-default-implementation "pry")
          (inf-ruby-implementations
           '(("ruby" . "irb --prompt default --noreadline -r irb/completion")
             ("jruby" . "jruby -S irb --prompt default --noreadline -r irb/completion")
             ("rubinius" . "rbx -r irb/completion")
             ("pry" . "pry -f")))

  :commands (inf-ruby)
  :hook (ruby-mode . inf-ruby-minor-mode))

;; get documentation from the ri command
(use-package yari
  :commands (yari)
  :bind ("C-h y" . yari)
  :hook (ruby-mode . (lambda () (evil-leader/set-key "y" 'yari))))

(use-package robe
  :defer t
  :diminish (robe-mode)
  :hook (ruby-mode . robe-mode))

(use-package ruby-end
  :defer t
  :diminish (ruby-end-mode)
  :hook (ruby-mode . ruby-end-mode))

(use-package rubocop
  :diminish (rubocop-mode)
  :hook (ruby-mode . rubocop-mode))

(use-package realgud-pry
  :commands (realgud:pry))

(use-package realgud-byebug
  :commands (realgud:byebug))

(use-package yard-mode
  :diminish (yard-mode)
  :hook (ruby-mode . yard-mode))

(use-package evil-ruby-text-objects
  :after (evil)
  :hook (ruby-mode . evil-ruby-text-objects-mode))

;;;; ocaml
(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode)
  :config (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el"))

(use-package merlin
  :after (tuareg)
  :custom-face (merlin-type-face ((t (:inherit caml-types-expr-face :background "MistyRose4"))))
  :hook ((tuareg-mode caml-mode) . merlin-mode)
  :config
  (progn
    (setq merlin-command 'opam)))

(use-package flycheck-ocaml
  :hook (tuareg-mode . flycheck-mode)
  :init
  (progn
    (setq merlin-error-after-save nil)
    (flycheck-ocaml-setup)))

(use-package dune
  :commands (dune-mode))

(use-package ocp-indent
  :hook ((tuareg-mode . ocp-setup-indent)
         (caml-mode . ocp-indent-caml-mode-setup)))

(use-package utop
  :commands (utop))

;;;; common lisp
(use-package slime
  :hook (lisp-mode . slime-mode)
  :diminish (slime-autodoc-mode)
  :config
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--core" "/enikar/lang/lisp/sbcl/sbcl.core-for-slime" "--dynamic-space-size" "2000")
                :coding-system utf-8-unix)
          (clisp ("clisp" "-I"))
          (ecl ("ecl")))
        slime-contribs '(slime-fancy))

  (slime-setup))

;; scheme


(use-package geiser
  :defer t
  :custom (geiser-default-implementation 'guile)
          (geiser-guile-manual-lookup-nodes '("Guile Reference" "guile-3.0" "Guile"))
          (geiser-guile-manual-lookup-other-window-p t)
          (geiser-guile-warning-level 'high)
  :hook (scheme-mode . flycheck-mode)
  :defines (geiser-active-implementations)
  :init (setq scheme-program-name "guile")
  :config
  (progn
    (setq geiser-active-implementations '(guile))
    (require 'flycheck-guile)))

(use-package geiser-guile)

(use-package racket-mode
  :mode "\\.rkt\\'"
  :hook (racket-mode . racktet-xp-mode)
  :config (require 'racket-xp))

(use-package geiser-racket)

;;;; python: removed in flavor of emacs's python.el
;; (use-package python-mode
;;   :custom (python-shell-interpreter "python3")
;;           (pylint-command "pylint3" t)
;;           (pylint-alternate-pylint-command "pylint")
;;           (py-shell-name "ipython3")
;;           (py-keep-windows-configuration t)
;;           (py-pdb-path "/usr/lib/python3.10/pdb.py")
;;           (py-split-window-on-execute t)
;;           (py-split-windows-on-execute-function 'split-window-horizontally)
;;   :mode "\\.py\\'"
;;   :hook (python-mode . flycheck-mode)
;;         (python-mode . anaconda-mode)
;;         (python-mode . (lambda() (require 'importmagic))))

;; anaconda + python.el is better than elpy !
(use-package anaconda-mode)

(add-hook 'python-mode-hook #'(lambda()
                                (flycheck-mode)
                                (anaconda-mode)
                                (anaconda-eldoc-mode)))
;; use ipython as interactive python shell
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i")

;; perl5
(add-hook 'perl-mode-hook #'flycheck-mode)
;; (setq flycheck-perlcritic-severity 5)

;; perl6
;; flycheck-raku is not available in melpa
;; so I git clone the repo, configure the loading
;; when raku-mode is activated
;; (use-package raku-mode
;;   :load-path "~/.emacs.d/elisp/flycheck-raku"
;;   :hook (raku-mode . (lambda ()
;;                        (require 'flycheck-raku)
;;                        (flycheck-mode)
;;                        )))

;; flycheck-raku is now available on melpa
(use-package flycheck-raku)
(use-package raku-mode
  :hook (raku-mode . flycheck-mode))

;; rust
;; rustic provide all functionnalities
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode))

;; lua
(use-package lua-mode
  :mode ("\\.lua\\'". lua-mode))

;; cmake
(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'"
        "\\.cmake\\'")

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package json-mode
  :mode "\\.json\\'")

(use-package nim-mode
  :mode ("\\.nim\\'"))


;; LaTeX
(use-package latex-extra
  :hook (LaTeX-mode .  latex-extra-mode))

(use-package latex-math-preview
  :defer t)

(use-package latex-preview-pane
  :defer t)

(use-package latexdiff
  :defer t)


(load "auctex.el")

;; edit vcard files (.vcf extension)
(use-package vcard)

;; maxima
(push  "~/.emacs.d/elisp/maxima" load-path)
(autoload 'maxima-mode "maxima" "Maxima mode" t)
(autoload 'maxima "maxima" "Maxima interaction" t)
(push  '("\\.mac\\'" . maxima-mode) auto-mode-alist)

;; recutils
(add-to-list 'load-path "~/.emacs/elisp/recutils")
(autoload 'rec-mode "rec-mode" "Recutils mode" t)
(push '("\\.rec\\'" . rec-mode) auto-mode-alist)

;; gforth
(push "~/.emacs.d/elisp/gforth" load-path)
(autoload 'forth-mode "gforth" "Forth mode" t)
(setq auto-mode-alist
  (append '(("\\.fs$" . forth-mode)
    ("\\.4th$" . forth-mode)
    ("\\.fth$" . forth-mode)) auto-mode-alist))


;; .desktop files
(push "~/.emacs.d/elisp/freedesktop" load-path)
(autoload 'desktop-entry-mode "desktop-entry-mode" "Desktop Entry mode" t)
(push '("\\.desktop\\(\\.in\\)?$" . desktop-entry-mode) auto-mode-alist)

;; asymptote
(push "~/.emacs.d/elisp/asymptote" load-path)
(autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
(autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
(autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
(push '("\\.asy$" . asy-mode) auto-mode-alist)

;; id-utils
(push "~/.emacs.d/elisp/id-utils" load-path)
(autoload 'gid "id-utils" "run id-utils' gid command" t)

;; latex help
(push "~/.emacs.d/elisp/latex-help" load-path)
(autoload 'latex-help "latex-help" "Latex help in info" t)
(define-key help-map "\C-l" 'latex-help)

(push "~/.emacs.d/elisp/pariemacs" load-path)
(autoload 'gp-mode "pari" nil t)
(autoload 'gp-script-mode "pari" nil t)
(autoload 'gp "pari" nil t)
(autoload 'gpman "pari" nil t)
(push '("\\.gp\\'" . gp-script-mode) auto-mode-alist)

(provide 'programming)
;;; programming.el ends here
