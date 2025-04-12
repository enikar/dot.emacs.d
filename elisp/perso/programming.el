;;; programming.el --- loads and configure package for programming -*- lexical-binding: t -*-
;;; Commentary:
;; use-package to init and set configuration when appropriate else load-path
;; auto-mode-alist, autoloads and hooks are used.

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'general-interface)


(add-hook 'prog-mode-hook #'abbrev-mode)

(use-package rainbow-delimiters
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package rainbow-identifiers)

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
  :defer t
  :diminish (highlight-indent-guides-mode)
  :init (leader-ala-vim "t i" #'highlight-indent-guides-mode)
        (setq highlight-indent-guides-auto-odd-face-perc 30
              highlight-indent-guides-auto-even-face-perc 60
              highlight-indent-guides-auto-character-face-perc 80))

(use-package highlight-numbers
  :hook ((prog-mode) . highlight-numbers-mode))

(use-package company
  :defer t)

;;;; syntax checking
;; toggle flycheck window (from spacemacs)
(defun my/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (let ((window (flycheck-get-error-list-window)))
    (if window
        (quit-window nil window)
        (flycheck-list-errors))))

(use-package flycheck
  :custom (flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc haskell-stack-ghc))
          (flycheck-mode-line-prefix "E|W")
          (flycheck-python-flake8-executable "python3")
          (flycheck-python-pycompile-executable "python3")
          (flycheck-shellcheck-follow-sources nil)
  :hook (sh-mode . flycheck-mode)
  :general ("<f8>"  #'my/toggle-flycheck-error-list
            "<f5>"  #'flycheck-first-error
            "<f6>"  #'flycheck-previous-error
            "<f7>"  #'flycheck-next-error)
  :commands (flycheck flycheck-mode)
  :init (leader-ala-vim
          :no-autload t
          "f" '(:ignore t :wk "Flycheck")
          "f f" #'flycheck-mode
          "f l" #'my/toggle-flycheck-error-list)
        (prefix-c-xt :no-autoload t "f" #'flycheck-mode)
  :config (which-key-add-key-based-replacements
           "C-c !" "Flycheck"))

(use-package consult-flycheck
  :defer t
  :init (leader-ala-vim "f c" #'consult-flycheck))

;;;; language C
(use-package xcscope
  :hook (c-mode . cscope-setup))

(use-package disaster
  :defer t
  :init (general-def c-mode-map "C-c D" #'disaster))

;;;; haskell
(defun my/no-auto-fill ()
  (auto-fill-mode 0))

;; TODO add a hook for inferior-haskell-hook
;; I should write another function than run-haskell to
;; split current window and launch ghci with the correct arguments.
;; run-haskell call inferior-haskell-process. This function needs to be
;; rewritten. So I can choose my way of doing things.
;; Another thing: the completion is not acceptable. Find a way
;; to have at least the same completion as in ghci.
(defun my/haskell-mode-hooks ()
  (flycheck-mode)
  (haskell-indentation-mode)
  (interactive-haskell-mode)
  (imenu-add-menubar-index))

(use-package haskell-mode
  :mode "\\.l?hs\\'"
  :hook ((haskell-mode . my/haskell-mode-hooks)
         (ghci-script-mode . my/no-auto-fill))
  :diminish (interactive-haskell-mode)
  :init
    (push 'haskell-error-mode my/mode-in-emacs-state)
    (push 'haskell-interactive-mode my/mode-in-emacs-state)
    (setq haskell-process-args-ghci '("+RTS" "-M5G" "-RTS" "-ferror-spans")
          haskell-process-log t
          haskell-process-suggest-hoogle-imports t
          haskell-process-suggest-remove-import-lines t
          haskell-process-suggest-restart nil)
  :config
  (general-def
    :states '(normal insert)
    :keymaps 'haskell-mode-map
    "C-c C-r" #'run-haskell
    "C-c C-d" #'haskell-navigate-imports
    "C-c C-a" #'haskell-mode-generate-tags)
  (general-def
    :states '(normal insert)
    :keymaps 'interactive-haskell-mode-map
    "M-."     #'haskell-mode-goto-loc
    "C-c C-t" #'haskell-mode-show-type-at))

;; I don't use speedbar anymore but it may be usefull
(with-eval-after-load 'speedbar
  (speedbar-add-supported-extension ".hs"))

;; I use dante flycheck instead of flycheck-haskell because it is
;; faster.
(defun my/set-flycheck-haskell-checker ()
  (flycheck-add-next-checker
   'haskell-dante
   '(warning . haskell-hlint)))

(use-package dante
  :diminish (dante-mode)
;;  :functions (flycheck-add-next-checker)
  :custom (dante-load-flags '("+c"
                              "-Wall"
                              "-fdiagnostics-color=never"
                              "-ferror-spans"
                              "-fdefer-typed-holes"
                              "-fdefer-type-errors"
                              "-Wwarn=missing-home-modules"
                              "-fno-diagnostics-show-caret"
                              "--make"
                              "+RTS"
                              "-M5G"
                              "-RTS"
                              "-with-rtsopts=\"-M5G\""
                              "-ignore-dot-ghci"))
  :hook ((haskell-mode . dante-mode)
         (dante-mode . my/set-flycheck-haskell-checker))
  :config
    (general-def
        :states '(normal insert)
        :keymaps 'dante-mode-map
        "M-?" #'xref-find-references
        "M-." #'xref-find-definitions)
    (general-def dante-mode-map
      "C-c :" #'dante-info))

(use-package company
  :defer t)

(use-package attrap
  :commands (attrap-attrap)
  :general (:keymaps 'dante-mode-map "M-!" #'attrap-attrap))

(use-package hlint-refactor
  :hook (dante-mode . hlint-refactor-mode)
  :diminish (hlint-refactor-mode)
  :config (which-key-add-key-based-replacements "C-c ," "Refactor"))

;; (use-package retrie
;;   :commands (retrie)
;;   :after (haskell-mode))

;;;; yaml (for stack)
(use-package flycheck-yamllint
  :defer t)

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :config (require 'flycheck-yamllint))

;;;; language Curry
(push "~/.emacs.d/elisp/curry" load-path)
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.curry$" . curry-mode)
                ("\\.lcurry$" . literate-curry-mode))))
(autoload 'curry-mode "curry-mode"
         "Major mode for editing Curry programs." t)
(autoload 'literate-curry-mode "curry-mode"
         "Major mode for editing literate Curry scripts." t)

(my/add-hooks 'curry-mode-hook
              #'turn-on-curry-font-lock
              #'turn-on-curry-decl-scan
              #'turn-on-curry-pakcs)

;; (add-hook 'curry-mode-hook 'turn-on-curry-font-lock)
;; (add-hook 'curry-mode-hook 'turn-on-curry-decl-scan)
;; (add-hook 'curry-mode-hook 'turn-on-curry-pakcs)

;;;; Erlang
(use-package erlang)

;;;; Elixir
(use-package elixir-mode)
  ;;:hook (elixir-mode . flycheck-mode) ;;; doesn't work
  ;;:config (require 'flycheck-elixir))
(use-package inf-elixir)

;;;; lfe (lisp flavour erlang)
(use-package lfe-mode)
  ;; :init (my/add-hooks 'lfe-mode-hook
  ;;                     #'highlight-parentheses-mode
  ;;                     #'rainbow-delimiters-mode))

;;;; ruby
;; TODO: de nouveau essayer realgud-byebug.
;; Il ne faut pas utiliser :realgud:byebug mais M-x realgud:byebug
(use-package evil-ruby-text-objects
  :commands (evil-ruby-text-objects-mode))

(defun my/ruby-settings ()
  "Set tab-width and shift-width to 2. Don't insert encoding magic comment."
  (setq tab-width 2
        evil-shift-width 2
        ruby-insert-encoding-magic-comment nil))

(my/add-hooks 'ruby-mode-hook
              #'flycheck-mode
              #'evil-ruby-text-objects-mode
              #'my/ruby-settings)

(use-package inf-ruby
  :custom (inf-ruby-default-implementation "pry")
          (inf-ruby-implementations
           '(("ruby" . "irb --prompt default --noreadline -r irb/completion")
             ("jruby" . "jruby -S irb --prompt default --noreadline -r irb/completion")
             ("rubinius" . "rbx -r irb/completion")
             ("pry" . "pry -f")))

  :commands (inf-ruby))

;;;; get documentation from the ri command
(use-package yari
  :commands (yari)
  :general (:keymaps 'help-map "y" #'yari))

;; The process to get robe working is difficult.
;; The pry process cannot be launch automatically (for some
;; reasons I don't understand). So I have to launch pry manually.
;; And then I can finally M-x robe-start, and it hopefully works.
(use-package robe
;;  :defer t
  :diminish (robe-mode)
  :hook (ruby-mode . robe-mode))

(use-package ruby-end
  ;;:defer t
  :hook (ruby-mode . ruby-end-mode)
  :diminish (ruby-end-mode))

(use-package realgud-pry
  :commands (realgud:pry))

(use-package realgud-byebug
  :commands (realgud:byebug))

(use-package yard-mode
  :diminish (yard-mode)
  :hook (ruby-mode . yard-mode))

;;;; Crystal
(use-package crystal-mode
  :mode "\\.cr\\'"
  :hook (crystal-mode . evil-ruby-text-objects-mode))
  ;; :init (push '("crystal" . #'crystal-mode) interpreter-mode-alist))

;;;; ocaml
(use-package tuareg
  :mode ("\\.ml\\'" . tuareg-mode)
  :config (require 'opam-user-setup "~/.emacs.d/var/opam-user-setup.el"))

(use-package merlin
  :custom-face (merlin-type-face ((t (:inherit caml-types-expr-face :background "MistyRose4"))))
  :hook ((tuareg-mode caml-mode) . merlin-mode)
  :init (setq merlin-command 'opam))
(use-package merlin-eldoc)


(use-package flycheck-ocaml
  :hook (tuareg-mode . flycheck-mode)
  :init (setq merlin-error-after-save nil)
        (flycheck-ocaml-setup))

(use-package dune
  :mode ("dune" . dune-mode))

(use-package ocp-indent
  :hook ((tuareg-mode . ocp-setup-indent)
         (caml-mode . ocp-indent-caml-mode-setup)))

(use-package utop
  :commands (utop))

;;;; common lisp
(use-package slime
  :hook ((lisp-mode . slime-mode))
        ;;(slime-mode . company-mode))
  :diminish (slime-autodoc-mode)
  :config
  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--core" "/enikar/lang/lisp/sbcl/sbcl.core-for-slime" "--dynamic-space-size" "2000")
                :coding-system utf-8-unix)
          (clisp ("clisp" "-I"))
          (ecl ("ecl")))
        slime-contribs '(slime-fancy))

  (slime-setup))

;;;; scheme
(use-package flycheck-guile
  :defer t)

(use-package geiser-guile
  :defer t)

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
    (require 'flycheck-guile)
    (require 'geiser-guile)))


(use-package racket-mode
  :mode "\\.rkt\\'"
  :hook (racket-mode . racktet-xp-mode)
  :config (require 'racket-xp)
          (require 'geiser-racket))

(use-package geiser-racket
  :defer t)


;;;; Fennel language: extension .fnl
(use-package fennel-mode)

;;;; python
;; anaconda + python.el is better than elpy !
(use-package anaconda-mode
  :commands (anaconda-mode anaconda-eldoc-mode)
  :custom (anaconda-mode-installation-directory
           (my/put-this-in-var "anaconda-mode")))

(my/add-hooks 'python-mode-hook
              #'flycheck-mode
              #'anaconda-mode
              #'anaconda-eldoc-mode)

;; use ipython as interactive python shell
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i")

;;;; perl5
(add-hook 'perl-mode-hook #'flycheck-mode)
;; (setq flycheck-perlcritic-severity 5)

;;;; perl6 aka raku
;; flycheck-raku is now available on melpa
(use-package flycheck-raku
  :defer t)
(use-package raku-mode
  :hook (raku-mode . flycheck-mode)
  :config (require 'flycheck-raku))

;;;; rust
;; rustic provide all functionnalities
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :custom (rustic-lsp-setup-p nil))

;;;; lua
(use-package lua-mode
  :mode ("\\.lua\\'". lua-mode))

;;;; cmake
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
(use-package jq-format)

(use-package smalltalk-mode)
(use-package systemd)
(use-package gitconfig)


(use-package typescript-mode
  :mode "\\.ts\\'")

;;;; nim. Nim-mode depends on flycheck-nimsuggest
;; Activating nimsuggest-mode activate flycheck-mode using
;; flycheck-nimsuggest.
(use-package nim-mode
  :mode ("\\.nim\\'")
  :hook (nim-mode . nimsuggest-mode))

;;;; LaTeX
;; auctex is very boring. They don't respect convention for
;; autoloading. Loading auctex.el slow down the emacs startup
;; (unless (package-installed-p 'auctex)
;;   (package-install 'auctex))
;; (load "auctex" nil t)
(use-package auctex)

(use-package latex-extra
  :hook (LaTeX-mode . latex-extra-mode))

;; (use-package latex-math-preview)


(use-package latex-preview-pane
  :commands (latex-preview-pane-mode))

;; (use-package latexdiff)

(defun my/set-tab-width-to-8 ()
  (setq tab-width 8))
(add-hook 'shell-mode-hook #'my/set-tab-width-to-8)


;;;; gforth
(push "~/.emacs.d/elisp/gforth" load-path)
(autoload #'forth-mode "gforth" "Forth mode" t)
(autoload #'run-forth "gforth" "Run an inferior Forth process, input and output via buffer *forth*." t)
(setq auto-mode-alist
  (append '(("\\.fs$" . #'forth-mode)
    ("\\.4th$" . #'forth-mode)
    ("\\.fth$" . #'forth-mode)) auto-mode-alist))

;; ;;;; latex help
(push "~/.emacs.d/elisp/latex-help" load-path)
;;(autoload #'latex-help "ltx-help" "Latex help in info" t)
(general-def help-map "C-l" #'latex-help)


(provide 'programming)
;;; programming.el ends here
