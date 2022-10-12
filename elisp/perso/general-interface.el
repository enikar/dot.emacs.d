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

(setq package-native-compile t)

(defun my/add-hooks (mode-hook hooks)
  "Add hooks in the list `hooks' to `mode-hook'"
  (dolist (hook hooks)
    (add-hook mode-hook hook)))

(use-package diminish
  :commands (diminish))

(use-package dimmer
  :custom (dimmer-fraction 0.16)
          (dimmer-adjustment-mode :foreground)
  :config (dimmer-mode))

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
          ;; (paradox-lines-per-entry 2)
  :commands (paradox-list-packages))

;; leader key ala vim.
(use-package evil-leader
  :custom (evil-leader/leader "_")
  :diminish (evil-leader-mode)
  :init (global-evil-leader-mode)
        (evil-leader/set-key "w" #'whitespace-mode))


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
        (calculator-mode . evil-emacs-state)

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

    (evil-ex-define-cmd "ls" #'ibuffer)
    (global-set-key (kbd "C-x C-b") #'ibuffer)
    ;; settings to use evil-numbers C-a and C-x in vim normal mode
    ;; But C-x is use by emacs, and it is convenient to keep it.
    ;; (define-key evil-normal-state-map (kbd "C-c +") #'evil-numbers/inc-at-pt)
    ;; (define-key evil-visual-state-map (kbd "C-c +") #'evil-numbers/inc-at-pt)
    ;; (define-key evil-normal-state-map (kbd "C-c -") #'evil-numbers/dec-at-pt)
    ;; (define-key evil-visual-state-map (kbd "C-c -") #'evil-numbers/dec-at-pt)
    (evil-leader/set-key
      "+" #'evil-numbers/inc-at-pt
      "-" #'evil-numbers/dec-at-pt)
    (evil-define-key 'normal 'global (kbd "Q") #'evil-fill-and-move)
    (evil-define-key 'normal (current-global-map) (kbd "C-w e") #'find-file-other-window)
    (evil-define-key 'normal (current-global-map) (kbd "C-w b") #'consult-buffer-other-window)
    (evil-define-key 'normal (current-global-map) (kbd "C-w C-l") #'evil-window-right)))

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
  :bind (("C-," . embrace-commander))
  :init (evil-leader/set-key "e" #'embrace-commander))

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
    "ci" #'evilnc-comment-or-uncomment-lines
    "cl" #'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" #'evilnc-copy-and-comment-lines
    "cp" #'evilnc-comment-or-uncomment-paragraphs
    "cr" #'comment-dwim
    "cv" #'evilnc-toggle-invert-comment-line-by-line
    "."  #'evilnc-comment-operator))

(use-package nocomments-mode
  :commands (nocomments-mode)
  :init (evil-leader/set-key "cn" #'nocomments-mode))

(use-package evil-visualstar
  :custom (evil-visualstar/persistent t)
  :init (global-evil-visualstar-mode t))

(use-package evil-org
  :hook (org-mode. evil-org-mode))

(use-package which-key
  :custom (which-key-sort-order 'which-key-key-order-alpha)
  :diminish (which-key-mode)
  :hook (after-init . which-key-mode)
  :init (unbind-key "C-h" help-map))

(use-package goto-chg
  :bind (("M-s M-e" . goto-last-change)
         ("M-s M-r" . goto-last-change-reverse)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package hydra
  :commands (defhydra))

(use-package vertico
  :config (vertico-mode)
  :custom (vertico-count 25)
  :bind (:map vertico-map
              ("C-'"       . #'vertico-quick-exit)
              ;; Have to rebind this because C-m is translated to RET.
              ("<return>"  . #'exit-minibuffer)
              ("C-m"       . #'vertico-insert)
              ("C-c SPC"   . #'vertico-quick-exit)
              ("DEL"       . #'vertico-directory-delete-char)))

(use-package consult
  ;;:config
  ;; (defun pt/yank-pop ()
  ;;   "As pt/yank, but calling consult-yank-pop."
  ;;   (interactive)
  ;;   (let ((point-before (point)))
  ;;     (consult-yank-pop)
  ;;     (indent-region point-before (point))))

  :bind (("C-x b"   . #'consult-buffer)
         ("C-x r l" . #'consult-bookmark)
         ("C-x C-f" . #'find-file)
         ("C-h a"   . #'consult-apropos))

  :custom (completion-in-region-function #'consult-completion-in-region)
          (xref-show-xrefs-function #'consult-xref)
          (xref-show-definitions-function #'consult-xref)
          (consult-project-root-function #'deadgrep--project-root) ;; ensure ripgrep works
          (consult-preview-key '(:debounce 0.3 any))

  :init (evil-leader/set-key "m" #'consult-imenu)
        (setq xref-show-xrefs-function #'consult-xref
              xref-show-definitions-function #'consult-xref)
        (setq register-preview-delay 0.5
              register-preview-function #'consult-register-format)
  :config (setq consult-narrow-key (kbd "C-+"))
          (setq consult-project-function #'(lambda (_) (locate-dominating-file "." ".git"))))

(defun immediate-which-key-for-narrow (fun &rest args)
  (let* ((refresh t)
         (timer (and consult-narrow-key
                     (memq :narrow args)
                     (run-at-time 0.05 0.05
                                  #'(lambda ()
                                    (if (eq last-input-event (elt consult-narrow-key 0))
                                        (when refresh
                                          (setq refresh nil)
                                          (which-key--update))
                                      (setq refresh t)))))))
    (unwind-protect
        (apply fun args)
      (when timer
        (cancel-timer timer)))))
(advice-add 'consult--read :around #'immediate-which-key-for-narrow)

(use-package marginalia
  :config (marginalia-mode))

(use-package orderless
  :init (setq completion-styles '(substring orderless basic)
              completion-category-defaults nil
              completion-category-overrides '((files (style partial-completion)))))

(use-package prescient
  :config (prescient-persist-mode))

(use-package affe
  :init ;; use orderless as the affe regexp compiler
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-."))
  (evil-leader/set-key "F" #'affe-find))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package ctrlf
  :custom (ctrlf-default-search-style 'fuzzy))
  :config (ctrlf-mode)

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

(use-package corfu-doc)

(use-package kind-icon
  :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config (push #'kind-icon-margin-formatter corfu-margin-formatters))

(defun dabbrev-completion-all-buffers ()
      "dabbrev-completion in *all* buffers"
    (interactive)
    (let ((current-prefix-arg '(16))) ; c-u c-u
      (call-interactively #'dabbrev-completion)))

(use-package dabbrev
  :bind (("M-²" . dabbrev-completion-all-buffers)
          ;; Swap M-/ and C-M-/
         ("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)) ;; dabbrev-expand is also provide by C-p in evil-insert-state
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package cape
  :init (push #'cape-dabbrev completion-at-point-functions)
        (push #'cape-file completion-at-point-functions)
  :bind (("M-&" . cape-dabbrev)))

(use-package avy
  :config (avy-setup-default))

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
    (global-set-key (kbd "M-s M-s")  #'avy-goto-char)
    (evil-leader/set-key
      "ac" #'avy-goto-char
      "aC" #'avy-goto-char-2
      "as" #'avy-goto-subword-1
      "ar" #'avy-resume
      "aw" #'avy-goto-word-0
      "aW" #'avy-goto-word-1
      "al" #'avy-goto-line
      "aa" #'evil-avy-mode))

(use-package ace-jump-mode
  :after (evil-leader)
  :commands (ace-jump-mode
             ace-jump-char-mode
             ace-jump-line-mode)
  :init
    (evil-leader/set-key
      "jw" #'ace-jump-mode
      "jc" #'ace-jump-char-mode
      "jl" #'ace-jump-line-mode))

(use-package ace-window
  :commands (ace-window)
  :init (evil-leader/set-key "o" #'ace-window)
  )

(use-package ace-link
  :commands (ace-link)
  :init (ace-link-setup-default)
  )

(use-package consult-flyspell
  :commands (consult-flyspell-correct-function)
  :init (setq consult-flyspell-correct-function #'(lambda () (flyspell-correct-at-point) (consult-flyspell)))
        (evil-leader/set-key "s" #'consult-flyspell-correct-function))

(use-package consult-recoll
  :commands (consult-recoll)
  :init (evil-leader/set-key "r" #'consult-recoll))

(use-package desktop
  :init (desktop-save-mode 1)
        (setq history-length 250)
        (setq desktop-globals-to-save
              (append
               '(consult--buffer-history
                 consult--apropos-history
                 ctrlf-search-history
                 Info-isearch-initial-history
                 Info-search-history
                 Man-topic-history
                 bookmark-history
                 buffer-name-history
                 calc-alg-entry-history
                 command-history
                 compile-history
                 cscope-prompt-minibuffer-history
                 evil-ex-history
                 evil-ex-search-history
                 evil-markers-alist
                 evil-search-backward-history
                 evil-search-forward-history
                 evil-lion--user-regex-history
                 eww-prompt-history
                 extended-command-history
                 face-name-history
                 flycheck-read-checker-history
                 geiser-doc--history
                 goto-line-history
                 grep-files-history
                 grep-regexp-history
                 hi-lock-face-history
                 hi-lock-regexp-history
                 info-lookup-history
                 kmacro-ring
                 magit-git-command-history
                 magit-revision-history
                 minibuffer-history
                 occur-collect-regexp-history
                 query-replace-history
                 read-char-history
                 read-expression-history
                 realgud:byebug-minibuffer-history
                 realgud:perldb-minibuffer-history
                 realgud:pry-minibuffer-history
                 regexp-history
                 set-variable-value-history
                 shell-command-history
                 slime-connect-host-history
                 slime-connect-port-history
                 slime-inferior-lisp-program-history
                 slime-minibuffer-history
                 slime-repl-shortcut-history
                 table-capture-columns-history
                 table-capture-justify-history
                 table-capture-min-cell-width-history
                 table-cell-height-history
                 table-cell-span-direction-history
                 table-cell-split-contents-to-history
                 table-cell-split-orientation-history
                 table-cell-width-history
                 table-col-delim-regexp-history
                 table-columns-history
                 table-insert-row-column-history
                 table-justify-history
                 table-row-delim-regexp-history
                 table-rows-history
                 table-sequence-count-history
                 table-sequence-increment-history
                 table-sequence-interval-history
                 table-sequence-justify-history
                 table-sequence-string-history
                 table-source-caption-history
                 table-source-language-history
                 table-target-history
                 tmm--history
                 transient-history
                 xref--history
                 xref--read-identifier-history
                 )
               desktop-globals-to-save)))

;; Install an advice when setup the doom modeline.
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

(use-package all-the-icons-completion
  :init (all-the-icons-completion-mode 1)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package emojify
  :diminish (emojify))
  ;:hook (after-init . global-emojify-mode))


(use-package iedit
  :bind ("C-;" . iedit-mode)
  :init (evil-leader/set-key ";" #'iedit-mode))

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
;;       "mw" #'mc/mark-next-like-this-word
;;       "mb" #'mc/mark-previous-like-this-word
;;       "mt" #'mc/mark-next-like-this
;;       "mT" #'mc/mark-previous-like-this
;;       "ma" #'mc/mark-all-like-this
;;       "mr" #'mc/mark-all-in-region
;;       "me" #'mc/mark-more-like-this-extended
;;       "mW" #'mc/mark-all-words-like-this
;;       "mS" #'mc/mark-all-symbols-like-this
;;       "mD" #'mc/mark-all-like-this-dwim)))

;; use `gr` prefix in normal mode to access mc functionalities
(use-package evil-mc
  :diminish (evil-mc-mode)
  :init (global-evil-mc-mode 1))


(use-package undo-tree
  :diminish (undo-tree-mode)
  :commands (undo-tree-visualize)
  :custom (evil-undo-system 'undo-tree)
  :init (evil-leader/set-key "U" #'undo-tree-visualize)
        (global-undo-tree-mode))

(use-package openwith
  :custom (openwith-confirm-invocation t)
          (openwith-associations
            '(("\\.\\(pdf\\|ps\\|djvu\\)\\'" "zathura" (file))
              ("\\.\\(mp3\\|flac\\|ogg\\|aac\\)\\'" "mplayer" (file))
              ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mkv\\|mp4\\|webm\\|ogv\\)\\'" "mplayer" ("-idx" file))
              ("\\.\\(od[sgtbfm]\\|st[icwd]\\|sx[gmdiwc]\\|ot[sgtp]\\|docx?\\|rtf\\|xl[sw]\\|pp[ts]\\)\\'" "libreoffice" nil)))
  :init (openwith-mode t))

(use-package magit
  :commands (magit))

(use-package libgit)
(use-package magit-libgit)

(use-package consult-ls-git
  :commands (consult-ls-git)
  :init (evil-leader/set-key "G" #'consult-ls-git))

(use-package treemacs
  :bind ("C-x t t" . treemacs)
  :custom (treemacs-width 40)
          (treemacs-indentation 1)
  :config
  (require 'treemacs-evil)
  (require 'treemacs-magit))

(use-package ripgrep
  :commands (ripgrep-regexp)
  :init (evil-leader/set-key
          "gr" #'ripgrep-regexp
          "gc" #'affe-grep))
(use-package deadgrep
  :commands (deadgrep)
  :init (evil-leader/set-key "gd" #'deadgrep))

(use-package ag
  :commands (ag
             ag-files
             ag-regexp
             ag-project
             ag-project-files
             ag-project-regexp)
  :init (setq ag-highlight-search t)
  (evil-leader/set-key
    "gaa" #'ag
    "gaf" #'ag-files
    "gar" #'ag-regexp
    "gap" #'ag-project
    "gaF" #'ag-project-files
    "gaR" #'ag-project-regexp))

(use-package consult-ag
  :commands (consult-ag)
  :init (evil-leader/set-key "gac" #'consult-ag))

(use-package helpful
  :hook (helpful-mode . evil-emacs-state)
  :bind (:map help-map ("k" . helpful-key))
  :commands (helpful-at-point
             helpful-callable
             helpful-variable
             helpful-command)
  :init
  (evil-leader/set-key
    "hk" #'helpful-key
    "hf" #'helpful-callable
    "hv" #'helpful-variable
    "hp" #'helpful-at-point
    "hc" #'helpful-command))

(use-package embark
  :commands (embark-act)
  :init (evil-leader/set-key "E" #'embark-act))

(use-package duplicate-thing
  :commands (duplicate-thing)
  :init (evil-leader/set-key "d" #'duplicate-thing))

(use-package parent-mode
  :commands (parent-mode-list parent-mode-is-derived-p))

(use-package paren
  :config (show-paren-mode)
  :custom (show-paren-style 'parenthesis))

(use-package fill-column-indicator
  :bind (("C-x t C-f" . fci-mode))
  :commands (fci-mode))

(use-package hl-todo
  :diminish (hl-todo-mode)
  :hook ((prog-mode) . hl-todo-mode))

(use-package vterm
  :init (setq vterm-always-compile-module t))

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;;;; To access universal argument in evil-normal-state
(evil-leader/set-key "u" #'universal-argument)

;;;; better dired mode
(autoload #'dired-omit-mode "dired-x")
(with-eval-after-load 'dired (load "dired-x"))
(defun my/set-dired-omit-mode()
  (dired-omit-mode 1))

(add-hook 'dired-mode-hook #'my/set-dired-omit-mode)
(add-hook 'text-mode-hook #'turn-on-auto-fill)

;; Peut poser un problème lorsqu'on édite un fichier
;; qui est destiné à être une liste de fichier pour tar
;; option -T de tar.
(setq require-final-newline t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq-default indicate-empty-lines t)

(defalias 'man-mode 'Man-mode)
(setq comint-scroll-show-maximum-output t
      comint-scroll-to-bottom-on-input t)

(setq save-interprogram-paste-before-kill t
      scroll-step 1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(setq truncate-string-ellipsis "…"
      use-short-answers t
      compilation-scroll-output 'first-error)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)
(setq executable-prefix-env t)

;; diminish some minor modes
(diminish 'auto-revert-mode "ARev")
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)

(global-so-long-mode)


(defun my/set-personnal-font ()
  "Restore my favorite font setting."
  (interactive)
  (set-frame-font "-PfEd-Inconsolata-normal-normal-normal-*-24-*-*-*-m-0-iso10646-1"))

;; perhaps I should setq disabled-command-function to nil
;; thus there were no longer disabled commands.
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(context-menu-mode 1)

(provide 'general-interface)
;;; general-interface.el ends here
