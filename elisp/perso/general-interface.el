;;; general-interface.el --- loads and configures pacakges for general interface. -*- lexical-binding: t; -*-
;;; Commentary:
;;  use-package is used when apropriate, else set load-path
;;  auto-mode-alist, autoloads and hooks using the bare emacs way.
;;; Code:

;; (eval-when-compile
;;   (require 'use-package))
(require 'use-package)
;; To add a :ensure for each use-package
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

(setq package-native-compile t)

(defun my/add-hooks (mode-hook &rest hooks)
  "Add hooks in the list `hooks' to `mode-hook'"
  (dolist (hook hooks)
    (add-hook mode-hook hook)))

(defun my/add-hook-multi (hook &rest mode-hooks)
  "Add a `hook' for multiple `mode-hooks'"
  (dolist (mode mode-hooks)
    (add-hook mode hook)))

;;;; general to bind keys in a convenient way.
(use-package general
  :config ;;(general-evil-setup)
          ;;(general-auto-unbind-keys)
          (general-create-definer prefix-c-xw :prefix "C-x w")
          (general-create-definer prefix-c-xt :prefix "C-x t")
          (general-unbind :states '(normal viusal operator) "SPC")
          (general-create-definer leader-ala-vim
            :states '(normal visual motion operator insert emacs)
            :prefix "SPC"
            :non-normal-prefix "M-SPC"
            ;;:prefix-command 'leader-ala-vim-command
            :prefix-map 'leader-ala-vim-map)

          (prefix-c-xw
            "f" #'find-file-at-point ;; gf
            "h" #'hexl-find-file
            "i" #'insert-file       ;;  :r
            "w" #'write-region      ;; visual, then :w
            "v" #'view-file
            "c" #'comment-dwim      ;; SPC cr
            "l" #'font-lock-mode
            "b" #'font-lock-fontify-block
            "a" #'font-lock-fontify-buffer
            "m" #'man-follow)

          ;; unbind all "C-x t" bindings (functions for using emacs tab).
          (general-unbind ctl-x-map "t")

          (prefix-c-xt
            "e"   #'recentf-edit-list
            "i"   #'indent-region
            "p"   #'pop-tag-mark)

          (leader-ala-vim
            ""      '(nil :wk "leader-ala-vim menu")
            "SPC"   #'execute-extended-command
            "M-SPC" #'just-one-space
            ":"     #'eval-expression
            "g"     '(:ignore t :wk "Searching")
            "gg"    #'rgrep
            "x"     '(:ignore t :wk "Xref")
            "xd"    #'xref-find-definitions
            "xr"    #'xref-find-references
            "xs"    #'xref-show-xrefs-function
            "w"     #'whitespace-mode)
          (general-def
            "<cancel>" #'keyboard-quit
            "M-RET"    #'hippie-expand
            "C-c h"    #'hippie-expand
            "<f9>"     #'compile
            "<f11>"    #'previous-error
            "<f12>"    #'next-error))

(use-package diminish
  :commands (diminish))

(use-package dimmer
  :custom (dimmer-fraction 0.16)
          (dimmer-adjustment-mode :foreground)
  :config (dimmer-mode))

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

;;(setq evil-want-keybinding nil)
(use-package evil
  :init (evil-mode 1)
  :custom (evil-ex-search-highlight-all t)
          (evil-ex-search-persistent-highlight nil)
          (evil-ex-search-case 'smart)
          ;(evil-ex-visual-char-range t)
          ;(evil-want-C-u-scroll t)
          (evil-search-module 'evil-search)
          (evil-want-Y-yank-to-eol nil)
          (evil-want-fine-undo t)
          (evil-want-C-i-jump nil)
  :diminish (evil-mode)
  :config
    (dolist (mode '(dired-mode
                    view-mode
                    ;;Info-mode
                    ;;help-mode
                    calculator-mode))
      (evil-set-initial-state mode 'emacs))

    (defun evil-ex-search-next-auto-clear-highlights ()
        (interactive)
        (evil-ex-search-next)
        (run-with-idle-timer 1 nil #'evil-ex-nohighlight))
    (defun evil-ex-search-previous-auto-clear-highlights ()
        (interactive)
        (evil-ex-search-previous)
        (run-with-idle-timer 1 nil #'evil-ex-nohighlight))

    (general-def
        :keymaps 'evil-motion-state-map
        "C-a" #'evil-jump-forward ; instead of C-i
        "/"   #'evil-ex-search-forward
        "?"   #'evil-ex-search-backward
        "n"   #'evil-ex-search-next-auto-clear-highlights
        "N"   #'evil-ex-search-previous-auto-clear-highlights)

    (evil-ex-define-cmd "ls" #'ibuffer)
    (general-def "C-x C-b" #'ibuffer)
    ;; settings to use evil-numbers C-a and C-x in vim normal mode
    ;; But C-x is use by emacs, and it is convenient to keep it.
    (leader-ala-vim :no-autolad t
      "+"   #'evil-numbers/inc-at-pt
      "-"   #'evil-numbers/dec-at-pt
      "TAB" `(,#'evil-switch-to-windows-last-buffer :wk "Last buffer"))
    (general-def
     :states 'normal
     :keymaps 'global
     "Q" #'evil-fill-and-move
     "C-w e" #'find-file-other-window
     "C-w b" #'consult-buffer-other-window)
    (general-unbind evil-window-map
      "C-h"    ; use by which-key
      ;; "gt"  ; bindings to emacs tab functions
      ;; "gT"
      "g"))    ; remove the prefix is sufficient

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
  :hook (org-mode-hook . embrace-org-mode-hook)
  :general ("C-,"  #'embrace-commander)
           (leader-ala-vim "e" #'embrace-commander))

(use-package evil-embrace
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
  :general
  ("M-;" #'evilnc-comment-or-uncomment-lines)
  (leader-ala-vim
    ";"  #'evilnc-comment-operator
    "c" '(:ignore t :wk "Comments")
    "cc" #'evilnc-copy-and-comment-lines
    "cj" #'evilnc-quick-comment-or-uncomment-to-the-line
    "cl" #'evilnc-comment-or-uncomment-lines
    "cp" #'evilnc-comment-or-uncomment-paragraphs
    "cr" #'comment-dwim
    "cv" #'evilnc-toggle-invert-comment-line-by-line))

(use-package nocomments-mode
  :init (leader-ala-vim "cn" #'nocomments-mode))

(use-package evil-visualstar
  :custom (evil-visualstar/persistent t)
  :init (global-evil-visualstar-mode t))

(use-package evil-org
  :hook (org-mode. evil-org-mode))

(use-package which-key
  :custom (which-key-sort-order 'which-key-key-order-alpha)
          (which-key-idle-delay 0.5)
  :diminish (which-key-mode)
  :hook (after-init . which-key-mode)
  :init (general-unbind help-map "C-h"))

(use-package goto-chg
  :general ("M-s M-s"  #'goto-last-change)
           ("M-s M-r"  #'goto-last-change-reverse))

(use-package expand-region
  :general ("C-="  #'er/expand-region)
           (leader-ala-vim "=" #'er/expand-region))

(use-package hydra
  :commands (defhydra))

(use-package vertico
  :demand t
  :config (vertico-mode)
  :custom (vertico-count 25)
  :general (:keymaps 'vertico-map
            :no-autoload t
            "C-'"        #'vertico-quick-exit
            ;; "<return>"   #'exit-minibuffer
            ;; "C-m"        #'vertico-insert
            ;; "C-c SPC"    #'vertico-quick-exit
            "C-c SPC"    #'vertico-insert
            "DEL"        #'vertico-directory-delete-char))

(use-package consult
  :commands (consult-customize) ;; for affe
  ;;:config
  ;; (defun pt/yank-pop ()
  ;;   "As pt/yank, but calling consult-yank-pop."
  ;;   (interactive)
  ;;   (let ((point-before (point)))
  ;;     (consult-yank-pop)
  ;;     (indent-region point-before (point))))

  :general
  ("C-x b"    #'consult-buffer
   "C-x r l"  #'consult-bookmark
   "C-x C-f"  #'find-file
   "C-h a"    #'consult-apropos)

  :custom (completion-in-region-function #'consult-completion-in-region)
          (xref-show-xrefs-function #'consult-xref)
          (xref-show-definitions-function #'consult-xref)
          (consult-project-root-function #'deadgrep--project-root) ;; ensure ripgrep works
          (consult-preview-key '(:debounce 0.3 any))

  :init (leader-ala-vim "m" #'consult-imenu)
        (prefix-c-xt "r" #'consult-recent-file)
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
  :init (marginalia-mode)
  :general (:keymaps 'minibuffer-local-map
                     "M-a"  #'marginalia-cycle))

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
  (leader-ala-vim
    "gf" #'affe-find
    "gc" #'affe-grep)
  :custom (affe-count 30)
  :config ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package consult-dir
  :general ("C-x C-d"  #'consult-dir)
           (:keymaps 'vertico-map
            "C-x C-d"  #'consult-dir
            "C-x C-j"  #'consult-dir-jump-file))

(use-package ctrlf
  :custom (ctrlf-default-search-style 'fuzzy)
  :config (ctrlf-mode))

;; (defun corfu-enable-in-minibuffer ()
;;   "Enable Corfu in the minibuffer if `completion-at-point' is bound."
;;   (when (where-is-internal #'completion-at-point (list (current-local-map)))
;;     ;; (setq-local corfu-auto nil) Enable/disable auto completion
;;     (corfu-mode 1)))
;;(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

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
  ;; :hook (minibuffer-setup-hook . corfu-enable-in-minibuffer)
  :init
  (global-corfu-mode))

(use-package kind-icon
  :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
          (kind-icon-default-style
           '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.7 :scale 1.0))
  :config (push #'kind-icon-margin-formatter corfu-margin-formatters))

(defun dabbrev-completion-all-buffers ()
      "dabbrev-completion in *all* buffers"
    (interactive)
    (let ((current-prefix-arg '(16))) ; c-u c-u
      (call-interactively #'dabbrev-completion)))

(use-package dabbrev
  :general
  ("M-²"  #'dabbrev-completion-all-buffers
   "s-²"  #'dabbrev-completion-all-buffers
   ;; Swap M-/ and C-M-/
   "M-/"  #'dabbrev-completion
   "C-M-/"  #'dabbrev-expand) ;; dabbrev-expand is also provide by C-p in evil-insert-state
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package cape
  :init (push #'cape-dabbrev completion-at-point-functions)
        (push #'cape-file completion-at-point-functions)
  :general ("M-&"  #'cape-dabbrev
            "M-³"  #'cape-dabbrev
            "s-³"  #'cape-dabbrev))

(use-package avy
  :config (avy-setup-default))

;; useless because the minor is not activated
;; But I setup some bindings with my leader-ala-vim
(use-package evil-avy
  :after (avy)
  :init
    (leader-ala-vim
      "a"   '(:ignore t :wk "Avy")
      "aa"  #'evil-avy-mode
      "ac"  #'avy-goto-char
      "aC"  #'avy-goto-char-2
      "al"  #'avy-goto-line
      "ar"  #'avy-resume
      "as"  #'avy-goto-subword-1
      "aw"  #'avy-goto-word-0
      "aW"  #'avy-goto-word-1))

(use-package ace-jump-mode
  :init
  (leader-ala-vim
    "j"   '(:ingore t :wk "Ace-jump")
    "ja"  #'ace-jump-mode
    "jc"  #'ace-jump-char-mode
    "jl"  #'ace-jump-line-mode))

(use-package ace-window
  :init (leader-ala-vim "O" #'ace-window))

(use-package ace-link
  :commands (ace-link)
  :init (ace-link-setup-default))

(use-package consult-flyspell
  :init (setq consult-flyspell-correct-function #'(lambda () (flyspell-correct-at-point) (consult-flyspell)))
        (leader-ala-vim "s" #'consult-flyspell-correct-function))

(use-package consult-recoll
  :init (leader-ala-vim "r" #'consult-recoll))

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
  :general ("C-;"  #'iedit-mode))

(use-package evil-multiedit
  :commands (evil-multiedit-ex-match)
  :general (general-def :states 'visual
                     "R" #'evil-multiedit-match-all
                     "M-d" #'evil-multiedit-match-and-next
                     "M-D" #'evil-multiedit-match-and-prev
                     "C-M-d" #'evil-multiedit-restore
           (general-def :states '(normal insert)
                     "M-d" #'evil-multiedit-match-symbol-and-next
                     "M-D" #'evil-multiedit-match-symbol-and-prev)
           (general-def :states 'insert
                     "C-M-d" #'evil-multiedit-toggle-marker-here))
  :init (evil-ex-define-cmd "ie[dit]" #'evil-multiedit-ex-match))

;; multiple-cursors for evil
;; see also https://github.com/fgallina/region-bindings-mode
;; to activate bindings when a region is selected
;; use `gr` prefix in normal mode to access mc functionalities
(use-package evil-mc
  :diminish (evil-mc-mode)
  :init (global-evil-mc-mode 1)
        (general-def
          :keymaps 'evil-mc-key-map
          :states '(normal visual)
          "C-t" #'pop-tag-mark ; want to keep pop tag
          "g C-t" #'evil-mc-skip-and-goto-next-match))

(use-package undo-tree
  :diminish (undo-tree-mode)
  :custom (evil-undo-system 'undo-tree)
  :init (leader-ala-vim "U" #'undo-tree-visualize)
        (global-undo-tree-mode))

(use-package browse-kill-ring
  :general (:states 'insert "M-y" #'browse-kill-ring))

;;;; Folding. There are several possibilities.
;; Use:
;; - hideshow: M-x hs-minor-mode
;; - origami: M-x origami-mode
;; - vdiff (vim diff): inside a vdiff session
;; - evil-vimish-fold: M-x evil-vimish-fold-mode
;; Folding is also possible in ouline-mode, org-mode and
;; hide-ifdef-mode (these are built in emacs).
;; Hideshow is built in emacs, so it has my preference.
(use-package origami
  :general (leader-ala-vim
             "o" '(:ignore t :wk "Origami")
             "oo" #'origami-open-node
             "oO" #'origami-open-node-recursively
             "os" #'origami-show-node
             "oS" #'origami-show-only-node
             "oc" #'origami-close-node
             "oC" #'origami-close-node-recursively
             "oa" #'origami-open-all-nodes
             "oA" #'origami-close-all-nodes
             "ot" #'origami-toggle-node
             "oT" #'origami-toggle-node-recursively
             "oH" #'origami-toggle-all-nodes
             "o<" #'origami-previous-fold
             "o>" #'origami-next-fold
             "of" #'origami-forward-fold
             "oF" #'origami-forward-fold-same-level
             "oB" #'origami-backward-fold-same-level
             "ou" #'origami-undo
             "or" #'origami-redo
             "oR" #'origami-reset))

;; Manual definition of folds ala vim..
(use-package evil-vimish-fold)

;; Choose a folding method
(require 'cl-lib)
(defun myfold/set-folding-method (fold-method)
  "Set folding method to `fold-method'."
  (let ((inhibit-message t))
    (dolist (m '(hs-minor-mode origami-mode evil-vimish-fold-mode))
      (funcall m -1)))
  (cl-case fold-method
    (hideshow (hs-minor-mode))
    (origami (origami-mode))
    (vimish  (evil-vimish-fold-mode))
    (none nil)))

(defun myfold/choose-folding-method (fold-method)
  "Choose a folding method among `hideshow', `origami', `vimish' or
   `none'."
  (interactive
   (list
    (let ((candidate
           (completing-read
            "Folding method: "
            '(hideshow origami vimish none))))
      candidate)))
  (myfold/set-folding-method (intern fold-method)))

(leader-ala-vim
  "F" `(,#'myfold/choose-folding-method :wk "Choose folding method"))

;; vimdiff, ediff is perfect but they aren't folding possibilities
;; with it. Vdiff is also very nice.
(use-package vdiff
  :general (leader-ala-vim
             "v" '(:ignore t :wk "Vdiff")
             "vv" #'vdiff-hydra/body
             "vf" #'vdiff-files
             "vF" #'vdiff-files3
             "vb" #'vdiff-buffers
             "vB" #'vdiff-buffers3
             "vc" #'vdiff-current-file
             "vm" #'vdiff-merge-conflict))

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
  :init (leader-ala-vim "G" #'consult-ls-git))

(use-package treemacs
  :commands (treemacs)
  :custom (treemacs-width 40)
          (treemacs-indentation 1)
  :init (prefix-c-xt "t" #'treemacs)
  :config (require 'treemacs-all-the-icons)
          (require 'treemacs-evil)
          (require 'treemacs-magit))

(use-package ripgrep
  :init (leader-ala-vim "gr" #'ripgrep-regexp))
(use-package deadgrep
  :init (leader-ala-vim "gd" #'deadgrep))

(use-package ag
  :init (setq ag-highlight-search t)
  (leader-ala-vim
    "ga"  '(:ignore t :wk "Ag")
    "gaa" #'ag
    "gaf" #'ag-files
    "gar" #'ag-regexp
    "gap" #'ag-project
    "gaF" #'ag-project-files
    "gaR" #'ag-project-regexp))

(use-package consult-ag
  :init (leader-ala-vim "gac" #'consult-ag))

(defun my/helpful-quit-window ()
  (general-def
    :keymaps 'helpful-mode-map
    :states 'normal
    "q" #'quit-window))

(use-package helpful
  :hook (helpful-mode . my/helpful-quit-window)
  :general (:keymaps 'help-map
                     "C" #'helpful-command
                     "f" #'helpful-callable
                     "k" #'helpful-key
                     "v" #'helpful-variable)
           ;; (:keymaps '(helpful-mode-map override) "q" #'quit-window)
  :init
  (leader-ala-vim
    "h"  '(:ignore t :wk "Helpful")
    "hk" #'helpful-key
    "hf" #'helpful-callable
    "hv" #'helpful-variable
    "hp" #'helpful-at-point
    "hc" #'helpful-command))

(use-package embark
  :general ("C-c e"  #'embark-act)
  :init (leader-ala-vim "E" #'embark-act))

(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

(use-package duplicate-thing
  :init (leader-ala-vim "d" #'duplicate-thing))

(use-package parent-mode
  :commands (parent-mode-list parent-mode-is-derived-p))

(use-package paren
  :config (show-paren-mode)
  :custom (show-paren-style 'parenthesis))

(use-package fill-column-indicator
  ;;:commands (fci-mode)     ;; managed by general.el
 :init (prefix-c-xt "C-f" #'fci-mode))

(use-package hl-todo
  :diminish (hl-todo-mode)
  :hook ((prog-mode) . hl-todo-mode))

(use-package vterm
  :init (setq vterm-always-compile-module t))

(setq save-interprogram-paste-before-kill t
      comint-scroll-show-maximum-output t
      comint-scroll-to-bottom-on-input t
      scroll-step 1
      confirm-kill-processes nil
      native-comp-async-report-warnings-errors 'silent
      initial-scratch-message nil
      ring-bell-function 'ignore
      visible-bell nil
      truncate-string-ellipsis "…"
      use-short-answers t
      enable-recursive-minibuffers t
      require-final-newline t
      executable-prefix-env t
      dired-dwim-target t
      compilation-scroll-output 'first-error)

(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(minibuffer-depth-indicate-mode)

(setq-default tab-width 4
              indent-tabs-mode nil
              tab-always-indent 'completion
              tab-first-completion 'word-or-paren-or-punct
              indicate-empty-lines t)

(require 'hl-line)
(my/add-hook-multi #'hl-line-mode 'prog-mode-hook 'text-mode-hook)
(prefix-c-xt "h" #'hl-line-mode)
(global-so-long-mode)

;;;; better dired mode
(autoload #'dired-omit-mode "dired-x")
(with-eval-after-load 'dired (require 'dired-x))
(defun my/set-dired-omit-mode()
  (dired-omit-mode 1))

(add-hook 'dired-mode-hook #'my/set-dired-omit-mode)

(defun dired-up-directory-same-buffer ()
  "Go up in the same buffer."
  (interactive)
  (find-alternate-file ".."))

;; To show only directories in dired
(fset 'dired-only-show-directories
      "*/tk")

(general-def
 :keymaps 'dired-mode-map
 "^" #'dired-up-directory-same-buffer
 "C-x C-k D" #'dired-only-show-directories)


;;;; for Info-mode, with this method we can't bind "SPC"
;; (with-eval-after-load 'info
;;   (define-key Info-mode-map (kbd "M-SPC") leader-ala-vim-map))

;; So, as we want to use SPC (next-page is also bound to C-f), we use
;; 'local of general-def :keymaps keyword, evil-local-set-key is used
;; by general in this case. The binding is local to the *buffer* not for every
;; buffers with the same mode. It's why Info-mode-hook is used.

;; Priorities of map :
;; general-override-mode-map > general-override-local-mode-map > global-map
(defun leader-ala-vim-info-mode ()
  (general-def
    :keymaps 'local
    :states '(normal motion visual operator emacs)
    "SPC" leader-ala-vim-map))
(add-hook 'Info-mode-hook #'leader-ala-vim-info-mode)

;;;; global auto-revert-mode borrows from spacemacs
;; Auto refresh
(global-auto-revert-mode 1)
;; ;; Also auto refresh dired, but be quiet about it
;; (setq global-auto-revert-non-file-buffers t
;;       auto-revert-verbose nil)
;; (push 'Buffer-menu-mode global-auto-revert-ignore-modes)

;;;; diminish some minor modes
(diminish 'auto-revert-mode)
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)

;;;; No disable commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;;(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(provide 'general-interface)
;;; general-interface.el ends here
