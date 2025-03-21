;;; general-interface.el --- loads and configures packages for general interface. -*- lexical-binding: t; -*-
;;; Commentary:
;;  use-package is used when apropriate, else set load-path
;;  auto-mode-alist, autoloads and hooks using the bare emacs way.

;;; Code:

(setq use-package-enable-imenu-support t)

(eval-when-compile
  (require 'use-package))
;; (require 'use-package)
;; To add a :ensure for each use-package
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(require 'cl-lib)

(defun my/add-hooks (mode-hook &rest hooks)
  "Add hooks in the list `hooks' to `mode-hook'"
  (dolist (hook hooks)
    (add-hook mode-hook hook)))

(defun my/add-hook-multi (hook &rest mode-hooks)
  "Add a `hook' for multiple `mode-hooks'"
  (dolist (mode mode-hooks)
    (add-hook mode hook)))

;;;; general to bind keys in a convenient way.
(use-package general)
;; (unless (package-installed-p 'general)
;;   (package-install 'general))
;; (require 'general)
;;(general-evil-setup)
;;(general-auto-unbind-keys)
(general-create-definer prefix-c-xw :prefix "C-x w")
(general-create-definer prefix-c-xt :prefix "C-x t")
(general-unbind :states '(normal viusal motion operator) "SPC")
(general-create-definer leader-ala-vim
  :states '(normal visual motion operator insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  ;;:prefix-command 'leader-ala-vim-command
  :prefix-map 'leader-ala-vim-map)

(prefix-c-xw
  "f" #'find-file-at-point ;; gf
  "h" #'hexl-find-file
   ;; "i" #'insert-file   ;; :r also `C-x i'
  "w" #'write-region      ;; visual, then :w ; also from embark `W'
  "v" #'view-file)

(general-unbind ctl-x-map
  "t"   ; unbind all "C-x t" bindings (functions for using emacs tab).
  "r g" ; insert-regsiter also bound to C-x r i
  "r x" ; copy-register also bound to C-x r s
  )
(general-unbind help-map
  "RET"
  "h"
  "g"
  "n"
  "C-a"
  "C-c"
  "C-d"
  "C-e"
  "C-f"
  "C-o"
  "C-t"
  "C-w"
  "C-\\")
(prefix-c-xt
  "a" #'font-lock-fontify-buffer
  "b" #'font-lock-fontify-block
  "c" #'comment-dwim      ;; SPC cr
  "e" #'recentf-edit-list
  "i" #'indent-region
  "l" #'font-lock-mode
  "m" #'man-follow        ;; But evil-lookup bound to K is better
  "p" #'pop-tag-mark)

(leader-ala-vim
  ""      '(nil :wk "leader-ala-vim menu")
  "SPC"   #'execute-extended-command
  "M-SPC" #'cycle-spacing
  ":"     #'eval-expression
  "*"     #'duplicate-dwim
  "g"     '(:ignore t :wk "Searching")
  "g R"    #'rgrep
  "q"     '(:ignore t :wk "Quitting")
  "q q"    #'save-buffers-kill-terminal
  "q r"    #'restart-emacs
  "t"     '(:ingore t :wk "Toggling")
  "t s"    #'flyspell-mode
  "t w"    #'whitespace-mode
  "x"     '(:ignore t :wk "Xref")
  "x d"    #'xref-find-definitions
  "x r"    #'xref-find-references)

(general-def
  "<cancel>"             #'keyboard-quit
  "<XF86Calculator>"     #'calc
  "M-RET"                #'hippie-expand
  "<f9>"                 #'compile
  "<f11>"                #'previous-error
  "<f12>"                #'next-error
  "M-s m"                #'multi-occur
  "C-x r e"              #'edit-bookmarks
  "C-x j"                #'duplicate-dwim
  "C-c l"                #'dictionary-search-dwim
  [remap eval-last-sexp] #'pp-eval-last-sexp)

;;;; Global settings
;; Tramp Var Directory
(defvar-local my/tvd
  (my/put-this-in-var "tramp"))

(setq save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t
      select-enable-clipboard nil
      ;; select-enable-primary t
      ;; mouse-drag-copy-region t
      comint-scroll-show-maximum-output t
      comint-scroll-to-bottom-on-input t
      compilation-scroll-output 'first-error
      compilation-auto-jump-to-first-error 'first-known
      scroll-step 1
      scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      ediff-merge-split-window-function 'split-window-horizontally
      ediff-split-window-function 'split-window-horizontally
      focus-follows-mouse nil
      sentence-end-double-space nil
      bidi-inhibit-bpa t
      confirm-kill-processes nil
      kill-buffer-query-functions
                   (remq 'process-kill-buffer-query-function
                          kill-buffer-query-functions)
      confirm-nonexistent-file-or-buffer nil
      history-delete-duplicates t
      native-comp-async-report-warnings-errors 'silent
      initial-scratch-message nil
      ring-bell-function 'ignore
      visible-bell nil
      split-width-threshold 140
      truncate-string-ellipsis "…"
      use-short-answers t
      enable-recursive-minibuffers t
      history-length 100
      recentf-menu-filter 'recentf-sort-basenames-ascending
      require-final-newline t
      executable-prefix-env t
      dired-ls-F-marks-symlinks t
      dired-dwim-target t
      dired-kill-when-opening-new-dired-buffer t
      dired-listing-switches "-alhF --group-directories-first"
      dired-switches-in-mode-line 'as-is
      dired-guess-shell-alist-user '(("\\.pdf\\'" "zathura")
                                     ("\\.tex\\'" "pdflatex")
                                     ("\\.lisp\\'" "sbcl --script")
                                     ("\\.ods\\'\\|\\.xlsx?\\'\\|\\.docx?\\'\\|\\.csv\\'"
                                      "libreoffice"))
      next-error-message-highlight t
      help-enable-symbol-autoload t
      describe-bindings-outline t
      completions-detailed t
      view-read-only t
      nobreak-char-display t
      nobreak-char-ascii-display nil
      apropos-do-all t
      idlwave-config-directory (my/put-this-in-var "idlwave")
      eww-download-directory "~/download/"
      url-configuration-directory (my/put-this-in-var "url")
      url-cookie-file (my/put-this-in-var "url/cookie")
      treesit-extra-load-path `(,(my/put-this-in-var "tree-sitter"))
      calendar-week-start-day 1
      calendar-latitude 45.1877777778 ; for M-x sunrise-sunset
      calendar-longitude 5.72694444445
      calendar-location-name "Grenoble, France"
      org-directory "~/org"
      org-agenda-files (file-expand-wildcards org-directory)
      ;; time-stamp-active t
      ;; time-stamp-line-limit 10
      ;; time-stamp-format "%Y-%02m-%02d %02H:%02M:%02S (%u)"
;;;; ibuffer
      ibuffer-expert t
      ibuffer-default-sorting-mode 'major-mode
;;;; isearch
      search-exit-option 'edit
      isearch-allow-scroll t
      isearch-lazy-highlight t
      isearch-lazy-count t
;;;; desktop variables
      desktop-base-file-name "emacs-desktop.el"
      desktop-base-lock-name "emacs-desktop.lock"
      desktop-dirname (my/put-this-in-var "")
      desktop-path `("." ,(my/put-this-in-var ""))
;;;; Tramp
      tramp-default-method "ssh"
      tramp-auto-save-directory (expand-file-name "autosave" my/tvd)
      tramp-backup-directory-alist `(("." . ,(expand-file-name "backup" my/tvd)))
      tramp-persistency-file-name (expand-file-name "connection-history" my/tvd)
      auto-save-list-file-prefix (my/put-this-in-var "auto-save-list/.saves-")
      project-list-file (my/put-this-in-var "project")
      eshell-directory-name (my/put-this-in-var "eshell")
      request-storage-directory (my/put-this-in-var "request")
      shared-game-score-directory (my/put-this-in-var "games")
      warning-minimum-level :error
      warning-suppress-log-types '((comp))
      warning-suppress-types '((use-package))
      vc-make-backup-files t)


(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
;; (add-hook 'before-save-hook #'time-stamp)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(save-place-mode t)
(minibuffer-depth-indicate-mode)
(tooltip-mode -1)

;; TODO a function for read-only-mode-hook.
;; Since I put this mode in view-mode and I set view-mode to emacs state.
;; when I quit read-only-mode the buffer stays in emacs-state…
;; What I would like, is to restore the previous state of the buffer…
;; or perhaps decide what state apply based on the major mode.
;; Also fix the case for buffer already in read-only-mode when emacs
;; is started. For now that doesn't work. These buffers remain in
;; evil-normal-state.
(setq-default tab-width 4
              indent-tabs-mode nil
              tab-always-indent 'complete
              tab-first-completion 'word-or-paren-or-punct
              bidi-paragraph-direction 'left-to-right
              indicate-empty-lines t
              fill-column 72)

;; (set-input-meta-mode 'encoded) ; for terminal

;; Add bindings for the find-library command
(find-function-setup-keys)

(require 'hl-line)
(my/add-hook-multi #'hl-line-mode 'prog-mode-hook 'text-mode-hook)
(leader-ala-vim "t H" #'hl-line-mode)
(global-so-long-mode)
(delete-selection-mode t)
(column-number-mode t)

(defun my/ibuffer-delete-M-o()
  (general-unbind ibuffer-mode-map "M-o"))
(my/add-hooks 'ibuffer-mode-hook
              #'my/ibuffer-delete-M-o
              #'ibuffer-auto-mode)

;;;; better dired mode
(autoload #'dired-omit-mode "dired-x")
(with-eval-after-load 'dired
  (setq dired-x-hands-off-my-keys nil)
  (require 'dired-x))
(defun my/set-dired-omit-mode()
  (dired-omit-mode 1))

(add-hook 'dired-mode-hook #'my/set-dired-omit-mode)
;; (setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode)
;;                                    auto-mode-alist))
(push '("[^/]\\.dired$" . dired-virtual-mode) auto-mode-alist)

;; To show only directories in dired
(fset 'dired-only-show-directories
      "*/tk")

(general-def
 :keymaps 'dired-mode-map
 ;;"^" #'dired-up-directory-same-buffer
 "C-x C-k D" #'dired-only-show-directories)

;;;; for Info-mode, with this method we can't bind "SPC"
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
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(push 'Buffer-menu-mode global-auto-revert-ignore-modes)

(push `("draft/neomutt-" . ,#'mail-mode) auto-mode-alist)
(add-hook 'mail-mode-hook #'flyspell-mode)

;; Switch nicely to an eshell buffer in the default-directory of the
;; current buffer.
;; From: https://www.blogbyben.com/2013/08/a-tiny-eshell-add-on-jump-to-shell.html
;; Inspired by: http://www.emacswiki.org/emacs/EshellControlFromOtherBuffer
;; XXX That needs some improvements to work as I wish.
;; (defun my/eshell-switch-to-and-change-dir ()
;;   "Switch to eshell and make sure we're in the directory the current buffer is in."
;;   (interactive)
;;   (let ((dir default-directory))
;;     (let ((b (if (boundp 'eshell-buffer-name)
;;                  (get-buffer  eshell-buffer-name))))
;;       (unless b
;;         (eshell)))
;;     (display-buffer eshell-buffer-name t)
;;     (switch-to-buffer-other-window eshell-buffer-name)
;;     (end-of-buffer)
;;     (unless (equal dir default-directory)
;;       (cd dir)
;;       (eshell-send-input)
;;       (end-of-buffer))))
(defun my/eshell-switch-to-and-change-dir ()
  "Switch to eshell and make sure we're in the directory the current buffer is in."
  (interactive)
  (let ((dir default-directory))
    (if (boundp 'eshell-buffer-name)
        (progn
          (display-buffer eshell-buffer-name t)
          (switch-to-buffer-other-window eshell-buffer-name)
          (end-of-buffer))
      (eshell))
    (unless (equal dir default-directory)
      (cd dir)
      (eshell-send-input)
      (end-of-buffer))))

(general-def "C-c e" #'my/eshell-switch-to-and-change-dir)

;;;; Transient settings
(defvar-local transient-directory-cache
  (my/put-this-in-var "transient"))
(apply #'custom-set-variables
       (mapcar
        (lambda(it)
          (list (car it)
                (expand-file-name (cdr it) transient-directory-cache)))
        '((transient-levels-file . "levels.el")
          (transient-values-file . "values.el")
          (transient-history-file . "history.el"))))

(use-package diminish
  :commands (diminish))

(use-package dimmer
  :hook (after-init . dimmer-mode)
  :custom (dimmer-fraction 0.16)
          (dimmer-adjustment-mode :foreground))

(use-package auto-compile
  :diminish (auto-compile-mode
             auto-compile-on-load-mode
             auto-compile-on-save-mode)
  :init
  (my/add-hooks 'emacs-lisp-mode-hook
    #'auto-compile-on-load-mode
    #'auto-compile-on-save-mode))

(defvar my/mode-in-emacs-state
  '(calculator-mode
    calendar-mode
    diff-mode
    dired-mode
    finder-mode
    shortdoc-mode
    ;;view-mode ; doesn't work for this because it's a minor mode
    )
  "List of mode that we want to be in initial emacs-state.
To use it: (push 'a-mode my/mode-in-emacs-state)")

(defun my/set-mode-in-emacs-state ()
  (dolist (mode my/mode-in-emacs-state)
    (evil-set-initial-state mode 'emacs)))

(defun evil-ex-search-next-auto-clear-highlights ()
  (interactive)
  (evil-ex-search-next)
  (run-with-idle-timer 1 nil #'evil-ex-nohighlight))
(defun evil-ex-search-previous-auto-clear-highlights ()
  (interactive)
  (evil-ex-search-previous)
  (run-with-idle-timer 1 nil #'evil-ex-nohighlight))

;;(setq evil-want-keybinding nil)
(use-package evil
  :hook ((after-init . evil-mode)
         (view-mode . evil-emacs-state))
  :custom (evil-ex-search-highlight-all t)
          (evil-ex-search-persistent-highlight nil)
          (evil-ex-search-case 'smart)
          ;(evil-ex-visual-char-range t)
          ;(evil-want-C-u-scroll t)
          (evil-search-module 'evil-search)
          (evil-want-Y-yank-to-eol nil)
          (evil-want-fine-undo t)
          (evil-want-C-i-jump nil)
          (evil-kbd-macro-suppress-motion-error t)
  :diminish (evil-mode)
  :config
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
     "Q"     #'evil-fill-and-move
     "C-w e" #'find-file-other-window)
    (general-unbind evil-window-map
      "C-h"    ; use by which-key
      ;; "gt"  ; bindings to emacs tab functions
      ;; "gT"
      "g"))    ; remove the prefix is sufficient

(use-package evil-numbers)

(use-package evil-quickscope
  :hook (after-init . global-evil-quickscope-mode)
  :diminish (evil-quickscope-mode))

(use-package evil-lion
  :hook (after-init . evil-lion-mode))

(use-package evil-surround
  :hook (after-init . global-evil-surround-mode)
  :diminish (evil-surround-mode))

(use-package embrace
  :defer t
  :hook (org-mode-hook . embrace-org-mode-hook)
  :init (general-def "C-,"  #'embrace-commander)
        (leader-ala-vim "," #'embrace-commander)
  :config (require 'evil-embrace))

(use-package evil-embrace
  :defer t
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
  :hook (after-init . global-evil-matchit-mode))

(use-package evil-nerd-commenter
  :init
  (general-def "M-;" #'evilnc-comment-or-uncomment-lines)
  (leader-ala-vim
    ";"  #'evilnc-comment-operator
    "c" '(:ignore t :wk "Comments")
    "c c" #'evilnc-copy-and-comment-lines
    "c d" #'comment-dwim
    "c i" #'evilnc-comment-or-uncomment-lines
    "c l" #'evilnc-quick-comment-or-uncomment-to-the-line
    "c p" #'evilnc-comment-or-uncomment-paragraphs
    "c r" #'comment-or-uncomment-region
    "c v" #'evilnc-toggle-invert-comment-line-by-line))

(use-package nocomments-mode
  :defer t
  :init (leader-ala-vim "c n" #'nocomments-mode))

(use-package evil-visualstar
  :hook (after-init . global-evil-visualstar-mode)
  :custom (evil-visualstar/persistent t))

(use-package evil-org
  :hook (org-mode . evil-org-mode))

(use-package which-key
  :custom (which-key-sort-order 'which-key-key-order-alpha)
          (which-key-idle-delay 0.6)
  :diminish (which-key-mode)
  :hook (after-init . which-key-mode)
  :init (general-unbind help-map "C-h")
        (general-unbind esc-map "C-h")
  :config (which-key-add-key-based-replacements
            "C-x r" "Reg+Rect+Bmk"
            "C-x n" "Narrowing"
            "C-x a" "Abbrevs"
            "C-x RET" "Coding Syst."
            "C-x 8" "Insert UTF8"
            "C-x 4" "Other window"
            "C-x 5" "Other frame"
            "C-x p" "Project"
            "C-x x" "Buffer various"
            "C-x X" "Edebug"
            "C-x C-a" "Edebug"
            "M-s" "Searching"
            "M-s h" "Highlight"
            "M-g" "Goto…"))

(use-package goto-chg
  :defer t
  :general ("M-g M-g"  #'goto-last-change)
           ("M-g M-h"  #'goto-last-change-reverse))

(use-package expand-region
  :defer t
  :init (general-def "C-="  #'er/expand-region)
        (leader-ala-vim "$" #'er/expand-region))

(use-package hydra
  :commands (defhydra))

;; Addon to avy from:
;; https://karthinks.com/software/avy-can-do-anything/

(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)


(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)


(defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)


(use-package avy
  :init (general-def "M-j" #'avy-goto-char-timer)
  :custom (avy-timeout-seconds 1.0)
  :config (avy-setup-default)

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line))

;; Useless because the minor is not activated, as well that changes
;; only fFtT operators. I setup some bindings with my leader-ala-vim
(use-package evil-avy
  :defer t
  :init
    (leader-ala-vim
      "a"   '(:ignore t :wk "Avy")
      "a a"  #'evil-avy-mode
      ;;"a c"  #'avy-goto-char
      "a c"  #'avy-goto-char-2
      "a j"  #'avy-goto-char-timer
      "a l"  #'avy-goto-line
      "a r"  #'avy-resume
      ;;"a s"  #'avy-goto-subword-1
      ;;"a 0"  #'avy-goto-word-0
      "a w"  #'avy-goto-word-1))
(use-package casual-avy
  :defer t
  :init
  (leader-ala-vim
    "a m" #'casual-avy-tmenu))

(use-package casual-suite)
(use-package ace-window
  :defer t
  :init (general-def "M-o" #'ace-window)
        (leader-ala-vim "a o" #'ace-window))

(use-package ace-link
  :defer t
  :init (ace-link-setup-default)
        (leader-ala-vim "a L" #'ace-link))

;; avy-isearch is not compatible with ctrlf because they don't use
;; the same variable. TODO: write a command avy-ctrlf draw from
;; avy-isearch. Finally, isearch is better imho.
;; (use-package ctrlf
;;   :custom (ctrlf-default-search-style 'fuzzy)
;;           (ctrlf-alternate-search-style 'fuzzy-regexp)
;;   ;;:init (general-def ctrlf-minibuffer-mode-map "M-j" #'avy-isearch) ; doesn't work
;;   :config (ctrlf-mode))

;; vertico + consult + embark + marginalia + orderless + prescient…
;; Initial configuration comes from: https://blog.sumtypeofway.com/posts/emacs-config.html
(use-package vertico
  :hook (after-init . vertico-mode)
  :custom (vertico-count 15)
          (vertico-resize nil)
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

  :custom (completion-in-region-function #'consult-completion-in-region)
          (xref-show-xrefs-function #'consult-xref)
          (xref-show-definitions-function #'consult-xref)
          (consult-project-root-function #'deadgrep--project-root) ;; ensure ripgrep works
          (consult-preview-key '(:debounce 1 any))

  :init (general-def
          "C-x b"    #'consult-buffer
          "C-x 4 b"  #'consult-buffer-other-window
          "C-x r l"  #'consult-bookmark
          "C-x C-f"  #'find-file
          "C-c m"    #'consult-imenu
          "M-y"      #'consult-yank-pop
          "M-s M-i"  #'consult-info
          [remap repeat-complex-command] #'consult-complex-command)
        (general-def
          :states  'normal
          :keymaps 'global
          "C-w b"  #'consult-buffer-other-window)
        (prefix-c-xt    "r"  #'consult-recent-file)
        (leader-ala-vim "/"  #'consult-line
                        "g c" #'consult-ripgrep)
        (setq xref-show-xrefs-function #'consult-xref
              xref-show-definitions-function #'consult-xref)
        (setq register-preview-delay 0.5
              register-preview-function #'consult-register-format)
  :config (setq consult-narrow-key "C-+")
          (setq consult-project-function #'(lambda (_) (locate-dominating-file "." ".git"))))

;; ;; Use `consult-completion-in-region' if Vertico is enabled.
;; ;; Otherwise use the default `completion--in-region' function.
;; (setq completion-in-region-function
;;       (lambda (&rest args)
;;         (apply (if vertico-mode
;;                    #'consult-completion-in-region
;;                  #'completion--in-region)
;;                args)))

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
                     "M-m"  #'marginalia-cycle))

(use-package orderless
  :init (setq completion-styles '(substring orderless basic)
              completion-category-defaults nil
              completion-category-overrides '((files (style partial-completion)))))

(use-package prescient
  :hook (after-init . prescient-persist-mode))

(use-package vertico-prescient
  :hook (vertico-mode . vertico-prescient-mode))

(use-package affe
  :defer t
  :init ;; use orderless as the affe regexp compiler
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  (leader-ala-vim
    "g f" #'affe-find
    "g G" #'affe-grep)
  :custom (affe-count 30)
  :config ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key "M-."))

(use-package consult-dir
  :defer t
  :init (general-def "C-x C-d"  #'consult-dir)
        (general-def :keymaps 'vertico-map
          "C-x C-d"  #'consult-dir
          "C-x C-j"  #'consult-dir-jump-file))

;; Add actions to use ace-window from embark.
;; From: https://karthinks.com/software/fifteen-ways-to-use-embark/
(eval-when-compile
  (defmacro my/embark-ace-action (fn)
    `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

(use-package embark
  :defer t
  :custom (embark-help-key "?")
  :init
  (general-def :keymaps 'minibuffer-mode-map "C-;" #'embark-act)
  (general-def "C-c b"  #'embark-act)
  (leader-ala-vim "RET" #'embark-act)
  (general-def :keymaps 'embark-file-map     "o" (my/embark-ace-action find-file))
  (general-def :keymaps 'embark-buffer-map   "o" (my/embark-ace-action consult-buffer))
  (general-def :keymaps 'embark-bookmark-map "o" (my/embark-ace-action consult-bookmark))
  (general-def :keymaps 'help-map "B" #'embark-bindings))
;; Use which key to show the embark's actions.
;; From: https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
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

(defun su-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting su"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|su:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/su:root@localhost:" file))))
(general-def embark-file-map "C-r" #'su-find-file)

(use-package embark-consult)


(use-package avy-embark-collect
  :commands (avy-embark-collect-act avy-embark-collect-choose))


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
  ;; :init
  ;; (global-corfu-mode))
  :hook (after-init . global-corfu-mode)
  :config (require 'kind-icon)
          (push #'kind-icon-margin-formatter corfu-margin-formatters))

;;  ** need to change the corfu--state-vars to corfu--initial-state in corfu-prescient
(use-package corfu-prescient
  :hook (corfu-mode . corfu-prescient-mode))
  ;; :config (corfu-prescient-mode))

(use-package kind-icon
  :defer t
  :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
          (kind-icon-default-style
           '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.7 :scale 1.0)))
  ;; :config (push #'kind-icon-margin-formatter corfu-margin-formatters))

;; (defun dabbrev-completion-all-buffers ()
;;       "dabbrev-completion in *all* buffers"
;;     (interactive)
;;     (let ((current-prefix-arg '(16))) ; c-u c-u
;;       (call-interactively #'dabbrev-completion)))

;; Since I use cape, I can replace #'dabbrev-completion-all-buffers
;; with #'cape-dabbrev. As well I don't use it…
(use-package dabbrev
  :defer t
  :general
  (;; "M-³"   #'dabbrev-completion-all-buffers
   ;; "s-³"   #'dabbrev-completion-all-buffers
   ;; "M-&"   #'dabbrev-completion-all-buffers
   ;; Swap M-/ and C-M-/
   "M-/"   #'dabbrev-completion
   "C-M-/" #'dabbrev-expand) ;; dabbrev-expand is also provide by C-p in evil-insert-state
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-case-replace nil)
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;;;; Fonctions for completion-at-point-functions hook provide
;;   by the cape package.
;; cape-dabbrev
;; cape-file
;; cape-history
;; cape-keyword
;; cape-symbol
;; cape-abbrev
;; cape-ispell
;; cape-dict
;; cape-line

(defun my/cape-prog-mode ()
  (add-hook 'completion-at-point-functions #'cape-keyword nil 'local))

(defun my/cape-elisp-mode ()
  (add-hook 'completion-at-point-functions #'cape-symbol nil 'local))

(defun my/cape-text-mode ()
  (add-hook 'completion-at-point-functions #'cape-dict nil 'local))

(use-package cape
  :hook ((prog-mode . my/cape-prog-mode)
         (text-mode . my/cape-text-mode)
         (emacs-lisp-mode . my/cape-elisp-mode))

  :general ("M-²"  #'cape-dabbrev
            "s-²"  #'cape-dabbrev)

  :init (my/add-hooks 'completion-at-point-functions
                       #'cape-file
                       #'cape-dabbrev))

(use-package symbol-overlay
  :defer t
  :init (leader-ala-vim
             "j"  '(:ignore t :wk "Symbol overlay")
             "j c" `(,#'symbol-overlay-remove-all :wk "clear overlay")
             "j j" #'symbol-overlay-put))
             ;; "j f" #'symbol-overlay-jump-first
             ;; "j h" #'symbol-overlay-put
             ;; "j l" #'symbol-overlay-jump-last
             ;; "j m" #'symbol-overlay-mode
             ;; "j n" #'symbol-overlay-jump-next
             ;; "j p" #'symbol-overlay-jump-prev
             ;; "j r" `(,#'symbol-overlay-remove-all :wk "clear overlay")))

(use-package casual-symbol-overlay)

(use-package consult-flyspell
  :defer t
  :init (setq consult-flyspell-correct-function (lambda () (flyspell-correct-at-point) (consult-flyspell)))
        (leader-ala-vim "=" #'consult-flyspell))

(use-package flyspell-correct)

(use-package consult-recoll
  :defer t
  :init (leader-ala-vim "g r" #'consult-recoll))

;; Install an advice when setup the doom modeline to try
;; to always have the same information.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom (doom-modeline-minor-modes t)
          (doom-modeline-battery nil)
          (doom-modeline-irc nil)
          (doom-modeline-gnus nil)
          (doom-modeline-unicode-fallback t)
          (doom-modeline-window-width-limit 81)
          (doom-modeline-bar-width 10)
          (doom-modeline-hud t)
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
       'package-menu-mode-hook
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
  ;; Turn around for not truncating the mode-line
  ;;(set-face-attribute 'mode-line nil :font "DejaVu Sans 14")
  (set-face-attribute 'mode-line nil :font "Noto Sans 18")
  ;; (set-face-attribute 'mode-line-active nil :family "Noto Sans" :height 0.8) ;; that make emacs hang!
  ;; (set-face-attribute 'mode-line-inactive nil :family "Noto Sans" :height 0.8)
  (advice-add #'doom-modeline-mode :after #'my/modeline-advice))

(use-package anzu
  :diminish (anzu-mode)
  :hook (after-init . global-anzu-mode))

(use-package evil-anzu
  :defer t
  :init (with-eval-after-load 'evil
          (require 'evil-anzu)))

(use-package all-the-icons
  :diminish (all-the-icons-mode))


(use-package all-the-icons-dired
  :diminish (all-the-icons-dired-mode)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package all-the-icons-completion
  :config (all-the-icons-completion-mode 1)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(use-package iedit
  :defer t
  :general ("C-;"  #'iedit-mode))

(use-package evil-iedit-state)

(use-package evil-multiedit
  :commands (evil-multiedit-ex-match)
  :init (general-def :states 'visual
          "R" #'evil-multiedit-match-all
          "M-d" #'evil-multiedit-match-and-next
          "M-D" #'evil-multiedit-match-and-prev
          "C-M-d" #'evil-multiedit-restore
        (general-def :states '(normal insert)
          "M-d" #'evil-multiedit-match-symbol-and-next
          "M-D" #'evil-multiedit-match-symbol-and-prev)
        (general-def :states 'insert
          "C-M-d" #'evil-multiedit-toggle-marker-here))
        (evil-ex-define-cmd "ie[dit]" #'evil-multiedit-ex-match))

;; multiple-cursors for evil
;; see also https://github.com/fgallina/region-bindings-mode
;; to activate bindings when a region is selected
;; use `gr` prefix in normal mode to access mc functionalities
(use-package evil-mc
  :hook (after-init . global-evil-mc-mode)
  :diminish (evil-mc-mode)
  :init (general-def
          :keymaps 'evil-mc-key-map
          :states '(normal visual)
          "C-t" #'pop-tag-mark ; want to keep pop tag
          "g C-t" #'evil-mc-skip-and-goto-next-match))

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :diminish (undo-tree-mode)
  :custom (evil-undo-system 'undo-tree)
          (undo-tree-enable-undo-in-region t)
          (undo-tree-auto-save-history nil)
          (undo-tree-history-directory-alist
           `(("." . ,(my/put-this-in-var "undo-tree/"))))
  :init (leader-ala-vim "_" #'undo-tree-visualize))

(use-package undo-fu-session
  :commands (undo-fu-session-global-mode)
  :diminish (undo-fu-session)
  :custom (undo-fu-session-directory
           (my/put-this-in-var "undo-fu-session"))
  :init (undo-fu-session-global-mode))

;; Folding. There are several possibilities.
;; Use:
;; - hideshow: M-x hs-minor-mode
;; - origami: M-x origami-mode
;; - vdiff (vim diff): inside a vdiff session
;; - evil-vimish-fold: M-x evil-vimish-fold-mode
;; Folding is also possible in ouline-mode, org-mode and
;; hide-ifdef-mode (these are built in emacs).
;; Hideshow is built in emacs, so it has my preference.
;; the :load-path of use-package doesn't work. I wonder why.
(push (expand-file-name "origami.d/" "~/.emacs.d/elisp") load-path)
(autoload #'origami-mode "origami")
(leader-ala-vim
  "o" '(:ignore t :wk "Origami")
  "o o" #'origami-open-node
  "o O" #'origami-open-node-recursively
  "o s" #'origami-show-node
  "o S" #'origami-show-only-node
  "o c" #'origami-close-node
  "o C" #'origami-close-node-recursively
  "o a" #'origami-open-all-nodes
  "o A" #'origami-close-all-nodes
  "o t" #'origami-toggle-node
  "o T" #'origami-toggle-node-recursively
  "o H" #'origami-toggle-all-nodes
  "o <" #'origami-previous-fold
  "o >" #'origami-next-fold
  "o f" #'origami-forward-fold
  "o F" #'origami-forward-fold-same-level
  "o B" #'origami-backward-fold-same-level
  "o u" #'origami-undo
  "o r" #'origami-redo
  "o R" #'origami-reset)

;; Manual definition of folds ala vim..
(use-package evil-vimish-fold
  :commands (evil-vimish-fold-mode))

;; Choose a folding method
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
    (let ((candidate
           (completing-read
            "Folding method: "
            '(hideshow origami vimish none))))
      `(,candidate)))
  (myfold/set-folding-method (intern fold-method)))

(leader-ala-vim
  "t f" `(,#'myfold/choose-folding-method :wk "Choose folding method"))

;; vimdiff, ediff is perfect but they aren't folding possibilities
;; with it. Vdiff is also very nice but it is buggy.
;; Sometimes, a error occur in a diff session, that makes
;; vdiff unusable. It needs to be restart.
(use-package vdiff
  :defer t
  :init (leader-ala-vim
          "v" '(:ignore t :wk "Vdiff")
          "v v" #'vdiff-hydra/body
          "v f" #'vdiff-files
          "v F" #'vdiff-files3
          "v b" #'vdiff-buffers
          "v B" #'vdiff-buffers3
          "v c" #'vdiff-current-file
          "v m" #'vdiff-merge-conflict))

(use-package openwith
  :hook (after-init . openwith-mode)
  :custom (openwith-confirm-invocation t)
          (openwith-associations
            ;;'(("\\.\\(pdf\\|ps\\|djvu\\)\\'" "zathura" (file))
           '(("\\.\\(mp3\\|flac\\|ogg\\|aac\\)\\'" "mplayer" (file))
             ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mkv\\|mp4\\|webm\\|ogv\\)\\'" "mplayer" ("-idx" file))
             ("\\.\\(od[sgtbfm]\\|st[icwd]\\|sx[gmdiwc]\\|ot[sgtp]\\|docx?\\|rtf\\|xl[sw]\\|pp[ts]\\)\\'" "libreoffice" nil))))

(use-package magit
  :defer t
  :init (general-def "C-c g" #'magit-file-dispatch)
        (leader-ala-vim
          "m" '(:ignore t :wk "Magit")
          "m m" #'magit
          "m d" #'magit-file-dispatch))

(use-package magit-delta
  :commands (magit-delta-mode))

;; works on emacs-state
(use-package git-timemachine
  :defer t
  :init (leader-ala-vim "m t" #'git-timemachine)
        (push 'git-timemachine-mode my/mode-in-emacs-state))

(use-package consult-git-log-grep)

(use-package consult-ls-git
  :defer t
  :init (leader-ala-vim "m f" #'consult-ls-git))

(use-package deadgrep
  :defer t
  :init (leader-ala-vim "g d" #'deadgrep)
        (push 'deadgrep-mode my/mode-in-emacs-state))

(use-package rg
  :init (leader-ala-vim "g g" #'rg-dwim)
  :custom (rg-keymap-prefix ["C-c s"])
          (rg-ignore-case [smart])
          (rg-use-transient-menu t)
  :config (rg-enable-menu))

(use-package wgrep
  :config (general-def
            :keymaps 'wgrep-mode-map
            "C-c C-a" #'wgrep-save-all-buffer))

(use-package visual-regexp
  :defer t
  :init (leader-ala-vim "?" #'vr/query-replace
                        "!" #'vr/replace)
        (general-def "C-c r" #'vr/replace
                     "C-c q" #'vr/query-replace))

(use-package dumb-jump
  :commands (xref-find-definitions
             xref-find-references)
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 90)
  :custom (dumb-jump-prefer-searcher 'rg))

(defun my/helpful-help-bindings ()
  (general-def
    :keymaps 'helpful-mode-map
    :states '(normal emacs)
    "c" #'help-customise
    "q" #'quit-window
    "i" #'help-goto-info
    "s" #'help-view-source))

(use-package helpful
  :defer t
  :hook (helpful-mode . my/helpful-help-bindings)
  :general (:keymaps 'help-map
                     "C" #'helpful-command
                     "f" #'helpful-callable
                     "k" #'helpful-key
                     "o" #'helpful-symbol
                     "v" #'helpful-variable)
  :init
  (leader-ala-vim
    "h"  '(:ignore t :wk "Help")
    "h k" #'helpful-key
    "h f" #'helpful-callable
    "h v" #'helpful-variable
    "h p" #'helpful-at-point
    "h c" #'helpful-command
    "h d" #'shortdoc-display-group
    "h o" #'describe-symbol))


;;;; additionnal actions for avy
(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(defun avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(defun dictionary-search-dwim (&optional arg)
  "Search for definition of word at point. If region is active,
search for contents of region instead. If called with a prefix
argument, query for word to search."
  (interactive "P")
  (if arg
      (dictionary-search nil)
    (if (use-region-p)
        (dictionary-search (buffer-substring-no-properties
                            (region-beginning)
                            (region-end)))
      (if (thing-at-point 'word)
          (dictionary-lookup-definition)
        (dictionary-search-dwim '(4))))))

(defun avy-action-lookup-dictionnary (pt)
  (save-excursion
    (goto-char pt)
    (dictionary-search-dwim))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(with-eval-after-load 'avy
  (setf (alist-get ?. avy-dispatch-alist) #'avy-action-embark)
  (setf (alist-get ?H avy-dispatch-alist) #'avy-action-helpful)
  (setf (alist-get ?= avy-dispatch-alist) #'avy-action-lookup-dictionnary))

(use-package paren
  :config (show-paren-mode)
  :custom (show-paren-style 'parenthesis))

(use-package treemacs-all-the-icons
  :defer t)
(use-package treemacs-evil
  :defer t)
(use-package treemacs-magit
  :defer t)
(use-package treemacs
  :commands (treemacs)
  :custom (treemacs-width 40)
          (treemacs-indentation 1)
          (treemacs-persist-file (my/put-this-in-var "treemacs-persist"))
  :init (prefix-c-xt "t" #'treemacs)
  :config (require 'treemacs-all-the-icons)
          (require 'treemacs-evil)
          (require 'treemacs-magit))

(use-package fill-column-indicator
  :defer t
  ;;:commands (fci-mode)     ;; managed by general.el
  :init (prefix-c-xt "C-f" #'fci-mode))

(use-package hl-todo
  :diminish (hl-todo-mode)
  :hook ((prog-mode) . hl-todo-mode))

(use-package svg-lib
  :custom (svg-lib-icons-dir (my/put-this-in-var "svg-lib")))

(use-package pdf-tools
  :defer t)

(use-package vterm
  :defer t
  :init (setq vterm-always-compile-module t)
        (general-def "C-c v" #'vterm)
        (push 'vterm-mode my/mode-in-emacs-state))

(use-package eshell-vterm
  :hook (eshell-mode . eshell-vterm-mode))

(use-package exec-path-from-shell)
(use-package pkg-info
  :defer t)

;;;; diminish some minor modes
(diminish 'auto-revert-mode)
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)

;;;; No disable commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
;;(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(provide 'general-interface)
;;; general-interface.el ends here
