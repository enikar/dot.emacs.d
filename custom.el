(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-fontify (eq window-system 'x))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auth-source-save-behavior nil)
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(cursor-type 'box)
 '(custom-enabled-themes '(tsdh-modified))
 '(custom-safe-themes
   '("bad41d0c4e81991a29e6eae5a7f6c2900a81023ee762d34fae6050ac5c57c76b" default))
 '(dabbrev-case-replace nil)
 '(dired-dwim-target t)
 '(dired-listing-switches "-alhF --group-directories-first")
 '(dired-ls-F-marks-symlinks t)
 '(dired-mode-hook
   '(dired-extra-startup all-the-icons-dired-mode doom-modeline-set-my/project-modeline evil-emacs-state))
 '(display-fill-column-indicator-character 124)
 '(ediff-merge-split-window-function 'split-window-horizontally)
 '(ediff-split-window-function 'split-window-horizontally)
 '(fill-column 78)
 '(focus-follows-mouse nil)
 '(fringe-mode nil nil (fringe))
 '(geiser-guile-manual-lookup-other-window t nil nil "Customized with use-package geiser")
 '(help-window-select t)
 '(highlight-parentheses-colors
   '("SpringGreen3" "Orange" "IndianRed1" "IndianRed3" "IndianRed4") nil nil "Customized with use-package highlight-parentheses")
 '(highlight-parentheses-delay 0.2 nil nil "Customized with use-package highlight-parentheses")
 '(history-delete-duplicates t)
 '(ibuffer-default-sorting-mode 'major-mode)
 '(ibuffer-expert t)
 '(ibuffer-mode-hook '(ibuffer-auto-mode))
 '(indicate-buffer-boundaries 'right)
 '(load-prefer-newer t)
 '(make-backup-files t)
 '(package-selected-packages
   '(fringe-helper typescript-mode jq-format symbol-overlay hlint-refactor restart-emacs vdiff evil-vimish-fold evil-mc evil-mc-extras treemacs-all-the-icons general wgrep wgrep-ag package crystal-mode realgud-byebug realgud-pry robe yard-mode consult-flycheck consult-flyspell inf-ruby evil-ruby-text-objects ruby-end yari consult-dir affe consult consult-ag consult-ls-git consult-recoll consult-yasnippet embark-consult vertico all-the-icons-completion py-snippets evil-org kind-icon cape corfu deadgrep duplicate-thing embark popper ctrlf marginalia orderless prescient vterm dimmer smalltalk-mode geiser-guile geiser-racket flycheck-raku nim-mode racket-mode libgit magit-libgit erlang systemd lua-mode fzf latex-extra latex-math-preview latex-preview-pane latexdiff nocomments-mode json-mode evil-visualstar ripgrep emojify evil-nerd-commenter evil-string-inflection attrap cmake-mode dune ocp-indent utop hasky-extensions hasky-stack evil-lion flycheck-guile ace-link ag all-the-icons all-the-icons-dired all-the-icons-ibuffer anzu auctex auto-compile avy cider cider-hydra clojure-mode clojure-mode-extra-font-locking clojure-snippets dante diminish doom-modeline evil evil-anzu evil-avy evil-embrace evil-goggles evil-iedit-state evil-matchit evil-multiedit evil-numbers evil-quickscope evil-surround evil-tutor eyebrowse fill-column-indicator flycheck flycheck-ocaml flycheck-yamllint geiser gitconfig-mode go-dlv go-eldoc go-gopath go-imenu go-imports go-mode go-snippets haskell-mode haskell-snippets helpful highlight-indent-guides highlight-numbers highlight-parentheses hydra iedit importmagic magit markdown-mode merlin merlin-eldoc openwith paradox persistent-scratch rainbow-delimiters rainbow-identifiers raku-mode retrie rtags rustic slime transient treemacs treemacs-evil treemacs-magit tuareg undo-tree use-package w3m which-key with-editor xcscope yaml-mode yasnippet yasnippet-snippets))
 '(prog-mode-hook '(abbrev-mode))
 '(recentf-menu-filter 'recentf-sort-basenames-ascending)
 '(ruby-insert-encoding-magic-comment nil)
 '(save-place-mode t)
 '(show-paren-mode t)
 '(tramp-default-method "ssh")
 '(unibyte-display-via-language-environment t)
 '(vc-make-backup-files t)
 '(warning-minimum-level :error)
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "gray17" :foreground "white smoke"))))
 '(merlin-type-face ((t (:inherit caml-types-expr-face :background "MistyRose4")))))
