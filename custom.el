(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-fontify (display-graphic-p))
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
   '("2a154c0238fb9d965e5ecfd90dfbe942c6c07c8053d84c6536bf73bada2c587b" default))
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
 '(ibuffer-default-sorting-mode 'major-mode)
 '(ibuffer-expert t)
 '(ibuffer-mode-hook '(ibuffer-auto-mode) t)
 '(indicate-buffer-boundaries 'right)
 '(load-prefer-newer t)
 '(make-backup-files t)
 '(package-selected-packages
   '(slime-company git-timemachine eshell-vterm magit magit-delta anaconda-mode attrap dash-functional deadgrep doom-modeline elisp-refs exec-path-from-shell expand-region flyspell-correct helpful hl-todo json-reformat utop affe avy-menu cape consult-flycheck ctrlf disaster sml-modeline dabbrev transient-dwim corfu-prescient vertico-prescient gitconfig yasnippet-snippets visual-regexp avy-embark-collect dumb-jump fringe-helper typescript-mode jq-format symbol-overlay hlint-refactor restart-emacs vdiff evil-vimish-fold evil-mc treemacs-all-the-icons general wgrep wgrep-ag package crystal-mode realgud-byebug realgud-pry robe yard-mode consult-flyspell inf-ruby evil-ruby-text-objects ruby-end yari consult-dir consult consult-ag consult-ls-git consult-recoll consult-yasnippet embark-consult vertico all-the-icons-completion py-snippets evil-org kind-icon corfu duplicate-thing marginalia orderless prescient vterm dimmer smalltalk-mode geiser-guile geiser-racket flycheck-raku nim-mode racket-mode systemd lua-mode fzf latex-extra latex-math-preview latex-preview-pane latexdiff nocomments-mode json-mode evil-visualstar ripgrep emojify evil-nerd-commenter evil-string-inflection cmake-mode dune ocp-indent evil-lion flycheck-guile ace-link ag all-the-icons all-the-icons-dired all-the-icons-ibuffer anzu auctex auto-compile avy dante diminish evil evil-anzu evil-avy evil-embrace evil-goggles evil-iedit-state evil-matchit evil-multiedit evil-numbers evil-quickscope evil-surround eyebrowse fill-column-indicator flycheck flycheck-ocaml flycheck-yamllint geiser haskell-mode haskell-snippets highlight-indent-guides highlight-numbers highlight-parentheses hydra iedit merlin merlin-eldoc openwith persistent-scratch rainbow-delimiters rainbow-identifiers raku-mode rustic slime transient treemacs-evil tuareg undo-tree use-package which-key with-editor xcscope yaml-mode))
 '(prog-mode-hook '(abbrev-mode))
 '(recentf-menu-filter 'recentf-sort-basenames-ascending)
 '(save-place-mode t)
 '(show-paren-mode t)
 '(transient-history-file "/home/enikar/.emacs.d/var/transient/history.el")
 '(transient-levels-file "/home/enikar/.emacs.d/var/transient/levels.el")
 '(transient-values-file "/home/enikar/.emacs.d/var/transient/values.el")
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
 '(merlin-type-face ((t (:inherit caml-types-expr-face :background "MistyRose4"))) t))
