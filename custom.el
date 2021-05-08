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
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(cursor-type 'box)
 '(custom-enabled-themes '(tsdh-modified))
 '(custom-safe-themes
   '("477f3e50d768e2191554f82d611fa1d82ba4b3691ed6e14e3d84a20072b91edc" default))
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
 '(help-window-select t)
 '(highlight-parentheses-colors
   '("SpringGreen3" "Orange" "IndianRed1" "IndianRed3" "IndianRed4") nil nil "Customized with use-package highlight-parentheses")
 '(highlight-parentheses-delay 0.2 nil nil "Customized with use-package highlight-parentheses")
 '(history-delete-duplicates t)
 '(ibuffer-default-sorting-mode 'major-mode)
 '(ibuffer-expert t)
 '(ibuffer-mode-hook '(ibuffer-auto-mode))
 '(load-prefer-newer t)
 '(make-backup-files t)
 '(package-selected-packages
   '(alchemist flycheck-elixir geiser-guile geiser-racket vterm hl-todo yari flycheck-raku nim-mode racket-mode libgit magit-libgit realgud-byebug realgud-pry elixir-mode erlang systemd lua-mode fzf latex-extra latex-math-preview latex-preview-pane latexdiff nocomments-mode ivy-historian json-mode evil-visualstar pt ripgrep ivy-avy emojify evil-nerd-commenter evil-string-inflection attrap cmake-mode dune ocp-indent utop flx hasky-extensions hasky-stack evil-lion flycheck-guile ace-link ag all-the-icons all-the-icons-dired all-the-icons-ibuffer all-the-icons-ivy-rich anzu auctex auto-compile avy avy-flycheck cider cider-hydra clojure-mode clojure-mode-extra-font-locking clojure-snippets company-anaconda company-auctex company-cabal company-fuzzy company-ghci company-go company-inf-ruby company-jedi company-plsense company-rtags company-shell counsel counsel-etags counsel-gtags counsel-org-capture-string counsel-org-clock counsel-projectile counsel-tramp dante diminish doom-modeline dumb-jump elpy evil evil-anzu evil-args evil-avy evil-better-visual-line evil-embrace evil-goggles evil-iedit-state evil-leader evil-magit evil-matchit evil-mc evil-mc-extras evil-multiedit evil-numbers evil-org evil-quickscope evil-ruby-text-objects evil-smartparens evil-surround evil-tutor eyebrowse fill-column-indicator flycheck flycheck-ocaml flycheck-yamllint flyspell-correct-ivy geiser gitconfig-mode go-dlv go-eldoc go-gopath go-imenu go-imports go-mode go-projectile go-snippets haskell-mode haskell-snippets helpful highlight-indent-guides highlight-numbers highlight-parentheses hydra ibuffer-projectile iedit importmagic inf-ruby ivy-explorer ivy-hydra ivy-rich ivy-yasnippet ix magit markdown-mode merlin merlin-eldoc multiple-cursors openwith paradox persistent-scratch persp-mode persp-mode-projectile-bridge python-docstring rainbow-delimiters rainbow-identifiers rainbow-mode raku-mode retrie robe rspec-mode rtags rubocop ruby-end rustic semi session slime slime-company smartparens swiper transient treemacs treemacs-evil treemacs-magit treemacs-persp treemacs-projectile tuareg undo-tree use-package w3m which-key with-editor xcscope yaml-mode yard-mode yasnippet yasnippet-snippets))
 '(prog-mode-hook '(abbrev-mode))
 '(python-flymake-command '("pyflakes3"))
 '(recentf-menu-filter 'recentf-sort-basenames-ascending)
 '(ruby-insert-encoding-magic-comment nil)
 '(save-place-mode t)
 '(session-globals-include
   '((kill-ring 20 t)
     (session-file-alist 50 t)
     (file-name-history 50 t)
     search-ring regexp-search-ring))
 '(session-globals-max-size 200)
 '(session-use-package t nil (session))
 '(show-paren-mode t)
 '(tramp-default-method "ssh")
 '(unibyte-display-via-language-environment t)
 '(vc-make-backup-files t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(merlin-type-face ((t (:inherit caml-types-expr-face :background "MistyRose4"))))
 '(which-func ((t (:foreground "gold")))))
