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
 '(company-fuzzy-sorting-backend 'flx)
 '(counsel-find-file-at-point t)
 '(counsel-find-file-ignore-regexp "\\.bak$\\|\\.elc$\\|~$")
 '(cursor-type 'box)
 '(custom-enabled-themes '(tsdh-modified))
 '(custom-safe-themes
   '("b8c8cf83f0d27368ac0c10071e8292ff68f4d9d9bfaafa02f19806ad5e3f10dc" default))
 '(dante-load-flags
   '("+c" "-Wall" "-ferror-spans" "-fdefer-typed-holes" "-fdefer-type-errors" "-Wwarn=missing-home-modules" "-fno-diagnostics-show-caret" "--make" "-ignore-dot-ghci"))
 '(dired-dwim-target t)
 '(dired-listing-switches "-alhF --group-directories-first")
 '(dired-ls-F-marks-symlinks t)
 '(display-fill-column-indicator-character 124)
 '(doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
 '(doom-modeline-minor-modes nil)
 '(doom-modeline-persp-name nil)
 '(ediff-merge-split-window-function 'split-window-horizontally)
 '(ediff-split-window-function 'split-window-horizontally)
 '(evil-ex-search-highlight-all t)
 '(evil-ex-search-persistent-highlight nil)
 '(evil-ex-visual-char-range t)
 '(evil-leader/leader "_")
 '(evil-snipe-scope 'line)
 '(evil-want-C-u-scroll t)
 '(evil-want-Y-yank-to-eol nil)
 '(evil-want-fine-undo t)
 '(eyebrowse-mode-line-style 'always)
 '(fill-column 78)
 '(flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-mypy-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(flycheck-shellcheck-follow-sources nil)
 '(focus-follows-mouse nil)
 '(geiser-default-implementation 'guile)
 '(geiser-guile-manual-lookup-nodes '("Guile Reference" "guile-2.0" "Guile"))
 '(geiser-guile-manual-lookup-other-window-p t)
 '(geiser-guile-warning-level 'high)
 '(help-window-select t)
 '(history-delete-duplicates t)
 '(ibuffer-default-sorting-mode 'major-mode)
 '(ibuffer-expert t)
 '(ibuffer-mode-hook '(ibuffer-auto-mode))
 '(icomplete-mode t)
 '(inf-ruby-default-implementation "pry")
 '(inf-ruby-implementations
   '(("ruby" . "irb --prompt default --noreadline -r irb/completion")
     ("jruby" . "jruby -S irb --prompt default --noreadline -r irb/completion")
     ("rubinius" . "rbx -r irb/completion")
     ("yarv" . "irb1.9 -r irb/completion")
     ("pry" . "pry -f")))
 '(make-backup-files t)
 '(openwith-associations
   '(("\\.\\(pdf\\|ps\\|djvu\\)\\'" "zathura"
      (file))
     ("\\.\\(mp3\\|flac\\|ogg\\|aac\\)\\'" "mplayer"
      (file))
     ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mkv\\|mp4\\|webm\\|ogv\\)\\'" "mplayer"
      ("-idx" file))
     ("\\.\\(?:jp?g\\|png\\|gif\\)\\'" "qiv"
      (file))
     ("\\.\\(od[sgtbfm]\\|st[icwd]\\|sx[gmdiwc]\\|ot[sgtp]\\|docx?\\|rtf\\|xl[sw]\\|pp[ts]\\)\\'" "libreoffice" nil)))
 '(openwith-confirm-invocation t)
 '(package-selected-packages
   '(cmake-mode dune ocp-indent utop flx hasky-extensions hasky-stack evil-lion flycheck-guile ace-link ace-mc ag all-the-icons all-the-icons-dired all-the-icons-ibuffer all-the-icons-ivy-rich anzu auctex auto-compile avy avy-flycheck cider cider-hydra clojure-mode clojure-mode-extra-font-locking clojure-snippets company-anaconda company-auctex company-cabal company-fuzzy company-ghci company-go company-inf-ruby company-jedi company-plsense company-rtags company-shell company-statistics counsel counsel-etags counsel-gtags counsel-org-capture-string counsel-org-clock counsel-projectile counsel-tramp dante diminish doom-modeline dumb-jump ein elpy evil evil-anzu evil-args evil-avy evil-better-visual-line evil-embrace evil-goggles evil-iedit-state evil-leader evil-magit evil-matchit evil-mc evil-mc-extras evil-multiedit evil-numbers evil-org evil-quickscope evil-ruby-text-objects evil-smartparens evil-surround evil-tutor eyebrowse fill-column-indicator flycheck flycheck-ocaml flycheck-yamllint flyspell-correct-ivy geiser gitconfig-mode go-dlv go-eldoc go-gopath go-imenu go-imports go-mode go-projectile go-snippets goto-last-change haskell-mode haskell-snippets helpful highlight-indentation highlight-indent-guides highlight-numbers highlight-parentheses hydra ibuffer-projectile iedit importmagic inf-ruby ivy-explorer ivy-hydra ivy-rich ivy-yasnippet ix magit magit-popup markdown-mode merlin merlin-eldoc multiple-cursors openwith org-plus-contrib paradox persistent-scratch persp-mode persp-mode-projectile-bridge python-docstring python-mode rainbow-delimiters rainbow-identifiers rainbow-mode raku-mode realgud-byebug realgud-pry realgud-rdb2 retrie robe rspec-mode rtags rubocop ruby-end rustic semi session slime slime-company smartparens swiper transient treemacs treemacs-evil treemacs-icons-dired treemacs-magit treemacs-persp treemacs-projectile tuareg undo-tree use-package w3m which-key with-editor xcscope yaml-mode yard-mode yasnippet yasnippet-snippets))
 '(paradox-automatically-star nil)
 '(paradox-execute-asynchronously t)
 '(paradox-github-token t)
 '(persp-nil-name "default")
 '(prog-mode-hook '(abbrev-mode prettify-symbols-mode))
 '(projectile-completion-system 'ivy)
 '(projectile-verbose nil)
 '(pylint-alternate-pylint-command "pylint")
 '(pylint-command "pylint3")
 '(recentf-menu-filter 'recentf-sort-basenames-ascending)
 '(ruby-insert-encoding-magic-comment nil)
 '(session-globals-include
   '((kill-ring 20 t)
     (session-file-alist 50 t)
     (file-name-history 50 t)
     search-ring regexp-search-ring))
 '(session-globals-max-size 200)
 '(session-use-package t nil (session))
 '(treemacs-width 30)
 '(vc-make-backup-files t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-evil-emacs-state ((t (:inherit (escape-glyph bold)))))
 '(doom-modeline-evil-insert-state ((t (:inherit (doom-modeline-info bold)))))
 '(doom-modeline-evil-normal-state ((t (:inherit warning))))
 '(doom-modeline-evil-visual-state ((t (:inherit (all-the-icons-yellow bold)))))
 '(geiser-font-lock-repl-output ((t (:foreground "gold"))))
 '(geiser-font-lock-repl-prompt ((t (:inherit font-lock-comment-face)))))
