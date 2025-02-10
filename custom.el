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
 '(connection-local-criteria-alist
   '(((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((eshell-connection-default-profile
      (eshell-path-env-list))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
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
 '(ibuffer-mode-hook '(ibuffer-auto-mode))
 '(indicate-buffer-boundaries 'right)
 '(make-backup-files t)
 '(package-selected-packages
   '(undo-fu-session casual-suite unisonlang-mode casual-avy debian-el latex-extra auctex corfu-prescient rg pdf-tools fennel-mode lfe-mode elixir-mode elixir-yasnippets inf-elixir systemd disaster eshell-vterm lua-mode affe all-the-icons all-the-icons-completion all-the-icons-ibuffer anaconda-mode attrap auto-compile avy avy-embark-collect caml cape cfrs cmake-mode commenter consult consult-ag consult-flycheck consult-flyspell consult-git-log-grep consult-ls-git corfu crystal-mode dash dash-functional deadgrep doom-modeline dune embark embark-consult evil evil-matchit evil-numbers evil-string-inflection evil-surround expand-region flycheck flycheck-guile flycheck-nimsuggest flycheck-yamllint fzf geiser geiser-guile general git-timemachine goto-chg haskell-mode helpful hl-todo inf-ruby kind-icon macrostep magit magit-section marginalia markdown-mode merlin merlin-eldoc nerd-icons orderless persistent-scratch pkg-info prescient racket-mode realgud reformatter robe ruby-end rust-mode rustic slime string-inflection symbol-overlay treemacs treemacs-all-the-icons treemacs-evil treemacs-magit typescript-mode utop vertico vimish-fold vterm wgrep wgrep-ag which-key with-editor yaml-mode yard-mode yasnippet-snippets emacs-gc-stats tuareg xterm-color vertico-prescient slime-company magit-delta uniquify-files json-reformat gitconfig flyspell-correct exec-path-from-shell exec-path-from-the-shell evil-iedit-states avy-menu consult-embark ace-window embrace yasnippet visual-regexp dumb-jump fringe-helper jq-format hlint-refactor vdiff evil-vimish-fold evil-mc evil-mc-extras package realgud-byebug realgud-pry evil-ruby-text-objects yari consult-dir consult-recoll consult-yasnippet py-snippets evil-org popper dimmer smalltalk-mode geiser-racket flycheck-raku nim-mode libgit magit-libgit erlang latex-math-preview latex-preview-pane latexdiff nocomments-mode json-mode evil-visualstar ripgrep evil-nerd-commenter ocp-indent hasky-extensions hasky-stack evil-lion ace-link ag all-the-icons-dired anzu cider cider-hydra clojure-mode clojure-mode-extra-font-locking clojure-snippets dante diminish evil-anzu evil-avy evil-embrace evil-goggles evil-iedit-state evil-multiedit evil-quickscope evil-tutor eyebrowse fill-column-indicator flycheck-ocaml gitconfig-mode go-dlv go-eldoc go-gopath go-imenu go-imports go-mode go-snippets haskell-snippets highlight-indent-guides highlight-numbers highlight-parentheses hydra iedit importmagic openwith rainbow-delimiters rainbow-identifiers raku-mode retrie rtags undo-tree xcscope))
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
 )
