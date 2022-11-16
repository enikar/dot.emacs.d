;; Time-stamp: <2022-11-16 19:25:03 enikar>
(package-intialize)
(dolist (p '(auctex
             general
             use-package))
  (package-install p))
