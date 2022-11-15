;;; miscellaneous.el --- Miscellaneous routines. -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; pkg-info is abandoned, damage!

(require 'package)
(require 'pkg-info)

(defun my/remove-packages (&rest package-list)
  (dolist (pack package-list)
    (let* ((version (pkg-info-format-version (pkg-info-package-version pack)))
           (pack-rd (concat pack "-" version))
           (pack-dir (concat package-user-dir package-rd)))
      (if (file-directory-p pack-dir)
          (package-delete pack-rd)
        (message "Can't find package directory for: %s" pack)))))

;; From Emacs Reddux (author of Prelude, cider, projectile, rubocop…)
;; https://metaredux.com/
;; https://emacsredux.com/
;; https://batsov.com/archive/
(defun er-reinstall-package (pkg)
  (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

(defun my/package-installed-p (pkg)
  (not (null (member pkg (mapcar #'car package-alist)))))

;; From: https://irreal.org/blog/?p=2226
(defmacro with-region-or-buffer (args &rest body)
  "Execute BODY with BEG and END bound to the beginning and end of the
current region if one exists or the current buffer if not."
  (declare (indent 1))
  `(let ((,(car args) (if (use-region-p) (region-beginning) (point-min)))
         (,(cadr args) (if (use-region-p) (region-end) (point-max))))
     ,@body))

(provide 'miscellaneous)
;;; misc.el ends here
