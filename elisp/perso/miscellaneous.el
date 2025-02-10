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
;; usage: (defun foo (blablah)
;;         (with-region-or-buffer (begin end)
;;            (the remainder of things to do in a region or the buffer)))
;; That miss save-excursion things. So that moves the point.

(defmacro with-region-or-buffer (args &rest body)
  "Execute BODY with BEG and END bound to the beginning and end of the
current region if one exists or the current buffer if not."
  (declare (indent 1))
  `(let ((,(car args) (if (use-region-p) (region-beginning) (point-min)))
         (,(cadr args) (if (use-region-p) (region-end) (point-max))))
     ,@body))

;; Select a font automatically from a preslected list of fonts at startup
;; (require 'cl-lib)
;; (require 'personal-commands)
;; (cl-loop
;;  for font in my/favorite-fonts
;;  if (font-available-p font)
;;  do (set-frame-font font t)
;;     (cl-return))

;; More functionnal way. We can also use cl-some, but for simple task
;; dash is more efficient, although it is not built into emacs. In any
;; event cl-some or -some are more efficient than cl-loop.
;; (require 'dash)
;; (require 'personal-commands)
;; (save-window-excursion
;;   (let ((font (-some #'font-available-p my/favorite-fonts)))
;;     (if font
;;         (set-frame-font font t))))


;;;; A macro to mesure the running time of a block of code.
;; From: https://nullprogram.com/blog/2009/05/28/

(defmacro measure-time (&rest body)
  "Measure and return the running time of the code block."
  (declare (indent defun))
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,@body
       (- (float-time) ,start))))

;; From: http://justinhj.github.io/2009/04/15/running-elisp-function-on-each-marked.html
(defun for-each-dired-marked-file(fn)
  "Do stuff for each marked file, only works in dired window"
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((filenames (dired-get-marked-files)))
        (mapcar fn filenames))
    (error (format "Not a Dired buffer \(%s\)" major-mode))))

;; From: https://mbork.pl/2019-02-17_Inserting_the_current_file_name_at_point
(defun insert-current-file-name-at-point (&optional full-path)
  "Insert the current filename at point.
With prefix argument, use full path."
  (interactive "P")
  (let* ((buffer
	      (if (minibufferp)
	          (window-buffer (minibuffer-selected-window))
	        (current-buffer)))
	     (filename (buffer-file-name buffer)))
    (if filename
	    (insert (if full-path filename (file-name-nondirectory filename)))
      (error (format "Buffer %s is not visiting a file" (buffer-name buffer))))))

;; From: https://oremacs.com/2017/03/18/dired-ediff/
(defun my/dired-ediff-files ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "no more than 2 files should be marked"))))

;; From: https://www.masteringemacs.org/article/fun-emacs-calc
(require 'cl-lib)
(defun hms-to-dec (hms-str)
  (let ((hms (split-string hms-str "[°'′\"″NWE ]" t)))
    (cl-flet ((to-deg ()
                   (string-to-number
                    (calc-eval (format "deg(%s@ %s' %s\")"
                                       (pop hms) (pop hms) (pop hms))))))
      (list (to-deg) (to-deg)))))

(provide 'miscellaneous)
;;; misc.el ends here
