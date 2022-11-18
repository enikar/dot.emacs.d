;; From: https://nullprogram.com/blog/2012/09/14/

(require 'cl-lib)

;; To use evaluate:  (sierpinski (expt 3 5))
(defun sierpinski (s)
  (pop-to-buffer (get-buffer-create "*sierpinski*"))
  (fundamental-mode) (erase-buffer)
  (cl-labels ((fill-p (x y)
                   (cond ((or (zerop x) (zerop y)) "0")
                         ((and (= 1 (mod x 3)) (= 1 (mod y 3))) "1")
                         (t (fill-p (/ x 3) (/ y 3))))))
    (insert (format "P1\n%d %d\n" s s))
    (cl-dotimes (y s) (cl-dotimes (x s) (insert (fill-p x y) " "))))
  (image-mode))

;; And the amazement…
(defun mandelbrot ()
  (pop-to-buffer (get-buffer-create "*mandelbrot*"))
  (let ((w 400) (h 300) (d 32))
    (fundamental-mode) (erase-buffer)
    (set-buffer-multibyte nil)
    (insert (format "P6\n%d %d\n255\n" w h))
    (cl-dotimes (y h)
      (cl-dotimes (x w)
        (let* ((cx (* 1.5 (/ (- x (/ w 1.45)) w 0.45)))
               (cy (* 1.5 (/ (- y (/ h 2.0)) h 0.5)))
               (zr 0) (zi 0)
               (v (cl-dotimes (i d d)
                    (if (> (+ (* zr zr) (* zi zi)) 4) (cl-return i)
                      (cl-psetq zr (+ (* zr zr) (- (* zi zi)) cx)
                             zi (+ (* (* zr zi) 2) cy))))))
          (insert-char (floor (* 256 (/ v 1.0 d))) 3))))
    (image-mode)))

;; This came with mandelbrot function but I don't know how to use it.
(defun colormap (v)
  "Given a value between 0 and 1.0, insert a P6 color."
  (dotimes (i 3)
    (insert-char (floor (* 256 (min 0.99 (sqrt (* (- 3 i) v))))) 1)))
