;; copy from purcell https://gist.github.com/purcell/34824f1b676e6188540cdf71c7cc9fc4
(defun key-quiz--shuffle-list (list)
  "Shuffles LIST randomly, modying it in-place."
  (dolist (i (reverse (number-sequence 1 (1- (length list)))))
    (let ((j (random (1+ i)))
	  (tmp (elt list i)))
      (setf (elt list i) (elt list j))
      (setf (elt list j) tmp)))
  list)

(defun split-string-every (str n)
  (let ((result '()))
    (dotimes (i (ceiling (/ (length str) n)))
      (push (substring str (* i n) (min (+ (* i n) n) (length str))) result))
    (nreverse result)))

;;; typepad-lib.el end here
(provide 'typepad-lib)
