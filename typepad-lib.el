;;; typepad-lib.el --- lib function -*- lexical-binding: t; -*-
;; copy from purcell https://gist.github.com/purcell/34824f1b676e6188540cdf71c7cc9fc4
(defun key-quiz--shuffle-list (list)
  "Shuffles LIST randomly, modying it in-place."
  (dolist (i (reverse (number-sequence 1 (1- (length list)))))
    (let ((j (random (1+ i))) ;; save this
	  (tmp (elt list i)))
      (setf (elt list i) (elt list j))
      (setf (elt list j) tmp)))
  list)

(defun FY-shuffle-list (list)
  "Shuffles LIST randomly, modifying it in-place.
   Returns a list of typepad-indices used for shuffling."
  (let ((ind '()))
    (dolist (i (reverse (number-sequence 1 (1- (length list)))))
      (let ((j (random (1+ i)))
             (tmp (elt list i)))
        (setf (elt list i) (elt list j))
        (setf (elt list j) tmp)
        (push j ind)))
    ind))

(defun FY-reproduce-shuffle (list ind)
  (let* ((jlist (reverse ind))
          (ilist (reverse (number-sequence 1 (1- (length list))))))
    (cl-loop for (i j) in (cl-mapcar #'list ilist jlist)
      do (let ((tmp (elt list i)))
           (setf (elt list i) (elt list j))
           (setf (elt list j) tmp)))))

(defun split-string-every (str n)
  (let ((result '()))
    (dotimes (i (ceiling (/ (length str) n)))
      (push (substring str (* i n) (min (+ (* i n) n) (length str))) result))
    (nreverse result)))

;; `FIXME'
(defun get-txt-file-details (directory)
  (let ((file-details '()))
    ;; 递归获取指定路径下所有文件以及子目录的文件名和路径
    (dolist (file (directory-files-recursively directory ".*\\.txt\\'"))
      ;; 如果获取到的是文件而不是目录
      (when (file-regular-p file)
        ;; 获取文件名
        (let ((file-name (file-name-sans-extension (file-name-nondirectory file))))
          ;; 将文件名和路径存入列表
          (push (list file-name file) file-details))))
    file-details))

;; modify from https://howardism.org/Technical/Emacs/alt-completing-read.html
(defun alt-completing-read (prompt collection &optional predicate require-match initial-input hist def inherit-input-method)
  "Calls `completing-read' but returns the list from COLLECTION."

  ;; Yes, Emacs really should have an `alistp' predicate to make this code more readable:
  (cl-flet ((assoc-list-p (obj) (and (listp obj) (consp (car obj)))))

    (let* ((choice
            (completing-read prompt collection predicate require-match initial-input hist def inherit-input-method))
           (results (cond
                     ((hash-table-p collection) (gethash choice collection))
                     ((assoc-list-p collection) (alist-get choice collection def nil 'equal))
                     (t                         choice))))
      (if (listp results)
        (progn
          (list choice (cl-first results)))
        '(choice results)))))

;; from https://github.com/mschuldt/buffer-focus-hook
(defvar buffer-focus-hook--current-buffer nil
  "Buffer currently in focus.")

(defvar buffer-focus-hook--in nil
  "Normal hook run when a buffers window gains focus.")

(defvar buffer-focus-hook--out nil
  "Normal hook run when a buffers window looses focus.")

(defun buffer-focus-out-callback (callback &optional buffer)
  "Set the CALLBACK to be run when BUFFER or current buffer window looses focus."
  (with-current-buffer (or buffer (current-buffer))
    (add-hook 'buffer-focus-hook--out callback nil t)))

(defun buffer-focus-in-callback (callback &optional buffer)
  "Set the CALLBACK to be run when BUFFER or current buffer window gains focus."
  (with-current-buffer (or buffer (current-buffer))
    (add-hook 'buffer-focus-hook--in callback nil t)))

(defun buffer-focus-hook--updater ()
  "Main buffer focus hook update function added for ‘buffer-list-update-hook’."
  (when (not (buffer-live-p buffer-focus-hook--current-buffer))
    (setq buffer-focus-hook--current-buffer nil))
  (when (and (eq (window-buffer (selected-window))
                 (current-buffer))
             (not (eq buffer-focus-hook--current-buffer
                      (current-buffer))))
    ;; selected window has current buffer
    (when buffer-focus-hook--current-buffer
      ;; current buffer lost focus
      (with-current-buffer buffer-focus-hook--current-buffer
        (run-hooks 'buffer-focus-hook--out)
        (setq buffer-focus-hook--current-buffer nil)))

    (when (or buffer-focus-hook--in
              buffer-focus-hook--out)
      ;; current buffer gaining focus
      (setq buffer-focus-hook--current-buffer (current-buffer))
      (run-hooks 'buffer-focus-hook--in))))

(add-hook 'buffer-list-update-hook 'buffer-focus-hook--updater)

;; from http://xahlee.info/emacs/emacs/elisp_relative_path.html
(defun xah-get-fullpath (@file-relative-path)
  "Return the full path of *file-relative-path, relative to caller's file location."
  (concat (file-name-directory (or load-file-name buffer-file-name)) @file-relative-path))


(provide 'typepad-lib)
;;; typepad-lib.el ends here
