;; define a mode for typepad writable buffer
(define-derived-mode typepad-mode fundamental-mode
  "typepad"
  "Major mode for typepad."
  (setq-local cursor-type 'bar)
  ;; (setq-local cursor-in-non-selected-windows nil)
  )

;; define a mode for typepad readonly buffer
(define-derived-mode typepad-readonly-mode fundamental-mode
  "typepad-readonly"
  "Major mode for typepad readonly."
  (setq-local cursor-type nil)
  (setq-local cursor-in-non-selected-windows nil))

(defun typepad-create-window ()
  "Create two windows, one for readonly text, one for writable text."
  (interactive)
  (delete-other-windows)
  (let ((readonly-buffer-name "*发文区*")
      (writable-buffer-name "*跟打区*")
         (readonly-text "this is readonly text, you can't edit it"))
    ;; delete same name buffer before create))
    (when (get-buffer readonly-buffer-name)
      (kill-buffer readonly-buffer-name))
    (when (get-buffer writable-buffer-name)
      (kill-buffer writable-buffer-name))
    (with-selected-window (split-window-right)
      (switch-to-buffer (get-buffer-create writable-buffer-name))
      (typepad-mode))
    (other-window 1)
    (with-current-buffer (get-buffer-create readonly-buffer-name)
      (erase-buffer)
      (insert readonly-text)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (read-only-mode 1)
      (switch-to-buffer-other-window readonly-buffer-name)
      (typepad-readonly-mode)
      (other-window 1))))

;; count the total key count in the writable buffer
(defun typepad-count-key ()
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq count (1+ count))
        (forward-char)))
    ;;delete the old key count in the mode line
    (setq mode-line-misc-info
          (delete (car (last mode-line-misc-info 2))
                  mode-line-misc-info))
    ;; directly set the mode line, not append
    (setq mode-line-misc-info
      (cons (format " %d" count) mode-line-misc-info))))

;; only count the key in the writable buffer
;; (add-hook 'typepad-mode-hook
;;           (lambda ()
;;             (add-hook 'post-command-hook 'typepad-count-key nil t)))

;; define the face for diff highlight
(defface typepad-delete-face
  '((t (:background "red")))
  "Face for delete text in diff.")
(defface typepad-insert-face
  '((t (:background "green")))
  "Face for insert text in diff.")



;; define diff-string function
(defun diff-string (str1 str2)
  "Return a list of diff between str1 and str2."
  (let ((diff (compare-list
                (split-string str1 "")
                (split-string str2 "")
                )))
    (dolist (item diff)
      (let ((type (car (cdr item))))
        (when (not (eq type ?=))
          (setcar (cdr item) (car item))
          (setcar item (car (cdr item))))))
    diff))

;; use overlay to highlight diff of the two buffer
(defun typepad-highlight-diff ()
  (interactive)
  (let ((writable-buffer (get-buffer "*跟打区*"))
        (readonly-buffer (get-buffer "*发文区*")))
    (when (and writable-buffer readonly-buffer)
      (let ((writable-buffer-text (buffer-substring-no-properties
                                   (point-min) (point-max)))
            (readonly-buffer-text (with-current-buffer readonly-buffer
                                    (buffer-substring-no-properties
                                     (point-min) (point-max)))))
        (when (not (string= writable-buffer-text readonly-buffer-text))
          (let ((diff (diff-string writable-buffer-text readonly-buffer-text)))
            (with-current-buffer writable-buffer
              (remove-overlays (point-min) (point-max) 'typepad t)
              (dolist (item diff)
                (let ((start (car item))
                      (end (car (cdr item)))
                      (type (car (cdr (cdr item)))))
                  (overlay-put (make-overlay start end) 'typepad t)
                  (overlay-put (make-overlay start end) 'face
                               (cond ((eq type ?-) 'typepad-delete-face)
                                 ((eq type ?+) 'typepad-insert-face))))))))))))

;; do the highlight when the writable buffer changed
;; (add-hook 'typepad-mode-hook
;;           (lambda ()
;;             (add-hook 'post-command-hook 'typepad-highlight-diff nil t)))
