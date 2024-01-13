;; define a mode for typepad writable buffer
(define-derived-mode typepad-mode fundamental-mode
  "typepad"
  "Major mode for typepad."
  (setq-local cursor-type 'bar)
  (setq-local visual-fill-column-center-text t)
  ;; set font height local
  (setq-local face-remapping-alist '((default (:height 280))))
  )

;; define a mode for typepad readonly buffer
(define-derived-mode typepad-readonly-mode fundamental-mode
  "typepad-readonly"
  "Major mode for typepad readonly."
  (setq-local face-remapping-alist '((default (:height 280))))
  (setq-local visual-fill-column-center-text t)
  (setq-local cursor-type nil)
  (setq-local cursor-in-non-selected-windows nil))

(defun typepad-create-window ()
  "Create two windows, one for readonly text, one for writable text."
  (interactive)
  (delete-other-windows)
  (let ((readonly-buffer-name "*发文区*")
      (writable-buffer-name "*跟打区*")
         (readonly-text "不者时今青任尽代形机"))
    ;; delete same name buffer before create))
    (when (get-buffer readonly-buffer-name)
      (kill-buffer readonly-buffer-name))
    (when (get-buffer writable-buffer-name)
      (kill-buffer writable-buffer-name))
    (with-selected-window (split-window-below)
      (switch-to-buffer (get-buffer-create writable-buffer-name))
      (typepad-mode))
    ;; (other-window 1)
    (with-current-buffer (get-buffer-create readonly-buffer-name)
      (erase-buffer)
      (insert readonly-text)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (read-only-mode 1)
      (switch-to-buffer readonly-buffer-name)
      ;; (org-switch-to-buffer-other-window readonly-buffer-name)
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
(defface typepad-diff-face
  '((t (:background "red")))
  "Face for different text.")
(defface typepad-same-face
  '((t (:background "green")))
  "Face for same text.")

;; total char num in the readonly buffer
(defun typepad-count-char ()
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq count (1+ count))
        (forward-char)))
    'count))

;; def overlay for diff highlight
(defun typepad-diff ()
  (interactive)
  (let ((readonly-buffer-name "*发文区*")
         (writable-buffer-name "*跟打区*"))
    (with-current-buffer writable-buffer-name
      (save-excursion
        (goto-char (point-min))
        (let (
               (writable-char-list (split-string (buffer-substring-no-properties (point-min) (point-max)) "" t))
               (diff-range (point-max))) ;; char-after return nil if at the end of buffer
          (with-current-buffer readonly-buffer-name
            (save-excursion ;; save-excursion restore the point after execute the code
              ;; clear all overlay
              (remove-overlays)
              (goto-char (point-min))
              (let ((readonly-char (char-after)))
                (cl-loop for writable-char in writable-char-list
                  do (progn
                       ;; convert writable-char to ASCII
                       (setq writable-char (string-to-char writable-char))
                       ;; (setq diff-range (min diff-range (point)))
                       ;; (while (and (and writable-char readonly-char)
                       ;;          (< (point) diff-range))
                       ;; debug message
                       ;; (message "writable-char: %s, readonly-char: %s" writable-char readonly-char)
                       (if (not (equal writable-char readonly-char)) ;; compare char
                         ;; make overlay for different char
                         (let ((overlay (make-overlay (point) (1+ (point)))))
                           (overlay-put overlay 'face 'typepad-diff-face))
                         (let ((overlay (make-overlay (point) (1+ (point)))))
                           (overlay-put overlay 'face 'typepad-same-face)))
                       (forward-char)
                       (setq readonly-char (char-after))
                       )
                  until (eobp))))))))))


;; when the writable buffer change, compare it with readonly buffer
(add-hook 'typepad-mode-hook
          (lambda ()
            (add-hook 'post-command-hook 'typepad-diff nil t)))

;; enable visual-fill-column-mode in typepad-mode and typepad-readonly-mode
(add-hook 'typepad-mode-hook 'visual-fill-column-mode)
(add-hook 'typepad-readonly-mode-hook 'visual-fill-column-mode)

;; (require 'typepad-pyim)
