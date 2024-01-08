;; define a mode for typepad writable buffer
(define-derived-mode typepad-mode fundamental-mode
  "typepad"
  "Major mode for typepad."
  (setq-local cursor-type 'bar)
  (setq-local cursor-in-non-selected-windows nil))

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
(add-hook 'typepad-mode-hook
          (lambda ()
            (add-hook 'post-command-hook 'typepad-count-key nil t)))


