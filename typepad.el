(require 'typepad-lib)
;; define a mode for typepad writable buffer
(define-derived-mode typepad-mode fundamental-mode
  "typepad"
  "Major mode for typepad."
  (setq-local cursor-type 'bar)
  (setq-local visual-fill-column-center-text t)
  ;; set font height local
  (setq-local face-remapping-alist '((default (:height 280)))))

;; define a mode for typepad readonly buffer
(define-derived-mode typepad-readonly-mode fundamental-mode
  "typepad-readonly"
  "Major mode for typepad readonly."
  (setq-local face-remapping-alist '((default (:height 280))))
  (setq-local visual-fill-column-center-text t)
  (setq-local cursor-type nil)
  (setq-local cursor-in-non-selected-windows nil))

(defvar readonly-buffer-name "*发文区*"
  "The name of readonly buffer.")

(defvar writable-buffer-name "*跟打区*"
  "The name of writable buffer.")

(defvar sending-text "不者时今青任尽代形机"
  "The char read from readonly buffer.")

(defun random-sending-text ()
  (interactive)
  (let ((sending-text-list (split-string sending-text "")))
    (key-quiz--shuffle-list sending-text-list)
    (setq sending-text (mapconcat 'identity sending-text-list ""))
    ))

(defun typepad-create-window ()
  "Create two windows, one for readonly text, one for writable text."
  (interactive)
  (delete-other-windows)
  (let (
         (readonly-text sending-text))
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
      ;; (setq buffer-read-only t)
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
    (message "count: %s" count)
    count))

;; def overlay for diff highlight
(defun typepad-diff ()
  (interactive)
  (let ((readonly-buffer-name "*发文区*")
        )
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

;; key rate goal
(defvar typepad-key-rate-goal 4.00
  "The goal of key rate.")

;; whether or not use key rate goal
(defvar typepad-use-key-rate-goal t
  "Whether or not use key rate goal.")

;; total char num in the readonly buffer
(defvar typepad-char-num 20
  "Total char num in the readonly buffer.")

;; redraw readonly buffer
(defun typepad-redraw-readonly-buffer ()
  (interactive)
  (with-current-buffer readonly-buffer-name
    (read-only-mode -1)
    (erase-buffer)
    (insert sending-text)
    (read-only-mode 1)))


;; `FIXME'
(defun typepad-paragraph-end ()
  (let ((input-text (buffer-string))
         (last-char (char-before))
         (last-readonly (string-to-char (substring sending-text -1))))
    (if (and (equal typepad-char-num (string-width input-text))
          (equal last-char last-readonly))
      (progn
        (message "键准: %.3f 击键: %.3f 码长: %.3f"
          (typepad-calc-key-acc)
          typepad-key-rate
          (typepad-calc-code-len))
      (if typepad-use-key-rate-goal
        (if (> typepad-key-rate typepad-key-rate-goal)
          (progn
            (typepad-diff)
            (erase-buffer)
            (other-window 1)
            )
          (progn
            (erase-buffer)
            (random-sending-text)
            (typepad-redraw-readonly-buffer)
            ))
        (progn
          (other-window 1)
          (erase-buffer))))
      )))


;; hook to typepad-mode
(add-hook 'typepad-mode-hook
          (lambda ()
            (add-hook 'post-self-insert-hook 'typepad-paragraph-end)))


(setq-default company-global-modes '(not typepad-mode))

(add-hook 'typepad-mode-hook
  (lambda () (company-mode -1)))


;; 计算码长
(defvar typepad-code-len 0
  "The code length of user input.")

(defun typepad-calc-code-len ()
  (setq typepad-code-len
    (/ (float pyim--key-press-count)
      (/ typepad-char-num 2.000)))
  typepad-code-len)

(defvar typepad-key-acc nil)

(defun typepad-calc-key-acc ()
  (setq typepad-key-acc
    (/ (float tp-pyim-delete)
      (float pyim--key-press-count)))
  typepad-key-acc)

(defvar typepad-short nil
  "short text.")

(defvar typepad-text-path nil
  "path to text")

(defvar typepad-split-size 10
  "每n个字符分隔一次")

;; load short text from file top500.txt as string and split it to list every 20 char
(defun typepad-load-short-text ()
  (interactive)
  (if typepad-text-path
    (let ((short-text (with-temp-buffer
                        (insert-file-contents typepad-text-path)
                        (buffer-string))))
      (setq typepad-short (split-string-every short-text typepad-split-size))
      (setq typepad-total-paragraph (length typepad-short))
      (typepad-send-text))
    (message "未设置 typepad-text-path")))


(defvar typepad-total-paragraph 1
  "当前发文的总段数")

(defvar typepad-current-paragraph 1
  "当前在发的段数")

;; send text
(defun typepad-send-text ()
  (interactive)
  (setq sending-text (nth (- typepad-current-paragraph 1) typepad-short))
  (typepad-redraw-readonly-buffer))

(defun typepad-send-next ()
  (interactive)
  (let ((current typepad-current-paragraph))
    (if (<= typepad-total-paragraph current)
      (message "当前已是最后一段")
      (progn
        (setq typepad-current-paragraph (1+ current))
        (typepad-send-text)
        ))))

(defun typepad-send-prev ()
  (interactive)
  (let ((current typepad-current-paragraph))
    (if (eq current 1)
      (message "当前已是第一段")
      (progn
        (setq typepad-current-paragraph (1- current))
        (typepad-send-text)
        ))))

;; load other modules
(require 'typepad-pyim)
(require 'typepad-time)


;;; typepad.el end here
(provide 'typepad)
