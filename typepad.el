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

(defun random-all-text (txt)
  (interactive)
  (let ((txt-list (split-string txt "")))
    (key-quiz--shuffle-list txt-list)
    (setq typepad-short (mapconcat 'identity txt-list ""))))

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

;; `DROP?' count the total key count in the writable buffer
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

;; `FIXME' def overlay for diff highlight
(defun typepad-diff ()
  (interactive)
  (with-current-buffer writable-buffer-name
    (save-excursion
      (goto-char (point-min))
      (let ((input-list (split-string
                          (buffer-substring-no-properties
                            (point-min) (point-max)) "" t))
             (diff-range (point-max))) ;; `ISSUE' why?
        (with-current-buffer readonly-buffer-name
          (save-excursion
            (remove-overlays)
            (goto-char (point-min))
            (let ((readonly-char (char-after)))
              (cl-loop for writable-char in input-list
                do (progn
                     ;; convert writable-char to ASCII
                     (setq writable-char (string-to-char writable-char))
                     (if (not (equal writable-char readonly-char)) ;; compare char
                       (let ((overlay (make-overlay (point) (1+ (point)))))
                         (overlay-put overlay 'face 'typepad-diff-face))
                       (let ((overlay (make-overlay (point) (1+ (point)))))
                         (overlay-put overlay 'face 'typepad-same-face)))
                     (forward-char)
                     (setq readonly-char (char-after)))
                until (eobp)))))))))


;; when the writable buffer change, compare it with readonly buffer
(add-hook 'typepad-mode-hook
          (lambda ()
            (add-hook 'post-command-hook 'typepad-diff nil t)))

;; key rate goal
(defvar typepad-key-rate-goal 4.00
  "The goal of key rate.")

(defvar typepad-key-acc-goal 1
  "The goal of key acc")

;; whether or not use key rate goal
(defvar typepad-use-key-rate-goal t
  "Whether or not use key rate goal.")

(defvar typepad-use-key-acc-goal t
  "Whether or not use key acc goal.")

;; total char num in the readonly buffer
(defvar typepad-char-num 20
  "Total char num in the readonly buffer.")

(defun check-standards (use-acc use-rate acc rate)
  (let ((acc-passed t)
        (rate-passed t))
    (when use-acc
      (setq acc-passed (= typepad-key-acc 1)))
    (when use-rate
      (setq rate-passed (> typepad-key-rate typepad-key-rate-goal)))
    (and acc-passed rate-passed)))

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
        (message "速度: %.2f 键准: %.2f%% 击键: %.3f 码长: %.3f"
          typepad-speed
          (* (typepad-calc-key-acc) 100)
          typepad-key-rate
          (typepad-calc-code-len))
        (if (check-standards typepad-use-key-acc-goal
              typepad-use-key-rate-goal
              typepad-key-acc-goal typepad-key-rate-goal)
            (progn
              (typepad-diff)
              (erase-buffer)
              (other-window 1))
            (progn
              (erase-buffer)
              (random-sending-text)
              (typepad-redraw-readonly-buffer)))
        ))))


;; hook to typepad-mode
(add-hook 'typepad-mode-hook
          (lambda ()
            (add-hook 'post-self-insert-hook 'typepad-paragraph-end nil t)))


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
    (- 1 (/ (float tp-pyim-delete)
      (float pyim--key-press-count))))
  typepad-key-acc)

(defvar typepad-short nil
  "short text.")

(defvar typepad-text-path nil
  "path to text")

(defvar typepad-split-size 10
  "每n个字符分隔一次")

(defvar typepad-randomp t
  "是否乱序全文")

(defvar typepad-name "example"
  "当前发文的文章名")

(defvar typepad-article-list nil)

(defun typepad-load-dir ()
  (interactive)
  (if typepad-text-path
    (setq typepad-article-list (get-txt-file-details typepad-text-path))
    (message "未设置 typepad-text-path")))

(defun typepad-load-short-text (path)
  (let ((short-text (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-string))))
    (if typepad-randomp
      (setq short-text (random-all-text short-text)))
    (setq typepad-short (split-string-every short-text typepad-split-size))
    (setq typepad-total-paragraph (length typepad-short))
    (typepad-send-text)))

(defun tp-set-split (n)
  (interactive)
  (if (numberp n)
    (setq typepad-split-size n)
    (message "输入数字")))

;; `FIXME'
(defun typepad-load ()
  (interactive)
  (if typepad-article-list
    (let ((article (alt-completing-read "Choose an article: " typepad-article-list)))
      (setq typepad-name (car article))
      (typepad-load-short-text (nth 1 article)))
    (typepad-load-dir)))


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
