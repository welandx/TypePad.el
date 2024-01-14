;; init pyim--key-press-count
(defvar pyim--key-press-count 0)

(defvar tp-pyim-delete 0)

;; advice pyim-self-insert-command, 用来计数每一次按键
(defun pyim-count-key (orig-fun &rest args)
  "count key press times."
  (setq pyim--key-press-count (1+ pyim--key-press-count))
  (when (= pyim--key-press-count 1)
    (setq typepad-init-time (current-time))
    (typepad-timer-func)
    (message "typepad duration: %s" typepad-time-duration)
    (setq typepad-timer (run-with-idle-timer 0.1 t 'typepad-timer-func)))
  (apply orig-fun args))

(defun tp-record-del (orig-fun &rest args)
  "count del"
  (setq tp-pyim-delete (1+ tp-pyim-delete))
  (apply orig-fun args))

(advice-add 'pyim-self-insert-command :around #'pyim-count-key)

(advice-add 'pyim-select-word :around #'pyim-count-key)

(advice-add 'pyim-select-word-by-number :around #'pyim-count-key)
;; advice pyim-delete-backward-char
(advice-add 'pyim-delete-backward-char :around #'pyim-count-key)
(advice-add 'pyim-delete-backward-char :around #'tp-record-del)

;; pyim-delete-forward-char
(advice-add 'pyim-delete-forward-char :around #'pyim-count-key)

;; when key DEL press, count
(defun pyim--key-press-count-clear-when-del ()
  "`pyim--key-press-count' when key DEL press."
  (when (eq last-command-event ?\d)
    (setq tp-pyim-delete (1+ tp-pyim-delete))
    (setq pyim--key-press-count (+ pyim--key-press-count 1))))

(add-hook 'pre-command-hook #'pyim--key-press-count-clear-when-del)

;; when key 'a-z' or 'A-Z' press, count
(defun pyim--key-press-count-clear-when-letter ()
  "Clear `pyim--key-press-count' when key 'a-z' or 'A-Z' press."
  ;; at first, last-command-event should be a number or marker
  (when (number-or-marker-p last-command-event)
    (when (and (>= last-command-event ?A)
            (<= last-command-event ?Z))
      (setq pyim--key-press-count (+ pyim--key-press-count 1)))
    (when (and (>= last-command-event ?a)
            (<= last-command-event ?z))
      (setq pyim--key-press-count (+ pyim--key-press-count 1)))))

(add-hook 'post-command-hook #'pyim--key-press-count-clear-when-letter)
;; just message pyim--key-press-count when post-self-insert-hook
(defun pyim--key-press-count-message ()
  "Message `pyim--key-press-count' when `post-self-insert-hook'."
  ;; conunt +1 and message
  (message "PYIM: %d 次按键" pyim--key-press-count))

;; dynamic display pyim--key-press-count in mode-line at typepad-mode
(defun pyim--key-press-count-display ()
  "Display `pyim--key-press-count' in mode-line at `typepad-mode'."
  (setq mode-line-format
                (append mode-line-format
                        '((:eval (format " PYIM: %d 次按键" pyim--key-press-count))))))

;; hook to typepad-mode
(add-hook 'typepad-mode-hook #'pyim--key-press-count-display)
;; (add-hook 'post-self-insert-hook #'pyim--key-press-count-message)

;; clear pyim--key-press-count when pyim-process-terminate
(defun pyim--key-press-count-clear ()
  (interactive)
  "Clear `pyim--key-press-count' when `pyim-process-terminate'."
  (setq pyim--key-press-count 0))
(setq-default pyim-punctuation-translate-p '(no yes auto))
;; 当光标位于 buffer 的开头时, 清除 pyim--key-press-count
(defun pyim--key-press-count-clear-when-buffer-beg ()
  "Clear `pyim--key-press-count' when buffer begin."
  (when (= (point) (point-min))
    (setq pyim--key-press-count 0)))

(defun pyim-autoselector--xingma (split-length entered candidates last-candidates)
  "`pyim-autoselector-xingma' 内部使用的函数。"
  (cond
   ((and (= (length entered) split-length)
         (= (length candidates) 1)
         ;; 如果没有候选词，pyim 默认将用户输入当做候选词，这时不能自动上屏，
         ;; 因为这种情况往往是用户输入有误，自动上屏之后，调整输入就变得麻烦了。
         (not (equal entered (car candidates))))
    '(:select current))
   ((and (> (length entered) split-length)
         (equal (substring entered 0 split-length)
                (car last-candidates)))
    ;; 自动清除错误输入模式，类似微软五笔：敲第五个字母的时候，前面四个字母自
    ;; 动清除。
    '(:select last :replace-with ""))
    ((> (length entered) split-length)
      '((:select last)
         (setq pyim--key-press-count (- pyim--key-press-count 1))))
   (t nil)))

(add-hook 'post-command-hook #'pyim--key-press-count-clear-when-buffer-beg)


;;; typepad-pyim.el end here
(provide 'typepad-pyim)
