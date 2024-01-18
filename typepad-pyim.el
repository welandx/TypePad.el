;;; typepad-pyim.el --- input of pyim -*- lexical-binding: t; -*-

(defvar pyim--key-press-count 0)

(defvar tp-pyim-delete 0)

(defun pyim-count-key (orig-fun &rest args)
  "count key press times."
  (setq pyim--key-press-count (1+ pyim--key-press-count))
  (when (= pyim--key-press-count 1)
    (setq typepad-init-time (current-time))
    (typepad-timer-func)
    (setq typepad-timer (run-with-idle-timer 0.1 t 'typepad-timer-func)))
  (apply orig-fun args))

(defun tp-record-del (orig-fun &rest args)
  "count del"
  (setq tp-pyim-delete (1+ tp-pyim-delete))
  (apply orig-fun args))

(advice-add 'pyim-self-insert-command :around #'pyim-count-key)
(advice-add 'pyim-select-word :around #'pyim-count-key)
(advice-add 'pyim-select-word-by-number :around #'pyim-count-key)
(advice-add 'pyim-delete-backward-char :around #'pyim-count-key)
(advice-add 'pyim-delete-backward-char :around #'tp-record-del)
(advice-add 'pyim-delete-forward-char :around #'pyim-count-key)

(defun pyim--key-press-count-clear-when-del ()
  "`pyim--key-press-count' when key DEL press."
  (when (eq last-command-event ?\d)
    (setq tp-pyim-delete (1+ tp-pyim-delete))
    (setq pyim--key-press-count (+ pyim--key-press-count 1))))

(add-hook 'typepad-mode-hook
  (lambda ()
    (add-hook 'pre-command-hook #'pyim--key-press-count-clear-when-del nil t)))

(defun pyim--key-press-count-letter ()
  "count++ when key `a-z' or `A-Z' press."
  (when (number-or-marker-p last-command-event)
    (when (and (>= last-command-event ?A)
            (<= last-command-event ?Z))
      (setq pyim--key-press-count (+ pyim--key-press-count 1)))
    (when (and (>= last-command-event ?a)
            (<= last-command-event ?z))
      (setq pyim--key-press-count (+ pyim--key-press-count 1)))))

(add-hook 'typepad-mode-hook
  (lambda ()
    (add-hook 'post-command-hook #'pyim--key-press-count-letter nil t)))

(defun pyim--key-press-count-display ()
  "Display `pyim--key-press-count' in mode-line at `typepad-mode'."
  (setq mode-line-format
                (append mode-line-format
                        '((:eval (format " PYIM: %d 次按键" pyim--key-press-count))))))

(add-hook 'typepad-mode-hook #'pyim--key-press-count-display)

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
         (setq pyim--key-press-count (- pyim--key-press-count 1)))) ;; 自动上屏时减去一次选择
   (t nil)))

;; 当光标位于 buffer 的开头时, 清除 pyim--key-press-count
(defun tp-pyim-clear-when-buffer-beg ()
  "Clear `pyim--key-press-count' when buffer begin."
  (when (= (point) (point-min))
    (setq tp-pyim-delete 0) ;; clear del
    (setq pyim--key-press-count 0)))

(add-hook 'typepad-mode-hook
  (lambda ()
    (add-hook 'post-command-hook #'tp-pyim-clear-when-buffer-beg nil t)))

(defun typepad-pyim-key-acc ()
  "计算键准"
  (let ((key-acc (- 1 (/ (float tp-pyim-delete)
                        (float pyim--key-press-count)))))
    key-acc))

(defun typepad-pyim-code-len (num)
  "计算码长"
  (let ((len (/ (float pyim--key-press-count)
               (/ num 2.000))))
    len))

(provide 'typepad-pyim)
;;; typepad-pyim.el ends here
