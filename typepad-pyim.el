;;; typepad-pyim.el --- input of pyim -*- lexical-binding: t; -*-

(defvar typepad-pyim-key-press 0)

(defvar typepad-pyim-delete 0)

(defun typepad-get-del ()
  typepad-pyim-delete)

(defun typepad-get-key ()
  typepad-pyim-key-press)

(declare-function typepad-start-timer () "start timer")

(defun typepad-pyim-count-key (orig-fun &rest args)
  "count key press times."
  (when  (eq major-mode 'typepad-mode)
    (setq typepad-pyim-key-press (1+ typepad-pyim-key-press))
    (typepad-start-timer)
    )
  ;; (typepad-timer-func)
  (apply orig-fun args))

(defun typepad-record-del (orig-fun &rest args)
  "count del"
  (when  (eq major-mode 'typepad-mode)
    (setq typepad-pyim-key-press (1+ typepad-pyim-key-press))
    (typepad-start-timer)
    )
  (setq typepad-pyim-delete (1+ typepad-pyim-delete))
  (apply orig-fun args))

(advice-add 'pyim-self-insert-command :around #'typepad-pyim-count-key)
(advice-add 'pyim-select-word :around #'typepad-pyim-count-key)
(advice-add 'pyim-select-word-by-number :around #'typepad-pyim-count-key)
(advice-add 'pyim-delete-backward-char :around #'typepad-pyim-count-key)
(advice-add 'pyim-delete-backward-char :around #'typepad-record-del)
(advice-add 'pyim-delete-forward-char :around #'typepad-pyim-count-key)

(defun typepad-pyim-count-del ()
  "`typepad-pyim-key-press' when key DEL press."
  (when (eq last-command-event ?\d)
    (setq typepad-pyim-delete (1+ typepad-pyim-delete))
    (setq typepad-pyim-key-press (+ typepad-pyim-key-press 1))))

(add-hook 'typepad-mode-hook
  (lambda ()
    (add-hook 'pre-command-hook #'typepad-pyim-count-del nil t)))

(defun typepad-pyim-count-letter ()
  "count++ when key `a-z' or `A-Z' press."
  (when (= (point) (point-min))
    (setq typepad-pyim-delete 0))
  (when (number-or-marker-p last-command-event)
    (when (and (>= last-command-event ?A)
            (<= last-command-event ?Z))
      (setq typepad-pyim-key-press (+ typepad-pyim-key-press 1)))
    (when (and (>= last-command-event ?a)
            (<= last-command-event ?z))
      (setq typepad-pyim-key-press (+ typepad-pyim-key-press 1)))))

(add-hook 'typepad-mode-hook
  (lambda ()
    (add-hook 'post-command-hook #'typepad-pyim-count-letter nil t)))

;; `FIXME' psearch?
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
         (setq typepad-pyim-key-press (- typepad-pyim-key-press 1)))) ;; 自动上屏时减去一次选择
   (t nil)))

(defun typepad-pyim-clear-bobp ()
  "当光标位于 buffer 的开头时, 清除 typepad-pyim-key-press"
  (when (and (= (point-min) (point-max))
          (= (point) (point-min)))
    (setq typepad-pyim-delete 0) ;; clear del
    (setq typepad-pyim-key-press 0)))

(add-hook 'typepad-mode-hook
  (lambda ()
    (add-hook 'post-command-hook #'typepad-pyim-clear-bobp nil t)))

(defun typepad-pyim-key-acc ()
  "计算键准"
  (if (and (= 0 typepad-pyim-delete)
        (= 0 typepad-pyim-key-press))
    1
    (let ((key-acc (- 1 (/ (float typepad-pyim-delete)
                        (float typepad-pyim-key-press)))))
    key-acc)))

(defun typepad-pyim-code-len (num)
  "计算码长"
  (let ((len (/ (float typepad-pyim-key-press)
               (/ num 2.000))))
    len))

(provide 'typepad-pyim)
;;; typepad-pyim.el ends here
