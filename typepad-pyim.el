;; init pyim--key-press-count
(defvar pyim--key-press-count 0)
;; advice pyim-self-insert-command, 用来计数每一次按键
(defun pyim-count-key (orig-fun &rest args)
  "conunt key press times."
  (setq pyim--key-press-count (1+ pyim--key-press-count))
  (apply orig-fun args))

(advice-add 'pyim-self-insert-command :around #'pyim-count-key)

(advice-add 'pyim-select-word :around #'pyim-count-key)

(advice-add 'pyim-select-word-by-number :around #'pyim-count-key)
;; advice pyim-delete-backward-char
(advice-add 'pyim-delete-backward-char :around #'pyim-count-key)
;; pyim-delete-forward-char
(advice-add 'pyim-delete-forward-char :around #'pyim-count-key)

;; just message pyim--key-press-count when post-self-insert-hook
(defun pyim--key-press-count-message ()
  "Message `pyim--key-press-count' when `post-self-insert-hook'."
  ;; conunt +1 and message
  (message "PYIM: %d 次按键" pyim--key-press-count))

(add-hook 'post-self-insert-hook #'pyim--key-press-count-message)
;; (remove-hook 'post-self-insert-hook #'pyim--key-press-count-message)

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

(add-hook 'post-command-hook #'pyim--key-press-count-clear-when-buffer-beg)
