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

;; when key DEL press, count
(defun pyim--key-press-count-clear-when-del ()
  "Clear `pyim--key-press-count' when key DEL press."
  (when (eq last-command-event ?\d)
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
;;; dedededfdjaffdededededdededededdededededede
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
(add-hook 'post-self-insert-hook #'pyim--key-press-count-message)

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
