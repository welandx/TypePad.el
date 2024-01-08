  ;; init pyim--key-press-count
  (defvar pyim--key-press-count 0)
  ;; advice pyim-self-insert-command, 用来计数每一次按键
  (defun pyim-self-insert-command-count (orig-fun &rest args)
    "Advice `pyim-self-insert-command' to count key press times."
    (setq pyim--key-press-count (1+ pyim--key-press-count))
    (apply orig-fun args))
  (advice-add 'pyim-self-insert-command :around #'pyim-self-insert-command-count)

  ;; message pyim--key-press-count when post-command-hook
  (defun pyim--key-press-count-message ()
    "Message `pyim--key-press-count' when `post-command-hook'."
    ;; if this command is self-insert-command, conunt +1 and message
    (when (eq this-command 'self-insert-command)
        (setq pyim--key-press-count (1+ pyim--key-press-count))
        (message "PYIM: %d 次按键" pyim--key-press-count)))

(add-hook 'post-command-hook #'pyim--key-press-count-message)
(remove-hook 'post-command-hook #'pyim--key-press-count-message)
  ;; clear pyim--key-press-count when pyim-process-terminate
  (defun pyim--key-press-count-clear ()
    (interactive)
    "Clear `pyim--key-press-count' when `pyim-process-terminate'."
    (setq pyim--key-press-count 0))
