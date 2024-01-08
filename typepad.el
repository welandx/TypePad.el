(let ((readonly-buffer-name "*发文区*")
      (writable-buffer-name "*跟打区*")
      (readonly-text "这是一个中文示例文本。
这是第二行。
这是第三行。"))
  (with-selected-window (split-window-right)
    (switch-to-buffer (get-buffer-create writable-buffer-name)))
  (with-current-buffer (get-buffer-create readonly-buffer-name)
    (setq buffer-read-only t)
    (erase-buffer)
    (insert readonly-text)
    (goto-char (point-min))
    (read-only-mode 1)
    (switch-to-buffer-other-window readonly-buffer-name)))
