;;; typepad-time.el --- timer -*- lexical-binding: t; -*-

(defvar typepad-init-time 'nil
  "输入的开始时间")

(defvar typepad-current-time 'nil
  "当前时间")

(defvar typepad-time-duration 'nil
  "输入时间")

(defvar typepad-key-rate 0.0
  "击键速度, 每秒击键数")

(defvar typepad-speed '0.0
  "输入速度, 每分钟输入的字数")

(defvar typepad-timer 'nil
  "计时器")

(defun typepad-get-key-rate ()
  typepad-key-rate)

(defun typepad-get-speed ()
  typepad-speed)

(defun typepad-get-duration ()
  typepad-time-duration)

(declare-function typepad-get-key () "get key")

;; `FIXME' timer should start if no timer start
;; `Note' (cancel-function-timers 'typepad-timer-func) can cancel all timer
(defun typepad-start-timer ()
  "Start timer when typepad-mode is enabled"
  (when (= (typepad-get-key) 1)
    (setq typepad-init-time (current-time))
    (typepad-timer-func)
    (unless typepad-timer
      (setq typepad-timer (run-at-time t 0.6 'typepad-timer-func)))
    ;; (unless (bound-and-true-p typepad-timer)
    ;;   (setq typepad-timer (timer-create))
    ;;   (timer-set-function typepad-timer 'typepad-timer-func)
    ;;   (timer-set-time typepad-timer '(0.1 repeat))
    ;;   (timer-activate typepad-timer))
    ))

(add-hook 'typepad-mode-hook 'typepad-start-timer) ;; `FIXME' seems not used

(defun typepad-timer-func ()
  "时间计算"
  (setq typepad-current-time (current-time))
  ;; time duration
  (setq typepad-time-duration (time-subtract typepad-current-time typepad-init-time))
  (let ((time (time-to-seconds typepad-time-duration))
         (words (/ (string-width (buffer-string)) 2.00)))
    (setq typepad-speed (* 60 (/ words time)))
    (setq typepad-key-rate (/ (float (typepad-get-key)) time)))
  (force-mode-line-update t))

(defface typepad-mode-line-face
  '((t (:box "grey"
         :inherit highlight)))
  "face for typepad mode-line info")

;; display key rate in mode line
(defun get-key-rate ()
  "Get key rate"
  (format " 击键: %.3f 速度: %.2f " typepad-key-rate typepad-speed))

(add-hook 'typepad-readonly-mode-hook
  (lambda ()
    (setq-local mode-line-format
      (cons '(:eval
               (propertize (get-key-rate)
                 'face 'typepad-mode-line-face))
        mode-line-format))))

(defun typepad-time-clear ()
  "Clear time when at begin of buffer"
  (when (and (= (point-min) (point-max))
          (= (point) (point-min)))
    (setq typepad-init-time (current-time))
    (setq typepad-current-time (current-time))
    (setq typepad-time-duration 0)
    (setq typepad-key-rate 0.0)
    (setq typepad-speed 0)
    ;; kill timer
    (cancel-function-timers 'typepad-timer-func)))

;; `FIXME' migrate above
(defun typepad-time-end ()
  (setq typepad-time-duration 0)
  (setq typepad-key-rate 0.0)
  (setq typepad-speed 0)
  (cancel-function-timers 'typepad-timer-func))

(add-hook 'typepad-mode-hook
  (lambda ()
    (add-hook 'post-self-insert-hook 'typepad-time-clear nil t)))

(defun typepad-focus-out (buf r-buf)
  (if (and (buffer-live-p (get-buffer buf))
        (buffer-live-p (get-buffer r-buf)))
    (let ((r-point-max (with-current-buffer r-buf
                         (point-max))))
      (unless (or (= (point) (point-min))
                (= (point) r-point-max)) ;; `ISSUE' ?
        (when typepad-timer
          (typepad-timer-func)
          (cancel-function-timers 'typepad-timer-func)
          (with-current-buffer buf
            (read-only-mode 1))
          (setq typepad-timer nil))))))

(defun typepad-focus-return ()
  (interactive)
  (setq typepad-init-time (time-subtract (current-time) typepad-time-duration))
  (unless typepad-timer
    (setq typepad-timer (run-at-time t 0.6 'typepad-timer-func)))
  (read-only-mode -1))


(provide 'typepad-time)
;;; typepad-time.el ends here
