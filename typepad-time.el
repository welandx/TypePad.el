;; defvar init-time
(defvar typepad-init-time 'nil)

;; defvar current-time
(defvar typepad-current-time 'nil)

;; defvar time-duration
(defvar typepad-time-duration 'nil)

;; defvar key-rate
(defvar typepad-key-rate 0.0)

(defvar typepad-speed 'nil)

;; start a timer when first typing in typepad, every 0.1 s
(defun typepad-start-timer ()
  (when (= pyim--key-press-count 1)
    (setq typepad-init-time (current-time))
    (typepad-timer-func)
    (setq typepad-timer (run-with-idle-timer 0.1 t 'typepad-timer-func))))

;; start timer first
(add-hook 'typepad-mode-hook 'typepad-start-timer)

(defun typepad-timer-func ()
  (setq typepad-current-time (current-time))
  ;; time duration
  (setq typepad-time-duration (time-subtract typepad-current-time typepad-init-time))
  (let ((time (time-to-seconds typepad-time-duration))
         (words (/ (string-width (buffer-string)) 2.00)))
    (setq typepad-speed (* 60 (/ words time)))
    (setq typepad-key-rate (/ (float pyim--key-press-count) time))))

;; display key rate in mode line
(defun get-key-rate ()
  (format " %.3f keys/s" typepad-key-rate))

(add-hook 'typepad-mode-hook
  (lambda ()
    (setq-local mode-line-format (cons '(:eval (get-key-rate)) mode-line-format))))

(defun typepad-time-clear ()
  "Clear time when at begin of buffer"
  (when (= (point) (point-min))
    (setq typepad-init-time (current-time))
    (setq typepad-current-time (current-time))
    (setq typepad-time-duration 0)
    (setq typepad-key-rate 0.0)
    (setq typepad-speed 0)
    (setq tp-pyim-delete 0) ;; clear delete `FIXME' delete should be cleared when post-command
    ;; kill timer
    (cancel-timer typepad-timer)))

(add-hook 'typepad-mode-hook
  (lambda ()
    (add-hook 'post-self-insert-hook 'typepad-time-clear nil t)))


;;; typepad-time.el end here
(provide 'typepad-time)
