;; defvar init-time
(defvar typepad-init-time 'nil)

;; defvar current-time
(defvar typepad-current-time 'nil)

;; defvar time-duration
(defvar typepad-time-duration 'nil)

;; defvar key-rate
(defvar typepad-key-rate 'nil)

;; start a timer when first typing in typepad, every 0.1 s
(defun typepad-start-timer ()
  (when (= pyim--key-press-count 1)
    (setq typepad-init-time (current-time))
    (typepad-timer-func)
    (message "typepad duration: %s" typepad-time-duration)
    (setq typepad-timer (run-with-idle-timer 0.1 t 'typepad-timer-func))))

;; (add-hook 'pre-command-hook 'typepad-start-timer)
;; (remove-hook 'pre-command-hook 'typepad-start-timer)
;; (remove-hook 'post-command-hook 'typepad-start-timer)
(defun typepad-timer-func ()
  (setq typepad-current-time (current-time))
  ;; time duration
  (setq typepad-time-duration (time-subtract typepad-current-time typepad-init-time))
  ;; calc key rate
  (setq typepad-key-rate (/ (float pyim--key-press-count) (float (time-to-seconds typepad-time-duration)))))

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
    ;; kill timer
    (cancel-timer typepad-timer)))

(add-hook 'post-command-hook 'typepad-time-clear)
