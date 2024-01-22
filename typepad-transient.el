;;; typepad-transient.el --- transient ui  -*- lexical-binding: t; -*-

(require 'transient)

(transient-define-suffix typepad-suffix ()
  "Prefix that waves with macro-defined suffix."
  :transient t
  :key "T"
  :description "wave from macro definition"
  (interactive)
  (message "?"))

(transient-define-prefix typepad-prefix ()
  "Prefix typepad"
  [""
    ["load"
      ""
      ("l" "载文" typepad-load)
      ("o" "继续发文" typepad-continue-send)
      ("q" "退出" typepad-exit)]
    ["set -- 载文之前设置"
      ""
      ("d" "短文单字" typepad-load-short);; `FIXME' make this suffix?
      ("c" "长文" typepad-load-long)
      ("s" "size" typepad-set-split)
      ("r" "乱序全文" typepad-toggle-random)]]
  [""
    ["goal"
    ("-" "击键目标-1" typepad-key-rate-) ;; `FIXME' make this suffix?
    ("=" "击键目标+1" typepad-key-rate+)
    ("tr" "开/关击键目标" typepad-toggle-rate-goal)
      ("ta" "开/关键准目标" typepad-toggle-acc-goal)]
    ["send"
      ("n" "next" typepad-send-next)
      ("p" "prev" typepad-send-prev)
      ("h" "nth" typepad-send-nth)
      ("b" "重打" typepad-resend)
      ("f" "继续" typepad-focus-return)]
    ]
  )

(provide 'typepad-transient)
;;; typepad-transient.el ends here
