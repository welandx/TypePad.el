;;; typepad-transient.el --- transient ui  -*- lexical-binding: t; -*-

(transient-define-suffix typepad-suffix ()
  "Prefix that waves with macro-defined suffix."
  :transient t
  :key "T"
  :description "wave from macro definition"
  (interactive)
  (message "Waves from a macro definition at: %s" (current-time-string)))

(transient-define-prefix typepad-prefix ()
  "Prefix typepad"
  [""
    ["load"
      ""
      ("l" "载文" typepad-load)]
    ["set -- 载文之前设置"
      ""
      ("d" "短文单字" typepad-load-short)
      ("c" "长文" typepad-load-long)
      ("s" "size" typepad-set-split)
      ("r" "乱序全文" typepad-load)]]
  [
      ("-" "击键目标-1" typepad-key-rate-)
    ("=" "击键目标+1" typepad-key-rate+)]
  )

(provide 'typepad-transient)
;;; typepad-transient.el ends here
