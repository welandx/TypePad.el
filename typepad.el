(require 'typepad-lib)

(defgroup typepad ()
  "Customize group for typepad, a typing practice tool."
  :group 'extensions)

(defvar readonly-buffer-name "*发文区*"
  "The name of readonly buffer.")

(defvar writable-buffer-name "*跟打区*"
  "The name of writable buffer.")

(defvar sending-text "不者时今青任尽代形机"
  "The char read from readonly buffer.")

(defcustom typepad-key-rate-goal 4.00
  "The goal of key rate.")

(defcustom typepad-key-acc-goal 1
  "The goal of key acc")

;; whether or not use key rate goal
(defcustom typepad-use-key-rate-goal t
  "Whether or not use key rate goal.")

(defcustom typepad-use-key-acc-goal t
  "Whether or not use key acc goal.")

;; total char num in the readonly buffer
(defvar typepad-char-num 20
  "Total char num in the readonly buffer.")

(defvar typepad-code-len 0
  "The code length of user input.")

(defvar typepad-key-acc nil)

(defvar typepad-short nil
  "short text.")

(defcustom typepad-text-path nil
  "path to text")

(defcustom typepad-split-size 10
  "每n个字符分隔一次")

(defcustom typepad-randomp t
  "是否乱序全文")

(defcustom typepad-auto-next nil
  "是否自动下一段")

(defvar typepad-name "example"
  "当前发文的文章名")

(defvar typepad-article-list nil
  "所有的文章列表")

(defvar typepad-total-paragraph 1
  "当前发文的总段数")

(defvar typepad-current-paragraph 1
  "当前在发的段数")

(defvar typepad-current-hash nil)

(defvar tp-article-hash nil)

(defface typepad-diff-face
  '((t (:background "red")))
  "Face for different text.")

(defface typepad-same-face
  '((t (:background "green")))
  "Face for same text.")

(define-derived-mode typepad-mode fundamental-mode
  "typepad"
  "Major mode for typepad."
  (setq-local cursor-type 'bar)
  (setq-local visual-fill-column-center-text t)
  ;; set font height local
  (setq-local face-remapping-alist '((default (:height 280)))))

;; define a mode for typepad readonly buffer
(define-derived-mode typepad-readonly-mode fundamental-mode
  "typepad-readonly"
  "Major mode for typepad readonly."
  (setq-local face-remapping-alist '((default (:height 280))))
  (setq-local visual-fill-column-center-text t)
  (setq-local cursor-type nil)
  (setq-local cursor-in-non-selected-windows nil))

(defun random-sending-text ()
  "Randomize the sending text."
  (interactive)
  (let ((sending-text-list (split-string sending-text "")))
    (key-quiz--shuffle-list sending-text-list)
    (setq sending-text (mapconcat 'identity sending-text-list ""))))

(defun random-all-text (txt)
  "Randomize the loaded article"
  (interactive)
  (let ((txt-list (split-string txt "")))
    (key-quiz--shuffle-list txt-list)
    (setq typepad-short (mapconcat 'identity txt-list ""))))

(defun typepad-create-window ()
  "initialize window layout"
  (interactive)
  (delete-other-windows)
  (let ((readonly-text sending-text))
    (when (get-buffer readonly-buffer-name)
      (kill-buffer readonly-buffer-name))
    (when (get-buffer writable-buffer-name)
      (kill-buffer writable-buffer-name))
    (with-selected-window (split-window-below)
      (switch-to-buffer (get-buffer-create writable-buffer-name))
      (typepad-mode))
    (with-current-buffer (get-buffer-create readonly-buffer-name)
      (erase-buffer)
      (insert readonly-text)
      (goto-char (point-min))
      ;; (setq buffer-read-only t)
      (read-only-mode 1)
      (switch-to-buffer readonly-buffer-name)
      ;; (org-switch-to-buffer-other-window readonly-buffer-name)
      (typepad-readonly-mode)
      (other-window 1))))

;; `FIXME' def overlay for diff highlight
(defun typepad-diff ()
  "Highlight the diff between readonly buffer and writable buffer."
  (interactive)
  (with-current-buffer writable-buffer-name
    (save-excursion
      (goto-char (point-min))
      (let ((input-list (split-string
                          (buffer-substring-no-properties
                            (point-min) (point-max)) "" t))
             (diff-range (point-max))) ;; `ISSUE' why?
        (with-current-buffer readonly-buffer-name
          (save-excursion
            (remove-overlays)
            (goto-char (point-min))
            (let ((readonly-char (char-after)))
              (cl-loop for writable-char in input-list
                do (progn
                     ;; convert writable-char to ASCII
                     (setq writable-char (string-to-char writable-char))
                     (if (not (equal writable-char readonly-char)) ;; compare char
                       (let ((overlay (make-overlay (point) (1+ (point)))))
                         (overlay-put overlay 'face 'typepad-diff-face))
                       (let ((overlay (make-overlay (point) (1+ (point)))))
                         (overlay-put overlay 'face 'typepad-same-face)))
                     (forward-char)
                     (setq readonly-char (char-after)))
                until (eobp)))))))))

(add-hook 'typepad-mode-hook
          (lambda ()
            (add-hook 'post-command-hook 'typepad-diff nil t)))

(defun check-standards (use-acc use-rate acc rate)
  "Check if the user has reached the goal."
  (let ((acc-passed t)
        (rate-passed t))
    (when use-acc
      (setq acc-passed (= typepad-key-acc 1)))
    (when use-rate
      (setq rate-passed (> typepad-key-rate typepad-key-rate-goal)))
    (and acc-passed rate-passed)))

;; redraw readonly buffer
(defun typepad-redraw-readonly-buffer ()
  "Redraw readonly buffer."
  (interactive)
  (with-current-buffer readonly-buffer-name
    (read-only-mode -1)
    (erase-buffer)
    (insert sending-text)
    (setq typepad-char-num (string-width (buffer-string)))
    (read-only-mode 1)))

;; `FIXME'
(defun typepad-paragraph-end ()
  "End of paragraph"
  (let ((input-text (buffer-string))
         (last-char (char-before))
         (last-readonly (string-to-char (substring sending-text -1))))
    (if (and (equal typepad-char-num (string-width input-text))
          (equal last-char last-readonly))
      (progn
        (message "第 %d 段 速度: %.2f 键准: %.2f%% 击键: %.3f 码长: %.3f [%s] %d/%d"
          typepad-current-paragraph
          typepad-speed
          (* (typepad-calc-key-acc) 100)
          typepad-key-rate
          (typepad-calc-code-len)
          typepad-name
          typepad-current-paragraph
          typepad-total-paragraph)
        (typepad-hash)
        (typepad-sql-stat)
        ;; (tp-save-to-sql)
        (if (check-standards typepad-use-key-acc-goal
              typepad-use-key-rate-goal
              typepad-key-acc-goal typepad-key-rate-goal)
            (progn
              (typepad-diff)
              (erase-buffer)
              (if typepad-auto-next
                (typepad-send-next)
                (other-window 1)))
            (progn
              (erase-buffer)
              (random-sending-text)
              (typepad-redraw-readonly-buffer)))
        ))))

(add-hook 'typepad-mode-hook
          (lambda ()
            (add-hook 'post-self-insert-hook 'typepad-paragraph-end nil t)))

(defun typepad-calc-code-len ()
  "计算码长"
  (setq typepad-code-len
    (/ (float pyim--key-press-count)
      (/ typepad-char-num 2.000)))
  typepad-code-len)

(defun typepad-calc-key-acc ()
  "计算键准"
  (setq typepad-key-acc
    (- 1 (/ (float tp-pyim-delete)
      (float pyim--key-press-count))))
  typepad-key-acc)

(defun tp-load-long ()
  "设置标准为长文模式"
  (interactive)
  (setq typepad-auto-next t)
  (setq typepad-split-size 100)
  (setq typepad-use-key-acc-goal nil)
  (setq typepad-use-key-rate-goal nil)
  (setq typepad-randomp nil))

(defun tp-load-short ()
  "设置标准为短文模式"
  (interactive)
  (setq typepad-auto-next nil)
  (setq typepad-split-size 10)
  (setq typepad-use-key-acc-goal t)
  (setq typepad-use-key-rate-goal t)
  (setq typepad-key-rate-goal 6.00)
  (setq typepad-randomp t))

(defun typepad-load-dir ()
  "Load all txt files in the directory."
  (interactive)
  (if typepad-text-path
    (setq typepad-article-list (get-txt-file-details typepad-text-path))
    (message "未设置 typepad-text-path")))

(defun typepad-load-short-text (path)
  "Load short text from the file."
  (let ((short-text (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-string))))
    (typepad-hash-article short-text)
    (if typepad-randomp
      (setq short-text (random-all-text short-text)))
    (setq typepad-short (split-string-every short-text typepad-split-size))
    (setq typepad-total-paragraph (length typepad-short))
    (typepad-sql-article)
    (typepad-send-text)))

(defun tp-set-split ()
  "设置每段字数"
  (interactive)
  (let ((n (read-number "请输入每段字数: ")))
    (setq typepad-split-size n)))

;; `FIXME' load-dir
(defun typepad-load ()
  "选择文章"
  (interactive)
  (if typepad-article-list
    (progn
      (setq typepad-current-paragraph 1)
    (let ((article (alt-completing-read "Choose an article: " typepad-article-list)))
      (setq typepad-name (car article))
      (typepad-load-short-text (nth 1 article))))
    (progn
      (message "load")
      (typepad-load-dir))))


;; send text
(defun typepad-send-text ()
  "发文"
  (interactive)
  (setq sending-text (nth (- typepad-current-paragraph 1) typepad-short))
  (typepad-redraw-readonly-buffer))

(defun typepad-send-next ()
  "发下一段"
  (interactive)
  (let ((current typepad-current-paragraph))
    (if (<= typepad-total-paragraph current)
      (message "当前已是最后一段")
      (progn
        (setq typepad-current-paragraph (1+ current))
        (typepad-send-text)
        ))))

(defun typepad-send-prev ()
  "发上一段"
  (interactive)
  (let ((current typepad-current-paragraph))
    (if (eq current 1)
      (message "当前已是第一段")
      (progn
        (setq typepad-current-paragraph (1- current))
        (typepad-send-text)
        ))))

(defun typepad-send-nth ()
  "发第n段"
  (interactive)
  (let ((number (read-number "请输入段数: ")))
    (setq typepad-current-paragraph number)
    (typepad-send-text)))

(defun typepad-hash ()
  "计算当前段的hash值"
  (let* ((current-time (current-time))
          (timestamp (format-time-string "%Y-%m-%d %H:%M:%S" current-time))
          (num (number-to-string typepad-current-paragraph))
          (input (concat typepad-name num timestamp))
          (hash (secure-hash 'sha256 input)))
    (setq typepad-current-hash hash)))

(defun typepad-hash-article (text)
  "计算全文的hash值"
  (let* ((size typepad-split-size)
          (input (concat (string size) text))
          (hash (secure-hash 'sha256 input)))
    (setq tp-article-hash hash)))

;;; sqlite
(require 'sqlite)

(defun typepad-create-sqlite ()
  "Create sqlite database."
  (let ((tp-db (sqlite-open (concat user-emacs-directory "typepad.db")))
         (time (time-to-seconds typepad-time-duration)))
    (sqlite-execute tp-db
      (concat
        "CREATE TABLE IF NOT EXISTS article "
        "(id INTEGER PRIMARY KEY, hash TEXT, name TEXT, length INTEGER, split INTEGER);"))
    (sqlite-execute tp-db
      (concat
        "CREATE TABLE IF NOT EXISTS statistics "
        "(hash TEXT, id INTEGER PRIMARY KEY, ArticleHash TEXT, KeyRate REAL, speed REAL, "
        "KeyAcc REAL, CodeLen REAL, Paragraph INTEGER, DEL INTEGER, "
        "Time REAL, Date DATETIME DEFAULT CURRENT_TIMESTAMP);"
        ))
    (sqlite-close tp-db)))

(defun typepad-sql-stat ()
  "Save statistics to sqlite."
  (let ((tp-db (sqlite-open (concat user-emacs-directory "typepad.db"))))
    (sqlite-execute tp-db
      (concat "INSERT INTO statistics (hash, ArticleHash, KeyRate, "
        "speed, KeyAcc, CodeLen, Paragraph, DEL, Time) VALUES "
        (format "('%s', '%s', %f, %f, %f, %f, %d, %d, %f);"
          typepad-current-hash tp-article-hash
          typepad-key-rate typepad-speed typepad-key-acc
          typepad-code-len typepad-current-paragraph tp-pyim-delete
          (time-to-seconds typepad-time-duration))))
    (sqlite-close tp-db)))

(defun typepad-sql-article ()
  "Save article to sqlite."
  (let* ((tp-db (sqlite-open (concat user-emacs-directory "typepad.db")))
          (hashp (sqlite-execute tp-db
                   (format "SELECT COUNT(*) FROM article WHERE hash = '%s'" tp-article-hash)))
          (val (car (car hashp))))
    (if (= val 0) (sqlite-execute tp-db
                (concat "INSERT INTO article (hash, name, length, split) VALUES "
                  (format "('%s', '%s', %d, %d);" tp-article-hash typepad-name
                    typepad-total-paragraph typepad-split-size))))
    (sqlite-close tp-db)))

;; load other modules
(require 'typepad-pyim)
(require 'typepad-time)

(provide 'typepad)
;;; typepad.el end here
