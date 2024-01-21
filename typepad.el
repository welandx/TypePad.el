;;; typepad.el --- typepad in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  welandx

;; Author: welandx <welandx@skiff.com>
;; URL: https://github.com/welandx/TypePad.el
;; Version: 0.2.3
;; Package-Requires: ((emacs "29.1") (pyim))
;; Keywords: typing

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; typepad in Emacs
;;

;;; Code:

(require 'typepad-lib)
(require 'typepad-pyim)
(require 'typepad-time)
(require 'sqlite)

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
  "The goal of key rate."
  :group 'typepad
  :type 'number)

(defcustom typepad-key-acc-goal 1
  "The goal of key acc"
  :group 'typepad
  :type 'number)

;; whether or not use key rate goal
(defcustom typepad-use-key-rate-goal nil
  "Whether or not use key rate goal."
  :group 'typepad
  :type 'boolean)

(defcustom typepad-use-key-acc-goal nil
  "Whether or not use key acc goal."
  :group 'typepad
  :type 'boolean)

;; total char num in the readonly buffer
(defvar typepad-char-num 20
  "Total char num in the readonly buffer.")

(defvar typepad-code-len 0
  "The code length of user input.")

(defvar typepad-key-acc nil)

(defvar typepad-short nil
  "short text.")

(defcustom typepad-text-path nil
  "path to text"
  :group 'typepad
  :type 'string)

(defcustom typepad-db-path (concat user-emacs-directory "typepad.db")
  "path to db"
  :group 'typepad
  :type 'string)

(defcustom typepad-split-size 10
  "每n个字符分隔一次"
  :group 'typepad
  :type 'integer)

(defcustom typepad-randomp nil
  "是否乱序全文"
  :group 'typepad
  :type 'boolean)

(defcustom typepad-auto-next nil
  "是否自动下一段"
  :group 'typepad
  :type 'boolean)

(defvar typepad-name "example"
  "当前发文的文章名")

(defvar typepad-article-list nil
  "所有的文章列表")

(defvar typepad-total-paragraph 1
  "当前发文的总段数")

(defvar typepad-current-paragraph 1
  "当前在发的段数")

(defvar typepad-current-hash nil)

(defvar typepad-article-hash nil)

(defvar typepad-indices '())

(defface typepad-diff-face
  '((t (:inherit diff-removed)))
  "Face for different text.")

(defface typepad-same-face
  '((t (:inherit diff-added)))
  "Face for same text.")

(defvar typepad-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f5>") #'typepad-resend)
    map)
  "typepad 的 keymap")

(define-derived-mode typepad-mode fundamental-mode
  "typepad"
  (setq-local visual-fill-column-center-text t)
  (setq-local face-remapping-alist '((default (:height 280)))))

;; define a mode for typepad readonly buffer
(define-derived-mode typepad-readonly-mode fundamental-mode
  "typepad-readonly"
  "Major mode for typepad readonly."
  (setq-local face-remapping-alist '((default (:height 280))))
  (setq-local visual-fill-column-center-text t)
  (setq-local cursor-type nil)
  (setq-local cursor-in-non-selected-windows nil))

(defun typepad-random-sending-text ()
  "Randomize the sending text."
  (interactive)
  (let ((sending-text-list (split-string sending-text "")))
    (key-quiz--shuffle-list sending-text-list)
    (setq sending-text (mapconcat 'identity sending-text-list ""))))

(defun typepad-random-all-text (txt)
  "Randomize the loaded article"
  (interactive)
  (let* ((txt-list (split-string txt "")))
    (setq typepad-indices (FY-shuffle-list txt-list))
    (typepad-sql-indices-save typepad-indices)
    (setq typepad-short (mapconcat 'identity txt-list ""))))

;;;###autoload
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

(defun typepad-re-window ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer readonly-buffer-name)
  (with-selected-window (split-window-below)
    (switch-to-buffer writable-buffer-name))
  (other-window 1))

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
             ;; (diff-range (point-max)) ;; `ISSUE' why?
             )
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

(defun check-standards (use-acc use-rate)
  "Check if the user has reached the goal."
  (let ((acc-passed t)
        (rate-passed t))
    (when use-acc
      (setq acc-passed (= typepad-key-acc 1)))
    (when use-rate
      (setq rate-passed (> (typepad-get-key-rate) typepad-key-rate-goal)))
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
        (typepad-timer-func)
        (typepad-time-end)
        (setq typepad-key-acc (typepad-pyim-key-acc))
        (setq typepad-code-len (typepad-pyim-code-len typepad-char-num))
        (message "第 %d 段 速度: %.2f 键准: %.2f%% 击键: %.3f 码长: %.3f [%s] %d/%d"
          typepad-current-paragraph
          (typepad-get-speed)
          (* typepad-key-acc 100)
          (typepad-get-key-rate)
          typepad-code-len
          typepad-name
          typepad-current-paragraph
          typepad-total-paragraph)
        (typepad-hash)
        (typepad-sql-stat)
        (if (check-standards typepad-use-key-acc-goal
              typepad-use-key-rate-goal)
            (progn
              (typepad-diff)
              (erase-buffer)
              (if typepad-auto-next
                (typepad-send-next)
                (other-window 1)))
            (progn
              (erase-buffer)
              (typepad-random-sending-text)
              (typepad-redraw-readonly-buffer)))
        ))))

(add-hook 'typepad-mode-hook
          (lambda ()
            (add-hook 'post-self-insert-hook 'typepad-paragraph-end nil t)))

(defun typepad-load-long ()
  "设置标准为长文模式"
  (interactive)
  (setq typepad-auto-next t)
  (setq typepad-split-size 100)
  (setq typepad-use-key-acc-goal nil)
  (setq typepad-use-key-rate-goal nil)
  (setq typepad-randomp nil))

(defun typepad-load-short ()
  "设置标准为短文模式"
  (interactive)
  (setq typepad-auto-next nil)
  (setq typepad-split-size 10)
  (setq typepad-use-key-acc-goal t)
  (setq typepad-use-key-rate-goal t)
  (setq typepad-randomp t))

(defun typepad-key-rate+ ()
  (interactive)
  (setq typepad-key-rate-goal (1+ typepad-key-rate-goal)))

(defun typepad-key-rate- ()
  (interactive)
  (setq typepad-key-rate-goal (1- typepad-key-rate-goal)))

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
      (setq short-text (typepad-random-all-text short-text))
      (typepad-sql-indices-save '(1)))
    (setq typepad-short (split-string-every short-text typepad-split-size))
    (setq typepad-total-paragraph (length typepad-short))
    (typepad-sql-article)
    (typepad-send-text)))

(defun typepad-set-split ()
  "设置每段字数"
  (interactive)
  (let ((n (read-number "请输入每段字数: ")))
    (setq typepad-split-size n)))

;; `FIXME' load-dir
;;;###autoload
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
  (if (string= (buffer-name) readonly-buffer-name)
    (other-window 1))
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
  (if (string= (buffer-name) readonly-buffer-name)
    (other-window 1))
  (let ((current typepad-current-paragraph))
    (if (eq current 1)
      (message "当前已是第一段")
      (progn
        (setq typepad-current-paragraph (1- current))
        (typepad-send-text)
        ))))

(defun typepad-resend ()
  (interactive)
  (with-current-buffer writable-buffer-name
    (if (/= (point-min) (point-max))
      (progn
        (read-only-mode -1)
        (erase-buffer)
        (typepad-time-clear)  )
      (message "未开始"))))

(defun typepad-send-nth ()
  "发第n段"
  (interactive)
  (if (string= (buffer-name) readonly-buffer-name)
    (other-window 1))
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
    (setq typepad-article-hash hash)
    hash))

;;;###autoload
(defun typepad-create-sqlite ()
  "Create sqlite database."
  (unless (file-exists-p typepad-db-path)
    (let ((tp-db (sqlite-open  typepad-db-path)))
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
    (sqlite-execute tp-db
      (concat
        "CREATE TABLE IF NOT EXISTS sort "
        "(hash TEXT, randomp INTEGER, indices TEXT DEFAULT 'nil');"))
    (sqlite-close tp-db))))

(defun typepad-sql-stat ()
  "Save statistics to sqlite."
  (let ((tp-db (sqlite-open  typepad-db-path)))
    (sqlite-execute tp-db
      (concat "INSERT INTO statistics (hash, ArticleHash, KeyRate, "
        "speed, KeyAcc, CodeLen, Paragraph, DEL, Time) VALUES "
        (format "('%s', '%s', %f, %f, %f, %f, %d, %d, %f);"
          typepad-current-hash typepad-article-hash
          (typepad-get-key-rate) (typepad-get-speed) typepad-key-acc
          typepad-code-len typepad-current-paragraph (typepad-get-del)
          (time-to-seconds (typepad-get-duration)))))
    (sqlite-close tp-db)))

(defun typepad-sql-article ()
  "Save article to sqlite."
  (let* ((tp-db (sqlite-open  typepad-db-path))
          (hashp (sqlite-execute tp-db
                   (format "SELECT COUNT(*) FROM article WHERE hash = '%s'" typepad-article-hash)))
          (val (car (car hashp))))
    (if (= val 0) (sqlite-execute tp-db
                (concat "INSERT INTO article (hash, name, length, split) VALUES "
                  (format "('%s', '%s', %d, %d);" typepad-article-hash typepad-name
                    typepad-total-paragraph typepad-split-size))))
    (sqlite-close tp-db)))

(defun typepad-sql-indices-save (indices)
  (let* ((serialized (prin1-to-string indices))
          (encoded (base64-encode-string serialized))
          (tp-db (sqlite-open  typepad-db-path)))
    (sqlite-execute tp-db
      (format "INSERT INTO sort (hash, randomp, indices) VALUES ('%s', %d , '%s')"
        typepad-article-hash (if typepad-randomp 1 0) encoded)
      )))

(defun typepad-continue-send ()
  (interactive)
  (if typepad-article-list
    (progn
      (let* ((article (alt-completing-read "Choose an article: " typepad-article-list))
             (path (nth 1 article))
             (text (with-temp-buffer
                      (insert-file-contents path)
                     (buffer-string)))
              (hash (typepad-hash-article text))
              (tp-db (sqlite-open  typepad-db-path))
              (result (sqlite-execute tp-db
                        (format "SELECT randomp, indices FROM sort
WHERE hash='%s' ORDER BY rowid DESC LIMIT 1;" hash)))
              (n (sqlite-execute tp-db
                       (format "SELECT Paragraph FROM statistics WHERE ArticleHash ='%s'
ORDER BY id DESC LIMIT 1;" hash))))
        (setq typepad-name (nth 0 article))
        (let ((row (car result)))
          ;; (message "%s" (elt row 0)) ;debug
          ;; (message "%s" (elt row 1))
          (if (eq (elt row 0) 1)
            (progn
              (setq typepad-indices (typepad-decode (elt row 1)))
              (typepad-re-article text))
            (progn
              (setq typepad-short (split-string-every text typepad-split-size))
              (setq typepad-total-paragraph (length typepad-short)))))
        (let ((para (car (car n))))
          (if (> para 0)
            (progn
              (setq typepad-current-paragraph para)
              (typepad-send-next))
            (message "无记录")))
        (sqlite-close tp-db)
        ))
    (progn
      (message "load")
      (typepad-load-dir))))

(defun typepad-re-article (text)
  (let* ((text-list (split-string text "")))
    (FY-reproduce-shuffle text-list typepad-indices)
    (let ((all (mapconcat 'identity text-list "")))
      (setq typepad-short (split-string-every all typepad-split-size))
      (setq typepad-total-paragraph (length typepad-short)))))

(defun typepad-decode (ind)
  (read (base64-decode-string ind)))

(let ((buf writable-buffer-name)
       (r-buf readonly-buffer-name))
  (add-hook 'typepad-mode-hook
    (lambda ()
      (buffer-focus-out-callback (lambda () (typepad-focus-out buf r-buf)))
      (add-function :after after-focus-change-function
        (lambda () (typepad-focus-out buf r-buf))))))

(defun typepad-print (name split)
  (interactive)
  (let* ((tp-db (sqlite-open typepad-db-path))
          (res (sqlite-execute tp-db
                 (format "SELECT s.* FROM statistics s 
JOIN article a ON s.ArticleHash = a.hash 
WHERE a.name = '%s' AND a.split = %d;
" name split))))
    ))

(defun typepad-exit ()
  (interactive)
  (delete-other-windows)
  (when (get-buffer readonly-buffer-name)
    (kill-buffer readonly-buffer-name))
  (when (get-buffer writable-buffer-name)
    (kill-buffer writable-buffer-name)))

;;;###autoload
(defun typepad ()
  (interactive)
  (typepad-load-dir)
  (typepad-create-sqlite)
  (if (and (buffer-live-p (get-buffer readonly-buffer-name))
        (buffer-live-p (get-buffer writable-buffer-name)))
    (typepad-re-window)
    (typepad-create-window)))


(provide 'typepad)
;;; typepad.el ends here
