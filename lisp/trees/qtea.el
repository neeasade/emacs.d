;; vibed qtea stuff

(with-eval-after-load 'qml-ts-mode
  (define-key qml-ts-mode-map (kbd "C-c '") #'ns/qml-edit-qtea))

(defvar ns/qtea-edit-source-buffer nil
  "Source buffer for qtea editing.")

(defun ns/qtea--detect-indent (content)
  "Detect the common leading indent in CONTENT."
  (let ((lines (split-string content "\n"))
         (min-indent nil))
    (dolist (line lines)
      (when (string-match "^\\( +\\)" line)
        (let ((indent (length (match-string 1 line))))
          (when (or (null min-indent) (< indent min-indent))
            (setq min-indent indent)))))
    (or min-indent 0)))

(defun ns/qtea--remove-indent (content indent)
  "Remove INDENT spaces from the start of each line in CONTENT."
  (let ((regex (format "^%s" (make-string indent ?\s))))
    (replace-regexp-in-string regex "" content)))

(defun ns/qtea--add-indent (content indent)
  "Add INDENT spaces to the start of each line in CONTENT."
  (let ((prefix (make-string indent ?\s)))
    (replace-regexp-in-string "^" prefix content)))

(defun ns/qml-edit-qtea ()
  "Edit qtea block in a separate buffer with clojure-mode."
  (interactive)
  (save-excursion
    (let* ((start-re "/\\*\\s-*qtea")
            (end-re "\\*/")
            (start-pos (when (re-search-backward start-re nil t)
                         (forward-line 1)
                         (point)))
            (end-pos (when (and start-pos (re-search-forward end-re nil t))
                       (beginning-of-line)
                       (point))))
      (unless (and start-pos end-pos)
        (user-error "Not inside a qtea block"))
      (let* ((content (buffer-substring-no-properties start-pos end-pos))
              (indent (ns/qtea--detect-indent content))
              (dedented (ns/qtea--remove-indent content indent))
              (source-buf (current-buffer))
              (edit-buf (get-buffer-create "*qtea-edit*")))
        (with-current-buffer edit-buf
          (erase-buffer)
          (insert dedented)
          (clojure-mode)
          (setq-local ns/qtea-edit-source-buffer source-buf)
          (setq-local ns/qtea-edit-start start-pos)
          (setq-local ns/qtea-edit-end end-pos)
          (setq-local ns/qtea-edit-indent indent)
          (goto-char (point-min))
          (local-set-key (kbd "C-c '") #'ns/qtea-edit-finish)
          (local-set-key (kbd "C-c C-k") #'ns/qtea-edit-abort)
          (message "C-c C-c to save, C-c C-k to abort"))
        (switch-to-buffer-other-window edit-buf)))))

(defun ns/qtea-edit-finish ()
  "Save qtea edits back to source buffer and close."
  (interactive)
  (let* ((content (buffer-string))
          (indent ns/qtea-edit-indent)
          (indented (ns/qtea--add-indent content indent))
          (source-buf ns/qtea-edit-source-buffer)
          (start ns/qtea-edit-start)
          (end ns/qtea-edit-end))
    (unless (buffer-live-p source-buf)
      (user-error "Source buffer no longer exists"))
    (with-current-buffer source-buf
      (save-excursion
        (delete-region start end)
        (goto-char start)
        (insert indented)))
    (quit-window t)))

(defun ns/qtea-edit-abort ()
  "Abort qtea editing without saving."
  (interactive)
  (quit-window t))
