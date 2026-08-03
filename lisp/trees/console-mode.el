(defface ns/console-prompt-face
  '((t :inherit comint-highlight-prompt))
  "Face used for shell prompts in `console-mode'.")

(defface ns/console-output-face
  '((t :inherit default))
  "Face used for command output in `console-mode'.")



;; (add-hook 'ns/theme-hook)

(comment
  (ns/face 'ns/console-output-face
    :foreground
    (myron-get :alt))

  )

(defvar ns/console--shell-face-cache (make-hash-table :test 'equal))

(defun ns/console--shell-faces (command)
  (or (gethash command ns/console--shell-face-cache)
    (puthash command
      (with-temp-buffer
        (insert command)
        (sh-mode)
        (font-lock-ensure)
        (let ((point (point-min))
               faces)
          (while (< point (point-max))
            (let ((next-point (or (next-single-property-change point 'face nil (point-max))
                                (point-max)))
                   (face (get-text-property point 'face)))
              (when face
                (push (list point next-point face) faces))
              (setq point next-point)))
          (nreverse faces)))
      ns/console--shell-face-cache)))

(defun ns/console-unfontify-region (start end &optional _loudly)
  (with-silent-modifications
    (remove-list-of-text-properties start end '(face font-lock-face))))

(defun ns/console-fontify-region (start end &optional _loudly)
  (save-excursion
    (save-match-data
      (let ((start (save-excursion (goto-char start) (line-beginning-position)))
             (end (save-excursion (goto-char end) (line-end-position))))
        (ns/console-unfontify-region start end)
        (goto-char start)
        (while (< (point) end)
          (let ((line-start (line-beginning-position))
                (line-end (line-end-position)))
            (if (looking-at "^\\([$!]\\)\\(?: \\(.*\\)\\)?$")
                (progn
                  (put-text-property (match-beginning 1) (match-end 1) 'face 'ns/console-prompt-face)
                  (-when-let (command (match-string-no-properties 2))
                    (let ((command-start (match-beginning 2)))
                      (dolist (shell-face (ns/console--shell-faces command))
                        (pcase-let ((`(,face-start ,face-end ,face) shell-face))
                          (put-text-property (+ command-start (1- face-start))
                                             (+ command-start (1- face-end))
                                             'face face))))))
              (put-text-property line-start (min (1+ line-end) (point-max))
                                 'face 'ns/console-output-face)))
          (forward-line 1))))))

(define-derived-mode console-mode fundamental-mode "Console"
  "Major mode for console transcript source blocks."
  (setq-local font-lock-defaults '(nil))
  (setq-local font-lock-fontify-region-function #'ns/console-fontify-region)
  (setq-local font-lock-unfontify-region-function #'ns/console-unfontify-region))

(add-to-list 'org-src-lang-modes '("console" . console))
