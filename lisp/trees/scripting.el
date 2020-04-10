;; do not enable lexical binding here.

;; enable calling emacs lisp scripts in running emacs server via 'elisp' script
(add-to-list 'interpreter-mode-alist '("elisp" . emacs-lisp-mode))

;; cf https://stackoverflow.com/questions/30568113/result-value-of-elisp-code-stored-in-a-file
(defun ns/eval-file (file &rest ns-args)
  "Execute FILE and return the result of the last expression."
  (eval
    ;; (ignore-errors
    (read-from-whole-string
      (concat "(progn "
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
        ")"))))

;; helper for unpacking args provided by eval-file
;; (ns/let-script-args (named named2) body)
(defmacro ns/let-script-args (args &rest content)
  `(let (,@(mapcar
             (fn (list (nth <> args)
                   (nth <> ns-args)))
             (number-sequence 0 (- (length ns-args) 1))))
     ,@content
     ))
