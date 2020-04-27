;; enable calling emacs lisp scripts in running emacs server via 'elisp' script
;; NOTE: don't enable lexical binding here, the ns/let-script-args will break

(add-to-list 'interpreter-mode-alist '("elisp" . emacs-lisp-mode))

;; cf https://stackoverflow.com/questions/30568113/result-value-of-elisp-code-stored-in-a-file
(defun ns/eval-file (file *stdin-file* &rest ns-args)
  "Execute FILE and return the result of the last expression."
  (->> file f-read (format "(progn %s)") read eval))

;; helper for unpacking args provided by eval-file
;; use: (ns/let-script-args (named named2) body)
(defmacro ns/let-script-args (args &rest content)
  `(let (,@(mapcar
             (fn (list (nth <> args)
                   (nth <> ns-args)))
             (number-sequence 0 (- (length ns-args) 1))))
     ,@content
     ))

