(defmacro neeasade/shell-exec(command)
  "trim the newline from shell exec"
  `(replace-regexp-in-string "\n$" ""
     (shell-command-to-string ,command)))

(setq
  sys/windows? (eq system-type 'windows-nt)
  sys/linux? (eq system-type 'gnu/linux)
  enable-tp? sys/windows?
  neeasade/home? (string= (neeasade/shell-exec "hostname") "erasmus")
  )

;; docker container user, still act trimmed/assume windows
(if (string= (getenv "USER") "emacser")
  (setq
    sys/windows? t
    sys/linux? nil
    enable-tp? t
    ))

;; todo: on windows this should be USERPROFILE
(defun neeasade/homefile (path)
  (concat (getenv "HOME") "/" path)
  )

(let ((extend-file (neeasade/homefile "extend.el")))
  (when (file-exists-p extend-file)
    (eval-and-compile (load extend-file))
    )
  )

(defun mapcar* (f &rest xs)
  "MAPCAR for multiple sequences F XS."
  (if (not (memq nil xs))
    (cons (apply f (mapcar 'car xs))
      (apply 'mapcar* f (mapcar 'cdr xs)))))

;; setq namespace
(defmacro setq-ns (namespace &rest lst)
  (require 'seq)
  `(mapcar*
     (lambda (pair)
       (let ((key (car pair))
              (value (car (cdr pair))))
         (set
           (intern (concat (prin1-to-string ',namespace) "-" (prin1-to-string key)))
           (eval value)
           )))
     (seq-partition ',lst 2)
     ))

(defun get-resource (name)
  "Get X resource value, with a fallback value NAME."
  (let* (
          (xrdb-fallback-values
            ;; for when we're away from $HOME.
            '(
               ("Emacs.theme"          . "base16-grayscale-light")
               ("Emacs.powerlinescale" . "1.1")
               ("st.font"              . "Go Mono-10")
               ("st.borderpx"          . "15")
               ("emacs.powerline"      . "bar")
               ("*.background"         . (face-attribute 'default :background))
               ))
          (default (eval (cdr (assoc name xrdb-fallback-values))))
          )
    (if (executable-find "xrq")
      (let ((result
              ;; shell-command-to-string appends newline
              (neeasade/shell-exec (concat "xrq '" name "' 2>/dev/null"))
              ))
        (if (string= result "")
          ;; we didn't find it in xrdb.
          default
          result
          ))
      default
      )))

;; wrap passwordstore
(defun pass (key)
  (neeasade/shell-exec
    (if sys/windows?
      (concat "pprint.bat " key)
      (concat "pass " key " 2>/dev/null"))
    )
  )

(defun reload-init()
  "Reload init.el."
  (interactive)
  (straight-transaction
    (straight-mark-transaction-as-init)
    (message "Reloading init.el...")
    (load user-init-file nil 'nomessage)
    (message "Reloading init.el... done.")))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; ref: https://emacs.stackexchange.com/questions/3197/best-way-to-retrieve-values-in-nested-assoc-lists
(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

;; binding wrappers
(defun neeasade/bind (&rest binds)
  (apply 'general-define-key
    :states '(normal visual)
    :prefix "SPC"
    binds))

(defun neeasade/bind-mode(keymaps &rest binds)
  (apply 'general-define-key
    :prefix "SPC"
    :states '(visual normal)
    :keymaps keymaps
    binds))

(defun neeasade/bind-leader-mode(mode &rest binds)
  (apply 'general-define-key
    :prefix ","
    :states '(visual normal)
    :keymaps (intern (concat (symbol-name mode) "-mode-map"))
    binds))

(defun nop()
  nil
  )

;; ref https://github.com/energos/dotfiles/blob/master/emacs/init.el#L162
(defun neeasade/install-dashdoc (docset)
  "Install dash DOCSET if dashdocs enabled."

  (if (bound-and-true-p neeasade-dashdocs)
    (if (helm-dash-docset-installed-p docset)
      (message (format "%s docset is already installed!" docset))
      (progn (message (format "Installing %s docset..." docset))
        (helm-dash-install-docset (subst-char-in-string ?\s ?_ docset)))
      )
    )
  )

;; todo: have the above do something like this
;; implies change to  have mode passed/arg diff
;; (defun energos/dash-elisp ()
;; 	(setq-local helm-dash-docsets '("Emacs Lisp")))
;; (add-hook 'emacs-lisp-mode-hook 'energos/dash-elisp)

;; this was removed
;; ref: https://github.com/abo-abo/swiper/pull/1570/files#diff-c7fad2f9905e642928fa92ae655e23d0L4500
(defun counsel-switch-to-buffer-or-window (buffer-name)
  "Display buffer BUFFER-NAME and select its window.

 This behaves as `switch-to-buffer', except when the buffer is
 already visible; in that case, select the window corresponding to
 the buffer."
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
      (shell buffer-name)
      (let (window-of-buffer-visible)
        (catch 'found
          (walk-windows (lambda (window)
                          (and (equal (window-buffer window) buffer)
                            (throw 'found (setq window-of-buffer-visible window))))))
        (if window-of-buffer-visible
          (select-window window-of-buffer-visible)
          (switch-to-buffer buffer))))))

(defun neeasade/find-or-open (filepath)
  "Find or open FILEPATH."
  (interactive)
  (let
    ((filename (file-name-nondirectory filepath)))
    (if (get-buffer filename)
      (counsel-switch-to-buffer-or-window filename)
      (find-file filepath)
      )))

(defun what-line ()
  "Print the current line number (in the buffer) of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

;; todo: this isn't working with anchors in other frames
(defun neeasade/eww-browse-existing-or-new (url)
  "If eww is displayed, use that for URL, else open here."
  (if (get-buffer-window "*eww*" 0)
    (url-retrieve url 'eww-render
      (list url nil (get-buffer "*eww*")))
    (eww url)))
