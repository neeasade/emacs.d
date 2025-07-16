;; -*- lexical-binding: t; -*-
;;; dirt.el --- Lay the plot
;;; Commentary:
;;; I'm a sugar lover. This file is about getting emacs-lisp sugar to use everywhere else.
;;; Code:

(setq load-prefer-newer t)

(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'dash)
(require 'dash)
(straight-use-package 's)
(require 's)

(setq
  ns/enable-windows-p (eq system-type 'windows-nt)
  ns/enable-linux-p (eq system-type 'gnu/linux)
  ns/enable-mac-p (eq system-type 'darwin)
  ns/enable-home-p (string= (getenv "USER") "neeasade")
  ;; ns/enable-work-p ns/enable-mac-p
  ns/enable-work-p nil

  ns/term? (not window-system)
  ns/home-directory (getenv (if ns/enable-windows-p "USERPROFILE" "HOME"))
  ns/emacs-directory user-emacs-directory

  mac-option-modifier 'meta
  mac-command-modifier 'super
  mac-control-modifier 'control
  )

(defmacro ns/use (pkg-def &rest body)
  "Load a PKG-DEF with straight, require it, and then eval BODY."
  (-let* ((pkg (-first-item (-list pkg-def)))
           (pkg-mode (intern (format "%s-mode" (s-chop-suffix "-mode" (prin1-to-string pkg))))))
    `(let ((ns-use-time (current-time)))
       (message ": ns/use: %s..." ',pkg)
       (straight-use-package ',pkg-def)
       (require ',pkg nil t)
       (require ',pkg-mode nil t)
       ,@body
       (message ": ns/use: %s... done." ',pkg)
       ;; (let ((time-passed (float-time (time-since ns-use-time))))
       ;;   (if (> time-passed 2)
       ;;     (message ": ns/use: %s... done (%.02fs) 🕑." ',pkg time-passed)
       ;;     (message ": ns/use: %s... done." ',pkg)))
       )))


;; use the builtin org that ships with emacs
(add-to-list 'straight-built-in-pseudo-packages 'org)
(require 'org)

;; org-element--list-struct: Tab width in Org files must be 8, not 4
(defun ns/org-mode-tab-width () (setq-local tab-width 8))
(add-hook 'org-mode-hook 'ns/org-mode-tab-width)

;; load org early so that require's use the correct package
;; (ns/use org)

;; elisp enhancers
(ns/use fn)      ; function
(ns/use f)       ; file
(ns/use ht)      ; hash table
(ns/use a)       ; assoc lists
(ns/use async)   ; async
(ns/use ts)      ; timestamps
(ns/use pcre2el) ; sane regex
(ns/use memoize) ; caching
;; (ns/use plz)     ; curl-backed request interface (nice in theory, unused in practice)

;; other/emacs enhancers
(ns/use hydra)
(ns/use general (general-override-mode t))

(ns/use request)
(ns/use shut-up)

(require 'man)
(require 'seq)
(require 'cl-macs)
(require 'cl-seq)
(require 'cl-lib)

(ns/use named-timer)

(defmacro fn! (&rest body) `(lambda () (interactive) ,@body))
(defmacro ns/comment (&rest body) nil)
(defmacro comment (&rest body) nil)

(defalias 'first 'car)
(defalias 'second 'cadr)
(defalias 'third 'caddr)
(defalias 'when-not 'unless)
(defalias 'which 'executable-find)

(defmacro if-not (condition &rest body)
  `(if (not ,condition)
     ,@body))

(defalias 'pr-str 'prin1-to-string)
(defalias '-join '-interpose)

(defun prn (&rest sexp)
  (message "%s" (s-join " " (-map 'pr-str sexp))) nil)

(defmacro measure-time (&rest body)
  "Measure the time (in seconds) it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defmacro llet (args &rest body)
  ;; the append is to convert [vectors] to lists
  `(-let* ,(-partition 2 (append args nil)) ,@body))

(defmacro defun! (label args &rest body)
  (llet [docstring (when (stringp (first body))
                     (first body))
          body (if docstring (-drop 1 body) body)]
    `(defalias ',label
       (lambda ,args
         (interactive)
         ,@body)
       ,docstring)))

(defmacro fn!! (&rest body)
  "Create an interactive function prefixed with ia/ and no arguments (optionally, infer name from first sexp)"
  ;; todo: should this yell if there's a naming conflict?
  (let* ((has-name? (symbolp (first body)))
          (fnname (intern (format "ia/%s" (prin1-to-string
                                            (if has-name?
                                              (first body)
                                              (first (first body))))))))
    `(defun! ,fnname ()
       ,@(if has-name?
           (-drop 1 body)
           body))))

(defmacro setq-ns (namespace &rest pairs)
  `(setq ,@(->> (-partition 2 pairs)
             (-mapcat (-lambda ((k v))
                        (list (intern (format "%s-%s" namespace k)) v))))))

(defun sh-impl (toss? &rest args)
  ;; 🤪
  (if (= 1 (length args))
    (sh-impl toss? (which "bash") "-c" (first args))
    (llet [process-environment (if toss? process-environment (cons "CALLED_FROM_EMACS=t" process-environment))
            (cmd . args) args
            result (s-trim
                     (with-output-to-string
                       (with-current-buffer standard-output
                         (apply 'call-process cmd nil
                           (list (if toss? 0 t)
                             t)         ; nil to discard stderr, t to mix
                           nil args))))]
      (when-not (s-blank? result)
        result))))

(defun sh (&rest args)
  "Run shell command and return trimmed output.
Given (CMD ARGS...), runs CMD with ARGS. Given string, runs via bash."
  (apply 'sh-impl nil args))

(defun sh-toss (&rest args)
  "Run shell command asynchronously, discarding output.
Given (CMD ARGS...), runs CMD with ARGS. Given string, runs via bash."
  (apply 'sh-impl t args))

(defun sh-lines (&rest args)
  (s-split-lines (apply 'sh args)))

(defun ns/path (&rest paths)
  "Make a path normal. Normal means:
- call expand-file-name (~ to $HOME)
- end in '/' if a directory
- replace '//' ('/+') with '/'

if path doesn't exist, returns without trailing '/'"
  ;; when (f-exists? path)
  (llet [p (s-join "/" paths)
          p (expand-file-name p)
          p (s-replace-regexp "/+" "/" p)
          ;; todo: this probably has perf implications (esp if tramp)
          dir? (when (f-exists? p) (f-directory? p))
          slashed? (s-ends-with-p "/" p)]
    (if dir?
      (if slashed? p (concat p "/"))
      (if slashed? (substring p -1) p))))

(defun ~ (&rest args)
  (apply 'ns/path ns/home-directory args))

(defun ~e (&rest args)
  (apply 'ns/path ns/emacs-directory args))

(defun ns/bind (&rest binds)
  (llet [states '(normal visual)
          keymap 'general-override-mode-map]
    (apply 'general-define-key
      :states states
      :keymaps keymap
      :prefix "SPC"
      binds)))

(defun ns/bind-mode (mode &rest binds)
  (llet [states '(normal visual)
          keymap (intern (format "%s-mode-map" (symbol-name mode)))] ; convention
    (apply 'general-define-key
      :states states
      :keymaps keymap
      :prefix "SPC"
      binds)))

;; todo: reconsider
(defun ns/bind-leader-mode (mode &rest binds)
  (llet [states '(normal visual)
          keymap (intern (format "%s-mode-map" (symbol-name mode)))]
    (apply 'general-define-key
      :states states
      :keymaps keymap
      :prefix ","
      binds)))

;; this was removed
;; cf https://github.com/abo-abo/swiper/pull/1570/files#diff-c7fad2f9905e642928fa92ae655e23d0L4500
(defun ns/switch-to-buffer-or-window (buffer-name)
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

(defun! ns/find-or-open (filepath)
  "If FILEPATH is open in a buffer, switch to that."
  (llet (filename (file-name-nondirectory filepath))
    (if (get-buffer filename)
      (ns/switch-to-buffer-or-window filename)
      (find-file filepath))))

;; wrap passwordstore
(defun pass (key)
  (and (which "rbw") (sh "rbw" "get" key)))

(defun! ns/reload-init ()
  "Reload init.el with straight.el."
  (load user-init-file nil 'nomessage))

(defun ns/inmap (keymap &rest key-func-pairs)
  "imap + nmap"
  (-map (-lambda ((key fn))
          (general-imap :keymaps keymap key fn)
          (general-nmap :keymaps keymap key fn))
    (-partition 2 key-func-pairs)))

;; this is overridden with eros eval later on
(defun! ns/smart-elisp-eval ()
  (if (use-region-p)
    (eval-region (region-beginning) (region-end))
    (if (s-blank-p (s-trim (thing-at-point 'line)))
      (eval-last-sexp nil)
      (eval-defun nil))))

(ns/bind-mode 'emacs-lisp "e" 'ns/smart-elisp-eval)

(defmacro ns/defconfig (label &rest body)
  (let* ((conf-string (pr-str label))
          (enabled-symbol (intern (format "ns/enable-%s-p" conf-string )))
          (function-name (intern (format "ns/conf-%s" conf-string))))
    `(progn
       (setq ,enabled-symbol nil)
       (defun ,function-name nil
         (interactive)
         (let ((config-name ,conf-string)
                (config-start-time (current-time)))
           (message (format "::: %s..." ',function-name))
           ,@body
           (let ((time-passed (float-time (time-since config-start-time))))
             (if (> time-passed 2)
               (message (format "::: %s... done (%.02fs) slow 🕑.." ',function-name time-passed))
               (message (format "::: %s... done." ',function-name)))))))))

(defun ns/file-mode (file-extension mode)
  (let ((pattern (format  "\\.%s\\'" file-extension)))
    (add-to-list 'auto-mode-alist `(,pattern . ,mode))))

(defun s-clean (s)
  "Remove text properies from S."
  (set-text-properties 0 (length s) nil s) s)

(defun s-split-lines (s)
  "Remove text properies from S."
  (s-split "\n" s))

(defun -ht (&rest kvs)
  "A builder for ht.el with less parentheses"
  (let ((table (ht-create)))
    (-map (-applify (-partial 'ht-set table))
      (-partition 2 kvs))
    table))

(defalias 'slurp 'f-read)

(defun spit (f content)
  (f-write content 'utf-8 f))

(defun ns/str (&rest vals)
  "Coerce VAL to string. nil is empty string."
  (apply 'concat
    (-map (lambda (val)
            (cond
              ((stringp val) val)
              ((keywordp val) (substring (pr-str val) 1))
              ((bufferp val) (buffer-name val))
              ((characterp val) (char-to-string val))
              ;; ((listp val) (ns/make-lines val))
              ;; ((numberp val) (number-to-string val))
              ((eq val nil) "")
              (t (pr-str val))))        ; ?
      vals)))

(defmacro condp (pred expr &rest clauses)
  `(cond
     ,@(-map (-lambda ((p1 p2))
               `((,pred ,expr ,p1) ,p2))
         (-partition 2 clauses))
     (t ,(when (cl-oddp (length clauses))
           (-last-item clauses)))))

(defmacro ns/t (time-in)
  "Get value as seconds such as: 30m, 2h45m, 2d, 10s"
  (->> (ns/str time-in)
    (s-match-strings-all (pcre-to-elisp "(([^0-9]|^)([0-9]+)([a-zA-Z]{1}))+"))
    (-map (-lambda ((_ _ _ count type))
            (llet [count (string-to-number count)
                    type (string-to-char type)]
              (* count (condp = type
                         ?s 1
                         ?m 60
                         ?h (* 60 60)
                         ?d (* 60 60 24))))))
    (apply '+)))

(ns/comment
  (ns/t 10m)   ;; => 600
  (ns/t 1d)    ;; => 86400
  (ns/t 1h30m)) ;; => 5400

(ns/use persist
  (defmacro ns/persist (symbol &optional initial)
    "Persist symbol between emacs sessions"
    ;; ensure the cache dir exists (upstream bug)
    (f-mkdir-full-path (f-parent (persist--file-location symbol)))
    `(persist-defvar ,symbol ,initial
       ,(format "docstring value for %s" symbol)))

  ;; the default is to only persist on quit
  (named-timer-idle-run :persist-save (* 60 5) t 'persist--save-all))

(defun ns/face (faces &rest kvs)
  (--map (apply 'set-face-attribute it nil kvs)
    (-list faces)))

(ns/use alert
  (setq alert-default-style 'libnotify)

  ;; I could not get the (alert :persistent t keyword to work)
  (defun alert! (&rest alert-args)
    (let ((alert-fade-time 0))
      (apply 'alert alert-args))))


;; in anticipation of it existing one day
;; https://github.com/magnars/dash.el/pull/404
(when-not (fboundp '-shuffle)
  (defun -to-head (n list)
    "Return a new list that move the element at Nth to the head of old LIST."
    (declare (pure t) (side-effect-free t))
    (if (> n (1- (length list)))
      (error "Index %d out of the range of list %S" n list))
    (let* ((head (-take n list))
            (rest (-drop n list))
            (target (pop rest)))
      (cons target (nconc head rest))))

  (defun -shuffle (list &optional rng)
    "Return a new shuffled LIST, shuffling using RNG. "
    (declare (pure t) (side-effect-free t))
    (let* ((len (length list))
            (random-nums (-map (or rng #'random) (number-sequence len 1 -1)))
            result)
      (--each random-nums
        (setq list (-to-head it list))
        (push (pop list) result))
      (nreverse result))))

(defun ns/random-list (list)
  "pick random item from list"
  (first (-shuffle list)))

(setenv "ATUIN_SESSION" (sh "bash" "-ic" "echo $ATUIN_SESSION"))

(defun ns/atuin-add-dir (cwd)
  (when (which "atuin")
    (sh-toss "atuin" "kv" "set" "-n" "dirs" "--key" (ns/path cwd) "_")))

(defun ns/atuin-list-dirs ()
  ;; get session
  (when (which "atuin")
    (->> (-concat
           (s-split-lines
             (or (sh "atuin history list --format {directory} | sort | uniq")
               ""))
           (s-split-lines (sh "atuin kv list -n dirs")))
      (-distinct)
      (--remove (not (f-exists-p it)))
      (-map 'ns/path)
      (-map #'consult--fast-abbreviate-file-name))))

(when ns/term?
  (add-to-list 'load-path "~/.emacs.d/lisp/kitty/")

  (setq kitty-kbp-modifiers-alist
    ;; original
    ;; '((1 . shift) (2 . alt) (4 . control) (8 . super) (16 . hyper) (32 . meta))
    ;; swap meta and alt (get my home keyboard to work)
    '((1 . shift) (2 . meta) (4 . control) (8 . alt) (16 . hyper) (32 . super)))

  (setq kitty-kbp-delete-backspace-workaround t)

  (require 'term/xterm-kitty)

  ;; (terminal-init-xterm-kitty)
  )
