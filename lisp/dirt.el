;; get straight.el
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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-cache-autoloads t)

;; elisp enhancers
(use-package fn)   ;function
(use-package s)    ;string
(use-package f)    ;file
(use-package ht)   ;hash table
(use-package dash) ;list
(use-package a)    ;assoc lists
;; todo: use from github? -- nah just need to update your reference
;;(use-package ts)    ;timestamps

;; other
(use-package hydra)
(use-package general)
(use-package request)
(use-package shut-up)
(require 'seq)
(require 'cl-macs)

(defmacro fn! (&rest body) `(lambda () (interactive) ,@body))

;; call a func, but with backtick quoting
;; (@ 'message ,@'("asdf"))
(defmacro @ (&rest input) (eval (eval `(backquote (list ,@input)))))

(defun s-clean (s)
  "Remove text properies from S."
  (set-text-properties 0 (length s) nil s) s)

;; setq namespace
(defmacro setq-ns (namespace &rest lst)
  (let ((namespace (prin1-to-string namespace)))
    (->>
      (-partition 2 lst)
      (mapcar (fn (list
                    (intern (format "%s-%s" namespace (car <>)))
                    (cadr <>))))
      (cons 'setq)
      (-flatten-n 1)
      )))

;; todo: make this smart about tramp?
(defun ~ (path)
  (concat
    (getenv (if ns/enable-windows-p "USERPROFILE" "HOME"))
    (if ns/enable-windows-p "\\" "/") path))

;; todo: take a look at general-describe-keybindings later
;; binding wrappers
(defmacro ns/bind (&rest binds)
  `(general-define-key
     :states '(normal visual)
     :prefix "SPC"
     ,@binds))

(defmacro ns/bind-mode (mode &rest binds)
  `(general-define-key
     :prefix "SPC"
     :states '(visual normal)
     :keymaps (intern (concat (symbol-name ,mode) "-mode-map"))
     ,@binds))

(defmacro ns/bind-leader-mode (mode &rest binds)
  `(general-define-key
     :prefix ","
     :states '(visual normal)
     :keymaps (intern (concat (symbol-name ,mode) "-mode-map"))
     ,@binds))

;; this was removed
;; cf https://github.com/abo-abo/swiper/pull/1570/files#diff-c7fad2f9905e642928fa92ae655e23d0L4500
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

(defmacro ns/shell-exec (command)
  "trim the newline from shell exec"
  `(replace-regexp-in-string "\n$" ""
     (shell-command-to-string ,command)))

(defun ns/shell-exec-dontcare (command)
  (let* ((bufname (concat "*killme-shell" (number-to-string (random)) "*"))
          (junk-buffer (get-buffer-create bufname)))
    (shut-up
      (shell-command command junk-buffer)
      (kill-buffer junk-buffer)
      )))

;; wrap passwordstore
(defun pass (key)
  (if (executable-find "pass")
    (ns/shell-exec (format "pass %s 2>/dev/null" key)) ""))

(defun get-resource (name)
  "Get X resource value, with a fallback value NAME."
  (let ((default (cdr (assoc name ns/xrdb-fallback-values)))
         (result (if (executable-find "xrq")
                   (ns/shell-exec (format "xrq '%s' 2>/dev/null" name))
                   "")))
    (if (string= result "") default result)))

(defun reload-init ()
  "Reload init.el with straight.el."
  (interactive)
  (straight-transaction
    (straight-mark-transaction-as-init)
    (message "Reloading init.el...")
    (load user-init-file nil 'nomessage)
    (message "Reloading init.el... done.")))

;; a macro for when something is not on melpa yet (assumes github)
(use-package el-patch)
(defmacro ns/use-package (name repo &rest config)
  `(progn
     (straight-register-package
       '(,name :type git :host github
	      :repo ,repo
	      :depth full))

     (straight-use-package ',name)
     ,@(cdr config)
     ))

;; imap + nmap
(defun ns/inmap (keymap key func)
  (general-imap :keymaps keymap key func)
  (general-nmap :keymaps keymap key func))

(defun ns/save-file (filename data)
  (with-temp-file filename
    (prin1 data (current-buffer))))

;; this is overridden with eros eval later on
(defun ns/smart-elisp-eval ()
  (interactive)
  (if (use-region-p)
    (eval-region (region-beginning) (region-end))
    (if (s-blank-p (s-trim (thing-at-point 'line)))
      (eval-last-sexp nil)
      (eval-defun nil))))

(ns/bind-mode 'emacs-lisp "e" 'ns/smart-elisp-eval)


(defmacro defconfig-base (label &rest body)
  `(defun ,(intern (concat "ns/" (prin1-to-string label)))
     nil ,@body))

(defmacro defconfig (label &rest body)
  `(progn
     (setq ,(intern (format "ns/enable-%s-p" (prin1-to-string label))) nil)
     (defconfig-base ,label
       (let ((config-name ,(prin1-to-string label)))
         (message (concat "loading " config-name "..."))
         (catch 'config-catch
           ,@body
           (setq ,(intern (format "ns/enable-%s-p" (prin1-to-string label))) t))))))

(defmacro ns/guard (&rest conditions)
  (if (not (eval (cons 'and conditions)))
    '(when t (throw 'config-catch (concat "config guard " config-name)))))

(defmacro defcommand (label args &rest body)
  `(defun ,(intern (concat "ns/" (prin1-to-string label))) ,args
     (interactive) ,@body))

(defcommand find-or-open (filepath)
  "Find or open FILEPATH."
  (let ((filename (file-name-nondirectory filepath)))
    (if (get-buffer filename)
      (counsel-switch-to-buffer-or-window filename)
      (find-file filepath))))
