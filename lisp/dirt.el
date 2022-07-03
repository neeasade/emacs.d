;; -*- lexical-binding: t; -*-
;;; dirt.el --- Lay the plot
;;; Commentary:
;;; I'm a sugar lover. This file is about getting emacs-lisp sugar to use
;;; everywhere else.
;;; Code:

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

;; todo: catch errors here, log and bubble up
;; (as opposed to logging all the time)
(defmacro ns/use (name &rest args)
  `(progn
     (message (format ":: ns/use: %s..." ',name))
     (use-package ,name ,@args)
     (message (format ":: ns/use: %s... done." ',name))))

;; elisp enhancers
(ns/use fn)      ; function
(ns/use s)       ; string
(ns/use f)       ; file
(ns/use ht)      ; hash table
(ns/use dash)    ; list
(ns/use a)       ; assoc lists
(ns/use async)   ; async
(ns/use ts)      ; timestamps
(ns/use pcre2el) ; sane regex
(ns/use memoize) ; caching
(ns/use plz)     ; curl-backed request interface

;; other/emacs enhancers
(ns/use hydra)
(ns/use general :config (general-override-mode t))
(ns/use request)
(ns/use shut-up)
(require 'seq)
(require 'cl-macs)
(require 'cl-seq)
(require 'man)

(ns/use named-timer :config (require 'named-timer))

(defmacro fn! (&rest body) `(lambda () (interactive) ,@body))
(defmacro ns/comment (&rest body) nil)

(defalias 'first 'car)
(defalias 'second 'cadr)
(defalias 'third 'caddr)
(defalias 'when-not 'unless)

(defmacro if-not (condition &rest body)
  `(if (not ,condition)
     ,@body))

(defalias 'pr-str 'prin1-to-string)
(defalias '-join '-interpose)

(defun prn (&rest sexp)
  (message (s-join " " (-map 'pr-str sexp))) nil)

(defmacro llet (args &rest body)
  ;; the append is to convert [vectors] to lists
  `(-let* ,(-partition 2 (append args nil)) ,@body))

(defmacro defun! (label args &rest body)
  `(defun ,label ,args
     (interactive) ,@body))

(defmacro setq-ns (namespace &rest pairs)
  (->> (-partition 2 pairs)
    (-mapcat (fn (list
                   (intern (format "%s-%s"
                             (prin1-to-string namespace)
                             (car <>)))
                   (cadr <>))))
    (cons 'setq)))

(defun ~ (&rest args)
  (apply #'f-join (cons ns/home-directory args)))

(defun ~e (&rest args)
  (apply #'f-join (cons ns/emacs-directory args)))

;; todo: consider conflict management/at the time of binding yell about the takeover

;; binding wrappers
(defmacro ns/bind (&rest binds)
  `(general-define-key
     :states '(normal visual)
     ;; note: 'override means we squash anyone with overlapping keybinds
     :keymaps 'override
     :prefix "SPC"
     ,@binds))

(defmacro ns/bind-soft (&rest binds)
  "a version of ns/bind that will not be present via an override mode"
  `(general-define-key
     :states '(normal visual)
     :prefix "SPC"
     ,@binds))

(defun ns/bind-mode (mode &rest binds)
  ;; unbind anything that might be bound in ~mode~ already
  ;; todo: this better later

  ;; (that is, allow local bindings to override global ones)
  (ns/comment
    (apply 'general-unbind
      `(
         '(normal visual)
         :with 'ignore
         ,(intern (format "%s-mode-map" (symbol-name mode)))
         ,(keys (->> binds
                  (-partition 2)
                  (-map 'car)
                  (-flatten))))))

  (apply 'general-define-key
    `(:prefix "SPC"
       :states '(visual normal)
       ;; note: this depends on modes playing nice wrt conventions
       :keymaps ,(intern (format "%s-mode-map" (symbol-name mode)))
       ,@binds)))

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

(defun! ns/find-or-open (filepath)
  "If FILEPATH is open in a buffer, switch to that."
  (let ((filename (file-name-nondirectory filepath)))
    (if (get-buffer filename)
      (counsel-switch-to-buffer-or-window filename)
      (if (f-exists-p filepath)
        (find-file filepath)
        (message (format "no file found: %s" filepath))))))

(defun sh (command)
  "trim the newline from shell exec"
  (replace-regexp-in-string "\n$" ""
    (shell-command-to-string command)))

(defmacro ns/shell-exec (command)
  "trim the newline from shell exec"
  `(replace-regexp-in-string "\n$" ""
     (shell-command-to-string ,command)))

(defun ns/shell-exec-dontcare (command)
  (let* ((bufname (concat "*killme-shell-" (number-to-string (random)) "*"))
          (junk-buffer (get-buffer-create bufname)))
    (shut-up
      (shell-command command junk-buffer)
      (kill-buffer junk-buffer))))

;; wrap passwordstore
(defun pass (key)
  (when (executable-find "pass")
    (sh (format "pass %s 2>/dev/null" key)) ""))

(defun get-resource (name)
  "Get X resource value, with a fallback value NAME."
  (llet [default (cdr (assoc name ns/xrdb-fallback-values))
          xrq-result (when (executable-find "xrq")
                       (ns/shell-exec (format "xrq '%s' 2>/dev/null" name)))
          theme-result (when (executable-find "theme")
                         (ns/shell-exec (format "theme -q '%s' 2>/dev/null" name)))]
    (->> (list theme-result xrq-result default)
      (-remove (fn (s-blank-p <>)))
      (first))))

(defun! ns/reload-init ()
  "Reload init.el with straight.el."
  (message "Reloading init.el...")
  (load user-init-file nil 'nomessage)
  (message "Reloading init.el... done."))

(defalias 'ns/init 'reload-init)

;; a macro for when something is not on melpa yet (assumes github)
(ns/use el-patch)

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
         (let ((config-name ,conf-string))
           (message (format "::: %s..." ',function-name))
           (catch 'config-catch
             ,@body
             (setq ,enabled-symbol t))
           (message (format "::: %s... done." ',function-name)))))))

(defmacro ns/guard (check)
  `(when-not ,check
     (throw 'config-catch (concat "conf-guard" config-name))))

;; extension to s.el
(defun s-clean (s)
  "Remove text properies from S."
  (set-text-properties 0 (length s) nil s) s)

