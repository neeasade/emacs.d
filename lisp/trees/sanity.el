;; -*- lexical-binding: t; -*-

;; should this be in dirt
(use-package no-littering
  :config
  (require 'no-littering)
  (require 'recentf)
  ;; note: defaults are:
  ;; ~/.emacs.d/var/
  ;; ~/.emacs.d/etc/
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(setq
  ;; todo: relook at this setting
  auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
  backup-directory-alist `(("." . ,(~ ".emacs.d/backups")))
  coding-system-for-read 'utf-8
  coding-system-for-write 'utf-8
  delete-old-versions -1
  global-auto-revert-mode t
  inhibit-startup-screen t
  initial-scratch-message ""
  ring-bell-function 'ignore
  sentence-end-double-space nil
  vc-follow-symlinks t ;; auto follow symlinks
  vc-make-backup-files t
  version-control t
  network-security-level 'high
  frame-resize-pixelwise t

  ;; set to 0 for default/centering behavior
  scroll-conservatively 1

  completion-ignore-case  t
  dabbrev-case-fold-search nil

  ;; allow for much more than the default amount of lisp bindings
  max-specpdl-size 13000

  ;; only wrap comments in programming modes when it's enabled there
  comment-auto-fill-only-comments t

  ;; cf https://www.reddit.com/r/emacs/comments/43b42y/i_just_realized_emacs_has_a_fast_infix_calculator/czh7djn/
  calc-multiplication-has-precedence nil
  )

;; todo: reconsider this, auto wrap large operations or something
(setq gc-cons-threshold
  (->> "free -b | awk '/^Mem/{print $2}'"
    (ns/shell-exec)
    (string-to-number)
    (* 0.70)
    (floor)))

;; trim gui
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; cursor
(show-paren-mode 1)
(blink-cursor-mode 0)

;; custom
(defconst custom-file (~ ".emacs.d/custom.el"))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load custom-file 'noerr)

;; persistent session:
;; note: (desktop-clear) to clean/kill everything.
(make-directory (~ ".emacs.desktop") t)

(setq-ns desktop
  auto-save-timeout 30
  ;; this  is set by the no-littering package
  ;; path (list (~ ".emacs.desktop"))
  )

;; disabling in favor of recentf
(desktop-save-mode 0)

(setq browse-url-browser-function 'browse-url-generic)

(when ns/enable-windows-p
  (setq browse-url-browser-function 'browse-url-default-windows-browser))

(when (executable-find "qutebrowser")
  (setq browse-url-generic-program "qutebrowser"))

(when (getenv "BROWSER")
  (setq browse-url-generic-program (getenv "BROWSER")))

;; Removes *scratch* from buffer after the mode has been set.
(defun ns/after-change-major-mode-hook ()
  (when (get-buffer "*scratch*")
    (kill-buffer "*scratch*"))

  t
  )

(add-hook 'after-change-major-mode-hook 'ns/after-change-major-mode-hook)

(fset 'yes-or-no-p 'y-or-n-p)
(fset 'which 'executable-find)

;; don't ask to kill running processes when killing a buffer.
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; don't popup buffers with output when launching things (ns/shell-exec-dontcare)
(add-to-list 'display-buffer-alist
  (cons "\\*Async Shell Command\\*.*"
    (cons #'display-buffer-no-window nil)))

;; save recent files
(recentf-mode 1)
(setq recentf-max-menu-items 1000)
(setq recentf-max-saved-items 1000)

(named-timer-run :sync-recentf-list
  t (* 5 60)
  (fn (shut-up (recentf-save-list))))

(setq whitespace-line-column 120)

(defun! ns/insert-filename ()
  (insert (f-filename (buffer-file-name))))

(defun! ns/insert-filepath ()
  (insert (buffer-file-name)))

;; a report toggle command for debugging on keybind
(require 'profiler)
(defun! ns/toggle-report ()
  (if (profiler-running-p)
    (progn (profiler-report) (profiler-stop))
    (profiler-cpu-start profiler-sampling-interval)))

(defun! ns/proced-init ()
  ;; note: default update rate is 5 seconds
  ;; (proced-toggle-auto-update)
  (message "we are reaching here")
  (delete-other-windows)
  )

;; (advice-remove 'proced-mode #'ns/proced-init)
(advice-add 'proced-mode :after #'ns/proced-init)

;; (add-hook 'proced-mode-hook 'ns/proced-init)
(remove-hook 'proced-mode-hook 'ns/proced-init)

;; (remove-hook 'proced-mode-hook 'ns/proced-init)

(defun p-proced-format-args (oldformat &rest args)
  (let ((args (mapcar (lambda (arg)
                        (replace-regexp-in-string "/nix/store/[^/]+"
                          "{nix}"
                          arg))
                args)))
    (apply oldformat args)))

(advice-add #'proced-format-args :around #'p-proced-format-args)

(ns/bind
  "ns" (fn! (ns/find-or-open (~ ".emacs.d/lisp/scratch.el")))
  "nS" (fn! (ns/find-or-open (~ ".emacs.d/lisp/scratch.txt")))
  "nm" (fn! (counsel-switch-to-buffer-or-window  "*Messages*"))
  "nU" 'undo-tree-visualize

  "t" '(:ignore t :which-key "Toggle")
  "tw" 'whitespace-mode
  "tn" (fn! (setq-local display-line-numbers (if display-line-numbers nil 'relative)))
  "tl" 'toggle-truncate-lines
  "ts" 'ns/style
  "ti" (fn! (reload-init) (ns/style))
  "m" 'ns/toggle-modeline
  "tp" 'ns/toggle-report

  "i" '(:ignore t :which-key "Insert")
  "ic" 'insert-char
  "if" 'ns/insert-filename
  "ip" 'ns/insert-filepath
  )
