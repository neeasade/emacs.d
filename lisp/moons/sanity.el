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

  ;; todo: not working for multi caps path case
  ;; eg /thing/Thing/README.md
  ;; doesn't complete at /thing/Thing/rea
  completion-ignore-case  t
  dabbrev-case-fold-search nil
  )

;; 1G
(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

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
  restore-eager 5
  auto-save-timeout 30
  path (list (~ ".emacs.desktop"))
  )

;; disabling in favor of recentf
;; (desktop-save-mode 1)

(setq browse-url-browser-function 'browse-url-generic)

(if ns/enable-windows-p
  (if (executable-find "qutebrowser")
    (setq browse-url-generic-program "qutebrowser")
    (setq browse-url-browser-function 'browse-url-default-windows-browser)
    )
  (setq browse-url-generic-program (getenv "BROWSER"))
  )

;; Removes *scratch* from buffer after the mode has been set.
(add-hook 'after-change-major-mode-hook
  (fn (if (get-buffer "*scratch*") (kill-buffer "*scratch*"))))

;; disable semantic mode, this may bite me lets try it out
(with-eval-after-load 'semantic
  (add-to-list 'semantic-inhibit-functions (lambda () t))
  )

(fset 'yes-or-no-p 'y-or-n-p)

(defcommand toggle-modeline ()
  (make-local-variable 'ns/modeline)

  (if mode-line-format
    (progn
      (setq ns/modeline mode-line-format)
      (setq mode-line-format nil))
    (setq mode-line-format '("%e" (:eval (spaceline-ml-main)))))
  (redraw-frame))

;; don't ask to kill running processes when killing a buffer.
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; don't popup buffers with output when launching things
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; save recent files
(recentf-mode 1)
(setq recentf-max-menu-items 300)
(setq recentf-max-saved-items 300)

(defun ns/save-files()
  (let ((inhibit-message t))
    (recentf-save-list)))

(ns/add-firstrun-action '(run-at-time nil (* 5 60) 'ns/save-files))

(setq whitespace-line-column 120)

(defcommand insert-filename ()
  (insert (f-filename (buffer-file-name))))

(defcommand insert-filepath ()
  (insert (buffer-file-name)))

;; a report toggle command for debugging on keybind
(require 'profiler)
(defcommand toggle-report ()
  (if (profiler-running-p)
    (progn
      (profiler-report)
      (profiler-stop))
    (profiler-cpu-start profiler-sampling-interval)))

;; todo: replace linum-mode with this, maybe check running emacs version first?
;; (display-line-numbers-mode)
;; (setq display-line-numbers 'relative)
(display-line-numbers-mode 0)

(ns/bind
  "ns" (fn! (ns/find-or-open (~ ".emacs.d/lisp/scratch.el")))
  "nS" (fn! (ns/find-or-open (~ ".emacs.d/lisp/scratch.txt")))
  "nm" (fn! (counsel-switch-to-buffer-or-window  "*Messages*"))

  "t" '(:ignore t :which-key "Toggle")
  "tw" 'whitespace-mode
  "tn" 'linum-mode
  "tl" 'toggle-truncate-lines
  "ts" 'ns/style
  "ti" 'reload-init
  "tm" 'ns/toggle-modeline
  "m" 'ns/toggle-modeline
  "tp" 'ns/toggle-report

  "i" '(:ignore t :which-key "Insert")
  "ic" 'insert-char
  "if" 'ns/insert-filename
  "ip" 'ns/insert-filepath
  )
