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
  )

;; todo: reconsider this, auto wrap large operations or something
(setq gc-cons-threshold (eval-when-compile (* 5 1024 1024 1024)))
(defun ns/idle () (garbage-collect))
(ns/add-firstrun-action '(run-with-idle-timer 2 t 'ns/idle))

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
  restore-eager 0
  auto-save-timeout 30
  path (list (~ ".emacs.desktop")))

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

  (when
    (and (s-starts-with-p "qutebrowser-editor-"
           (buffer-name (current-buffer)))
      (string= (system-name) "bridge"))

    (ns/shell-exec-dontcare
      (format "popup_window.sh %s"
        (frame-parameter nil 'outer-window-id)))

    (ns/set-buffer-face-variable)
    (evil-insert 0))
  t
  )

(add-hook 'after-change-major-mode-hook 'ns/after-change-major-mode-hook)

(fset 'yes-or-no-p 'y-or-n-p)
(fset 'which 'executable-find)

(defun! ns/toggle-modeline ()
  (make-local-variable 'ns/modeline)

  (if mode-line-format
    (progn
      (setq ns/modeline mode-line-format)
      (setq mode-line-format nil))
    (setq mode-line-format '("%e" (:eval (spaceline-ml-main)))))
  (redraw-frame))

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

(defun ns/save-files()
  (shut-up (recentf-save-list)))

(ns/add-firstrun-action '(run-at-time nil (* 5 60) 'ns/save-files))

(setq whitespace-line-column 120)

(defun! ns/insert-filename ()
  (insert (f-filename (buffer-file-name))))

(defun! ns/insert-filepath ()
  (insert (buffer-file-name)))

;; a report toggle command for debugging on keybind
(require 'profiler)
(defun! ns/toggle-report ()
  (if (profiler-running-p)
    (progn
      (profiler-report)
      (profiler-stop))
    (profiler-cpu-start profiler-sampling-interval)))

;; (defun ns/proced-init ()
;;   (interactive)
;;   ;; note: default update rate is 5 seconds
;;   (proced-toggle-auto-update)
;;   (delete-other-windows)
;;   )

;; (advice-remove 'proced-mode #'ns/proced-init)
;; (remove-hook 'proced-mode-hook 'ns/proced-init)

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
  "ti" 'reload-init
  "tm" 'ns/toggle-modeline
  "m" 'ns/toggle-modeline
  "tp" 'ns/toggle-report

  "i" '(:ignore t :which-key "Insert")
  "ic" 'insert-char
  "if" 'ns/insert-filename
  "ip" 'ns/insert-filepath
  )
