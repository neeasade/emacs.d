;; -*- lexical-binding: t; -*-

(ns/use exec-path-from-shell
  (exec-path-from-shell-initialize))

(ns/use no-littering
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(setq
  auto-save-file-name-transforms `((".*" ,(~e "auto-save-list/") t))
  backup-directory-alist `(("." . ,(~e "backups")))
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
  frame-title-format '(multiple-frames "%b"
		                    ("" "%b"))

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

;; (see below for when we garbage collect)
(setq gc-cons-threshold
  (->>
    (cond
      (ns/enable-linux-p "free -b | awk '/^Mem/{print $2}'")
      (ns/enable-mac-p "sysctl -a | awk '/memsize/{print $2}'")
      (ns/enable-windows-p "echo 8000000000"))
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
(defconst custom-file (~e "custom.el"))
(when-not (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load custom-file 'noerr)

(setq browse-url-browser-function 'browse-url-generic)

(when ns/enable-windows-p
  (setq browse-url-browser-function 'browse-url-default-windows-browser))

(when (executable-find "qutebrowser")
  (setq browse-url-generic-program "qutebrowser"))

(when (getenv "BROWSER")
  (setq browse-url-generic-program (getenv "BROWSER")))

;; (when ns/enable-work-p (setq browse-url-generic-program "/Applications/Google Chrome.app/Contents/MacOS/google chrome"))

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
  (fn (when
        ;; don't interrupt me.
        ;; recentf-list normally syncs on a graceful exit, but we don't always have that luxury
        ;; having this on an idle timer got really annoying
        ;; so giving recentf the chance to maybe save every 5 min, if idle.
        (> (org-user-idle-seconds) 10)
        (shut-up (recentf-save-list)))))

(setq whitespace-line-column 80)

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
  (delete-other-windows))

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

(defun ns/media-playing-p ()
  (defun ns/sh-has-content-p (cmd)
    (-> cmd ns/shell-exec s-blank-p not))

  ;; todo: this should detect if in zoom meeting
  (-any-p 'ns/sh-has-content-p
    '("playerctl metadata 2>/dev/null | grep -i netflix"
       "playerctl metadata 2>/dev/null | grep -i 'prime video'"
       "playerctl metadata 2>/dev/null | grep -i 'hulu'"
       "playerctl metadata 2>/dev/null | grep -i 'youtube movies'"
       "pgrep mpv"
       "pgrep vlc"

       ;; adhoc hack (uncomment this when viewing something in an unaccounted for medium)
       ;; "echo foo"
       )))

(named-timer-run :maybe-garbage-collect
  ;; run garbage collection every ~5 minutes when we've been away for longer than 5 minutes.
  ;; this means you won't have to garbage collect for literal minutes when we leave emacs running
  ;; all night long

  ;; run the first time in 30 seconds
  ;; relative times are.. strings? cf https://www.gnu.org/software/emacs/manual/html_node/elisp/Timers.html
  "30 sec"
  (* 5 60)
  (fn (when (> (org-user-idle-seconds)
              (* 5 60))
        (garbage-collect)

        (when (not (ns/media-playing-p))
          (ns/org-clock-out))

        ;; auto revert any who have changed on disk
        (auto-revert-buffers)

        ;; save everyone
        (save-some-buffers t))))

(ns/bind
  ;; todo: dated scratch buffer/dir
  ;; todo: per-mode scratch? should text be org? ugh
  "ns" (fn! (ns/find-or-open (~e "lisp/scratch.el")))
  "nS" (fn! (ns/find-or-open (~e "lisp/scratch.txt")))
  "nm" (fn! (counsel-switch-to-buffer-or-window  "*Messages*"))
  "nU" 'undo-tree-visualize

  "t" '(:ignore t :which-key "Toggle")
  "tw" 'whitespace-mode
  "tn" (fn! (setq-local display-line-numbers (if display-line-numbers nil 'relative)))
  "tl" 'toggle-truncate-lines
  ;; "ts" 'ns/style
  "ts" 'ns/load-theme
  "ti" (fn! (ns/reload-init) (ns/conf-style))
  "m" 'ns/toggle-modeline
  "tp" 'ns/toggle-report

  "i" '(:ignore t :which-key "Insert")
  "ic" 'insert-char
  "if" (fn! (insert (buffer-file-name)))
  "id" (fn! (org-time-stamp t))
  "iD" (fn! (org-time-stamp nil)))
