;; -*- lexical-binding: t; -*-

(ns/use exec-path-from-shell
  (exec-path-from-shell-initialize))

(ns/use no-littering
  (require 'recentf)
  (no-littering-theme-backups)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(setq
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
  (->> (cond
         (ns/enable-linux-p "free -b | awk '/^Mem/{print $2}'")
         (ns/enable-mac-p "sysctl -a | awk '/memsize/{print $2}'")
         (ns/enable-windows-p "echo 8000000000"))
    (sh)
    (string-to-number)
    (* 0.70)
    (floor)))

(llet [eight-gb 8000000000]
  (when (> gc-cons-threshold eight-gb)
    (setq gc-cons-threshold eight-gb)))

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

;; save recent files
(recentf-mode 1)
(setq recentf-max-menu-items 1000)
(setq recentf-max-saved-items 1000)

(named-timer-run :sync-recentf-list
  t (ns/t 5m)
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
    (-> cmd sh s-blank-p not))

  ;; todo: this should detect if in zoom meeting
  ;; todo: this should cover gaming too
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
  (ns/t 5m)
  (fn (when (> (org-user-idle-seconds)
              (ns/t 5m))
        (garbage-collect)

        ;; auto revert any who have changed on disk
        (auto-revert-buffers)

        ;; save everyone
        (save-some-buffers t))))

(ns/bind
  ;; todo: dated scratch buffer/dir
  ;; todo: per-mode scratch? should text be org? ugh
  "ns" '(:ignore t :which-key "Scratch")
  "nss" (fn!! goto-scratch-elisp (ns/find-or-open (~e "lisp/scratch/scratch.el")))
  "nst" (fn!! goto-scratch-text (ns/find-or-open (~e "lisp/scratch/scratch.txt")))
  "nso" (fn!! goto-scratch-org (ns/find-or-open (~e "lisp/scratch/scratch.org")))

  "nm" (fn!! goto-messages (ns/find-or-open  "*Messages*"))
  "nU" 'undo-tree-visualize

  "t" '(:ignore t :which-key "Toggle")
  "tw" 'whitespace-mode
  "tn" (fn!! line-numbers-toggle (setq-local display-line-numbers (if display-line-numbers nil 'relative)))
  "tN" (fn!! line-numbers-toggle (setq-local display-line-numbers (if display-line-numbers nil t)))
  "tl" 'toggle-truncate-lines
  ;; "ts" 'ns/style
  "ts" 'ns/load-theme
  "ti" (fn!! (ns/reload-init) (ns/conf-style))
  "m" 'ns/toggle-modeline
  "tp" 'ns/toggle-report

  "i" '(:ignore t :which-key "Insert")
  "ic" 'insert-char
  "ie" 'emoji-search
  "if" (fn!! insert-file-name (insert (buffer-file-name)))
  "id" (fn!! insert-time (org-time-stamp t))
  "iD" (fn!! insert-date (org-time-stamp nil)))
