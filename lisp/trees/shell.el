;; -*- lexical-binding: t; -*-

;; this mostly covers my usage of emacs shell-mode as my interactive shell
(require 'comint)

(when ns/enable-linux-p
  (setq explicit-shell-file-name (getenv "SHELL")))

(when ns/enable-windows-p
  (setenv "PATH"
    (format "%s;%s"
      (~ "scoop/apps/git-with-openssh/current/usr/bin/")
      (getenv "PATH")))
  (setq
    explicit-shell-file-name (executable-find "bash")
    explicit-bash.exe-args '("--login" "-i")))

;; cf https://stackoverflow.com/questions/25862743/emacs-can-i-limit-a-number-of-lines-in-a-buffer
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(setq comint-buffer-maximum-size 2000)
(setq comint-prompt-read-only t)

(ns/inmap
  'comint-mode-map
  "<up>"      'comint-previous-input
  "<down>"    'comint-next-input
  (kbd "C-e") 'comint-previous-input
  (kbd "C-n") 'comint-next-input)

(ns/use shx
  (shx-global-mode t)

  (defun shx-send-input-or-open-thing ()
    "Open thing at point, or send input if no identifiable thing."
    (interactive)
    (when (shx-point-on-input-p)
      (shx-send-input)))

  (when ns/term?
    (general-nmap shx-mode-map
      "RET" #'shx-send-input-or-open-thing)))

(ns/use shell-pop
  ;; idea: would like concurrent shell-pops

  (setq-ns shell-pop
    window-position "top"
    window-size 30 ;; percent
    full-span t)

  ;; interactive shell-pop bound to spc t index shell
  (defun ns/shell-pop (index)
    (let ((old-shell-index shell-pop-last-shell-buffer-index)
           (old-shell-buffer shell-pop-last-shell-buffer-name))
      (shell-pop index)

      (when (= index 9)
        (setq shell-pop-last-shell-buffer-index old-shell-index
          shell-pop-last-shell-buffer-buffer old-shell-buffer))))

  (-map (lambda (i) (ns/bind (concat "t" (number-to-string i)) (fn! (ns/shell-pop i))))
    (number-sequence 1 9))

  ;; treat 9 special, meant to be a long running buffer
  (ns/bind "'" (fn!
                 (shell-pop
                   (if (string= (buffer-name (current-buffer)) "*shell-9*")
                     shell-pop-last-shell-buffer-index nil))))

  (ns/bind "\"" (fn!
                  (if (string= (buffer-name (current-buffer)) "*shell-9*")
                    (evil-window-delete)
                    (progn
                      (shell-pop (if (string= (buffer-name (current-buffer)) "*shell-9*")
                                   shell-pop-last-shell-buffer-index nil))
                      (ns/shell-pop 9))))))

(defun! ns/windowshot ()
  "get a string that is the currently displayed text in emacs window"
  (with-current-buffer (window-buffer)
    (let ((result (-> (buffer-substring (window-start) (window-end))
                    (s-clean)
                    (s-trim))))
      (if (and (eq major-mode 'shell-mode)
            (s-ends-with-p "shellshot" result))
        (s-chop-suffix "shellshot" result)
        result))))

;; cf http://trey-jackson.blogspot.com/2008/08/emacs-tip-25-shell-dirtrack-by-prompt.html
(defun shell-sync-dir-with-prompt (string)
  (if (string-match "\\+Pr0mPT\\+\\([^+]*\\)\\+" string)
    (let ((cwd (match-string 1 string)))
      (setq default-directory (ns/path cwd))
      (ns/atuin-add-dir default-directory)
      (replace-match "" t t string 0))
    string))

(defun ns/monitor-exit-sentinel (process change)
  "delete a frame if it only has a single window with no process"
  (when (equal change "finished\n")
    (if (eq 1 (-> (selected-frame) (window-list) (length)))
      (delete-frame)
      (delete-window))))

(defun ns/shell-mode-init ()
  (shell-dirtrack-mode nil)
  (add-hook 'comint-preoutput-filter-functions 'shell-sync-dir-with-prompt nil t)
  (setq-local inhibit-field-text-motion nil)
  ;; weird colon highlighting thing
  (setq-local shell-font-lock-keywords
    (--remove (s-ends-with-p "]+:.*" (car it))
      shell-font-lock-keywords))

  (add-function :after
    (process-sentinel (get-buffer-process (current-buffer)))
    #'ns/monitor-exit-sentinel))

(add-hook 'shell-mode-hook 'ns/shell-mode-init)

(add-to-list 'display-buffer-alist
  (cons (regexp-quote "*spawn-shell-staged*")
    (cons #'display-buffer-no-window nil)))

(defun! ns/stage-terminal ()
  (save-window-excursion
    (let ((default-directory (~ "")))
      (shell "*spawn-shell-staged*"))))

(ns/stage-terminal)

(defun! ns/spawn-terminal (&optional cwd)
  (when-not ns/term?
    (select-frame (make-frame))
    (ns/pickup-shell cwd t))

  (when ns/term?
    ;; cf https://sw.kovidgoyal.net/kitty/remote-control/#kitty-launch
    ;; lags really bad after window creation
    ;; (kitty-rc-posted-command "launch" `(("type" . "os-window")
    ;;                                      ("args" . ("emacsclient" "-t"))))
    (sh (format "kitty --detach emacsclient -t -e '%s'" (pr-str `(ns/pickup-shell ,cwd t)))))

  ;; return t so that elisp ns/spawn-terminal call is true
  t)

(defun! ns/pickup-shell (&optional cwd terminal)
  (when (not (get-buffer "*spawn-shell-staged*"))
    (ns/stage-terminal))

  (if (file-remote-p (or cwd ""))
    (let ((default-directory cwd)
           ;; file-remote-p returns the tramp connection info without the path
           (process-environment (cons (format "TRAMP_INFO=%s" (file-remote-p cwd)) process-environment)))
      (message "handling this remote shell")
      (message cwd)
      (save-window-excursion (shell "*spawn-shell-remote-temp*"))
      (switch-to-buffer (get-buffer "*spawn-shell-remote-temp*")))
    (switch-to-buffer (get-buffer "*spawn-shell-staged*")))


  (rename-buffer
    (format "*spawn-shell-%s*"
      ;; get the pid of the running bash process
      (first
        (-map 'process-id
          (-filter
            (fn (eq (process-buffer <>)
                  (current-buffer)))
            (process-list))))))

  (when terminal
    (when (fboundp 'ns/toggle-modeline)
      (ns/toggle-modeline)))

  (when cwd (shell-pop--cd-to-cwd-shell cwd))

  ;; we don't care about how long it takes to stage the terminal
  (make-thread 'ns/stage-terminal)

  ;; (evil-insert nil)

  ;; t
  nil
  )

(ns/bind
  "at" 'ns/spawn-terminal
  "as" 'ns/pickup-shell
  )

;; fix for term, ansi term
;; https://github.com/hlissner/emacs-doom-themes/issues/54
(setq ansi-term-color-vector
  [term
    term-color-black
    term-color-red
    term-color-green
    term-color-yellow
    term-color-blue
    term-color-magenta
    term-color-cyan
    term-color-white])

;; completions when editing shell scripts:
;; works by spawning a subshell in the background, kinda weird
;; todo: can we do this with corfu
;; (ns/use company-shell
;;   (setq company-shell-clean-manpage t)
;;   (add-to-list 'company-backends 'company-shell)
;;   (add-to-list 'company-backends 'company-shell-env))

(defun ns/shell-send (string)
  (llet [last-shell-buffer (first (ns/buffers-by-mode 'shell-mode))] ; last shell buffer visited
    (with-current-buffer last-shell-buffer
      (goto-char (point-max))
      (insert (s-trim string))
      (comint-send-input))
    (set-window-point (get-buffer-window last-shell-buffer)
      (point-max))))

(defun! ns/smart-shell-eval ()
  (ns/shell-send
    (if (use-region-p)
      (buffer-substring (region-beginning) (region-end))
      (thing-at-point 'line))))

(ns/bind-mode 'sh "e" 'ns/smart-shell-eval)

(defun! ns/cleanup-shells ()
  "Clean up shell-mode buffers that have no children"
  (->> (ns/buffers-by-mode 'shell-mode)
    (-filter
      (lambda (b)
        (when (get-buffer-process b)
          (llet [pid (process-id (get-buffer-process b))
                  ;; -P works on macos and linux
                  children (sh (format "pgrep -P %s" pid))
                  visible? (get-buffer-window b)]
            (and (s-blank? children)
              (not visible?))))))
    (-map 'kill-buffer)))

(named-timer-run :maybe-cleanup-shells
  t                                     ; do not run initially
  ;; once a day I suppose?
  (ns/t 1d)
  (fn (when (> (org-user-idle-seconds)
              (ns/t 5m))
        (ns/cleanup-shells))))

