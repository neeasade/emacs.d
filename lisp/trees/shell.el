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
  (setq explicit-shell-file-name (car (s-split "\n" (ns/shell-exec "where bash"))))
  (setq explicit-bash.exe-args '("--login" "-i")))

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


(use-package shx
  :config
  (shx-global-mode 1)
  (defun shx-send-input-or-open-thing ()
    "Open thing at point, or send input if no identifiable thing."
    (interactive)
    (when (shx-point-on-input-p)
      (shx-send-input))))

(use-package shell-pop
  :config
  (setq-ns shell-pop
    window-position "top"
    window-size 33 ;; percent
    full-span t
    )

  ;; interactive shell-pop bound to spc t index shell
  (defun ns/shell-pop (index)
    (let ((old-shell-index shell-pop-last-shell-buffer-index)
           (old-shell-buffer shell-pop-last-shell-buffer-name))
      (shell-pop index)

      (when (= index 9)
        (setq shell-pop-last-shell-buffer-index old-shell-index
          shell-pop-last-shell-buffer-buffer old-shell-buffer)))

    ;; todo here: if looking at a popped shell with no long running process, close the window of the same name in other frames
    )

  (mapcar (lambda (i) (ns/bind (concat "t" (number-to-string i)) (fn! (ns/shell-pop i))))
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
    (let ((result (s-clean (buffer-substring (window-start) (window-end)))))
      (if (eq major-mode 'shell-mode)
        (substring result 0 (- (length result) (length "shellshot") 1))
        result))))

;; cf http://trey-jackson.blogspot.com/2008/08/emacs-tip-25-shell-dirtrack-by-prompt.html
(defun shell-sync-dir-with-prompt (string)
  (if (string-match "\\+Pr0mPT\\+\\([^+]*\\)\\+" string)
    (let ((cwd (match-string 1 string)))
      (setq default-directory
        (if (string-equal "/" (substring cwd -1))
          cwd
          (setq cwd (concat cwd "/"))))

      ;; accumulate directories
      (when (not (boundp 'ns/cd-dirs))
        (setq ns/cd-dirs (list)))
      (add-to-list 'ns/cd-dirs default-directory)

      (replace-match "" t t string 0))
    string))

(defun ns/monitor-exit-sentinel (process change)
  "delete a frame if it only has a single window with no process"
  (when (equal change "finished\n")
    (when (eq 1 (-> (selected-frame) (window-list) (length)))
      (delete-frame))))

(defun ns/shell-mode-init ()
  (shell-dirtrack-mode nil)
  (add-hook 'comint-preoutput-filter-functions 'shell-sync-dir-with-prompt nil t)
  (setq-local inhibit-field-text-motion nil)
  ;; weird colon highlighting thing
  (setq-local shell-font-lock-keywords
    (-remove
      (fn (s-ends-with-p "]+:.*" (car <>)))
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
  (select-frame (make-frame))
  (ns/pickup-shell cwd t)

  ;; todo here: ensure there is no modeline on spawned terminal -- also maybe mode line refresh
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
      (car (mapcar 'process-id
             (-filter
               (fn (eq (process-buffer <>)
                     (current-buffer)))
               (process-list))))))

  (when terminal
    (when (string= (get-resource "Emacs.padding_source") "st")
      (set-window-fringes nil 0 0)))

  (when cwd (shell-pop--cd-to-cwd-shell cwd))

  ;; we don't care about how long it takes to stage the terminal
  (make-thread (fn (ns/stage-terminal)))
  ;; t
  nil
  )

;; killing this for now -- maybe check if anything is running in the window
(defun! ns/kill-spawned-shell (frame)
  (let ((windows (window-list frame)))
    (when (eq 1 (length windows))
      (let ((buffer (window-buffer (car windows))))
        (when (s-match "\*spawn-shell.*" (buffer-name buffer))
          (kill-buffer buffer))))))

;; (add-hook 'delete-frame-hook 'ns/kill-spawned-shell)
;; (remove-hook 'delete-frame-hook 'ns/kill-spawned-shell)

(ns/bind "at" 'ns/spawn-terminal)

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
(use-package company-shell
  :config
  (setq company-shell-clean-manpage t)
  (add-to-list 'company-backends 'company-shell)
  ;; possibly a weird one to add
  (add-to-list 'company-backends 'company-shell-env)
  )

;; crude -- just inserts and sends
(defun ns/shell-send (string)
  (with-current-buffer
    (format "*shell-%s*" shell-pop-last-shell-buffer-index)
    (goto-char (point-max))
    (insert string)
    (comint-send-input)))

(defun! ns/shell-eval-in-popped ()
  (if (use-region-p)
    (ns/shell-send (buffer-substring (region-beginning) (region-end)))

    (if (s-blank-p (s-trim (thing-at-point 'line)))
      ;; top level function definition
      (ns/shell-send
        ;; (save-excursion (forward-line -1)
        ;;   (let ((last-line (s-clean (thing-at-point 'line))))
        ;;     (if (string= last-line "}\n")
        ;;       ;; search back/return entire function def
        ;;       "todo"
        ;;       )))
        )

      ;; (eros-eval-last-sexp nil)
      ;; (eros-eval-defun nil)
      ;; no notion of higher order thing
      (ns/shell-send (thing-at-point 'line)))))

(provide 'shell)
;;; shell.el ends here
