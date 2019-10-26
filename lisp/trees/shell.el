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

(ns/inmap 'comint-mode-map "<up>"      'comint-previous-input)
(ns/inmap 'comint-mode-map "<down>"    'comint-next-input)
(ns/inmap 'comint-mode-map (kbd "C-e") 'comint-previous-input)
(ns/inmap 'comint-mode-map (kbd "C-n") 'comint-next-input)


(use-package shx :config (shx-global-mode 1))

(use-package shell-pop
  :config
  (setq-ns shell-pop
    window-position "top"
    window-size 33 ;; percent
    full-span t
    )

  ;; interactive shell-pop bound to spc t index shell
  (defun makepop (index)
    (let ((funcname (intern (concat "shell-pop-" (number-to-string index)))))
      (eval `(progn
               (defun ,funcname () (interactive)
                 (let ((old-shell-index shell-pop-last-shell-buffer-index)
                        (old-shell-buffer shell-pop-last-shell-buffer-name))
                   (shell-pop ,index)

                   ,(when (= index 9)
                      '(setq
                         shell-pop-last-shell-buffer-index old-shell-index
                         shell-pop-last-shell-buffer-buffer old-shell-buffer))))
               (ns/bind ,(concat "t" (number-to-string index)) ',funcname)))))

  ;; give us 1-9
  (mapcar 'makepop (number-sequence 1 9))

  ;; treat 9 special, meant to be a long running buffer
  (ns/bind "'" (fn!
                 (shell-pop
                   (if (string= (buffer-name (current-buffer)) "*shell-9*")
                     shell-pop-last-shell-buffer-index nil))))

  ;; todo: if a shell-pop-1-8 is open, close it before doing this (not just the active window)
  (ns/bind "\"" (fn!
                  (if (string= (buffer-name (current-buffer)) "*shell-9*")
                    (evil-window-delete)
                    (progn
                      (shell-pop
                        (if (string= (buffer-name (current-buffer)) "*shell-9*")
                          shell-pop-last-shell-buffer-index nil))
                      (shell-pop-9)))))

  ;; cf https://github.com/kyagi/shell-pop-el/issues/51
  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
  )

(defcommand windowshot ()
  "get a string that is the currently displayed text in emacs window"
  (with-current-buffer (window-buffer)
    (let ((result (s-clean (buffer-substring (window-start) (window-end)))))
      (if (eq major-mode 'shell-mode)
        (substring result 0 (- (length result) (length "shellshot") 1))
        result))))


(defun shell-sync-dir-with-prompt (string)
  "A preoutput filter function (see `comint-preoutput-filter-functions')
which sets the shell buffer's path to the path embedded in a prompt string.
This is a more reliable way of keeping the shell buffer's path in sync
with the shell, without trying to pattern match against all
potential directory-changing commands, ala `shell-dirtrack-mode'.

In order to work, your shell must be configured to embed its current
working directory into the prompt.  Here is an example .zshrc
snippet which turns this behavior on when running as an inferior Emacs shell:

  if [ $EMACS ]; then
     prompt='|Pr0mPT|%~|[%n@%m]%~%# '
  fi

The part that Emacs cares about is the '|Pr0mPT|%~|'
Everything past that can be tailored to your liking.
"
  (if (string-match "|Pr0mPT|\\([^|]*\\)|" string)
    (let ((cwd (match-string 1 string)))
      (setq default-directory
        (if (string-equal "/" (substring cwd -1))
          cwd
          (setq cwd (concat cwd "/"))))

      ;; accumulate directories
      (when (not (boundp 'cd-dirs))
        (setq cd-dirs (list)))

      (setq cd-dirs (cons default-directory cd-dirs))

      (replace-match "" t t string 0))
    string))

(defcommand cd-dir-history ()
  (ivy-read "dir: "
    ;; todo: this should filter to the current tramp, rather than remove them all
    ;; could then change ivy prompt show host
    (-uniq (-filter (fn (not (s-starts-with-p "/ssh" <>))) cd-dirs))

    :action
    ;; todo: make sanity check cd is clear
    ;; maybe clear prompt if so
    (fn (goto-char (point-max))
      (insert (concat "cd \"" <> "\""))
      (comint-send-input)
      )))

(ns/bind-mode 'shell "nd" #'ns/cd-dir-history)

(defun ns/shell-track ()
  (shell-dirtrack-mode nil)
  (add-hook 'comint-preoutput-filter-functions 'shell-sync-dir-with-prompt nil t))
(add-hook 'shell-mode-hook 'ns/shell-track)

(defcommand stage-terminal ()
  (let ((default-directory (~ "")))
    (shell "*spawn-shell-staged*")
    (ns/toggle-modeline)
    (delete-window)))

(ns/stage-terminal)

(defcommand spawn-terminal ()
  (select-frame (make-frame))
  (ns/pickup-shell) t)

(defcommand pickup-shell ()
  (switch-to-buffer (get-buffer "*spawn-shell-staged*"))
  (rename-buffer (concat "*spawn-shell-" (number-to-string (random)) "*"))
  (delete-other-windows)

  (when (string= (get-resource "Emacs.padding_source") "st")
    (set-window-fringes nil 0 0))

  (ns/stage-terminal))

(defcommand kill-spawned-shell (frame)
  (let ((windows (window-list frame)))
    (when (eq 1 (length windows))
      (let ((buffer (window-buffer (car windows))))
        (when (s-match "\*spawn-shell.*" (buffer-name buffer))
          (kill-buffer buffer))))))

(add-hook 'delete-frame-hook 'ns/kill-spawned-shell)

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
