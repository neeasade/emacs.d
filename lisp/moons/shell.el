(require 'comint)

(when ns/enable-linux-p
  (setq explicit-shell-file-name (getenv "SHELL")))

(when (and ns/enable-windows-p (not ns/enable-docker-p))
  (setq explicit-shell-file-name (car (s-split "\n" (ns/shell-exec "where bash"))))
  (setq explicit-bash.exe-args '("--login" "-i"))

  (setenv "PATH"
    (format "%s;%s"
      (~ "scoop/apps/git-with-openssh/current/usr/bin/")
      (getenv "PATH"))))

;; cf https://stackoverflow.com/questions/25862743/emacs-can-i-limit-a-number-of-lines-in-a-buffer
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
(setq comint-buffer-maximum-size 2000)
(setq comint-prompt-read-only t)

(define-key comint-mode-map (kbd "<up>") 'comint-previous-input)
(define-key comint-mode-map (kbd "<down>") 'comint-next-input)

(when ns/enable-colemak
  (define-key comint-mode-map (kbd "C-n") 'comint-next-input)
  (define-key comint-mode-map (kbd "C-e") 'comint-previous-input))

(use-package shx
  :config
  (shx-global-mode 1)
  ;; todo: find a way to alias things
  ;; ie clear --> :clear, term, exit
  (defun shx-cmd-term (placeholder)
    (interactive)
    (let ((term (if ns/enable-windows-p "cmd" (getenv "TERMINAL")))
           ;; (default-directory (~ ""))
           )
      (shell-command (format "nohup %s &" term) nil nil))))

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
               (defun ,funcname () (interactive) (shell-pop ,index))
               (ns/bind ,(concat "t" (number-to-string index)) ',funcname)))))

  ;; give us 1-9
  (mapc 'makepop (number-sequence 1 9))
  (ns/bind "'" 'shell-pop)

  ;; cf https://github.com/kyagi/shell-pop-el/issues/51
  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

  ;; todo: make this use a fresh shell or something, maybe cleanup empty shells at some point
  (defcommand shell-pop-ranger-dir ()
    (let ((ranger-dir (expand-file-name default-directory)))
      (switch-to-buffer ns/last-shell)
      (shell-pop--cd-to-cwd-shell ranger-dir))
    ;; note: keep this outsite of let to close properly
    (ranger-kill-buffers-without-window)
    )

  (define-key ranger-mode-map (kbd "s") 'ns/shell-pop-ranger-dir)
  )

;; fix for term, ansi term
;; https://github.com/hlissner/emacs-doom-themes/issues/54
(setq ansi-term-color-vector [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white])
