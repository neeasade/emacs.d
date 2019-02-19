(require 'comint)

(when ns/enable-linux-p
  (setq explicit-shell-file-name (getenv "SHELL")))

(when (and ns/enable-windows-p (not ns/enable-docker-p))
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

;; todo: see if there is a smarter version of this
(add-hook 'shell-mode-hook 'shell-dirtrack-mode)

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

                   (when (= ,index 9)
                     (setq
                       shell-pop-last-shell-buffer-index old-shell-index
                       shell-pop-last-shell-buffer-buffer old-shell-buffer))))
               (ns/bind ,(concat "t" (number-to-string index)) ',funcname)))))

  ;; give us 1-9
  (mapc 'makepop (number-sequence 1 9))

  ;; treat 9 special, meant to be a long running buffer
  (ns/bind "'" (fn! (shell-pop
                      (if (string= (buffer-name (current-buffer)) "*shell-9*")
                        shell-pop-last-shell-buffer-index nil))))

  (ns/bind "\"" (fn!
                  (if (string= (buffer-name (current-buffer)) "*shell-9*")
                    (evil-window-delete)
                    (shell-pop-9)
                    )))

  ;; cf https://github.com/kyagi/shell-pop-el/issues/51
  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)
  (defcommand shell-pop-ranger-dir ()
    (let ((ranger-dir (expand-file-name default-directory)))
      (ns/pickup-shell)
      (shell-pop--cd-to-cwd-shell ranger-dir))
    ;; note: keep this outsite of let to close properly
    (ranger-kill-buffers-without-window))

  (define-key ranger-mode-map (kbd "s") 'ns/shell-pop-ranger-dir))

;; fix for term, ansi term
;; https://github.com/hlissner/emacs-doom-themes/issues/54
(setq ansi-term-color-vector [term term-color-black term-color-red term-color-green term-color-yellow term-color-blue term-color-magenta term-color-cyan term-color-white])
