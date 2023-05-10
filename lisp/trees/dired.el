
(ns/persist ns/cd-dirs (list))

(defun ns/dired-init()
  (hl-line-mode)

  ;; accumulate directories
  (add-to-list 'ns/cd-dirs (expand-file-name default-directory)))

;; cf. https://endlessparentheses.com/auto-focus-a-relevant-file-in-dired-buffers.html
(defun ns/dired-maybe-goto-file ()
  "focus the current file in dired when it exists"
  (when (and (bound-and-true-p ns/dired-last-file)
          (f-exists-p ns/dired-last-file))
    (dired-goto-file ns/dired-last-file)
    (setq ns/dired-last-file nil)))

(add-hook 'dired-initial-position-hook 'ns/dired-maybe-goto-file 'append)
(add-hook 'dired-mode-hook 'ns/dired-init)

;; Dired listing switches
;;  -a : Do not ignore entries starting with .
;;  -l : Use long listing format.
;;  -G : Do not print group names like 'users'
;;  -h : Human-readable sizes like 1K, 234M, ..
;;  -v : Do natural sort .. so the file names starting with . will show up first.
;;  -F : Classify filenames by appending '*' to executables,
;;       '/' to directories, etc.
(when-not (and ns/enable-mac-p (string= (which "ls") "/bin/ls"))
  (setq dired-listing-switches "-aAlGhvF --group-directories-first")) ; default: "-al"

(general-define-key
  :states '(normal)
  :keymaps 'dired-mode-map
  ;; the default 'r' only refreshes marked files. this gets everything
  "r" 'revert-buffer
  "h" 'dired-up-directory
  "l" 'dired-find-file
  (kbd "C-RET") (fn!! xdg-open
                  (->>
                    (dired-get-file-for-visit)
                    ;; (format "setsid nohup xdg-open \"%s\" &")
                    (format "xdg-open \"%s\"")
                    (sh-toss))
                  ;; (mapcar 'kill-buffer (ns/buffers-by-mode 'dired-mode))
                  )

  "s" (fn!! dired-shell
        (let ((existing-shell
                ;; existing-shell = a shell with the same cwd as the dired buffer we are looking at

                ;; there's a silly issue here.
                ;; when we call f-full tramp connections are realized but might not be connected, meaning lag/failure
                ;; but we need f-full because sometimes '~' is used in default directory
                ;; we can toss the tramp dirs before comparing with f-full/f-same-p to remove the delay
                (->> (ns/buffers-by-mode 'shell-mode)
                  (-remove (fn (s-starts-with-p "*shell-" (buffer-name <>))))
                  (-remove (fn (file-remote-p (buffer-local-value 'default-directory <>))))
                  (-filter (fn (f-same-p (buffer-local-value 'default-directory <>)
                                 default-directory)))
                  (first))))

          (if (and existing-shell
                (not (string= (buffer-name existing-shell) "*spawn-shell-staged*")))
            (switch-to-buffer existing-shell)
            (ns/pickup-shell (expand-file-name default-directory)))))

  ;; "q" (fn! (mapcar 'kill-buffer (ns/buffers-by-mode 'dired-mode)))
  "q" 'previous-buffer)

(ns/bind "d" (fn!! dired
               (setq ns/dired-last-file (buffer-file-name))
               (when (eq major-mode 'shell-mode)
                 (-when-let (b (get-buffer "*spawn-shell-staged*"))
                   (kill-buffer b))
                 (rename-buffer "*spawn-shell-staged*"))
               (dired ".")))
