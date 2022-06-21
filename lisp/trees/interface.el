;; -*- lexical-binding: t; -*-
(global-set-key (kbd "C-e") 'previous-line)

(use-package ivy
  :config

  (setq-ns ivy
    re-builders-alist '((ivy-switch-buffer . ivy--regex-plus)
                         (t . ivy--regex-fuzzy))
    initial-inputs-alist nil
    fixed-height-minibuffer t
    count-format "%d/%d "
    )

  (general-define-key
    :keymaps 'ivy-minibuffer-map
    (kbd "<C-return>") 'ivy-immediate-done)

  (setq ivy-height-alist '((t lambda (_caller) (/ (frame-height) 2))))

  (ivy-mode 1)

  (use-package prescient)
  (use-package ivy-prescient :config
    (ivy-prescient-mode)
    (prescient-persist-mode))

  (use-package company-prescient :config (company-prescient-mode))

  (prescient-persist-mode))

;; counsel
(use-package counsel
  :config
  (use-package rg)

  (when (executable-find "rg")
    (setq-ns counsel
      grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
      rg-base-command "rg -i -M 120 --hidden --no-heading --line-number --color never %s .")))

(defun ns/dired-init()
  (hl-line-mode)

  ;; accumulate directories
  (when (not (boundp 'ns/cd-dirs))
    (setq ns/cd-dirs (list)))
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
(setq dired-listing-switches "-aAlGhvF --group-directories-first") ; default: "-al"

(general-define-key
  :states '(normal)
  :keymaps 'dired-mode-map
  ;; the default 'r' only refreshes marked files. this gets everything
  "r" 'revert-buffer
  "h" 'dired-up-directory
  "l" 'dired-find-file
  (kbd "<C-return>") (fn!
                       (->>
                         (dired-get-file-for-visit)
                         ;; (format "setsid nohup xdg-open \"%s\" &")
                         (format "xdg-open \"%s\"")
                         (ns/shell-exec-dontcare))
                       ;; (mapcar 'kill-buffer (ns/buffers-by-mode 'dired-mode))
                       )

  "s"
  (fn!
    (let ((existing-shell
            ;; existing-shell = a shell with the same cwd as the dired buffer we are looking at

            ;; there's a silly issue here.
            ;; when we call f-full tramp connections are realized but might not be connected, meaning lag/failure
            ;; but we need f-full because sometimes '~' is used in default directory
            ;; we can toss the tramp dirs before comparing with f-full/f-same-p to remove the delay
            (->> (ns/buffers-by-mode 'shell-mode)
              (-filter (fn (not (s-starts-with-p "*shell-" (buffer-name <>)))))
              (-filter (fn (not (file-remote-p (buffer-local-value 'default-directory <>)))))
              (-filter (fn (f-same-p (buffer-local-value 'default-directory <>)
                             default-directory)))
              ;; todo: maybe should also check if shell buffer is idle as well
              (first))))

      (if existing-shell
        (switch-to-buffer existing-shell)
        (ns/pickup-shell (expand-file-name default-directory))))
    ;; todo: maybe:
    ;; (mapcar 'kill-buffer (ns/buffers-by-mode 'dired-mode))
    )
  ;; "q" (fn! (mapcar 'kill-buffer (ns/buffers-by-mode 'dired-mode)))
  "q" 'previous-buffer
  )

(defun! ns/kill-current-buffer()
  (kill-buffer nil))

(defun! ns/follow-mode ()
  (follow-mode)
  (delete-other-windows)
  (evil-window-vsplit))

(use-package alert
  :config (setq alert-default-style
            (cond
              (ns/enable-windows-p 'toaster)
              (ns/enable-mac-p 'osx-notifier)
              (t 'libnotify)))

  ;; I could not get the (alert :persistent t keyword to work)
  (defun alert! (&rest alert-args)
    (let ((alert-fade-time 0))
      (apply 'alert alert-args))))

(use-package which-key
  :config
  (setq-ns which-key
    idle-delay 1.5
    side-window-max-width 0.33
    sort-order 'which-key-key-order-alpha)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

(defun! ns/kill-other-buffers ()
  "Kill all other buffers."
  (mapc 'kill-buffer
    (delq (current-buffer)
      (remove-if-not 'buffer-file-name (buffer-list)))))

;; this multiply thing might be a dumb idea, maybe just prompt for desired font-size instead
(defun! ns/font-multiply ()
  (let ((multiplier (string-to-number (read-string (format "font multiplier: ")))))
    (set-face-attribute 'default nil
      :height
      (-> (get-resource "font.mono.spec")
        (ns/parse-font)
        (plist-get :height)
        (/ 10)
        (* multiplier)
        (round)
        (* 10)))))

(defun! ns/kill-buffers-missing-file ()
  "Kill buffers referencing a file that doesn't exist (EG, the file may have moved or been deleted)"
  (->> (buffer-list)
    (-keep
      (fn (llet [filename (buffer-local-value 'buffer-file-truename <>)]
            (when (and filename
                    (not (f-exists-p filename)))
              <>))))
    (-map #'kill-buffer)))

(defun! ns/kill-buffers-by-mode ()
  (ivy-read "mode to kill: "
    (->> (buffer-list)
      (-map (-partial 'buffer-local-value 'major-mode))
      (-uniq))
    :action
    (fn (-map #'kill-buffer (ns/buffers-by-mode (intern <>))))))

(ns/bind
  "nd"
  (fn!
    (ivy-read "directory: "
      (->> ns/cd-dirs
        (-uniq)
        (-filter (fn (s-equals-p (file-remote-p <>)
                       (file-remote-p default-directory)))))

      :action
      (lambda (dir)
        (cond
          ((eq major-mode 'dired-mode) (dired dir))
          ((eq major-mode 'shell-mode)
            (goto-char (point-max))
            (insert (concat "cd \""
                      (s-replace
                        (or (file-remote-p dir) "")
                        ""
                        dir
                        )
                      "\""))
            (comint-send-input))
          ;; (t (insert dir))
          (t (dired dir))
          )))))

(when-not window-system
  ;; (when running in a terminal)

  ;; utility:
  (xterm-mouse-mode 1)

  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line)

  (use-package xclip :config (xclip-mode t))

  ;; C-i and <tab> are equivalent in the terminal
  ;; (until kitty saves us all)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "C-i") #'org-cycle))

(winner-mode 1)

(ns/bind
  "/" (if (executable-find "rg") 'counsel-rg 'counsel-git-grep)

  ;; from the current dir down
  "?" (if (executable-find "rg")
        (fn! (counsel-rg nil default-directory))
        (fn! (counsel-git-grep nil default-directory)))

  "SPC" 'counsel-M-x

  ;; windows
  "w" '(:ignore t :which-key "Windows")
  "wh" 'evil-window-left
  "wn" 'evil-window-down
  "we" 'evil-window-up
  "wl" 'evil-window-right
  "wd" 'evil-window-delete
  "ww" 'other-window
  "wb" 'balance-windows-area
  "ws" (fn! (split-window-horizontally)
         (evil-window-right 1))
  "wS" (fn!
         (split-window-vertically)
         (evil-window-down 1))

  "wf" 'ns/follow-mode

  "wm" 'delete-other-windows ;; window-max

  "wo" 'winner-undo
  "wi" 'winner-redo

  ;; todo idea here: check if we are in a shell, if so, make that the staged shell (or 'dired' shell)
  ;; so we can have fluid state across dired transitions "s" <--> "SPC d"
  "d" (fn! (setq ns/dired-last-file (buffer-file-name)) (dired "."))

  "a" '(:ignore t :which-key "Applications")
  "q" '(:ignore t :which-key "Query")

  ;; "b" '(:ignore t :which-key "Buffers")
  ;; "bb" 'counsel-ibuffer
  ;; "bd" 'ns/kill-current-buffer
  ;; "bK" 'ns/kill-other-buffers
  ;; "bk" 'kill-matching-buffers
  ;; "bm" 'ns/kill-buffers-by-mode

  "n" '(:ignore t :which-key "Jump")
  "nh" 'counsel-imenu
  )
