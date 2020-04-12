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
  (set-face-attribute 'hl-line nil :background
    ;; todo: make lessen script a defun and use here
    (ns/color-tone (first evil-visual-state-cursor) -7 -7))
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
(setq dired-listing-switches "-alGhvF --group-directories-first") ; default: "-al"

(general-define-key
  :states '(normal)
  :keymaps 'dired-mode-map
  "h" 'dired-up-directory
  "l" 'dired-find-file
  (kbd "<C-return>") (fn!
                       (->>
                         (dired-get-file-for-visit)
                         (format "setsid nohup xdg-open \"%s\" &")
                         (ns/shell-exec-dontcare))
                       ;; (mapcar 'kill-buffer (ns/buffers-by-mode 'dired-mode))
                       )

  "s"
  (fn!
    (let ((existing-shell
            ;; there's a silly issue here.
            ;; when we call f-full tramp connections are realized but might not be connected, meaning lag/failure
            ;; but we need f-full because sometimes '~' is used in default directory
            ;; we can toss the tramp dirs before comparing with f-full/f-same-p to remove the delay
            (->> (ns/buffers-by-mode 'shell-mode)
              (-filter (fn (not (s-starts-with-p "*shell-" (buffer-name <>)))))
              (-filter (fn (not (file-remote-p (buffer-local-value 'default-directory <>)))))
              (-filter (fn (f-same-p (buffer-local-value 'default-directory <>)
                             default-directory)))
              (first))))

      (if existing-shell
        (switch-to-buffer existing-shell)
        (ns/pickup-shell (expand-file-name default-directory))))
    ;; todo: maybe:
    ;; (mapcar 'kill-buffer (ns/buffers-by-mode 'dired-mode))
    )
  "q" (fn! (mapcar 'kill-buffer (ns/buffers-by-mode 'dired-mode))))

(defun my-resize-margins ()
  (let ((margin-size (if ns/center (/ (- (frame-width) 120) 2) 0)))
    (set-window-margins nil margin-size margin-size)))

(defun! ns/toggle-margin ()
  (if (not (bound-and-true-p ns/center))
    (setq ns/center nil))

  (if ns/center
    (remove-hook 'window-configuration-change-hook #'my-resize-margins)
    (add-hook 'window-configuration-change-hook #'my-resize-margins))

  (setq ns/center (not ns/center))
  (my-resize-margins))

(defun! ns/kill-current-buffer()
  (kill-buffer nil))

(defun! ns/follow-mode ()
  (follow-mode)
  (delete-other-windows)
  (evil-window-vsplit))

(use-package alert
  :config (setq alert-default-style
            (if ns/enable-windows-p 'toaster 'libnotify)))

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

(ns/bind
  "/" (if (executable-find "rg") 'counsel-rg 'counsel-git-grep)

  ;; from the current dir down
  "?" (if (executable-find "rg")
        (fn! (counsel-rg nil default-directory))
        (fn! (counsel-git-grep nil default-directory)))

  "TAB" '(switch-to-other-buffer :which-key "prev buffer")
  "SPC" 'counsel-M-x

  ;; windows
  "w" '(:ignore t :which-key "Windows")
  "wh" 'evil-window-left
  "wn" 'evil-window-down
  "we" 'evil-window-up
  "wl" 'evil-window-right
  "wd" 'evil-window-delete
  "ww" 'other-window
  "ws" 'split-window-horizontally
  "wS" 'split-window-vertically
  "wf" 'ns/follow-mode
  "wc" 'ns/toggle-margin

  "wm" 'delete-other-windows ;; window-max
  "wo" 'other-frame

  ;; todo idea here: check if we are in a shell, if so, make that the staged shell (or 'dired' shell)
  ;; so we can have fluid state across dired transitions "s" <--> "SPC d"
  "d" (fn! (setq ns/dired-last-file (buffer-file-name))
        (dired "."))

  "a" '(:ignore t :which-key "Applications")
  "q" '(:ignore t :which-key "Query")

  "b" '(:ignore t :which-key "Buffers")
  "bb" 'counsel-ibuffer
  "bd" 'ns/kill-current-buffer
  "bK" 'ns/kill-other-buffers
  "bk" 'kill-matching-buffers

  "n" '(:ignore t :which-key "Jump")
  "nh" 'counsel-imenu
  )
