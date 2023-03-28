;; -*- lexical-binding: t; -*-

(global-set-key (kbd "C-e") 'previous-line)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
  '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(ns/use vertico)
(vertico-mode nil)


(ns/use ivy
  (setq-ns ivy
    re-builders-alist '((ivy-switch-buffer . ivy--regex-plus)
                         (t . ivy--regex-fuzzy))
    height-alist '((t lambda (_caller) (/ (frame-height) 2)))
    initial-inputs-alist nil
    fixed-height-minibuffer t
    count-format "%d/%d ")

  (general-define-key
    :keymaps 'ivy-minibuffer-map
    (kbd "C-RET") 'ivy-immediate-done)
  (general-define-key
    :keymaps 'ivy-minibuffer-map
    (kbd "C-<return>") 'ivy-immediate-done)

  (ivy-mode nil)

  )

(ns/use prescient)
(ns/use ivy-prescient (ivy-prescient-mode))
(ns/use company-prescient (company-prescient-mode))


(prescient-persist-mode)


(defun ns/pick (one &optional two)
  "Pick something from a list. accepts (prompt candidates) or (candidates)"
  (llet [(prompt candidates) (if two
                               (list (format "%s: " one) two)
                               (list "select: " one))]
    ;; (completing-read prompt candidates)
    (ivy-read prompt (-uniq candidates))))

;; counsel
(ns/use counsel
  (when (executable-find "rg")
    (setq-ns counsel
      grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
      rg-base-command "rg -i -M 120 --hidden --no-heading --line-number --color never %s .")))

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
(setq dired-listing-switches "-aAlGhvF --group-directories-first") ; default: "-al"

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
                    (ns/shell-exec-dontcare))
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
  "q" 'ns/maybe-prev)

(defun! ns/kill-current-buffer()
  (kill-buffer nil))

(ns/use alert
  (setq alert-default-style
    (cond
      (ns/enable-windows-p 'toaster)
      (ns/enable-mac-p 'osx-notifier)
      (t 'libnotify)))

  ;; I could not get the (alert :persistent t keyword to work)
  (defun alert! (&rest alert-args)
    (let ((alert-fade-time 0))
      (apply 'alert alert-args))))

(ns/use which-key
  (setq-ns which-key
    idle-delay 1.5
    side-window-max-width 0.33
    sort-order 'which-key-key-order-alpha)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

(defun! ns/kill-other-buffers ()
  "Kill all other buffers."
  (->> (buffer-list)
    (-remove (lambda (b) (eq b (current-buffer))))
    (-map 'kill-buffer)))

(defun! ns/font-change ()
  (llet [current-size (/ (face-attribute 'default :height) 10)
          new-size (read-number (format "new size (current-size: %s): " current-size))]
    (set-face-attribute 'default nil
      :height (* 10 new-size))))

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
  (->> (buffer-list)
    (-map (-partial 'buffer-local-value 'major-mode))
    (-uniq)
    (ns/pick "mode to kill")
    (intern)
    (ns/buffers-by-mode)
    (-map #'kill-buffer)))

(ns/bind
  "nd"
  (fn!! surf-dirs
    (llet [dir (ns/pick "directory: "
                 (->> ns/cd-dirs
                   (-uniq)
                   (-filter (fn (s-equals-p (file-remote-p <>)
                                  (file-remote-p default-directory))))))]
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
        (t (dired dir))))))

(when-not window-system
  ;; (when running in a terminal)

  ;; this doesn't work in kitty (might be related to new esc code things)
  (comment
    (xterm-mouse-mode 1)
    )
  ;; (xterm-mouse-mode nil)

  ;; these don't appear to be adding anything
  ;; (ns/use xclip (xclip-mode nil))
  ;; (ns/use clipetty (global-clipetty-mode t))

  ;; C-i and <tab> are equivalent in the terminal
  ;; (until kitty saves us all)
  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)

  (comment (define-key evil-motion-state-map (kbd "C-i") 'better-jumper-jump-forward))

  )

(winner-mode 1)

;; has a nice url regexp
(require 'rcirc)

(defun! ns/ivy-url-jump ()
  "jump to url in current window text"
  (let* ((window-text (s-clean (buffer-substring (window-start) (window-end))))
          (urls (s-match-strings-all rcirc-url-regexp window-text))
          (urls (-map 'car urls)))
    (if urls
      (browse-url (ns/pick urls))
      (message "no urls!"))))

(ns/bind "nu" 'ns/ivy-url-jump)

(ns/bind
  "/" (if (executable-find "rg") 'counsel-rg 'counsel-git-grep)

  ;; from the current dir down
  "?" (if (executable-find "rg")
        (fn! (counsel-rg nil default-directory))
        (fn! (counsel-git-grep nil default-directory)))

  "SPC" 'counsel-M-x
  ;; "SPC" (fn !! (execute-extended-command nil))

  ;; windows
  "w" '(:ignore t :which-key "Windows")
  "wh" 'evil-window-left
  "wn" 'evil-window-down
  "we" 'evil-window-up
  "wl" 'evil-window-right
  "wd" 'evil-window-delete
  "ww" 'other-window
  "wb" 'balance-windows-area

  ;; todo: a keybind to infer direction ala our external_rules bspwm scripts
  "ws" (fn!! (split-window-horizontally)
         (evil-window-right 1))

  "wS" (fn!! (split-window-vertically)
         (evil-window-down 1))

  "wf" (fn!! (follow-mode)
         (delete-other-windows)
         (evil-window-vsplit))

  "wm" 'delete-other-windows ;; "window max"

  "wo" 'winner-undo
  "wi" 'winner-redo

  "d" (fn!! dired
        (setq ns/dired-last-file (buffer-file-name))
        (when (eq major-mode 'shell-mode)
          (-when-let (b (get-buffer "*spawn-shell-staged*"))
            (kill-buffer b))
          (rename-buffer "*spawn-shell-staged*"))
        (dired "."))

  "a" '(:ignore t :which-key "Applications")
  "q" '(:ignore t :which-key "Query")

  ;; "b" '(:ignore t :which-key "Buffers")

  "bb" (fn!! surf-buffers
         (->> (ns/jump-file-candidates :buffers-without-files)
           (ns/pick "buffer")
           (counsel-switch-to-buffer-or-window)))

  "bm" (fn!! surf-buffers-mode
         (->> (ns/buffers-by-mode major-mode)
           (-map 'buffer-name)
           (ns/pick "buffer")
           (counsel-switch-to-buffer-or-window)))

  ;; "bd" 'ns/kill-current-buffer
  ;; "bK" 'ns/kill-other-buffers
  ;; "bk" 'kill-matching-buffers
  ;; "bm" 'ns/kill-buffers-by-mode

  "n" '(:ignore t :which-key "Jump")
  "nh" 'counsel-imenu
  )

(ns/use (deadgrep :host github :repo "Wilfred/deadgrep")
  (ns/bind "ss" 'deadgrep)
  (setq deadgrep-max-line-length 180)
  (general-nmap deadgrep-mode-map
    "RET" 'deadgrep-visit-result-other-window))
