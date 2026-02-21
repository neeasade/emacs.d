;; -*- lexical-binding: t; -*-
;; queries, inserts, surfers/jumpers
;; and some misc

(ns/use which-key
  (setq-ns which-key
    idle-delay 1.5
    side-window-max-width 0.33
    sort-order 'which-key-key-order-alpha)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

;; used for counsel-git-command
(ns/use counsel)

(defun ns/get-functions ()
  "Get all the defconfig entries in the forest."
  (->> (~e "lisp/forest.el")
    f-read
    (s-match-strings-all "^(ns/defconfig [^ \(\)]+")
    (mapcar (fn (->> (car <>) (s-chop-prefix "(ns/defconfig ") (s-chomp))))
    (append '("dirt" "init" "forest"))))

(defun! ns/check-for-orphans ()
  "Check to see if any defconfigs are missing from init."
  (let ((initfile (f-read (~e "init.el"))))
    (-map
      (lambda (conf)
        (when (not (s-contains? conf initfile))
          (message (concat "orphaned function!: " conf))))
      (ns/get-functions))))

(defun! ns/jump-config ()
  (llet [f (ns/pick "config" (ns/get-functions))]
    (condp string= f
      "dirt" (ns/find-or-open (~e "lisp/dirt.el"))
      "forest" (ns/find-or-open (~e "lisp/forest.el"))
      "init" (ns/find-or-open (~e "init.el"))
      "follow-dwim" (ns/find-or-open (~e "lisp/trees/follow.el"))
      (if (f-exists-p (format (~e "lisp/trees/%s.el") f))
        (ns/find-or-open (format (~e "lisp/trees/%s.el") f))
        (progn
          (ns/find-or-open (~e "lisp/forest.el"))
          (goto-char (point-min))
          (re-search-forward (format "defconfig %s" f))))))
  (recenter))

(defun ns/what-face (&optional point)
  (interactive)
  (let* ((point (or point (point)))
          (face (or (get-char-property point 'read-face-name)
                  (get-char-property point 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" point))))

(defun! ns/what-minor-modes ()
  "Show enabled minor modes"
  (->> minor-mode-alist
    (--keep (when-let (enabled (symbol-value (first it)))
              (first it)))
    (-map 'ns/str)
    (s-join " ")
    (message)))

(defun! ia/surf-dirs (&optional remote?)
  (llet [dir (ns/pick "directory" (ns/atuin-list-dirs remote?))]
    (if-not (eq major-mode 'shell-mode)
      (dired dir)
      (progn
        (goto-char (point-max))
        (insert
          (cond
            ((not remote?) (format "cd \"%s\"" (s-replace "~" (getenv "HOME") dir)))
            ((string= (file-remote-p dir) (file-remote-p default-directory))
              (format "cd \"%s\"" (s-replace (file-remote-p dir) "" dir)))
            (t (let ((default-directory dir))
                 (shell)))))
        (comint-send-input)))))

(ns/bind "nd" 'ia/surf-dirs)
(ns/bind "nD" (fn!! surf-remote-dirs (ia/surf-dirs t)))

(defun! ns/surf-urls ()
  "jump to url in current window text"
  ;; has a nice url regexp
  (require 'rcirc)

  (let* ((window-text (s-clean (buffer-substring (window-start) (window-end))))
          (urls (s-match-strings-all rcirc-url-regexp window-text))
          (urls (-map 'car urls)))
    (if urls
      (browse-url (ns/pick urls))
      (message "no urls!"))))

(ns/use (deadgrep :host github :repo "Wilfred/deadgrep")
  (setq deadgrep-max-line-length 180)

  (defun deadgrep--include-args (rg-args)
    (push "--hidden" rg-args) ;; consider hidden folders/files
    (push "--follow" rg-args)) ;; follow symlink

  (advice-add 'deadgrep--arguments :filter-return #'deadgrep--include-args))

(ns/bind
  "SPC" (fn!! (execute-extended-command nil))

  "t" '(:ignore t :which-key "Toggle")
  "th" 'hl-line-mode

  "/" (if (which "rg") 'consult-ripgrep 'consult-grep)
  "?" (fn!! grep-here
        (funcall (if (which "rg") 'consult-ripgrep 'consult-grep)
          default-directory))

  "s" '(:ignore t :which-key "Search")
  "ss" 'deadgrep

  "a" '(:ignore t :which-key "Applications")

  "q" '(:ignore t :which-key "Query")
  "qf" 'ns/what-face
  "qF" (fn!! grab-file-path
         (pbcopy (buffer-file-name))
         (message (pbpaste)))
  "qc" 'describe-char
  "qm" (fn!! what-major-mode (message "%s" major-mode))
  "qM" 'ns/what-minor-modes

  "n" '(:ignore t :which-key "Jump")
  ;; todo: idea, make jump-config preview location like consult does with files, would be cool
  "nc" 'ns/jump-config
  "nC" (fn!! jump-using
         (if-not (which "rg")
           (message "no ripgrep found!!")
           (llet [default-directory (~e "lisp")
                   options (sh-lines "rg '\\(ns/use \\(?([^ \")]+)' -o -r '$1'  --no-filename --no-line-number")
                   match (ns/pick options)
                   ;; line-info is eg "trees/git.el:23:(ns/use (magit-todos"
                   line-info (sh (format "rg  -U '\\(ns/use \\(?%s([^a-zA-Z-]|\n)' -n -o" match))
                   (file line-number . _) (s-split ":" line-info)]
             ;; nb: caution with this pattern, took a bit of poking
             (switch-to-buffer (find-file-existing (~e "lisp" file)))
             (goto-line (string-to-number line-number))
             (recenter))))
  "nh" 'consult-imenu
  "nu" 'ns/surf-urls)

(defun! ns/helpful-or-dashdoc ()
  (cond
    ((eq 'emacs-lisp-mode major-mode)
      (helpful-callable (helpful--symbol-at-point)))
    ((eq 'clojure-mode major-mode) (cider-apropos-documentation))
    (ns/enable-dashdocs-p (ns/counsel-dash-word))
    (t (message "no doc option available!"))))

(ns/bind "nH" 'ns/helpful-or-dashdoc)

;; "eemacs"
(--map (setenv it nil) '("SSH_ASKPASS" "SSH_CONNECTION" "SSH_TTY"))

;; this works one way (emacs -> clip, not clip -> emacs)
(ns/use clipetty
  (setenv "SSH_TTY" nil)                  ; pretty much never want this?
  (global-clipetty-mode (if ns/term? t -1))) ; osc 52

(defun clipetty--emit (string)
  "Emit STRING, optionally wrapped in a DCS, to an appropriate tty."
  (let ((tmux    (getenv "TMUX" (selected-frame)))
         (term    (getenv "TERM" (selected-frame)))
         (ssh-tty (getenv "SSH_TTY" (selected-frame))))
    ;; foot's osc52 limit is like 2gb
    (if (or (string= term "foot") (<= (length string) clipetty--max-cut))
      (write-region
        (clipetty--dcs-wrap string tmux term ssh-tty)
        nil
        (clipetty--tty ssh-tty tmux)
        t
        0)
      (message "Selection too long for osc52 (length: %d)" (length string)))))

(when ns/term?
  (when (not ns/kitty?)
    (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle))

  (xterm-mouse-mode 1)

  ;; scroll 5 lines at a time
  (setq mouse-wheel-scroll-amount
    '(5 ((shift) . hscroll) ((meta)) ((control meta) . global-text-scale)
       ((control) . text-scale))))

(ns/use (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
  (setq scroll-conservatively 101
    scroll-margin 0)
  (ultra-scroll-mode (if ns/term? -1 t)))
