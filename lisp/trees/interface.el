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
    (s-match-strings-all  "^(ns/defconfig [^ \(\)]+")
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
    (cond
      ((string= "dirt" f) (ns/find-or-open (~e "lisp/dirt.el")))
      ((string= "forest" f) (ns/find-or-open (~e "lisp/forest.el")))
      ((string= "init" f) (ns/find-or-open (~e "init.el")))
      ((string= "follow-dwim" f) (ns/find-or-open (~e "lisp/trees/follow.el")))
      ((f-exists-p (format (~e "lisp/trees/%s.el") f))
        (ns/find-or-open (format (~e "lisp/trees/%s.el") f)))
      (t
        (ns/find-or-open (~e "lisp/forest.el"))
        (goto-char (point-min))
        (re-search-forward (format "defconfig %s" f)))))
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

(ns/bind "nd"
  (fn!! surf-dirs
    (llet [dir (ns/pick "directory"
                 (->> ns/cd-dirs
                   (-uniq)
                   (-filter (fn (s-equals-p (file-remote-p <>)
                                  (file-remote-p default-directory))))))]
      (if-not (eq major-mode 'shell-mode)
        (dired dir)
        (progn
          (goto-char (point-max))
          (insert (format "cd \"%s\"" (s-replace (or (file-remote-p dir) "") "" dir)))
          (comint-send-input))))))

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
  (setq deadgrep-max-line-length 180))

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
  "qc" 'describe-char
  "qm" (fn!! what-major-mode (message "%s" major-mode))
  "qM" 'ns/what-minor-modes

  "n" '(:ignore t :which-key "Jump")
  "nc" 'ns/jump-config
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


;; where should this go
(when-not window-system
  ;; (when running in a terminal)

  ;; this doesn't work in kitty (might be related to new esc code things)
  (comment (xterm-mouse-mode 1))
  ;; (xterm-mouse-mode nil)

  ;; these don't appear to be adding anything
  ;; (ns/use xclip (xclip-mode nil))
  ;; (ns/use clipetty (global-clipetty-mode t))

  ;; C-i and <tab> are equivalent in the terminal
  ;; (until kitty saves us all)
  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)

  (define-key evil-motion-state-map (kbd "C-i")
    'better-jumper-jump-forward))
