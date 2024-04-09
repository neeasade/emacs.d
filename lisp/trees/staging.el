;; -*- lexical-binding: t; -*-

(ns/bind "nk" (fn!! goto-theme (find-file (executable-find "theme"))
                (goto-line 0)
                (re-search-forward (if ns/enable-work-p "work-theme" "home-theme"))
                (recenter)))

;; https://github.com/szermatt/emacs-bash-completion
;; comprehensive bash completion in emacs
;; testing out [Fri Dec 20 15:13:58 2019]
;; todo: this is broken, just freezes the shell
;; (ns/use bash-completion)
;; (bash-completion-setup)

(ns/use rainbow-mode
  (setq rainbow-html-colors nil
    rainbow-x-colors nil)
  (ns/bind "tc" 'rainbow-mode))

;; M-x direnv-update-environment
;; sync from the pov of the current file
(ns/use direnv)

(ns/use git-link (setq git-link-open-in-browser t)
  (ns/bind "ng" 'git-link))

;; this seems to be a little nicer:
;; (ns/use browse-at-remote)

;; (named-timer-run :show-periodic-reminder
;;   t
;;   (ns/t 2h)
;;   (fn
;;     (when (< (second (current-idle-time)) 120)
;;       (alert (let ((reminders
;;                      (org-ql-select org-default-notes-file
;;                        '(tags "reminders")
;;                        :action '(s-clean (org-get-heading t t)))
;;                      ))
;;                (nth (random (length reminders)) reminders))
;;         :severity 'normal
;;         :title "*Reminder*"
;;         ))))

;; whether or not to rely on notifications from the fs that files have changed
;; when set to nil, checks every 5 seconds
(setq auto-revert-use-notify nil)

;; for emacs <28 org-ql
(when-not (fboundp 'byte-run--set-speed)
  (defalias 'byte-run--set-speed
    #'(lambda (f _args val)
        (list 'function-put (list 'quote f)
          ''speed (list 'quote val)))))

;; (let ((org-super-agenda-groups
;;         '((:auto-group t))))
;;   (org-agenda-list))

(when ns/enable-work-p
  ;; somehow initialize is broken in macos at the moment
  (setq exec-path
    (--> (exec-path-from-shell-initialize)
      (second it)
      (cdr it)
      (s-split ":" it)
      (-snoc it (~ ".nix-profile/bin/"))
      (-snoc it "/run/current-system/sw/bin")))


  (setenv "PATH" (s-join ":" exec-path)))

;; (ns/use frog-jump-buffer
;;   
;;   (ns/bind "b" 'frog-jump-buffer)
;;   (ns/bind "B" 'frog-jump-buffer-other-window)

;;   (setq frog-jump-buffer-default-filter
;;     ;; 'frog-jump-buffer-filter-file-buffers
;;     ;; 'frog-jump-buffer-filter-same-project
;;     'frog-jump-buffer-filter-recentf
;;     ;; 'ns/jump-file-candidates
;;     )

;;   ;; (setq frog-menu-avy-padding)
;;   (setq frog-menu-avy-keys '(?a ?r ?s ?t ?g ?k ?n ?e ?i ?o))
;;   (setq frog-jump-buffer-max-buffers (length frog-menu-avy-keys))
;;   (setq frog-jump-buffer-include-current-buffer nil)
;;   (setq frog-jump-buffer-posframe-parameters
;;     `(;; cf https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-and-Color-Parameters.html
;;        (background-color . ,(tarp/get :background :weak))

;;        (foreground-color . ,(tarp/get :foreground :weak))
;;        (left . 0.0)
;;        ))

;;   ;; (set-face-attribute 'avy-lead-face nil :box (tarp/get :faded))
;;   (set-face-attribute 'avy-lead-face nil :box nil))

(defun frog-menu-type ()
  "Return variable `frog-menu-type' to use."
  (cond
    ((display-graphic-p)
      'avy-posframe)
    (t 'avy-side-window)))

;; (frog-menu-type)

;; want a shortcut to open:
;; associated PR
;; or just git repo generally

;; clean-buffer-list-delay-general
;; clean-buffer-list-delay-special
;; clean-buffer-list-kill-buffer-names
;; clean-buffer-list-kill-never-buffer-names
;; clean-buffer-list-kill-regexps
;; clean-buffer-list-kill-never-regexps

;; consider the current day to end at 3AM
;; (setq org-extend-today-until 0)

;; make timestamp processing functions aware of this
;; (setq org-use-effective-time nil)

;; todo: I'm not sure why we set this
(setq org-duration-format
  (quote h:mm))

(defun! ns/toggle-modeline ()
  "toggle the modeline in the current buffer"
  (setq mode-line-format
    (if mode-line-format
      nil '("%e" (:eval (doom-modeline-format--neeasade-doomline)))))
  (redraw-frame))

(ns/bind "tm" 'ns/toggle-modeline)

(setq undo-tree-enable-undo-in-region t)

(defun! ns/straight-check-sync-status ()
  (let ((versions-alist (straight--lockfile-read-all))
         (out-of-sync '()))
    (straight--map-repos
      ;; repo is eg (:type git :flavor melpa :host github :repo "spotify/dockerfile-mode" :package "dockerfile-mode" :local-repo "dockerfile-mode")
      (-lambda ((&plist :package :local-repo :type))
        (-when-let (recipe (ht-get straight--recipe-cache package))
          (when (and local-repo
                  (straight--repository-is-available-p recipe))
            (-when-let (commit (cdr (assoc local-repo versions-alist)))
              ;; todo: probably want to check if repo has unstaged changes too
              (when-not (string= (straight-vc-get-commit type local-repo) commit)
                (add-to-list 'out-of-sync package)))))))
    (-map 'message out-of-sync)
    (message "straight packages out of sync: %s" (length out-of-sync))))

(defhydra hydra-expand-region ()
  ("n" er/expand-region "expand")
  ("e" er/contract-region "contract"))

(general-define-key :states 'visual "v" #'hydra-expand-region/body)

(setq search-invisible t)

(defun ns/browse-url-slack (original-browse &rest args)
  (let* ((url (first args))
          (slack? (string-match-p (regexp-quote "slack.com") url))
          (original-url-generic-program browse-url-generic-program)
          (return (if (and slack? ns/enable-mac-p (boundp 'ns/slack-map))
                    (progn
                      (setq browse-url-generic-program "open")
                      (-let* (((_ domain channel-id message-id) (s-match (rx "https://" (group (+ any)) "/archives/" (group (+ any)) "/" (group (+ any)) eol) url))
                               (slack-app-id (ht-get ns/slack-map domain)))
                        (funcall original-browse (format "slack://channel?team=%s&id=%s&message=%s" slack-app-id channel-id message-id))))
                    (apply original-browse args))))
    (setq browse-url-generic-program original-url-generic-program)
    return))

(advice-add 'browse-url :around #'ns/browse-url-slack)

;; still used in emacs_dmenu currently
(ns/use ivy)

;; fun
(defmacro ns/let-setqs (kvs sexp)
  (llet [syms (-map 'first (-partition 2 kvs))
          syms-old (--map (intern (format "%s-old" it)) syms)]
    `(llet [,@(-interleave syms-old syms)
             ret nil]
       (setq ,@kvs
         ret ,sexp
         ,@(-interleave syms syms-old))
       ret)))

;; 10x the default, enable fat copy
(setq xterm-max-cut-length (* 10 100000))

;; (ns/use (kkp :host github :repo "benjaminor/kkp")
;;   (when-not window-system
;;     (global-kkp-mode +1)))

;; idea: a function for jumping to shell-modes with cwd (ie find where a lein run or docker something is held etc)

;; todo: revert all file buffers interactive fn

;; doesn't feel quite right to turn off warnings AND errors, but it's really annoying
(setq native-comp-async-report-warnings-errors nil)

(defadvice flycheck-error-list-refresh (around shrink-error-list activate)
  ;; ?
  ;; ad-do-it
  (-when-let (window (flycheck-get-error-list-window t))
    (with-selected-window window
      (fit-window-to-buffer window 30))))

(setq server-window 'pop-to-buffer)

(defun ns/org-open-region-as-pdf ()
  ;; render region with org defaults, open in pdf program to consider printing
  (ns/mustache (slurp (~e "org/print.org"))
    (-ht :content (if (region-active-p)
                    (buffer-substring (region-beginning) (region-end))
                    (buffer-string)))))

(defun! ns/org-pomodoro-short-break ()
  (ns/org-clock-out)
  (run-hooks 'org-pomodoro-finished-hook)
  (org-pomodoro-start :short-break))

(defun! ns/org-pomodoro-long-break ()
  (ns/org-clock-out)
  (run-hooks 'org-pomodoro-finished-hook)
  (org-pomodoro-start :long-break))

;; one day
;; (ns/use org-roam)

(named-timer-run :angy-self
  0 5
  (fn (when (org-clock-is-active)
        (llet [work-pomo? (and (s-contains? "pomo" org-clock-heading)
                            (s-contains? "work" org-clock-heading))
                active? (< (org-user-idle-seconds) 10)]
          (when (and work-pomo? active?)
            (--map (alert (ns/str "😠" it))
              (-iota 15)))))))

(defun! ns/generate-myron-cache ()
  (llet [cache (--mapcat
                 (progn
                   (ns/load-theme (intern (format "myron-%s" it)))
                   (list (intern (format "myron-%s" it))
                     (myron--create-meta-colors (funcall (intern (format "myron-%s-create" it))))))
                 '(dogman grayscale kobo mcfay room storm struan))
          cache-def `(defcustom myron--cache ,cache
                       "Cache value for the themes. Internal use only."
                       :type 'sexp
                       :group 'myron)]
    (spit (~e "straight/repos/myron-themes/myron-cache.el")
      (with-temp-buffer
        (insert (ns/str cache-def))
        (insert (ns/str '(provide 'myron-cache)))
        (emacs-lisp-mode)
        (pp-buffer)
        (buffer-string)))))

(provide 'staging)
;;; staging.el ends here
