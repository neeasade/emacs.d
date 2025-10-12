;; -*- lexical-binding: t; -*-

(ns/bind "nk" (fn!! goto-theme
                (find-file (~ ".dotfiles/bin/bin/themes/" (sh "hostname")))))

;; https://github.com/szermatt/emacs-bash-completion
;; comprehensive bash completion in emacs
;; testing out [Fri Dec 20 15:13:58 2019]
;; todo: this is broken, just freezes the shell
;; (ns/use bash-completion)
;; (bash-completion-setup)

;; M-x direnv-update-environment
;; sync from the pov of the current file
(ns/use direnv)

;; whether or not to rely on notifications from the fs that files have changed
;; when set to nil, checks every 5 seconds
(setq auto-revert-use-notify nil)

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

(ns/use frog-jump-buffer
  (ns/bind "u" 'frog-jump-buffer)
  (ns/bind "U" 'frog-jump-buffer-other-window)

  (setq frog-jump-buffer-default-filter
    'frog-jump-buffer-filter-file-buffers
    ;; 'frog-jump-buffer-filter-same-project
    ;; 'frog-jump-buffer-filter-recentf
    ;; 'ns/jump-file-candidates
    )

  ;; (setq frog-menu-avy-padding)
  (setq frog-menu-avy-keys '(?a ?r ?s ?t ?g ?k ?n ?e ?i ?o))
  (setq frog-jump-buffer-max-buffers (length frog-menu-avy-keys))
  (setq frog-jump-buffer-include-current-buffer nil)
  ;; only valid after a theme is loaded 😩
  (comment
    (setq frog-jump-buffer-posframe-parameters
      `(;; cf https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-and-Color-Parameters.html
         (background-color . ,(myron-get :background :weak))

         (foreground-color . ,(myron-get :primary :weak))
         (left . 0.0)
         )))
  )


;;   ;; (set-face-attribute 'avy-lead-face nil :box (myron-get :faded))
;;   (set-face-attribute 'avy-lead-face nil :box nil))

(defun frog-menu-type ()
  "Return variable `frog-menu-type' to use."
  (if ns/term?
    'avy-side-window
    'avy-posframe))

;; make timestamp processing functions aware of this
;; (setq org-use-effective-time nil)

;; todo: I'm not sure why we set this
(setq org-duration-format (quote h:mm))

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

(defun! ns/org-adhoc-timer ()
  (llet [duration (read-number "enter timer duration in minutes: ")]
    (ns/org-clock-out)
    (run-hooks 'org-pomodoro-finished-hook)
    (setq org-pomodoro-long-break-length duration)
    (org-pomodoro-start :long-break)))

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
  (llet [cache (--mapcat (progn (ns/load-theme (intern (format "myron-%s" it)))
                           (list (intern (format "myron-%s" it))
                             (myron-themes--create-meta-colors (funcall (intern (format "myron-%s-create" it))))))
                 '(dogman grayscale kobo mcfay room storm struan))
          cache-def `(defvar myron-themes-cache ',cache "Cache value for the themes. Internal use only.")
          fill-column 100]
    (spit (~e "straight/repos/myron-themes/myron-themes-cache.el")
      (with-temp-buffer
        (insert ";; -*- lexical-binding: t; -*-\n")
        (insert (pp-to-string (print cache-def)))
        (insert (ns/str '(provide 'myron-themes-cache)))
        (emacs-lisp-mode)
        ;; (pp-buffer)
        (buffer-string)))))

(named-timer-run :show-bandha-reminder
  t
  (ns/t 25m)
  (fn
    (when nil                           ; temp
      (when (< (org-user-idle-seconds) 60)
        (when-not (or (sh "pgrep obs") (sh "pgrep zoom"))
          (alert ""                     ; short notif (not `alert!`)
            :severity 'normal
            :title "engage bandhas!"))))))

(defun ns/add-heading-if-not-exists (heading)
  "Add HEADING to the current org file if it doesn't already exist."
  ;; this one was mostly generated by claude actually
  (goto-char (point-min))
  (when-not (re-search-forward (concat "^\\* \n" (regexp-quote heading)) nil t)
    (goto-char (point-max))
    (insert "\n* " heading)))

(defun! ns/start-pomodoro ()
  "Declare a pomodoro."
  ;; todo: clock cleanup, cancel pomo status?
  (llet [intent (s-trim (read-string "Pomodoro purpose: "))]
    (with-current-buffer (find-file-noselect (f-join (f-parent org-default-notes-file) "pomodoro.org"))
      (ns/add-heading-if-not-exists intent)
      (org-pomodoro))))

(ns/bind "op" 'ns/start-pomodoro)

;; this is only enforced after the printing is done? feels useless
(setq cider-print-quota 50000)

(setq cider-repl-buffer-size-limit 10000)

;; needed for interrupts on java >21
;; https://docs.cider.mx/cider/basics/up_and_running.html#enabling-nrepl-jvmti-agent
(setq cider-enable-nrepl-jvmti-agent t)

(ns/use
  (eat :type git
    :host codeberg
    :repo "akib/emacs-eat"
    :files ("*.el" ("term" "term/*.el") "*.texi"
             "*.ti" ("terminfo/e" "terminfo/e/*")
             ("terminfo/65" "terminfo/65/*")
             ("integration" "integration/*")
             (:exclude ".dir-locals.el" "*-tests.el"))))

;; (defvar shell-pop-internal-mode-func '(lambda () (shell)))
;; (setq shell-pop-shell-type '("eat" "*eat*" (lambda nil (eat))))
;; (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)

(when-not ns/term?
  (ns/use (ultra-scroll :host github :repo "jdtsmith/ultra-scroll")
    (setq scroll-conservatively 101
      scroll-margin 0)
    (ultra-scroll-mode t)))

;; using in python
(ns/use smart-dash)

(ns/use lorem-ipsum)

(add-hook 'prog-mode-hook 'outline-minor-mode)

;; this should really be in sanity lmao
(ns/use symbol-overlay (add-hook 'prog-mode-hook 'symbol-overlay-mode))

;; https://www.reddit.com/r/NixOS/comments/1aed1lf/ispell_not_working_on_emacs/
(when-not (f-exists? (~ ".cache/aspell.dict"))
  (f-mkdir-full-path (~ ".cache"))
  (sh "aspell -d en dump master | aspell -l en expand > ~/.cache/aspell.dict"))

(setq ispell-alternate-dictionary (~ ".cache/aspell.dict"))

(ns/inmap 'debugger-mode-map "q" 'delete-window)

(ns/use org-anki)

(progn
  ;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  (setq remote-file-name-inhibit-locks t
    tramp-use-scp-direct-remote-copying t
    remote-file-name-inhibit-auto-save-visited t
    tramp-copy-size-limit (* 1024 1024) ;; 1MB
    tramp-verbose 2)


  (defun ns/magit-tramp-check ()
    (when (file-remote-p default-directory)
      ;; don't show the diff by default in the commit buffer. Use `C-c C-d' to display it
      (setq magit-commit-show-diff nil)
      ;; don't show git variables in magit branch
      (setq magit-branch-direct-configure nil)
      ;; don't automatically refresh the status buffer after running a git command
      (setq magit-refresh-status-buffer nil)))

  (add-hook 'magit-status-mode-hook 'ns/magit-tramp-check)

  (connection-local-set-profile-variables
    'remote-direct-async-process
    '((tramp-direct-async-process . t)))


  (connection-local-set-profiles
    '(:application tramp :protocol "scp")
    'remote-direct-async-process)

  (setq magit-tramp-pipe-stty-settings 'pty)

  ;; adding this one - caution, default is 10s, risky?
  (setq remote-file-name-inhibit-cache (ns/t 10m)))

(general-define-key
  :states '(normal insert)
  :keymaps 'comint-mode-map
  (kbd "<return>")
  (fn!! comint-send-input
    (and (shx-point-on-input-p)
      (comint-send-input))))

(defun ns/random-splash-message ()
  (with-current-buffer (find-file-noselect (ns/path org-directory "reminders.org"))
    (->> (org-ml-parse-headlines 'all )
      (-map 'org-ml-to-trimmed-string)
      (ns/random-list))))

(defun! ns/splash (message)
  "display a splash message"
  (and (get-buffer "*tip*")
    (kill-buffer "*tip*"))
  (switch-to-buffer (get-buffer-create "*tip*"))
  (dotimes (i (/ (window-height) 4))
    (insert "\n"))
  (insert message)
  ;; todo: which frame?
  (delete-other-windows)
  (special-mode)
  ;; (ns/set-buffer-face-variable)
  (olivetti-mode))

(ns/inmap 'special-mode-map
  "q" (fn!! window-revert (kill-buffer) (winner-undo)))

(named-timer-idle-run :splash-screen (ns/t 30m) t
  (lambda ()
    (interactive)
    (ns/splash (ns/random-splash-message))))

;; todo: checkout https://github.com/sinic/ednc
