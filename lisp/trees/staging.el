;; -*- lexical-binding: t; -*-

(ns/use dumb-jump)

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

;; make timestamp processing functions aware of this
;; (setq org-use-effective-time nil)

;; todo: I'm not sure why we set this
(setq org-duration-format (quote h:mm))

(defun! ns/toggle-modeline ()
  "toggle the modeline in the current buffer"
  (setq mode-line-format
    (if mode-line-format nil
      '("%e" (:eval (doom-modeline-format--neeasade-doomline)))))
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
  (-when-let (window (flycheck-get-error-list-window t))
    (with-selected-window window
      (fit-window-to-buffer window 30))))

(setq server-window 'pop-to-buffer)

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

(ns/use smart-dash)
(ns/use lorem-ipsum)

;; bad hook? emacs client start
;; (add-hook 'prog-mode-hook 'outline-minor-mode)

;; this should really be in sanity lmao
(ns/use symbol-overlay (add-hook 'prog-mode-hook 'symbol-overlay-mode))

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
  (setq ns/splash-window-conf (current-window-configuration))
  (delete-other-windows)
  (special-mode)
  ;; (ns/set-buffer-face-variable)
  ;; nb: still working out olivetti width
  (olivetti-mode)

  (setq-local mode-line-format nil))

(named-timer-run :splash-screen
  "1 sec"
  (ns/t 5m)
  (fn (when (> (org-user-idle-seconds) (ns/t 5m))
        nil
        ;; (ns/splash (ns/random-splash-message))
        )))

;; (ns/splash (ns/random-splash-message))

(defun! ns/random-todo ()
  ;; goto a random todo in dotfiles or emacs
  (ns/random-list (sh-lines (format "rg --no-heading --line-number todo '%s' | grep -v '\"'" (~ ".dotfiles"))))

  ;; inkling: should tweak follow package
  (defun ns/create-marker (line)
    (ns/handle-potential-file-link
      "/home/neeasade/.emacs.d/lisp/trees/staging.el:350"))
  )

(ns/inmap 'special-mode-map "q" (fn!! window-revert
                                  (kill-buffer)
                                  ;; todo: this layout is only relevant for splash, but special-mode is used in other places
                                  (winner-undo)
                                  ;; (set-window-configuration ns/splash-window-conf)
                                  ))

;; todo: derive a splash mode from special mode (annoying to have it work on drawer popups)
;; (ns/inmap 'special-mode-map "q" (fn!! window-revert (quit-window) (winner-undo)))

(ns/use devdocs)

(run-at-time "11:59pm" "11:59pm" (fn!! message-delimiter
                                   (message "|")
                                   (message "------------------- %s the %s -------------------"
                                     (ts-day-name (ts-now))
                                     (ts-day (ts-now)))
                                   (message "|")))

(named-timer-idle-run :splash-screen (ns/t 30m) t
  (lambda ()
    (interactive)
    (ns/splash (ns/random-splash-message))))

(defun ns/diff-last-two-kills (&optional ediff?)
  "Diff last couple of things in the kill-ring. With prefix open ediff."
  (interactive "P")
  (let ((old-buffer (generate-new-buffer " *old-kill*"))
         (new-buffer (generate-new-buffer " *new-kill*")))
    (with-current-buffer new-buffer
      (insert (current-kill 0 t)))
    (with-current-buffer old-buffer
      (insert (current-kill 1 t)))
    (if ediff?
      (ediff-buffers old-buffer new-buffer)
      (diff old-buffer new-buffer nil t))))


(comment ns/use gptel
  (setq
    gptel-default-mode 'org-mode
    gptel-model 'claude-sonnet-4-5-20250929
    gptel-backend (gptel-make-anthropic "Claude"
                    :stream t :key chatgpt-shell-anthropic-key)))

(ns/use aidermacs
  ;; :bind (("C-c a" . aidermacs-transient-menu))
  (setenv "ANTHROPIC_API_KEY" chatgpt-shell-anthropic-key)

  ;; See the Configuration section below
  (setq aidermacs-default-chat-mode 'architect)
  (setq aidermacs-default-model "opus")
  (setq aidermacs-extra-args '("--yes"))
  (setq aidermacs-show-diff-after-change nil))

;; while flipping between vscode and here
(global-auto-revert-mode t)

;; aggressive

;; todo: consider buffer-terminator

(setq create-lockfiles nil)

(when (and ns/term?)

  (defun ns/osc52-read ()
    ;; taken from xterm.el
    (let* ((type 'CLIPBOARD)
            (query (concat "\e]52;" (xterm--selection-char type) ";")))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (xterm--query
          ;; Use ST as query terminator to get ST as reply terminator (bug#36879).
          (concat query "?\e\\")
          (list (cons query
                  (lambda ()
                    ;; Read data up to the string terminator, ST.
                    (let (char last)
                      (while (and (setq char (read-char
                                               nil nil
                                               xterm-query-timeout))
                               (not (and (eq char ?\\)
                                      (eq last ?\e))))
                        (when last
                          (insert last))
                        (setq last char))))))
          'no-async)
        (base64-decode-region (point-min) (point-max))
        (decode-coding-region (point-min) (point-max) 'utf-8-unix t))))

  ;; (s-replace "\r\n" "\n" (ns/osc52-read))
  ;; (sh "wl-paste | dos2unix")

  (defun ns/sync-terminal-clipboard ()
    (when (frame-focus-state)
      (when-let (clip (ns/osc52-read)
                  ;; (if ns/enable-wsl-p (sh "wl-paste | dos2unix") (ns/osc52-read))
                  )
        ;; (message (ns/str "killing " clip))
        (kill-new clip))))

  (add-function :after after-focus-change-function #'ns/sync-terminal-clipboard)

  ;; (remove-function after-focus-change-function #'ns/sync-terminal-clipboard)

  (ns/bind "ip" (fn!! insert-paste (insert (ns/osc52-read))))

  ;; get C-<backspace> in the windows terminal
  ;; temp workaround: focus stealing sometimes doesn't work - dtach thing?
  ;; (ns/bind "ip" (fn!! paste-gui (insert (sh "wl-paste | dos2unix"))))

  ;; make an assumption: wsl + xterm = windows terminal
  (llet [initial-terminal (getenv-internal "TERM" initial-environment)
          wt? (and (string= initial-terminal "xterm-256color") ns/enable-wsl-p)]
    (when wt?
      ;; C-<backspace> equivalent
      (general-define-key
        :states '(insert)
        :keymaps 'general-override-mode-map
        (kbd "C-h") 'sp-backward-delete-word))))

(ns/use typescript-mode)
;; (ns/use vue-mode)
(ns/use web-mode)
(ns/file-mode "vue" 'web-mode)

(defun! ns/shell-show ()
  ;; todo: split windows in some nice fashion
  (ns/cleanup-shells)
  (ns/buffers-by-mode 'shell-mode)

  ;; (balance-windows)
  )

(defun! ns/term-refresh ()
  (xterm-mouse-mode -1)
  (xterm-mouse-mode 1)
  (global-kkp-mode -1)
  (global-kkp-mode 1))

(comment

  ;; as a reminder to try later (prettier)
  ;; nb: for vue this seems aggressively wrong (or at least, disagrees with the vscode interpretation)
  (ns/use apheleia)

  ;; todo: try this w/ eglot
  (add-to-list 'eglot-server-programs
	  '(vue-ts-mode . ("vue-language-server" "--stdio" :initializationOptions '(:vue (:hybridMode :json-false)))))

  (ns/use tree-sitter-langs)

  ;; todo: checkout
  ;; https://github.com/8uff3r/vue-ts-mode
  (comment
    (setq treesit-language-source-alist
      '((vue "https://github.com/ikatyang/tree-sitter-vue")
         (css "https://github.com/tree-sitter/tree-sitter-css")
         (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
           "master" "tsx/src"
           )))

    (treesit-install-language-grammar 'vue)

    (treesit-install-language-grammar 'typescript)

    (-map 'treesit-install-language-grammar '(vue css typescript))

    (ns/use (vue-ts-mode :type git :host github :repo "8uff3r/vue-ts-mode" :files ("*.el")))
    ))

(ns/face 'mmm-default-submode-face :background nil)

(setq scroll-error-top-bottom t)
(setq scroll-preserve-screen-position t)


(ns/inmap 'general-override-mode-map
  ;; these names are flipped for my intuition
  (kbd "C-n") (fn!! scroll-up (if (minibufferp) (previous-line) (scroll-up-command)))
  (kbd "C-e") (fn!! scroll-down (if (minibufferp) (next-line) (scroll-down-command))))

;; broken for now (magit mode)
(ns/inmap 'general-override-mode-map
  (kbd "C-n") nil
  (kbd "C-e") nil)

(defun my-color-values (color)
  "Return RGB values of COLOR as list of 3 integers (0-65535)."
  (cond
    ;; #RGB
    ((string-match "^#\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)\\([0-9a-fA-F]\\)$" color)
      (mapcar (lambda (s) (* (string-to-number s 16) 4369))
        (list (match-string 1 color)
          (match-string 2 color)
          (match-string 3 color))))

    ;; #RRGGBB
    ((string-match "^#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)$" color)
      (mapcar (lambda (s) (* (string-to-number s 16) 257))
        (list (match-string 1 color)
          (match-string 2 color)
          (match-string 3 color))))

    ;; #RRRRGGGGBBBB
    ((string-match "^#\\([0-9a-fA-F]\\{4\\}\\)\\([0-9a-fA-F]\\{4\\}\\)\\([0-9a-fA-F]\\{4\\}\\)$" color)
      (mapcar (lambda (s) (string-to-number s 16))
        (list (match-string 1 color)
          (match-string 2 color)
          (match-string 3 color))))

    ;; Named color lookup
    (t (cdr (assoc-string color color-name-rgb-alist t)))))

(defun color-values (color &optional frame)
  (cond
    ((member color '(unspecified "unspecified-fg" "unspecified-bg"))
      nil)
    ((display-graphic-p frame) (xw-color-values color frame))
    (t (my-color-values color))))

;; fun, maybe we want this in the modeline instead
(ns/use breadcrumb
  (breadcrumb-mode nil)

  (ns/face 'breadcrumb-project-leaf-face
    :foreground (myron-get :assumed :weak)
    )

  (ns/face 'header-line
    ;; :background (myron-get :subtle :meta)
    :background (myron-get :background :weak)
    ;; :foreground nil
    :foreground (myron-get :foreground :weak)
    )

  )


(defun ns/on-save (command) (add-hook 'after-save-hook (lambda () (sh command)) nil t))

(comment


  ;; works: should we prefix with emacs?
  (ns/use (term-title :host github :repo "CyberShadow/term-title"))
  )
