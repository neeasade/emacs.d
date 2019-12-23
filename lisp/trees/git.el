
(use-package magit
  :config
  (setq-ns magit
    save-repository-buffers 'dontask
    repository-directories (list (~ "git"))
    )

  (magit-file-mode 0)

  ;; https://magit.vc/manual/magit/Performance.html
  (when ns/enable-windows-p
    (setq-ns magit
      ;; diff perf
      diff-highlight-indentation nil
      diff-highlight-trailing nil
      diff-highlight-hunk-body nil
      diff-paint-whitespace nil
      diff-refine-hunk nil
      refresh-status-buffer nil
      )

    ;; don't show diff when committing --
    ;; means reviewing will have to be purposeful before
    (remove-hook 'server-switch-hook 'magit-commit-diff)
    ))

(when (not ns/enable-windows-p)
  (ns/use-package magit-todos "alphapapa/magit-todos"
    :config
    (setq magit-todos-nice ns/enable-linux-p)
    (evil-define-key nil magit-todos-section-map "j" nil)
    (evil-define-key nil magit-todos-section-map "e" nil)
    (magit-todos-mode)))

(use-package magit-svn :config
  (add-hook 'magit-mode-hook 'magit-svn-mode))

(use-package evil-magit
  :config
  (general-nmap magit-mode-map "n" 'evil-next-line)
  (general-nmap magit-mode-map "e" 'evil-previous-line)
  (general-vmap magit-mode-map "n" 'evil-next-line)
  (general-vmap magit-mode-map "e" 'evil-previous-line)
  (general-nmap magit-mode-map "k" 'evil-search-next)
  (general-nmap magit-mode-map "K" 'evil-search-previous)
  (general-nmap magit-mode-map "?" 'evil-search-backward)
  (general-nmap magit-status-mode-map "e" 'evil-previous-line)
  (general-nmap magit-status-mode-map "n" 'evil-next-line))

(use-package git-gutter-fringe
  :config
  (setq git-gutter-fr:side 'right-fringe)
  ;; fails when too many buffers open on windows
  (if ns/enable-linux-p (global-git-gutter-mode t)))

;; todo: make this colemak
(defhydra git-smerge-menu ()
  "
      movement^^^^               merge action^^           other
      ---------------------^^^^  -------------------^^    -----------
      [_n_]^^    next hunk       [_b_] keep base          [_u_] undo
      [_N_/_p_]  prev hunk       [_m_] keep mine          [_r_] refine
      [_j_/_k_]  move up/down    [_a_] keep all           [_q_] quit
      ^^^^                       [_o_] keep other
      ^^^^                       [_c_] keep current
      ^^^^                       [_C_] combine with next"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("N" smerge-prev)
  ("j" evil-next-line)
  ("k" evil-previous-line)
  ("a" smerge-keep-all)
  ("b" smerge-keep-base)
  ("m" smerge-keep-mine)
  ("o" smerge-keep-other)
  ("c" smerge-keep-current)
  ("C" smerge-combine-with-next)
  ("r" smerge-refine)
  ("u" undo-tree-undo)
  ("q" nil :exit t))

;; define a minimal staging mode for when we're on windows.
(when ns/enable-windows-p
  ;; WORKAROUND https://github.com/magit/magit/issues/2395
  (define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
    "Mode for showing staged and unstaged changes."
    :group 'magit-status)
  (defun magit-staging-refresh-buffer ()
    (magit-insert-section (status)
      (magit-insert-untracked-files)
      (magit-insert-unstaged-changes)
      (magit-insert-staged-changes)))
  (defun magit-staging ()
    (interactive)
    (magit-mode-setup #'magit-staging-mode)))

;; cf https://github.com/alphapapa/unpackaged.el#improved-magit-status-command
;;;###autoload
(defun unpackaged/magit-status ()
  "Open a `magit-status' buffer and close the other window so only Magit is visible.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (let* ((buffer-file-path (when buffer-file-name
                             (file-relative-name buffer-file-name
                               (locate-dominating-file buffer-file-name ".git"))))
          (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
    (magit-status)
    (delete-other-windows)
    (when buffer-file-path
      (goto-char (point-min))
      (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                       (magit-section-show (magit-current-section))
                       (recenter)
                       t)
        do (condition-case nil
             (magit-section-forward)
             (error (cl-return (magit-status-goto-initial-section-1))))))))

(defun! ns/git-status()
  (if ns/enable-windows-p (magit-staging)
    (unpackaged/magit-status)))

;; todo: tryout this package
(use-package vdiff
  :config
  (evil-define-key 'normal vdiff-mode-map "," vdiff-mode-prefix-map)

  ;; todo: general-nmap this
  (evil-define-minor-mode-key 'normal 'vdiff-mode "]c" 'vdiff-next-hunk)
  (evil-define-minor-mode-key 'normal 'vdiff-mode "[c" 'vdiff-previous-hunk)
  (evil-define-minor-mode-key 'normal 'vdiff-mode "zc" 'vdiff-close-fold)
  (evil-define-minor-mode-key 'normal 'vdiff-mode "zM" 'vdiff-close-all-folds)
  (evil-define-minor-mode-key 'normal 'vdiff-mode "zo" 'vdiff-open-fold)
  (evil-define-minor-mode-key 'normal 'vdiff-mode "zR" 'vdiff-open-all-folds)
  (evil-define-minor-mode-key 'motion 'vdiff-mode "go" 'vdiff-receive-changes)
  (evil-define-minor-mode-key 'motion 'vdiff-mode "gp" 'vdiff-send-changes)
  )

(general-nmap
  "]g" 'git-gutter:next-hunk
  "[g" 'git-gutter:previous-hunk
  )

;; alias:

(ns/bind
  "g" '(:ignore t :which-key "git")
  "gb" 'magit-blame-addition
  "gl" 'magit-log-buffer-file
  "gL" 'magit-log
  "gm" 'git-smerge-menu/body
  "gd" 'vdiff-mode ; ,h for a hydra!
  "gs" 'ns/git-status
  )
