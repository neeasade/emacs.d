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
      diff-paint-whitespace nil
      diff-highlight-hunk-body nil
      diff-refine-hunk nil
      refresh-status-buffer nil
      )

    ;; don't show diff when committing --
    ;; means reviewing will have to be purposeful before
    (remove-hook 'server-switch-hook 'magit-commit-diff)
    ))

(macroexpand-1
  '(ns/use-package magit-todos "alphapapa/magit-todos"
     :config
     (setq magit-todos-nice ns/enable-linux-p)
     (evil-define-key nil magit-todos-section-map "j" nil)
     (magit-todos-mode))
  )

(use-package magit-svn :config
  (add-hook 'magit-mode-hook 'magit-svn-mode))

(use-package evil-magit
  :config
  (evil-define-key evil-magit-state magit-mode-map "?" 'evil-search-backward)
  (when ns/enable-colemak
    (evil-define-key evil-magit-state magit-mode-map "n" 'evil-next-line)
    (evil-define-key evil-magit-state magit-mode-map "e" 'evil-previous-line)
    (evil-define-key evil-magit-state magit-mode-map "k" 'evil-search-next)
    (evil-define-key evil-magit-state magit-mode-map "K" 'evil-search-previous)))

(use-package git-gutter-fringe
  :config
  (setq git-gutter-fr:side 'right-fringe)
  ;; fails when too many buffers open on windows
  (if ns/enable-linux-p (global-git-gutter-mode t))
  )

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
    (magit-mode-setup #'magit-staging-mode))
  )

(defcommand git-status()
  (if ns/enable-windows-p (magit-staging) (magit-status))
  (if (> (frame-pixel-height) (frame-pixel-width))
    (delete-other-windows)))


;; todo: tryout this package
(use-package vdiff
  :config
  (evil-define-key 'normal vdiff-mode-map "," vdiff-mode-prefix-map)

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
(defcommand magit-history () (magit-log-buffer-file))

(ns/bind
  "g" '(:ignore t :which-key "git")
  "gb" 'magit-blame
  "gl" 'magit-log-buffer-file
  "gm" 'git-smerge-menu/body
  "gd" 'vdiff-mode ; ,h for a hydra!
  "gs" 'ns/git-status
  "gh" 'ns/magit-history
  )
