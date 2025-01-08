;; -*- lexical-binding: t; -*-

;; note: initial use-package call for magit happens in evil.

(setq-ns magit
  save-repository-buffers 'dontask
  repository-directories (list (~ "git")))

;; https://magit.vc/manual/magit/Performance.html
(when ns/enable-windows-p
  (setq-ns magit-diff
    highlight-indentation nil
    highlight-trailing nil
    highlight-hunk-body nil
    paint-whitespace nil
    refine-hunk nil)

  (setq magit-refresh-status-buffer nil)

  ;; don't show diff when committing
  (remove-hook 'server-switch-hook 'magit-commit-diff))

(ns/use (magit-todos :host github :repo "alphapapa/magit-todos")
  (setq magit-todos-nice ns/enable-linux-p
    magit-todos-keywords-list (-mapcat (-juxt 's-upcase 's-downcase)
                                '("todo" "fixme" "temp")))

  (evil-define-key nil magit-todos-section-map "j" nil)
  (evil-define-key nil magit-todos-section-map "e" nil)

  (magit-todos-mode (not ns/enable-windows-p)))

(defun! ns/restore-magit-layout ()
  (when ns/magit-before-display-layout
    (set-window-configuration ns/magit-before-display-layout)))

(advice-add #'magit-mode-bury-buffer :after #'ns/restore-magit-layout)

(ns/use git-gutter-fringe
  (setq git-gutter-fr:side 'right-fringe))

(defhydra git-smerge-menu ()
  "
      movement^^^^               merge action^^           other
      ---------------------^^^^  -------------------^^    -----------
      [_n_]^^    next hunk       [_b_] keep base          [_u_] undo
      [_N_/_e_]  prev hunk       [_m_] keep mine          [_r_] refine
      [_j_/_k_]  move up/down    [_a_] keep all           [_q_] quit
      ^^^^                       [_o_] keep other
      ^^^^                       [_c_] keep current
      ^^^^                       [_C_] combine with next"
  ("n" smerge-next)
  ("e" smerge-prev)
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

(defvar ns/magit-before-display-layout nil)

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
    (call-interactively #'magit-status)
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

;; todo: tryout this package
(ns/use vdiff)

;; todo: want a shortcut to open:
;; associated PR
;; or just git repo generally


;; this seems to be a little nicer:
;; (ns/use browse-at-remote)

(ns/use git-link
  (setq git-link-open-in-browser t)
  (ns/bind "ng" 'git-link))

(general-nmap
  "]g" 'git-gutter:next-hunk
  "[g" 'git-gutter:previous-hunk)

;; alias:
(ns/bind
  "g" '(:ignore t :which-key "git")
  "gb" 'magit-blame-addition
  "gl" 'magit-log-buffer-file
  "gL" 'magit-log
  "gm" 'git-smerge-menu/body
  "gd" 'vdiff-current-file
  "gs" (fn!! git-status
         (setq ns/magit-before-display-layout (current-window-configuration))
         (unpackaged/magit-status))

  ;; open in place
  "gS" 'magit-status)
