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

  (add-hook 'window-configuration-change-hook 'dynamic-ivy-height)

  (defun dynamic-ivy-height()
    (setq ivy-height (/ (frame-total-lines) 2)))

  (dynamic-ivy-height)
  (ivy-mode 1)

  (use-package prescient :config (prescient-persist-mode))
  (use-package ivy-prescient :config
    (ivy-prescient-mode)
    (prescient-persist-mode))

  (use-package company-prescient :config (company-prescient-mode))

  (prescient-persist-mode))

;; counsel
(use-package counsel
  :config
  (use-package rg)
  (setq-ns counsel
    grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
    rg-base-command "rg -i -M 120 --hidden --no-heading --line-number --color never %s ."))

(use-package ranger
  :init (setq ranger-override-dired t)
  :config
  (setq-ns ranger
    show-literal nil
    show-hidden t
    cleanup-eagerly t)

  (define-key ranger-mode-map (kbd "n") 'ranger-next-file)
  (define-key ranger-mode-map (kbd "e") 'ranger-prev-file)

  ;; call with eg 'dired-mode
  (defcommand kill-buffers-by-mode (mode)
    (mapc (lambda (buffer)
            (when (eq mode (buffer-local-value 'major-mode buffer))
              (kill-buffer buffer)))
      (buffer-list)))

  (defcommand kill-ranger-buffers ()
    (ns/kill-buffers-by-mode 'ranger-mode))

  (advice-add #'ranger-close :after #'ns/kill-ranger-buffers)

  ;; (defcommand deer-with-last-shell ()
  ;;   (let ((current-buffer (buffer-name (current-buffer))))
  ;;     (if (or (s-match "\*spawn-shell.*" current-buffer)
  ;;           (s-match "\*shell-[1-9]\*" current-buffer))
  ;;       (setq ns/last-shell current-buffer)
  ;;       (setq ns/last-shell shell-pop-last-shell-buffer-name)))
  ;;   (deer))

  ;; (ns/bind "d" 'ns/deer-with-last-shell)
  (ns/bind "d" 'deer)

  (defcommand open () (ns/shell-exec-dontcare (format "xdg-open \"%s\"" (dired-get-file-for-visit))))
  (define-key ranger-normal-mode-map (kbd "RET") 'ns/open)
  )

(defun my-resize-margins ()
  (let ((margin-size (if ns/center (/ (- (frame-width) 120) 2) 0)))
    (set-window-margins nil margin-size margin-size)))

(defcommand toggle-margin ()
  (if (not (bound-and-true-p ns/center))
    (setq ns/center nil))

  (if ns/center
    (remove-hook 'window-configuration-change-hook #'my-resize-margins)
    (add-hook 'window-configuration-change-hook #'my-resize-margins))

  (setq ns/center (not ns/center))
  (my-resize-margins))

(defcommand kill-current-buffer()
  (kill-buffer nil))

(defcommand follow-mode ()
  (follow-mode)
  (delete-other-windows)
  (evil-window-vsplit))

(use-package alert
  :config (setq alert-default-style
            (if ns/enable-windows-p
              'toaster
              'libnotify
              )))

(use-package which-key
  :config
  (setq-ns which-key
    idle-delay 1.5
    side-window-max-width 0.33
    sort-order 'which-key-key-order-alpha)
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))

(defcommand kill-other-buffers ()
  "Kill all other buffers."
  (mapc 'kill-buffer
    (delq (current-buffer)
      (remove-if-not 'buffer-file-name (buffer-list)))))

(ns/bind
  "/" (if (executable-find "rg") 'counsel-rg 'counsel-git-grep)

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

  "a" '(:ignore t :which-key "Applications")
  "q" '(:ignore t :which-key "Query")

  "b" '(:ignore t :which-key "Buffers")
  "bb" 'counsel-ibuffer
  "bd" 'ns/kill-current-buffer
  "bK" 'ns/kill-other-buffers
  "bk" 'kill-matching-buffers

  "n" '(:ignore t :which-key "Jump")
  "nd" 'counsel-imenu
  )
