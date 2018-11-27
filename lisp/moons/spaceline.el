(defun spacemacs/compute-powerline-height ()
  "Return an adjusted powerline height."
  (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
                 powerline-scale 1)))
    (truncate (* scale (frame-char-height)))))

(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq-ns powerline
    scale (string-to-number (get-resource "Emacs.powerlinescale"))
    height (spacemacs/compute-powerline-height)
    default-separator (get-resource "Emacs.powerline")
    )

  ;; (set-face-attribute 'spaceline-highlight-face nil :background (face-attribute 'spaceline-evil-normal :background))

  ;; todo: make a circe segment

  ;; note to self: abandon this, look at switch-to-next, prev-buffer source, grab that and replace return
  (defun ns/next-buffer-name ()
    (-first (lambda (bufname) (not (ns/should-skip bufname)))
      (-map 'buffer-name (window-next-buffers))))

  (defun ns/prev-buffer-name ()
    (-first (lambda (bufname) (not (ns/should-skip bufname)))
      (-map 'buffer-name
        (-map 'first (window-prev-buffers)))))

  (spaceline-define-segment next-buffers
    "Docstring"
    ;; A single form whose value is the value of the segment.
    ;; It may return a string, an image or a list of such.
    (when t
      (concat
        (s-left 8 (ns/prev-buffer-name))
        " - "
        (s-left 8 (ns/next-buffer-name)))
      )
    :enabled t
    )

  ;; this is needed to set default for new buffers?
  (spaceline-spacemacs-theme)

  (spaceline-compile 'main
    '(
       anzu
       (remote-host projectile-root ">>" buffer-id buffer-modified)
       (flycheck-error flycheck-warning)
       process
       )
    '(
       ;; maybe
       (version-control :when active)
       (next-buffers :when active)
       (org-clock :when active)
       (org-pomodoro :when active)
       info-nodes
       ((line-column buffer-position)
         :separator " |" )
       (battery :when active)
       ))

  (setq mode-line-format '("%e" (:eval (spaceline-ml-main))))

  ;; set the modeline for all existing buffers
  ;; todo: make this unset modeline on not matching spawn
  (defcommand refresh-all-modeline (toggle)
    (dolist (buf (buffer-list))
      (when (not (s-starts-with-p "*spawn-shell" (buffer-name buf)))
        (with-current-buffer buf
          (setq mode-line-format (if toggle '("%e" (:eval (spaceline-ml-main))) " "))
          )))

    ;; for new buffers after:
    (setq-default mode-line-format (if toggle '("%e" (:eval (spaceline-ml-main))) " "))

    (setq window-divider-default-bottom-width (if toggle 0 1))

    (when (not toggle)
      (setq window-divider-default-right-width 1)
      ;; (setq window-divider-default-places 'right)
      (setq window-divider-default-places t)

      (set-face-attribute 'window-divider nil :foreground
        (face-attribute 'font-lock-comment-face :foreground))
      (set-face-attribute 'mode-line nil :background nil)
      (set-face-attribute 'mode-line-inactive nil :background nil)
      (window-divider-mode t)
      )

    ;; (force redraw of all frames)
    (ns/apply-frames (fn nil)))

  (ns/refresh-all-modeline t)
  (ns/bind "M" (fn! (ns/refresh-all-modeline (not mode-line-format))))
  )
