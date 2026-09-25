;; -*- lexical-binding: t; -*-
;; lisp is a ball of mud.

(defface ns/mode-line-middle
  '((t (:inherit mode-line)))
  "Face used for the space between the left and right mode line sections."
  :group 'mode-line-faces)

;; Keep these out of the face definitions so ns/load-theme refreshes them.
(ns/face 'ns/mode-line-middle :background (myron-get :background :strong))

(ns/face '(mode-line mode-line-inactive
            window-divider
            window-divider-first-pixel
            window-divider-last-pixel)
  :background (myron-get :background :weak)
  :foreground (myron-get :foreground :weak)
  :height (face-attribute 'default :height)
  :box nil)


(ns/face 'mode-line-inactive :foreground (myron-get :faded))

;; (ns/face '(window-divider
;;             window-divider-first-pixel
;;             window-divider-last-pixel)
;;   :foreground (myron-get :background :weak))

(when (eq 'myron-kobo (first custom-enabled-themes))
  (ns/face 'ns/mode-line-middle :background (myron-get :background :weak))
  (ns/face 'mode-line
    :background (myron-get :background :strong)
    :foreground (myron-get :foreground :strong))
  (ns/face 'mode-line-inactive
    :foreground (myron-get :faded :strong)
    :background (myron-get :background :strong)))

(defun ns/mode-line-right-align ()
  "Right-align the rest of the mode line and color the space it occupies."
  (let ((fill (mode--line-format-right-align)))
    (put-text-property
      0 1 'face
      (if (mode-line-window-selected-p)
        'ns/mode-line-middle
        'mode-line-inactive)
      fill)
    fill))

(setq mode-line-format-right-align
  '(:eval (ns/mode-line-right-align)))

(defun ns/mode-line-selection-info ()
  "Return a compact description of the active selection."
  (let ((evil-visual-p
          (and (bound-and-true-p evil-local-mode)
            (eq evil-state 'visual))))
    (when (and (mode-line-window-selected-p)
            (or (use-region-p) evil-visual-p))
      (let* ((beg (if evil-visual-p
                    evil-visual-beginning
                    (region-beginning)))
              (end (if evil-visual-p
                    evil-visual-end
                    (region-end)))
              (lines (count-lines beg (min end (point-max)))))
        (format " %dC%s "
          (abs (- end beg))
          (if (> lines 1) (format " %dL" lines) ""))))))

(defun ns/mode-line-matches ()
  "Return the current Anzu match count."
  (when (and (mode-line-window-selected-p)
          (bound-and-true-p anzu--state)
          (fboundp 'anzu--update-mode-line))
    (concat "" (anzu--update-mode-line) " ")))

(defun ns/mode-line-lispy-indicator ()
  "Return an indicator when Lispyville's structural bindings are active."
  (when (and (fboundp 'lispyville--lispy-keybindings-active-p)
          (lispyville--lispy-keybindings-active-p))
    "LISPY "))

(defun ns/mode-line-checker ()
  "Return the active syntax checker's native mode line construct."
  (cond
    ((bound-and-true-p flymake-mode) 'flymake-mode-line-format)
    ((bound-and-true-p flycheck-mode) 'flycheck-mode-line)))

(defvar ns/mode-line-format nil
  "The default mode line format.")

(defvar-local my-mode-line-padding ""
  "Buffer-local string used for dynamic mode line left padding.")

(defun my-update-mode-line-padding ()
  "Update `my-mode-line-padding` based on window position."
  (walk-windows
    (lambda (w)
      (with-current-buffer (window-buffer w)
        (setq my-mode-line-padding
          (if (= (car (window-pixel-edges w)) 0)
            " "  ;; <- Your desired spacing
            ""))))
    nil 'visible))

(add-hook 'window-state-change-hook #'my-update-mode-line-padding)

(setq ns/mode-line-format
  '("%e"
     ;; (:propertize " %b" face mode-line-buffer-id)
     ;; (:propertize " %b" face default)
     my-mode-line-padding
     "%b"
     (:eval (cond
              ((buffer-modified-p) "* ")
              (buffer-read-only "% ")
              (t " ")))
     (:eval (ns/mode-line-selection-info))
     (:eval (ns/mode-line-matches))
     (:eval (ns/mode-line-lispy-indicator))
     mode-line-format-right-align
     (:eval (ns/mode-line-checker))
     ;; mode-line-process ; shows ":run" ?
     mode-line-misc-info
     " %l:%c "))

(put 'ns/mode-line-format 'risky-local-variable t)

(column-number-mode 1)
(line-number-mode 1)

(defun ns/mode-line-update-selection ()
  "Update selection information after point moves."
  (when (or mark-active
          (and (bound-and-true-p evil-local-mode)
            (eq evil-state 'visual)))
    (force-mode-line-update)))
(add-hook 'post-command-hook 'ns/mode-line-update-selection)

(defun! ns/refresh-all-modeline (toggle)
  (ns/setq-local-all
    'mode-line-format
    (if toggle (list 'quote ns/mode-line-format) nil))

  (ns/frame-set-parameter 'bottom-divider-width (if toggle 0 1))

  ;; Force redraw of all frames.
  (ns/apply-frames (fn nil)))

(ns/bind "tM" (fn!! (ns/refresh-all-modeline (not mode-line-format))))

(ns/refresh-all-modeline t)
