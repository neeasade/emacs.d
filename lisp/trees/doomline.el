;; -*- lexical-binding: t; -*-

(defface ns/mode-line-middle
  `((t (:inherit (mode-line))))
  "middle mode line color" :group 'doom-modeline-faces)

;; keep this out of the defface so ns/load-theme refreshes the face
(ns/face 'ns/mode-line-middle :background (myron-get :background :strong))

(ns/face 'mode-line
  :background (myron-get :background :weak)
  :foreground (myron-get :foreground :weak)
  :height (face-attribute 'default :height)
  :box nil)

(ns/face 'mode-line-inactive :foreground (myron-get :faded :weak))

(ns/face '(doom-modeline-buffer-file doom-modeline-buffer-modified)
  :foreground (myron-get :foreground :weak))

;; upstream is really spacey
(doom-modeline-def-segment buffer-position
  "The buffer position information."
  (let* ((active (doom-modeline--active))
          (lc '(line-number-mode
                 (column-number-mode
                   (doom-modeline-column-zero-based "%l:%c" "%l:%C")
                   "%l")
                 (column-number-mode (doom-modeline-column-zero-based ":%c" ":%C"))))
          (face (if active 'mode-line 'mode-line-inactive))
          (mouse-face 'mode-line-highlight)
          (local-map mode-line-column-line-number-mode-map))
    (concat
      (doom-modeline-spc)
      (propertize (format-mode-line lc)
        'face face
        'help-echo "Buffer position - mouse-1: Display Line and Column Mode Menu"
        'mouse-face mouse-face
        'local-map local-map)
      (doom-modeline-spc))))

(doom-modeline-def-segment buffer-info-neeasade
  (concat
    (doom-modeline-spc)
    (doom-modeline--buffer-name)
    ;; (doom-modeline--buffer-file-name)
    (doom-modeline--buffer-state-icon)
    (doom-modeline-spc)))

(doom-modeline-def-segment lispy-indicator
  (if (lispyville--lispy-keybindings-active-p) "LISPY" ""))

(doom-modeline-def-segment next-buffers
  (when (doom-modeline--active)
    ;; kinda gross
    (cl-letf (((symbol-function 'set-window-buffer-start-and-point) (fn nil)))
      (llet [next-buffer (buffer-name (switch-to-next-buffer))
              prev-buffer (buffer-name (switch-to-prev-buffer))]
        (propertize
          (format "[%s:%s]" prev-buffer next-buffer)
          'face 'mode-line)))))

(doom-modeline-def-segment sep
  "Text style with whitespace."
  (propertize " " 'face (if (doom-modeline--active)
                          'ns/mode-line-middle
                          'mode-line-inactive)))

(column-number-mode) ; give us column info in the modeline

(setq-ns doom-modeline
  height (string-to-number (get-resource "panel.height"))
  bar-width 3
  percent-position nil
  icon nil
  ;; buffer-file-name-style 'truncate-with-project
  buffer-file-name-style 'buffer-name
  project-detection 'projectile
  enable-word-count nil
  buffer-encoding nil
  indent-info t
  vcs-max-length 12
  lsp t
  gnus-timer nil
  irc t ;; circe notifications? no idea if this works
  before-update-env-hook nil
  after-update-env-hook nil
  )

;; fork is to define a 'mode-line-middle face for the center of the modeline
(defun ns/doom-modeline-def-modeline (name lhs &optional rhs)
  "Defines a modeline format and byte-compiles it.
NAME is a symbol to identify it (used by `doom-modeline' for retrieval).
LHS and RHS are lists of symbols of modeline segments defined with
`doom-modeline-def-segment'.

Example:
  (doom-modeline-def-modeline 'minimal
    '(bar matches \" \" buffer-info)
    '(media-info major-mode))
  (doom-modeline-set-modeline 'minimal t)"
  (let ((sym (intern (format "doom-modeline-format--%s" name)))
         (lhs-forms (doom-modeline--prepare-segments lhs))
         (rhs-forms (doom-modeline--prepare-segments rhs)))
    (defalias sym
      (lambda ()
        (list lhs-forms
          (propertize
            " "
            'face (if (doom-modeline--active) 'ns/mode-line-middle 'mode-line-inactive)
            'display `((space
                         :align-to
                         (- (+ right right-fringe right-margin)
                           ,(* (let ((width (doom-modeline--font-width)))
                                 (or (and (= width 1) 1)
                                   (/ width (frame-char-width) 1.0)))
                              (string-width
                                (format-mode-line (cons "" rhs-forms))))))))
          rhs-forms))
      (concat "Modeline:\n"
        (format "  %s\n  %s"
          (prin1-to-string lhs)
          (prin1-to-string rhs))))))

(ns/doom-modeline-def-modeline 'neeasade-doomline
  '(
     ;; sep
     ;; remote-host
     buffer-info-neeasade
     sep
     ;; bar
     selection-info
     matches
     lispy-indicator
     )
  '(

     checker
     sep
     next-buffers
     sep
     ;; vcs ;; somehow this one makes the spacing off
     misc-info
     ;; input-method
     ;; buffer-encoding
     ;; major-mode
     ;; process
     ;; sep
     buffer-position
     ;; battery
     )
  )

(defun setup-custom-doom-modeline ()
  (doom-modeline-set-modeline 'neeasade-doomline 'default))
(add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)

(defun! ns/refresh-all-modeline (toggle)
  (ns/setq-local-all
    'mode-line-format
    ;; (doom-modeline-format--neeasade-doomline)
    (if toggle ''("%e" (:eval (doom-modeline-format--neeasade-doomline))) nil)
    ;; '(shell-mode circe-chat-mode circe-channel-mode)
    )

  (ns/frame-set-parameter 'bottom-divider-width (if toggle 0 1))

  ;; force redraw of all frames
  (ns/apply-frames (fn nil)))

;; refesh all mode lines based on the value of the modeline in the current buffer
(ns/bind "tM" (fn!! (ns/refresh-all-modeline (not mode-line-format))))

(ns/refresh-all-modeline t)
