;; -*- lexical-binding: t; -*-

(use-package doom-modeline)

(defface ns/mode-line-middle
  '((t (:inherit (mode-line))))
  "middle mode line color" :group 'doom-modeline-faces)

(defface ns/mode-line-sep
  '((t (:inherit (mode-line))))
  "sep" :group 'doom-modeline-faces)

(defface ns/mode-line-sep-edge
  '((t (:inherit (mode-line))))
  "sep-edge" :group 'doom-modeline-faces)

(set-face-attribute 'mode-line nil :background
  ;; (ns/color-lessen 3 (face-attribute 'default :background))
  (ns/color-lessen 5 (ht-get ns/theme :background+))
  ;; (ns/color-lessen 3 (face-attribute 'default :background+))
  ;; (face-attribute 'default :background)
  )

(set-face-attribute 'ns/mode-line-middle nil :background
  ;; (ns/color-lessen 3 (face-attribute 'default :background))
  (ht-get ns/theme :background+)
  ;; (face-attribute 'default :background)
  )

;; (face-attribute 'mode-line :foreground)
(set-face-attribute 'doom-modeline-buffer-file nil :foreground (ht-get ns/theme :accent2))
(set-face-attribute 'doom-modeline-buffer-modified nil :foreground (ht-get ns/theme :accent2))


(set-face-attribute 'ns/mode-line-sep-edge nil :background
  ;; (face-attribute 'default :background)
  (ns/color-lessen 20 (ht-get ns/theme :background+))
  ;; (ns/color-lessen 10 (face-attribute 'mode-line :background))
  )

(set-face-attribute 'ns/mode-line-sep nil :background
  (ns/color-lessen 20 (ht-get ns/theme :background+))
  ;; (ns/color-lessen 6 (face-attribute 'mode-line :background))
  )

(set-face-attribute 'mode-line-inactive nil :background
  (ns/color-lch-transform
    (ns/color-lessen 10 (ht-get ns/theme :background))
    (lambda (L C H)
      (list L C
        (third (apply 'color-lab-to-lch
                 (ns/color-name-to-lab
                   (ht-get ns/theme :accent1_)))))))

  ;; (ns/color-lch-transform
  ;;   (ht-get ns/theme :background+)
  ;;   (lambda (L C H)
  ;;     (list (- L 4) (* 0.5 C) H)))

  ;; (ns/color-greaten 20 (ht-get ns/theme :background+))
  ;; (ns/color-lessen 5 (ht-get ns/theme :background+))

  ;; (ns/color-lessen 5 (face-attribute 'default :background))
  )

(set-face-attribute 'mode-line nil :height 100)
(set-face-attribute 'mode-line-inactive nil :height 100)

;; some themes like to tweak the box
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

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
    (doom-modeline-spc)
    (doom-modeline--buffer-name)
    ;; (doom-modeline--buffer-file-name)
    (doom-modeline--buffer-state-icon)))

(defun ns/next-buffer-name ()
  (->> (window-next-buffers)
    ;; (-map 'first)
    (-map 'buffer-name)
    ;; (-drop 1)
    (-filter (fn (not (string= (buffer-name (current-buffer)) <>))))
    (-first (fn (not (ns/should-skip <>))))
    ;; (current-buffer)
    ))

(defun ns/prev-buffer-name ()
  (->> (window-prev-buffers)
    ;; (reverse)
    (-map 'first)
    (-map 'buffer-name)
    ;; (-drop 1)
    (-filter (fn (not (string= (buffer-name (current-buffer)) <>))))
    (-filter (fn (not (string= (ns/next-buffer-name) <>))))
    (-first (fn (not (ns/should-skip <>))))
    ;; (current-buffer)
    ))

(doom-modeline-def-segment lispy-indicator
  (if (lispyville--lispy-keybindings-active-p) "LISPY" ""))

(doom-modeline-def-segment next-buffers
  (when (doom-modeline--active)
    (propertize
      (concat
        (doom-modeline-spc)

        (format "[%s:%s]"
          (or (s-left 8 (ns/prev-buffer-name)) "<None>")
          (or (s-left 8 (ns/next-buffer-name)) "<None>"))

        (doom-modeline-spc)
        )
      'face 'mode-line
      )))

(doom-modeline-def-segment sep
  "Text style with whitespace."
  (propertize
    " "
    'face (if (doom-modeline--active)
            'ns/mode-line-sep
            'mode-line-inactive)))

(column-number-mode) ; give us column info in the modeline

(setq-ns doom-modeline
  height (string-to-number (get-resource "Emacs.doomlineheight"))
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
     remote-host
     buffer-info-neeasade
     sep
     checker
     ;; bar
     selection-info
     matches
     lispy-indicator
     )
  '(
     ;; bar
     ;; next-buffers
     ;; vcs ;; somehow this one makes the spacing off
     misc-info
     ;; input-method
     ;; buffer-encoding
     ;; major-mode
     ;; process
     ;; sep
     ;; sep-edge
     sep
     buffer-position
     ;; battery
     ;; " "
     )
  )

(defun setup-custom-doom-modeline ()
  (doom-modeline-set-modeline 'neeasade-doomline 'default))
(add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)

;; todo: maybe filter out shell-mode and circe buffers here
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
(ns/bind "M" (fn! (ns/refresh-all-modeline (not mode-line-format))))

(defun! ns/toggle-modeline ()
  "toggle the modeline in the current buffer"
  (make-local-variable 'ns/modeline)
  (if mode-line-format
    (progn
      (setq ns/modeline mode-line-format)
      (setq mode-line-format nil))
    (setq mode-line-format '("%e" (:eval (doom-modeline-format--neeasade-doomline)))))

  ;; (doom-modeline-format--neeasade-doomline)

  (redraw-frame))

(ns/bind "tm" 'ns/toggle-modeline)

(ns/refresh-all-modeline t)
