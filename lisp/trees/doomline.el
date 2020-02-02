;; trying out doom-modeline
(use-package doom-modeline)

;; doom stuff that I'm tweaking
;; overridding because upstream is really spacey
(doom-modeline-def-segment buffer-position
  "The buffer position information."
  (let* ((active (doom-modeline--active))
          (lc '(line-number-mode
                 (column-number-mode
                   (doom-modeline-column-zero-based "%l:%c" "%l:%C")
                   "%l")
                 (column-number-mode (doom-modeline-column-zero-based ":%c" ":%C"))))
          (face (if active 'ns/mode-line-segment
                  'mode-line-inactive))
          (mouse-face 'mode-line-highlight)
          (local-map mode-line-column-line-number-mode-map))
    (concat
      (doom-modeline-spc)
      (propertize (format-mode-line lc)
        'face face
        'help-echo "Buffer position - mouse-1: Display Line and Column Mode Menu"
        'mouse-face mouse-face
        'local-map local-map)
      (doom-modeline-spc)
      )))

;; we want segments to have different faces than the mid part, so we must invert the world.
(defface ns/mode-line-segment nil "segment face" :group 'doom-modeline-faces)

(set-face-attribute 'ns/mode-line-segment nil :background
  (ns/color-greaten 12 (face-attribute 'mode-line :background)))
;; (set-face-attribute 'ns/mode-line-segment-inactive nil :background (face-attribute 'mode-line-inactive :background))

(defsubst doom-modeline-spc ()
  "Text style with whitespace"
  (propertize " " 'face (if (doom-modeline--active)
                          'ns/mode-line-segment
                          'mode-line-inactive)))

(doom-modeline-def-segment buffer-info-neeasade
  (concat
    (doom-modeline-spc)
    (doom-modeline--buffer-name)
    ;; (doom-modeline--buffer-file-name)
    (doom-modeline--buffer-state-icon)
    ))

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
  )

;; (setq doom-modeline-buffer-state-icon nil)
;; (setq doom-modeline-buffer-state-icon nil)

(set-face-attribute 'mode-line nil :height 100)
(set-face-attribute 'mode-line-inactive nil :height 100)

;; Hooks that run before/after the modeline version string is updated
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)

;; note to self: abandon this, look at switch-to-next, prev-buffer source, grab that and replace return
(defun ns/next-buffer-name ()
  (-first
    (lambda (bufname) (not (ns/should-skip bufname)))
    (-map 'buffer-name (window-next-buffers)))
  )

(defun ns/prev-buffer-name ()
  (-first
    (lambda (bufname) (not (ns/should-skip bufname)))
    (-map 'buffer-name (window-prev-buffers))))


(doom-modeline-def-segment next-buffers
  (when (doom-modeline--active)
    (propertize
      (concat
        ;; todo: mabye [ <buf:-> ¦ <buf:-> ]
        (doom-modeline-spc)
        (s-left 8 (ns/prev-buffer-name))
        " - "
        (s-left 8 (ns/next-buffer-name))
        (doom-modeline-spc)
        )
      'face 'ns/mode-line-segment
      )))

(column-number-mode) ;; give us column info in the modeline
;; todo: get all the faces from these segments/make sure we change their face to be the same as ns/mode-line-segment

(mapcar
  (fn (set-face-attribute <> nil :background
        (face-attribute 'ns/mode-line-segment :background)
        ))
  '(
     doom-modeline-buffer-modified
     doom-modeline-buffer-file
     doom-modeline-info
     doom-modeline-debug
     )
  )

(doom-modeline-def-segment edge-bar
  "The bar regulates the height of the mode-line in GUI."
  (if (doom-modeline--active)
    doom-modeline--bar-active-edge
    doom-modeline--bar-inactive))

(doom-modeline-def-modeline 'neeasade-doomline
  '(matches
     bar
     remote-host
     buffer-info-neeasade
     checker
     ;; bar
     edge-bar
     selection-info
     )
  '(
     edge-bar
     next-buffers
     vcs
     misc-info
     minor-modes
     input-method
     ;; buffer-encoding
     ;; major-mode
     ;; process
     bar
     buffer-position
     battery
     bar
     )
  )

(defface doom-modeline-bar-edge nil "edge bar" :group 'doom-modeline-faces)

(set-face-attribute 'doom-modeline-bar-edge
  nil
  :background
  ;; (ns/color-lessen 10 (face-attribute 'mode-line :background))
  "#000000"
  )

(defun doom-modeline-refresh-bars (&optional width height)
  "Refresh mode-line bars with `WIDTH' and `HEIGHT'."
  (let ((width (or width doom-modeline-bar-width))
         (height (max (or height doom-modeline-height)
                   (doom-modeline--font-height))))
    (setq doom-modeline--bar-active
      (doom-modeline--make-xpm 'doom-modeline-bar width height)
      doom-modeline--bar-inactive
      (doom-modeline--make-xpm 'doom-modeline-bar-inactive width height)
      doom-modeline--bar-active-edge
      (doom-modeline--make-xpm 'doom-modeline-bar-edge width height)
      )))

(doom-modeline-def-segment bar
  "The bar regulates the height of the mode-line in GUI."
  (if (doom-modeline--active)
    doom-modeline--bar-active
    doom-modeline--bar-inactive))

(set-face-attribute 'doom-modeline-bar nil :background
  (ns/color-greaten 20 (face-attribute 'mode-line :background))
  ;; "#b9b9b9"
  )

(defun setup-custom-doom-modeline ()
  (doom-modeline-set-modeline 'neeasade-doomline 'default))

(add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)

;; todo: make this unset modeline on not matching spawn
(defun! ns/refresh-all-modeline (toggle)
  (ns/setq-local-all
    'mode-line-format
    (if toggle
      ''("%e" (:eval (doom-modeline-format--neeasade-doomline)))
      ;; if we don't want modeline, we still might want
      ;; padding on the bottom if we aren't using frame padding
      (if (s-equals-p (get-resource "Emacs.padding_source") "st") nil " " )
      ))

  (when (and (not (s-equals-p (get-resource "Emacs.padding_source") "st"))
          (not toggle))
    (set-face-attribute 'mode-line          nil :background nil)
    (set-face-attribute 'mode-line-inactive nil :background nil))

  (setq window-divider-default-bottom-width (if toggle 0 1))
  (ns/apply-frames (fn (set-frame-parameter <> 'bottom-divider-width (if toggle 0 1))))
  ;; force redraw of all frames
  (ns/apply-frames (fn nil))
  )

(ns/refresh-all-modeline t)

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
  (redraw-frame))


;; (list-faces-display)

;; (set-face-attribute 'mode-line nil :background "#ffffff")


;; (set-face-attribute 'mode-line nil :background nil)
;; (face-attribute 'mode-line :background)

;; (set-face-attribute 'doom-modeline-bar nil :background "#ffffff")

(ns/bind "tm" 'ns/toggle-modeline)

;; force redraw of all frames
(ns/apply-frames (fn nil))
