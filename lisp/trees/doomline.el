;;; -*- lexical-binding: t; -*-
;; lexical needed for our def-modeline override
(use-package doom-modeline)

(defface ns/mode-line-middle nil "middle mode line color" :group 'doom-modeline-faces)
(defface ns/mode-line-sep nil "sep" :group 'doom-modeline-faces)
(defface ns/mode-line-sep-edge nil "sep-edge" :group 'doom-modeline-faces)
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
      (doom-modeline-spc)
      )))

(defsubst doom-modeline-spc ()
  "Text style with whitespace (thin)."
  (propertize " " 'face (if (doom-modeline--active)
                          'mode-line
                          'mode-line-inactive)))

(doom-modeline-def-segment buffer-info-neeasade
  (concat
    (doom-modeline-spc)
    (doom-modeline-spc)
    (doom-modeline--buffer-name)
    ;; (doom-modeline--buffer-file-name)
    (doom-modeline--buffer-state-icon)
    ))

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
  (->>
    (window-prev-buffers)
    ;; (reverse)
    (-map 'first)
    (-map 'buffer-name)
    ;; (-drop 1)
    (-filter (fn (not (string= (buffer-name (current-buffer)) <>))))
    (-filter (fn (not (string= (ns/next-buffer-name) <>))))
    (-first (fn (not (ns/should-skip <>))))
    ;; (current-buffer)
    ))

(doom-modeline-def-segment next-buffers
  (when (doom-modeline--active)
    (propertize
      (concat
        (doom-modeline-spc)
        (format "[%s|%s]"
          (or (s-left 8 (ns/prev-buffer-name)) "<None>")
          (or (s-left 8 (ns/next-buffer-name)) "<None>"))
        (doom-modeline-spc))
      'face 'mode-line
      )))





(doom-modeline-def-segment sep
  "Text style with whitespace."
  (propertize " " 'face (if (doom-modeline--active)
                          'ns/mode-line-sep
                          'mode-line-inactive)))

(doom-modeline-def-segment sep-edge
  (propertize " " 'face (if (doom-modeline--active)
                          'ns/mode-line-sep-edge
                          'mode-line-inactive)))



(column-number-mode) ; give us column info in the modeline

(doom-modeline--prepare-segments
  '(
     ;; sep
     remote-host
     buffer-info-neeasade
     sep
     checker

     sep-edge
     ;; bar
     selection-info
     ;; matches
     )
  )

(defun ns/doom-modeline-def-modeline (name lhs rhs)
  (let ((sym (intern (format "doom-modeline-format--%s" name)))
         (lhs-forms (doom-modeline--prepare-segments lhs))
         (rhs-forms (doom-modeline--prepare-segments rhs)))
    (defalias sym
      (lambda ()
        (list lhs-forms
          (propertize
            " "
            'face (if (doom-modeline--active) 'ns/mode-line-middle 'mode-line-inactive)
            'display `((space :align-to (- (+ right right-fringe right-margin)
                                          ,(* (if (number-or-marker-p (face-attribute 'mode-line :height))
                                                (/ (doom-modeline--window-font-width)
                                                  (frame-char-width) 1.0)
                                                1)
                                             (-
                                               (string-width
                                                 (format-mode-line
                                                   (cons "" rhs-forms)))
                                               (round
                                                 (/
                                                   (seq-count
                                                     (lambda (c) (= c ? )); XXX there is a thin space here (literal charater ?).
                                                     (format-mode-line
                                                       (cons "" rhs-forms))
                                                     )
                                                   ;; the magic number
                                                   1.5
                                                   )))
                                             )))))
          rhs-forms))
      (concat "Modeline:\n"
        (format "  %s\n  %s"
          (prin1-to-string lhs)
          (prin1-to-string rhs)))))
  )


(set-face-attribute 'ns/mode-line-middle nil :background
  (face-attribute 'default :background))

(set-face-attribute 'ns/mode-line-sep-edge nil :background
  (face-attribute 'default :background)
  ;; "#000000"
  )

(set-face-attribute 'ns/mode-line-sep nil :background
  ;; (ns/color-greaten 10 (face-attribute 'mode-line :background))
  ;; "#8888cc"
  (face-attribute 'default :background)
  )

(set-face-attribute 'mode-line nil :background
  (ns/color-lessen 20 (face-attribute 'default :background))
  ;; (face-attribute 'default :background)
  )

(set-face-attribute 'mode-line nil :height 100)
(set-face-attribute 'mode-line-inactive nil :height 100)

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

(ns/doom-modeline-def-modeline 'neeasade-doomline
  '(
     ;; sep
     remote-host
     buffer-info-neeasade
     sep
     checker
     sep-edge
     ;; bar
     selection-info
     matches
     )
  '(
     ;; bar
     sep-edge
     next-buffers
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
    (if toggle
      ''("%e" (:eval (doom-modeline-format--neeasade-doomline)))
      ;; if we don't want modeline, we still might want
      ;; padding on the bottom if we aren't using frame padding
      (if (s-equals-p (get-resource "Emacs.padding_source") "st") nil " " )))

  (when (and (not (s-equals-p (get-resource "Emacs.padding_source") "st"))
          (not toggle))
    (set-face-attribute 'mode-line          nil :background nil)
    (set-face-attribute 'mode-line-inactive nil :background nil))

  (setq window-divider-default-bottom-width (if toggle 0 1))
  (ns/apply-frames (fn (set-frame-parameter <> 'bottom-divider-width (if toggle 0 1))))
  ;; force redraw of all frames
  (ns/apply-frames (fn nil))
  )

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

(ns/bind "tm" 'ns/toggle-modeline)

(ns/refresh-all-modeline t)
