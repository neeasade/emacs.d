;; -*- lexical-binding: t; -*-
(defun spacemacs/compute-powerline-height ()
  "Return an adjusted powerline height."
  (let ((scale (if (and (boundp 'powerline-scale) powerline-scale)
                 powerline-scale 1)))
    (truncate (* scale (frame-char-height)))))

(use-package spaceline)
(require 'spaceline-config)

(setq-ns powerline
  scale (string-to-number (get-resource "Emacs.powerlinescale"))
  height (spacemacs/compute-powerline-height)
  default-separator (get-resource "Emacs.powerline"))

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
      (s-left 8 (ns/next-buffer-name))))
  :enabled t
  )

(defun ns/set-spaceline ()
  (spaceline-spacemacs-theme)

  (spaceline-compile 'main
    '(
       anzu
       ;; (remote-host projectile-root ">>" buffer-id buffer-modified)
       (buffer-id buffer-modified)
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
       )))

;; set the modeline for all existing buffers
;; todo: make this unset modeline on not matching spawn
(defun! ns/refresh-all-modeline (toggle)
  (when toggle (ns/set-spaceline))

  (ns/setq-local-all
    'mode-line-format
    (if toggle
      ''("%e" (:eval (spaceline-ml-main)))
      ;; if we don't want modeline, we still might want
      ;; padding on the bottom if we aren't using frame padding
      (if (s-equals-p (get-resource "Emacs.padding_source") "st") nil " " )
      ))

  (when
    (and
      (not (s-equals-p (get-resource "Emacs.padding_source") "st"))
      (not toggle))
    (set-face-attribute 'mode-line          nil :background nil)
    (set-face-attribute 'mode-line-inactive nil :background nil))

  (setq window-divider-default-bottom-width (if toggle 0 1))
  (ns/apply-frames (fn (set-frame-parameter <> 'bottom-divider-width (if toggle 0 1))))
  ;; force redraw of all frames
  (ns/apply-frames (fn nil))
  )

(ns/refresh-all-modeline t)

(ns/bind "M"
  (fn! (ns/refresh-all-modeline
         (s-blank-p
           (s-trim
             (if (stringp mode-line-format)
               mode-line-format
               "not blank"))))))


(defun! ns/toggle-modeline ()
  (make-local-variable 'ns/modeline)

  (if mode-line-format
    (progn
      (setq ns/modeline mode-line-format)
      (setq mode-line-format nil))
    (setq mode-line-format '("%e" (:eval (spaceline-ml-main)))))
  (redraw-frame))

(ns/bind "tm" 'ns/toggle-modeline)
