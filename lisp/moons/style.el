;; todo: an xresources theme that doesn't suck/covers extensions that base16 covers
(use-package base16-theme)
;; https://github.com/waymondo/apropospriate-theme
;;(use-package ujelly-theme)

(let ((theme (intern (get-resource "Emacs.theme"))))
  (when (boundp 'ns/loaded-theme)
    (disable-theme ns/loaded-theme))
  (load-theme theme t)
  (setq ns/loaded-theme theme))

(set-face-attribute 'fringe nil :background nil)
(set-face-background 'font-lock-comment-face nil)

(defun ns/apply-frames (action)
  (mapc (lambda(frame)
          (interactive)
          (funcall action frame)
          (redraw-frame frame))
    (frame-list)))


;; todo: make this apply to all open buffers and futur buffers
;; see the dance we do in spaceline config
(setq header-line-format nil)

(fringe-mode)
(setq ns/frame-padding (string-to-number (get-resource "st.borderpx")))

(when (s-equals-p (get-resource "Emacs.padding_source") "auto")
  (setq ns/frame-padding 0)

  (setq header-line-format " ")
  (set-face-attribute 'header-line nil :background (face-attribute 'default :background))

  (fringe-mode (window-header-line-height)))

;; todo: combine the header footer padding toggle stuff with this
(ns/apply-frames
  (fn (set-frame-parameter <> 'internal-border-width
        ns/frame-padding)))

;; future frames
(when (alist-get 'internal-border-width default-frame-alist)
  (setq default-frame-alist (assq-delete-all 'internal-border-width default-frame-alist)))

(add-to-list 'default-frame-alist
  `(internal-border-width . ,ns/frame-padding))

;; sync w/ term background
;; (set-background-color (get-resource "*.background"))

;; assume softer vertical border by matching comment face
(set-face-attribute 'vertical-border
  nil :foreground (face-attribute 'font-lock-comment-face :foreground))

;; this doesn't persist across new frames even though the docs say it should
(set-face-attribute 'fringe nil :background nil)
(add-hook 'after-make-frame-functions
  (lambda (frame)
    (set-face-attribute 'fringe nil :background nil)))

;; set font on current and future
(set-face-attribute 'default nil :font (get-resource "st.font"))
(set-frame-font (get-resource "st.font") nil t)

(setq-default indicate-empty-lines nil)

(setq ns/colored-whitespace? nil)
(defun color-whitespace-mode(&rest maybe)
  (when (not ns/colored-whitespace?)
    (set-face-attribute 'whitespace-space nil :background nil)
    (set-face-attribute 'whitespace-tab nil :background nil)
    (set-face-attribute 'whitespace-newline nil
      :foreground (face-attribute 'whitespace-space :foreground))
    (setq ns/colored-whitespace? t)
    ))

(advice-add 'whitespace-mode :after #'color-whitespace-mode )

(use-package hl-todo
  :config
  (let* ((comment-color (face-attribute 'font-lock-comment-face :foreground))
          (highlight-color (ns/color-tone comment-color 30 30)))

    (setq hl-todo-keyword-faces
      `(("TODO" . ,highlight-color)
         ("todo" . ,highlight-color)
         ("NOTE" . ,highlight-color)
         ("note" . ,highlight-color)
         )))

  (global-hl-todo-mode)
  )

;; NO BOLD
;; (set-face-bold-p doesn't cover everything, some fonts use slant and underline as bold...)
(mapc (lambda (face)
        (set-face-attribute face nil
          :weight 'normal
          :slant 'normal
          :underline nil
          ;;:inherit nil
          ))
  (face-list))

(use-package dimmer
  :config (setq dimmer-fraction 0.5)
  (dimmer-mode 0))

;; gross colors, but need something so we have a signifier in unique match case
;; todo: maybe fix gross colors
;; (set-face-attribute 'avy-lead-face nil :background (ns/color-tone (face-attribute 'default :background) 30 30))
;; (set-face-attribute 'avy-lead-face nil :foreground (ns/color-tone (face-attribute 'default :foreground) 30 30))

(ns/spaceline)
