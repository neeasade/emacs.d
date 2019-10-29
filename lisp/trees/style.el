;; todo: an xresources theme that doesn't suck/covers extensions that base16 covers

(use-package base16-theme)
;;(use-package ujelly-theme)

(use-package apropospriate-theme)

(defun ns/update-xrdb-font (font &optional variable)
  (let ((key (if variable "st.font_variable" "st.font")))
    (setq ns/xrdb-fallback-values
      (delq (assoc key ns/xrdb-fallback-values) ns/xrdb-fallback-values))

    (setq ns/xrdb-fallback-values
      (cons `(,key . ,font) ns/xrdb-fallback-values))))

;; if no query, check for some fallback fonts.
(when (not (executable-find "xrq"))
  (mapc
    (fn (when (find-font (font-spec :name <>))
          (ns/update-xrdb-font <>)))
    '("Dejavu Sans Mono-14"
       "DejaVu Sans Mono-14"
       "Lucida Console-14"
       "Go Mono-14"))

  (mapc
    (fn (when (find-font (font-spec :name <>))
          (ns/update-xrdb-font <> t)))
    '("Dejavu Sans-14"
       "DejaVu Sans-14"
       "Lucida Console-14"
       "Go-14"
       "Charter-14")))

(let ((theme (intern (get-resource "Emacs.theme"))))
  (when (boundp 'ns/loaded-theme)
    (disable-theme ns/loaded-theme))

  (load-theme theme t)
  (setq ns/loaded-theme theme)

  (when (equal theme 'apropospriate-light)
    ;; (setq apropospriate-mode-line-height nil)
    (setq
      evil-normal-state-cursor '("#8B94C6" box)
      evil-insert-state-cursor '("#8B94C6" bar)
      evil-visual-state-cursor '("#BDC6F8" box))
    ))

(set-face-attribute 'fringe nil :background nil)
(set-face-background 'font-lock-comment-face nil)

(set-face-attribute 'comint-highlight-prompt nil
  :foreground (first evil-normal-state-cursor))

;; handle 2 padding approaches
;; use internal border on frames, or fake it with fringe mode and a header line on each buffer
;; if we are home, use 0 padding so that xpad can get everything.
(let ((st-padding-p (if ns/enable-home-p t (s-equals-p (get-resource "Emacs.padding_source") "st")))
       (st-padding (if ns/enable-home-p 0 (string-to-number (get-resource "st.borderpx")))))

  (ns/setq-local-all 'header-line-format
    (if st-padding-p nil " "))

  (when (not st-padding-p)
    (set-face-attribute 'header-line nil :background (face-attribute 'default :background)))

  ;; 8 is the default
  (fringe-mode (if st-padding-p 8 (window-header-line-height)))

  ;; current frame padding update
  (ns/apply-frames
    (fn (set-frame-parameter <> 'internal-border-width
          (if st-padding-p st-padding 0))))

  ;; future frames
  ;; todo: this generically? need to handle bottom border width
  (when (alist-get 'internal-border-width default-frame-alist)
    (setq default-frame-alist (assq-delete-all 'internal-border-width default-frame-alist)))

  (add-to-list 'default-frame-alist
    `(internal-border-width . ,(if st-padding-p st-padding 0))))

;; sync w/ term background
;; todo: revisit
;; (set-background-color (get-resource "*.background"))

;; window divider stuff
(setq window-divider-default-right-width 1)

(ns/apply-frames (fn (set-frame-parameter <> 'right-divider-width 1)))
(ns/apply-frames (fn (set-frame-parameter <> 'bottom-divider-width 0)))

(setq window-divider-default-places t)

;; assume softer vertical border by matching comment face
(set-face-attribute 'window-divider nil :foreground (face-attribute 'font-lock-comment-face :foreground))
(set-face-attribute 'vertical-border nil :foreground (face-attribute 'font-lock-comment-face :foreground))

(window-divider-mode t)

;; clear fringe background
(defun ns/set-fringe-bg (frame) (set-face-attribute 'fringe frame :background nil))
(ns/apply-frames 'ns/set-fringe-bg)
(add-hook 'after-make-frame-functions 'ns/set-fringe-bg)

;; set font on current and future
(set-face-attribute 'default nil :font (get-resource "st.font"))
(set-frame-font (get-resource "st.font") nil t)

(setq-default indicate-empty-lines nil)

(defun color-whitespace-mode (&rest _)
  (set-face-attribute 'whitespace-space nil :background nil)
  (set-face-attribute 'whitespace-tab nil :background nil)
  (set-face-attribute 'whitespace-newline nil
    :foreground (face-attribute 'whitespace-space :foreground))
  (setq ns/colored-whitespace? t))

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
         ))

    ;; todo: this doesn't seem to update magit-todos
    (when (bound-and-true-p magit-todos-mode)
      (setq magit-todos-keywords hl-todo-keyword-faces)
      (magit-todos-mode 0)
      (magit-todos-mode 1)
      )
    )

  (general-nmap
    "]t" 'hl-todo-next
    "[t" 'hl-todo-previous)

  (global-hl-todo-mode))

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

;; gross colors, but need something so we have a signifier in unique match case
;; todo: maybe fix gross colors
;; (set-face-attribute 'avy-lead-face nil :background (ns/color-tone (face-attribute 'default :background) 30 30))
;; (set-face-attribute 'avy-lead-face nil :foreground (ns/color-tone (face-attribute 'default :foreground) 30 30))

(set-face-attribute 'comint-highlight-prompt nil :foreground (face-attribute 'default :foreground))

(ns/spaceline)
