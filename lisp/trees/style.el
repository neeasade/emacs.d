;; -*- lexical-binding: t; -*-

(use-package base16-theme)

(add-to-list 'custom-theme-load-path (~ ".emacs.d/lisp/themes"))

;; todo: we are now depending on populated values in ns/theme in some places might be appropriate to
;; store a backup theme, because this depends on color.el/a static interface to it

;; todo: some stuff here could probably be pulled into the theme itself, actually

(defun! ns/font-reset ()
  (set-face-attribute 'default nil :font (get-resource "st.font"))
  (ns/apply-frames
    (fn (set-frame-parameter <> 'font (get-resource "st.font")))))

;; todo later: this let* is really a step-by-step labeled flow
;; the -as-> macro might fit nicely
;; also -- this multiply thing might be a dumb idea, maybe just prompt for desired font-size instead
(defun! ns/font-multiply ()
  (let* ((font (get-resource "st.font"))
          (multiplier (string-to-number
                        (read-string
                          (format "font multiplier (apply to '%s'): " font))))
          (old-num (string-to-number (second (s-split "-" font))))
          (new-num (round (* multiplier old-num)))
          (new-font (format "%s-%s"
                      (first (s-split "-" font))
                      (number-to-string new-num))))

    (message "new font: %s" new-font)
    (set-face-attribute 'default nil :font new-font)
    (set-frame-font (get-resource new-font) nil t)))

(defun ns/update-xrdb-font (font &optional variable)
  (let ((key (if variable "st.font_variable" "st.font")))
    (setq ns/xrdb-fallback-values
      (delq (assoc key ns/xrdb-fallback-values) ns/xrdb-fallback-values))

    (setq ns/xrdb-fallback-values
      (cons `(,key . ,font) ns/xrdb-fallback-values))))

(mapc
  (fn (when (find-font (font-spec :name <>))
        (ns/update-xrdb-font <>)))
  '("Dejavu Sans Mono-14"
     "DejaVu Sans Mono-14"
     "Lucida Console-14"
     "Noto Sans Mono-14"
     "Source Code Pro-14"
     "Go Mono-14"))

(mapc
  (fn (when (find-font (font-spec :name <>))
        (ns/update-xrdb-font <> t)))
  '("Dejavu Sans-14"
     "DejaVu Sans-14"
     "Lucida Console-14"
     "Noto Serif-14"
     "Charter-14"))

(when (not (boundp 'ns/loaded-theme))
  (setq ns/loaded-theme nil))

;; (load-theme  t)
(load-theme 'neea t)

;; (when (not (eq ns/loaded-theme theme))
;;   (disable-theme ns/loaded-theme)
;;   (load-theme theme t)
;;   (setq ns/loaded-theme theme))

;; ｃｏｎｆｏｒｍ
(set-face-attribute
  'font-lock-comment-delimiter-face nil
  :foreground (face-attribute 'font-lock-comment-face :foreground))

(let ((accent1 (ht-get ns/theme :accent1))
       (accent1_ (ht-get ns/theme :accent1_)))
  (setq
    evil-normal-state-cursor `(,accent1_ box)
    evil-insert-state-cursor `(,accent1_ bar)
    evil-visual-state-cursor `(,accent1_ box))
  ;; (set-face-attribute 'comint-highlight-prompt nil :foreground accent1)
  )

(set-face-attribute 'isearch nil :background nil)

;; (set-face-attribute
;;   'font-lock-type-face nil
;;   :foreground
;;   (-> (ht-get ns/theme :accent1__)
;;     (ns/color-lab-darken 10)
;;     (color-desaturate-name 10)
;;     ))

(set-face-attribute 'comint-highlight-prompt nil :foreground (face-attribute 'default :foreground))
(set-face-attribute 'fringe nil :background nil)
(set-face-attribute 'font-lock-comment-face nil :background nil)

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

;; window divider stuff
(setq window-divider-default-right-width 1)

(ns/apply-frames (fn (set-frame-parameter <> 'right-divider-width 1)))
(ns/apply-frames (fn (set-frame-parameter <> 'bottom-divider-width 1)))

(setq window-divider-default-places t)

;; assume softer vertical border by matching comment face
(set-face-attribute 'window-divider nil :foreground
  (face-attribute 'font-lock-comment-face :foreground))

(set-face-attribute 'vertical-border nil :foreground
  (face-attribute 'font-lock-comment-face :foreground))

(set-face-attribute 'vertical-border nil :foreground
  (face-attribute 'mode-line-inactive :background))

(window-divider-mode t)

;; clear fringe background
(defun ns/set-fringe-bg (frame) (set-face-attribute 'fringe frame :background nil))
(ns/apply-frames 'ns/set-fringe-bg)
(add-hook 'after-make-frame-functions 'ns/set-fringe-bg)

;; set font on current and future
(ns/font-reset)

(setq-default indicate-empty-lines nil)

(defun color-whitespace-mode (&rest _)
  (set-face-attribute 'whitespace-space nil :background nil)
  (set-face-attribute 'whitespace-tab nil :background nil)
  (set-face-attribute 'whitespace-newline nil
    :foreground (face-attribute 'whitespace-space :foreground)))

(advice-add 'whitespace-mode :after #'color-whitespace-mode)

(use-package hl-todo
  :config
  (let ((highlight-color
          (ns/color-lessen
            30
            (face-attribute 'font-lock-comment-face :foreground))))

    (setq hl-todo-keyword-faces
      `(("TODO" . ,highlight-color)
         ("todo" . ,highlight-color)
         ;; ("NOTE" . ,highlight-color)
         ;; ("note" . ,highlight-color)
         ))

    ;; todo: this doesn't seem to update magit-todos? - not seeing 'todo' show up
    (setq magit-todos-keywords hl-todo-keyword-faces)
    ;; highlight-color
    )

  (general-nmap
    "]t" 'hl-todo-next
    "[t" 'hl-todo-previous)

  (global-hl-todo-mode))

;; allow font effects in org mode faces, but not in other places
(->> (face-list)
  (-filter (fn (not (s-starts-with-p "org" (prin1-to-string <>)))))
  (-map (fn (set-face-attribute <> nil
              ;; :weight 'normal
              :slant 'normal
              :underline nil
              ;; :inherit nil
              ))))

;; todo: allow underlines in default face in org mode buffers -- see buffer-face-mode
;; todo: we also want to allow underlines in font-lock-comment probably -- spelling errs in code comments

(when (fboundp 'ns/style-circe) (ns/style-circe))
(when (fboundp 'ns/style-org) (ns/style-org))

(ns/doomline)
