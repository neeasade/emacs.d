;; -*- lexical-binding: t; -*-

(ns/use-package color-tools "neeasade/color-tools.el")

(ns/use-package tarps "neeasade/tarps"
  :config
  ;; todo: why isn't this happening automatically
  (require 'tarps)
  )

(defun ns/update-xrdb-font (font &optional toggle)
  "Update the fallback font for xrdb value"
  (let ((key (if toggle "st.font_variable" "st.font")))
    (setq ns/xrdb-fallback-values
      (delq (assoc key ns/xrdb-fallback-values) ns/xrdb-fallback-values))

    (setq ns/xrdb-fallback-values
      (cons `(,key . ,font) ns/xrdb-fallback-values))))

(mapc
  (fn (when (find-font (font-spec :name <>))
        (ns/update-xrdb-font <>)))
  (list
    (font-get (face-attribute 'default :font) :name)
    "Dejavu Sans Mono-14"
    "DejaVu Sans Mono-14"
    "Lucida Console-14"
    "Noto Sans Mono-14"
    "Source Code Pro-14"
    "Go Mono-14"
    "Menlo-14"
    ))

(mapc
  (fn (when (find-font (font-spec :name <>))
        (ns/update-xrdb-font <> t)))
  (list
    (font-get (face-attribute 'default :font) :name)
    "Dejavu Sans-14"
    "DejaVu Sans-14"
    "Lucida Console-14"
    "Noto Serif-14"
    "Charter-14"
    "Menlo-14"
    ))

;; unload anything currently loaded
(mapcar 'disable-theme custom-enabled-themes)

;; (load-theme 'tarp-struan t)
(load-theme 'tarp-mcfay t)
;; (load-theme 'tarp-marlowe t)

(setq ns/theme tarp/theme)

;; this tweak has to be done on every frame creation
(defun ns/set-fringe-bg (frame) (set-face-attribute 'fringe frame :background nil))
(add-hook 'after-make-frame-functions 'ns/set-fringe-bg)

;; evil
(let ((c (ht-get ns/theme :accent1_)))
  (setq
    evil-normal-state-cursor `(,c box)
    evil-insert-state-cursor `(,c bar)
    evil-visual-state-cursor `(,c box)))

;; frames:
(ns/frame-set-parameter 'internal-border-width (if ns/enable-home-p 0 6))
(ns/frame-set-parameter 'right-divider-width 1)
(ns/frame-set-parameter 'bottom-divider-width 1)
(ns/frame-set-parameter 'font (get-resource "st.font"))

;; fringe
;; (fringe-mode 8)

(setq window-divider-default-right-width 1)
(setq window-divider-default-places t)
(window-divider-mode t)

(setq-default indicate-empty-lines nil)

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
    `(("TODO" . ,(ht-get tarp/theme :foreground_))
       ("todo" . ,(ht-get tarp/theme :foreground_))))

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
              ;; :underline nil
              ;; :inherit nil
              ))))

(set-face-attribute 'italic nil :slant 'italic)

(when (fboundp 'ns/style-circe) (ns/style-circe))
(when (fboundp 'ns/style-org) (ns/style-org))

(ns/doomline)
