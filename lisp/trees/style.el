;; -*- lexical-binding: t; -*-

(use-package base16-theme)

(add-to-list 'custom-theme-load-path (~ ".emacs.d/lisp/themes"))

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

(mapcar 'disable-theme custom-enabled-themes)
;; (load-theme 'neea t)
(load-theme 'neeo t)

;; get the whitespace-mode faces:
;; (our tweaks require the faces to be loaded)
(require 'whitespace)

;; override some base16 decisions
;; (side note: I couldn't get this to work in the theme itself)
;; played with: custom-theme-set-faces and custom-theme-recalc-face and all sorts of dumb stuff
;; gave up and extending here:
(let ((base16-tweaks
        `(
           (avy-lead-face :background :accent1_)
           (avy-lead-face-0 :background :accent1)
           (avy-lead-face-2 :background :accent2)

           ;; face pace-part value
           ;; value may be a key from ns/theme
           (font-lock-comment-delimiter-face :foreground :foreground_)
           (isearch :foreground :background+)
           (isearch :background :foreground)
           (comint-highlight-prompt :foreground :foreground)
           (fringe :background nil)
           ;; (mode-line :background nil)
           (font-lock-comment-face :background nil)
           (magit-diff-context-highlight :background
             ,(ns/color-lab-darken (ht-get ns/theme :background) 4))
           (window-divider :foreground :foreground_)
           ;; match variables to functions
           ;; (font-lock-function-name-face :foreground :accent2)
           (font-lock-variable-name-face :foreground :accent1)
           ;; consider nulling out and using flat newlines org links
           ;; (org-link :foreground :accent1_)
           ;; (font-lock-type-face :foreground :accent1)

           (org-todo :background :background_)
           (org-done :background :background_)

           (org-todo :foreground :accent2_)
           (org-done :foreground :accent2)

           (org-date :underline nil)
           (org-date :foreground :accent1_)

           (org-drawer :foreground :accent1_)
           (org-block-begin-line :foreground :foreground_)
           (org-block-end-line :foreground :foreground_)

           (org-level-1 :foreground :foreground)
           (org-level-2 :foreground :foreground)
           (org-level-3 :foreground :foreground)
           (org-level-4 :foreground :foreground)
           (org-level-5 :foreground :foreground)
           (org-level-6 :foreground :foreground)

           (org-headline-done :foreground :foreground)
           (org-headline-done :background nil)
           (org-special-keyword :foreground :foreground_)

           (whitespace-space :background nil)
           (whitespace-tab :background nil)
           ;; (whitespace-newline :background nil)
           (flycheck-warning :underline nil)
           (flycheck-info :underline nil)
           )))

  ;; if we were doing this the /right/ rather than set face attributes we would
  ;; update theme faces and recalc them -- but then we'd have to know all properties we want to change
  ;; easier to shove one off's in the above list as I encounter them
  (-map (lambda (input)
          (apply (lambda (face part key)
                   (set-face-attribute face nil part
                     (if (-contains-p (ht-keys ns/theme) key)
                       (ht-get ns/theme key) key)))
            input)) base16-tweaks
    ))

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
  (let ((highlight-color (face-attribute 'font-lock-comment-face :foreground)))
    (setq hl-todo-keyword-faces
      `(("TODO" . ,highlight-color)
         ("todo" . ,highlight-color)
         ;; ("NOTE" . ,highlight-color)
         ;; ("note" . ,highlight-color)
         ))

    ;; todo: this doesn't seem to update magit-todos? - not seeing 'todo' show up
    (setq magit-todos-keywords hl-todo-keyword-faces)
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
              ;; :underline nil
              ;; :inherit nil
              ))))

(set-face-attribute 'italic nil :slant 'italic)

(when (fboundp 'ns/style-circe) (ns/style-circe))
(when (fboundp 'ns/style-org) (ns/style-org))

(ns/doomline)
