;; -*- lexical-binding: t; -*-
(when (not (-contains-p features 'tarps))
  (ns/use-package ct "neeasade/ct.el")
  (ns/use-package tarps "neeasade/tarps" :config (require 'tarps)))

(defun ns/update-xrdb-font (font &optional toggle)
  "Update the fallback font for xrdb value"
  (let ((key (if toggle "st.font_variable" "st.font")))
    (setq ns/xrdb-fallback-values
      (delq (assoc key ns/xrdb-fallback-values) ns/xrdb-fallback-values))

    (setq ns/xrdb-fallback-values
      (cons `(,key . ,font) ns/xrdb-fallback-values))))

(mapc (fn (when (find-font (font-spec :name <>))
            (ns/update-xrdb-font <>)))
  (list
    ;; (or (font-get (face-attribute 'default :font) :name) "")
    "Menlo-14"
    "Dejavu Sans Mono-14"
    "DejaVu Sans Mono-14"
    "Lucida Console-14"
    "Noto Sans Mono-14"
    "Source Code Pro-14"
    "Go Mono-14"))

(mapc (fn (when (find-font (font-spec :name <>))
            (ns/update-xrdb-font <> t)))
  (list
    ;; (or (font-get (face-attribute 'default :font) :name) "")
    "Menlo-14"
    "Dejavu Sans-14"
    "DejaVu Sans-14"
    "Lucida Console-14"
    "Noto Serif-14"
    "Charter-14"))

;; fringe
;; (fringe-mode 8)
(setq window-divider-default-right-width 1)
(setq window-divider-default-places t)
(window-divider-mode t)

(setq-default indicate-empty-lines nil)

(use-package hl-todo
  :config
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

(defun! ns/load-theme (&optional theme)
  (mapcar 'disable-theme custom-enabled-themes)

  (load-theme
    (or theme
      (intern
        (ivy-read "Load custom theme: "
          ;; (mapcar 'symbol-name (custom-available-themes))
          '(tarp-mcfay tarp-struan)
          :action 'identity)))
    t)

  (setq ns/theme tarp/theme)

  (setq hl-todo-keyword-faces
    `(("TODO" . ,(ht-get tarp/theme :foreground_))
       ("todo" . ,(ht-get tarp/theme :foreground_))))

  (->>
    `(internal-border-width ,(if ns/enable-home-p 0 10)
       right-divider-width 1
       bottom-divider-width 0
       font ,(get-resource "st.font"))
    (-partition 2)
    (-map (-applify #'ns/frame-set-parameter)))

  (when (fboundp 'ns/style-circe) (ns/style-circe))
  (when (fboundp 'ns/style-org) (ns/style-org))
  (when (fboundp 'ns/style-markdown) (ns/style-markdown))

  (ns/doomline)

  (when ns/enable-blog-p
    (ns/blog-set-htmlize-colors))
  )

;; export the theme as shell env variables:
(use-package theme-magic
  :config
  (defun ns/emacs-to-theme ()
    (s-join "\n"
      (append (seq-map-indexed
                (fn (format "color%s=%s" (number-to-string <2>)
                      (s-replace "#" "" <1>)))
                (theme-magic--auto-extract-16-colors))

        (-map
          (fn
            (format "%s=%s"
              (car <>)
              (s-replace "#" "" (ct-shorten (cadr <>)))))
          (-partition 2
            (append
              (list
                "foreground" (face-attribute 'default :foreground)
                "background" (face-attribute 'default :background)
                "cursorColor" (first evil-insert-state-cursor))
              (->>
                (list :normal :weak :strong :focused)
                (-mapcat
                  (lambda (bg-key)
                    (-mapcat (fn (list <> bg-key))
                      '(:background :foreground :faded :primary :alt :strings :assumed))))

                (-partition 2)
                (-mapcat
                  (lambda (parts)
                    (seq-let (fg-key bg-key) parts
                      (list
                        (format "e_%s_%s"
                          (-> fg-key pr-string (substring 1))
                          (-> bg-key pr-string (substring 1)))
                        (tarp/get fg-key bg-key)))))))))))))
