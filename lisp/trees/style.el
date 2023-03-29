;; -*- lexical-binding: t; -*-
;; nb: main entrypoint here is ns/load-theme

(setq-default indicate-empty-lines nil)

(ns/use (myron-themes :host github :repo "neeasade/myron-themes"))

(when (and (not window-system)
        (string= (getenv "TERM") "xterm-kitty"))
  (setq base16-theme-256-color-source 'colors))

(ns/use paren-face)

(defun ns/maybe-update-xrdb-font (key font)
  "Update the fallback font for xrdb value"
  (when (find-font (font-spec :name (plist-get (ns/parse-font font) :family)))
    (setq ns/xrdb-fallback-values
      (delq (assoc key ns/xrdb-fallback-values) ns/xrdb-fallback-values))

    (setq ns/xrdb-fallback-values
      (cons `(,key . ,font) ns/xrdb-fallback-values))))

(-map (-partial 'ns/maybe-update-xrdb-font "font.mono.spec")
  (list
    "Dejavu Sans Mono-14"
    "DejaVu Sans Mono-14"
    "Lucida Console-14"
    "Noto Sans Mono-14"
    "Source Code Pro-14"
    "Menlo-14"
    "Go Mono-14"
    ))

(-map (-partial 'ns/maybe-update-xrdb-font "font.variable.spec")
  (list
    "Menlo-14"
    "Dejavu Sans-14"
    "DejaVu Sans-14"
    "Lucida Console-14"
    "Noto Serif-14"
    "Charter-14"))

(ns/use hl-todo
  (general-nmap
    "]t" 'hl-todo-next
    "[t" 'hl-todo-previous)

  (global-hl-todo-mode))

;; allow font effects in org mode faces, but not in other places
(->> (face-list)
  (--remove (s-starts-with-p "org" (ns/str it)))
  (--map (set-face-attribute it nil
           ;; :weight 'normal
           :slant 'normal
           ;; :underline nil
           ;; :inherit nil
           )))

(set-face-attribute 'italic nil :slant 'italic)

(ns/use theme-magic
  (defun ns/emacs-to-theme ()
    (parseedn-print-str
      (ht
        (:colors (apply 'vector (theme-magic--auto-extract-16-colors)))
        (:color
          (ht-merge myron-theme*
            (ht (:cursor (first evil-insert-state-cursor)))))))))

(defun default-font-width ()
  "Return the width in pixels of a character in the current
window's default font.  More precisely, this returns the
width of the letter ‘m’.  If the font is mono-spaced, this
will also be the width of all other printable characters."
  (if-not window-system
    1                                   ; seems to not matter
    (let ((window (selected-window))
           (remapping face-remapping-alist))
      (with-temp-buffer
        (make-local-variable 'face-remapping-alist)
        (setq face-remapping-alist remapping)
        (set-window-buffer window (current-buffer))
        (insert "m")
        (aref (aref (font-get-glyphs (font-at 1) 1 2) 0) 4)))))

(defun ns/style-terminal ()
  (when-not window-system
    (ns/use evil-terminal-cursor-changer
      
      (defun etcc--in-xterm? ()
        "Runing in xterm."
        (or (string= (getenv "TERM") "xterm-kitty")
          (getenv "XTERM_VERSION")))
      (evil-terminal-cursor-changer-activate))

    (setq-default left-margin-width 1 right-margin-width 1)

    (defun! ns/windows-set-margins ()
      (-map (-rpartial 'set-window-margins left-margin-width right-margin-width)
        (window-list)))

    (ns/windows-set-margins)

    (setq flycheck-indication-mode 'left-margin)
    (set-face-attribute 'flycheck-error nil :underline nil)))

(defun! ns/load-theme (&optional theme)
  (ns/kill-buffers-no-file)

  (-map 'disable-theme custom-enabled-themes)

  (load-theme
    (or theme
      (->> (custom-available-themes)
        (-filter (fn (s-starts-with-p "myron-" (pr-str <>))))
        (ns/pick "theme")
        (intern)))
    t)

  (setq hl-todo-keyword-faces
    `(("TODO" . ,(myron-get :faded))
       ("todo" . ,(myron-get :faded))))

  (fringe-mode 8)
  (setq window-divider-default-right-width (default-font-width))
  (setq window-divider-default-places t)
  (window-divider-mode t) 

  (->>
    `(internal-border-width ,(if ns/enable-home-p 0 10)
       right-divider-width ,(default-font-width)
       bottom-divider-width 0
       font ,(get-resource "font.mono.spec"))
    (-partition 2)
    (-map (-applify #'ns/frame-set-parameter)))

  (-map (fn (when (fboundp <>)
              (message (pr-str <>))
              (funcall-interactively <>)))
    ;; '()
    '(ns/style-circe ns/style-org ns/style-markdown ns/style-terminal))

  (ns/conf-doomline)

  (let ((modeline-background (face-attribute 'mode-line-inactive :background)))
    (set-face-attribute 'vertical-border nil :foreground modeline-background)
    (set-face-attribute 'vertical-border nil :background modeline-background)
    (set-face-attribute 'window-divider nil :foreground modeline-background)
    (set-face-attribute 'window-divider nil :background modeline-background)
    (set-face-attribute 'window-divider-first-pixel nil :foreground modeline-background)
    (set-face-attribute 'window-divider-last-pixel nil :foreground modeline-background))

  ;; (when ns/enable-blog-p
  ;;   ;; this takes a bit
  ;;   (make-thread
  ;;     (fn (ns/blog-set-htmlize-colors))))
  t)
