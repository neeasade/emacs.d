;; -*- lexical-binding: t; -*-
;; nb: main entrypoint here is ns/load-theme

(setq-default indicate-empty-lines nil)

(when (not (-contains-p features 'tarps))
  (ns/use-package ct "neeasade/ct.el")
  (ns/use-package tarps "neeasade/tarps" :config (require 'tarps))

  ;; overriding to force truecolor terminal
  (defun base16-transform-face (spec colors)
    "Transform a face `SPEC' into an Emacs theme face definition using `COLORS'."
    (let* ((face             (car spec))
            (definition       (cdr spec))
            (shell-colors-256 (pcase base16-theme-256-color-source
                                ('terminal      base16-shell-colors)
                                ("terminal"     base16-shell-colors)
                                ('base16-shell  base16-shell-colors-256)
                                ("base16-shell" base16-shell-colors-256)
                                ('colors        colors)
                                ("colors"       colors)
                                (_              base16-shell-colors))))

      ;; This is a list of fallbacks to make us select the sanest option possible.
      ;; If there's a graphical terminal, we use the actual colors. If it's not
      ;; graphical, the terminal supports 256 colors, and the user enables it, we
      ;; use the base16-shell colors. Otherwise, we fall back to the basic
      ;; xresources colors.
      (list face `((((type graphic))   ,(base16-transform-spec definition colors))
                    (((min-colors 256)) ,(base16-transform-spec definition shell-colors-256))
                    (t                  ,(base16-transform-spec definition base16-shell-colors)))))))

(defun ns/maybe-update-xrdb-font (key font)
  "Update the fallback font for xrdb value"
  (when (find-font (font-spec :name font))
    (setq ns/xrdb-fallback-values
      (delq (assoc key ns/xrdb-fallback-values) ns/xrdb-fallback-values))

    (setq ns/xrdb-fallback-values
      (cons `(,key . ,font) ns/xrdb-fallback-values))))

(-map (-partial 'ns/maybe-update-xrdb-font "font.monospace")
  (list
    ;; (or (font-get (face-attribute 'default :font) :name) "")
    "Menlo-14"
    "Dejavu Sans Mono-14"
    "DejaVu Sans Mono-14"
    "Lucida Console-14"
    "Noto Sans Mono-14"
    "Source Code Pro-14"
    "Go Mono-14"))

(-map (-partial 'ns/maybe-update-xrdb-font "font.variable")
  (list
    ;; (or (font-get (face-attribute 'default :font) :name) "")
    "Menlo-14"
    "Dejavu Sans-14"
    "DejaVu Sans-14"
    "Lucida Console-14"
    "Noto Serif-14"
    "Charter-14"))

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

;; export the theme as shell env variables:
(use-package theme-magic
  :config
  (defun ns/emacs-to-theme ()
    (s-join "\n"
      (append
        (list
          (format "colors = ['%s']"
            (s-join "', '"
              (theme-magic--auto-extract-16-colors))))

        (-map
          (fn
            (format "%s = \"%s\""
              (car <>)
              (ct-shorten (cadr <>))))
          (-partition 2
            (append
              (list
                ;; todo: support same node table + value in toml
                ;; "color.foreground" (face-attribute 'default :foreground)
                ;; "color.background" (face-attribute 'default :background)
                "color.cursor" (first evil-insert-state-cursor))
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
                        (format "color.%s.%s"
                          (-> bg-key pr-string (substring 1))
                          (-> fg-key pr-string (substring 1)))
                        (tarp/get fg-key bg-key)))))))))))))

(defun default-font-width ()
  "Return the width in pixels of a character in the current
window's default font.  More precisely, this returns the
width of the letter ‘m’.  If the font is mono-spaced, this
will also be the width of all other printable characters."
  (let ((window (selected-window))
         (remapping face-remapping-alist))
    (with-temp-buffer
      (make-local-variable 'face-remapping-alist)
      (setq face-remapping-alist remapping)
      (set-window-buffer window (current-buffer))
      (insert "m")
      (aref (aref (font-get-glyphs (font-at 1) 1 2) 0) 4))))

(defun ns/style-terminal ()
  (when-not window-system
    (use-package evil-terminal-cursor-changer
      :config
      (evil-terminal-cursor-changer-activate))

    (setq-default left-margin-width 1 right-margin-width 1)

    (defun! ns/windows-set-margins ()
      (-map (-rpartial 'set-window-margins left-margin-width right-margin-width)
        (window-list)))

    (ns/windows-set-margins)

    (setq flycheck-indication-mode 'left-margin)
    (set-face-attribute 'flycheck-error nil :underline nil)))

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

  (fringe-mode 8)
  (setq window-divider-default-right-width (default-font-width))
  (setq window-divider-default-places t)
  (window-divider-mode t)               ;

  (->>
    `(internal-border-width ,(if ns/enable-home-p 0 10)
       right-divider-width ,(default-font-width)
       bottom-divider-width 0
       font ,(get-resource "font.monospace"))
    (-partition 2)
    (-map (-applify #'ns/frame-set-parameter)))

  (ns/doomline)

  (let ((modeline-background (face-attribute 'mode-line-inactive :background)))
    (set-face-attribute 'vertical-border nil :foreground modeline-background)
    (set-face-attribute 'vertical-border nil :background modeline-background)
    (set-face-attribute 'window-divider nil :foreground modeline-background)
    (set-face-attribute 'window-divider nil :background modeline-background)
    (set-face-attribute 'window-divider-first-pixel nil :foreground modeline-background)
    (set-face-attribute 'window-divider-last-pixel nil :foreground modeline-background))

  (-map (fn (when (fboundp <>)
              (funcall <>)))
    '(ns/style-circe
       ns/style-org
       ns/style-markdown
       ns/style-terminal))

  (when ns/enable-blog-p
    (ns/blog-set-htmlize-colors)))
