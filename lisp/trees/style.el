;; -*- lexical-binding: t; -*-
;; nb: main entrypoint here is ns/load-theme

(setq-default indicate-empty-lines nil)

(ns/use doom-modeline)
(ns/use (myron-themes :host github :repo "neeasade/myron-themes"
          :files ("*.el" "themes/*.el"))
  (setq base16-theme-256-color-source 'colors)
  (setq myron-use-cache (not ns/enable-home-p)))

(ns/use paren-face (global-paren-face-mode))

(ns/use hl-todo
  (general-nmap
    "]t" 'hl-todo-next
    "[t" 'hl-todo-previous)
  (global-hl-todo-mode))

(defun ns/emacs-to-theme ()
  (parseedn-print-str
    (-ht :colors (apply 'vector (myron-termcolors))
      :color (ht-merge myron-theme*
               (-ht :cursor (first evil-insert-state-cursor))))))

;; update buffer local variables across all open buffers
;; notmodes are modes to ignore
(defun ns/setq-local-all (symbol value &optional notmodes)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if notmodes
        (when (not (-contains-p notmodes major-mode))
          ;; don't remove these evals
          (eval `(setq-local ,symbol ,value)))
        (eval `(setq-local ,symbol ,value)))))
  (eval `(setq-default ,symbol ,value)))

;; callback on all open frames
(defun! ns/apply-frames (action)
  (mapc (lambda (frame)
          (funcall action frame)
          (redraw-frame frame))
    (frame-list)))

(defun! ns/frame-set-parameter (key value)
  "set a value on all current and future frames"
  ;; current:
  (ns/apply-frames (fn (set-frame-parameter <> key value)))

  ;; future:
  (setq default-frame-alist
    (assq-delete-all key default-frame-alist))

  (add-to-list 'default-frame-alist `(,key . ,value)))

(defun! ns/font-change ()
  (llet [current-size (/ (face-attribute 'default :height) 10.0)
          new-size (read-number (format "new size (current-size: %s): " current-size))]
    (ns/face 'default :height (* 10 new-size))))

(defun ns/parse-font (font)
  "translate 'Font Family-10' into emacs font information"
  (llet [font (s-replace "-" " " font)
          size (first (s-match (pcre-to-elisp " [0-9]+") font))
          family (s-replace size "" font)]
    `(:family ,family :height ,(* 10 (string-to-number size)))))

(defun ns/set-faces-variable (faces)
  (apply 'ns/face faces
    (ns/parse-font (get-resource "font.variable-big.spec"))))

(defun ns/set-faces-monospace (faces)
  (apply 'ns/face faces
    (ns/parse-font (get-resource "font.mono.spec"))))

(defun ns/set-buffers-face-variable (buffers)
  (llet [font (ns/parse-font (get-resource "font.variable-big.spec"))]
    (--map (with-current-buffer it
             (setq-local buffer-face-mode-face font)
             (buffer-face-mode t))
      buffers)))

(defun! ns/set-buffer-face-variable (&optional buffer)
  (ns/set-buffers-face-variable (list (or buffer (current-buffer)))))

(defun! ns/set-buffer-face-monospace (&optional buffer)
  (ns/set-buffers-face-monospace (list (or buffer (current-buffer)))))

(defun ns/set-buffers-face-monospace (buffers)
  (llet [font (ns/parse-font (get-resource "font.mono.spec"))]
    (--map (with-current-buffer it
             (setq-local buffer-face-mode-face font)
             (buffer-face-mode t))
      buffers)))

(defun ns/style-terminal ()
  (when ns/term?
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
    (ns/face 'flycheck-error :underline nil)))

(defun! ns/load-random-myron-theme ()
  (llet [theme (->> (custom-available-themes)
                 (-filter (fn (s-starts-with-p "myron-" (pr-str <>))))
                 (-remove (fn (s-contains-p "-test-" (pr-str <>))))
                 (-shuffle)
                 (first))]
    (funcall-interactively 'ns/load-theme theme)
    ;; (ns/load-theme theme)
    (message (ns/str "loaded " theme "!"))))

(defun! ns/load-theme (&optional theme)
  (ns/kill-buffers-no-file)

  (-map 'disable-theme custom-enabled-themes)
  (ns/refresh-resources)

  (load-theme
    (or theme
      (->> (custom-available-themes)
        (-filter (fn (s-starts-with-p "myron-" (pr-str <>))))
        (ns/pick "theme")
        (intern)))
    t)

  ;; turn off bold in most places
  (->> (face-list)
    (--remove (s-starts-with-p "org" (ns/str it)))
    (--remove (s-starts-with-p "magit" (ns/str it)))
    (--map (ns/face it
             ;; :weight 'normal
             :weight (llet [already-unspecified? (or (eq (face-attribute it :weight) 'unspecified)
                                                   (not (face-attribute it :weight)))]
                       (if already-unspecified? 'unspecified 'normal))
             ;; :slant 'normal
             ;; :underline nil
             )))

  (ns/face 'italic :slant 'italic)
  (ns/face 'bold :weight 'bold)
  (ns/face 'region :slant 'unspecified)

  ;; (ns/face 'font-lock-comment-face :slant 'normal)

  ;; (ns/face 'font-lock-constant-face :weight 'bold)

  ;; (ns/face 'font-lock-variable-name-face :weight 'bold)

  ;; todo: these should probably move into the theme
  (ns/face '(region evil-ex-search isearch lazy-highlight evil-ex-lazy-highlight) :weight 'unspecified)

  (setq hl-todo-keyword-faces
    `(("TODO" . ,(myron-get :faded))
       ("todo" . ,(myron-get :faded))))

  (fringe-mode 8)
  (setq window-divider-default-right-width (default-font-width))
  (setq window-divider-default-places t)
  (window-divider-mode t) 

  (->> `(internal-border-width ,(if ns/enable-home-p 0 10)
          right-divider-width ,(default-font-width)
          bottom-divider-width 0
          font ,(get-resource "font.mono.spec"))
    (-partition 2)
    (-map (-applify #'ns/frame-set-parameter)))

  (ns/conf-doomline)

  (ns/face '(vertical-border window-divider window-divider-first-pixel window-divider-last-pixel)
    :background (face-attribute 'mode-line-inactive :background)
    :foreground (face-attribute 'mode-line-inactive :background))

  (-map (lambda (f)
          (interactive)
          (when (fboundp f)
            (message (pr-str f))
            (funcall-interactively f)))
    '(ns/style-circe ns/style-org ns/style-markdown ns/style-adoc ns/style-terminal))

  ;; testing, lighter emphasis on codeblocks
  (ns/face 'org-block :background (ct-lessen (myron-get :background) 3))

  (f-mkdir-full-path (~ ".cache/rice/"))
  (spit (~ ".cache/rice/emacs-theme-name")
    (ns/str (first custom-enabled-themes)))
  (spit (~ ".cache/rice/emacs-theme-cache")
    (ns/emacs-to-theme))

  (when (and (called-interactively-p 'any)
          ns/enable-home-p)
    (sh-toss "kitty ltheme wm qutebrowser rofi kitty")
    ;; (sh-toss "awp disease")
    ;; (sh-toss "/home/neeasade/walls/3074ac6e6ba4ccc596b5fa4d3ae36e1998535d47d1a62df8d2d9bed0ca418807.awp")
    ;; (sh-toss "/home/neeasade/walls/4c3f11a0f90b4388b8a49f49b9ffbad88f36547b5d9ce1310aae72934604521e.awp")


    ;; (sh-toss "awp disease")
    ;; (sh-toss "")
    ;; (load-file (which "awp"))
    ;; (start-process "bgtint" nil "bgtint")
    )

  ;; (when ns/enable-blog-p
  ;;   ;; this takes a bit
  ;;   (make-thread
  ;;     (fn (ns/blog-set-htmlize-colors))))
  t)

(ns/use rainbow-mode
  (setq rainbow-html-colors nil
    rainbow-x-colors nil)
  (ns/bind "tc" 'rainbow-mode))
