;; -*- lexical-binding: t; -*-
;; nb: main entrypoint here is ns/load-theme

(setq-default indicate-empty-lines nil)
(setq tab-bar-separator " ")


(ns/use nerd-icons)                     ; dep of doom-modeline?
(ns/use doom-modeline)

(ns/use (myron-themes :host github :repo "neeasade/myron-themes"
          :files ("*.el" "themes/*.el"))
  (setq base16-theme-256-color-source 'colors))

;; force terminal colors to work in daemon mode
(defun base16-theme-transform-face (spec colors)
  "Transform a face `SPEC' into an Emacs theme face definition using `COLORS'."
  (let* ((face             (car spec))
          (definition       (cdr spec)))
    (list face `((t ,(base16-theme-transform-spec definition colors))))))

;; term color comparison makes this hang somehow
(when ns/term?
  (setq myron-themes-use-cache t))

(defalias 'myron-get 'myron-themes-get) ; compat

(ns/use paren-face (global-paren-face-mode))

(ns/use hl-todo
  (general-nmap
    "]t" 'hl-todo-next
    "[t" 'hl-todo-previous)
  (global-hl-todo-mode))

(ns/use parseedn)
(defun ns/emacs-to-theme ()
  (parseedn-print-str
    (-ht :colors (apply 'vector (myron-themes-termcolors))
      :color (ht-merge myron-themes-colors
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

(defun! ns/style-terminal ()
  (ns/use evil-terminal-cursor-changer
    (defun etcc--in-xterm? ()
      (or (string= (getenv "TERM") "xterm-kitty")
        (getenv "XTERM_VERSION")))
    (evil-terminal-cursor-changer-activate))

  (setq-default
    left-margin-width 1
    right-margin-width 1)

  (-map (-rpartial 'set-window-margins left-margin-width right-margin-width)
    (window-list))

  (setq flycheck-indication-mode 'left-margin)

  ;; todo: maybe do this on ec connect as well
  (and ns/term? (send-string-to-terminal (format "\e]11;%s\a" (myron-get :background))))

  ;; (ns/face 'flycheck-error :underline nil)
  )

(defun ns/sync-terminal-frame-background (_)
  (and ns/term? (send-string-to-terminal (format "\e]11;%s\a" (myron-get :background)))))

(add-to-list 'after-make-frame-functions #'ns/sync-terminal-frame-background)

(defun! ns/load-random-myron-theme ()
  (llet [theme (->> (custom-available-themes)
                 (-filter (fn (s-starts-with-p "myron-" (pr-str <>))))
                 (-remove (fn (s-contains-p "-test-" (pr-str <>))))
                 (-shuffle)
                 (first))]
    (funcall-interactively 'ns/load-theme theme)
    ;; (ns/load-theme theme)
    (message (ns/str "loaded " theme "!"))))

(defun ns/set-evil-cursor (color)
  ;;
  (set-cursor-color color)

  (and ns/term?
    (fboundp 'etcc--evil-set-cursor-color)
    (etcc--evil-set-cursor-color color))

  (setq evil-normal-state-cursor `(,color box)
    evil-insert-state-cursor `(,color bar)
    evil-visual-state-cursor `(,color box)))

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

  (setq ns/term? (not window-system))

  (ns/set-evil-cursor (face-attribute 'cursor :background))

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
          font "Go Mono-14")
    (-partition 2)
    (-map (-applify #'ns/frame-set-parameter)))

  (ns/face 'org-block :background (myron-get :subtle :meta))

  (when (and (called-interactively-p 'any)
          (not (getenv "NS_EMACS_BATCH")))

    (ns/conf-doomline)

    (ns/face '(vertical-border window-divider window-divider-first-pixel window-divider-last-pixel)
      :background (face-attribute 'mode-line-inactive :background)
      :foreground (face-attribute 'mode-line-inactive :background))

    (-map (lambda (f)
            (interactive)
            (when (fboundp f)
              (message (pr-str f))
              (funcall-interactively f)))
      '(ns/style-circe ns/style-org ns/style-markdown ns/style-adoc))

    (and ns/term? (funcall-interactively 'ns/style-terminal))

    (progn
      ;; todo: check if this looks like ass in gui emacs
      (defface my-truncation-face
        `((t :foreground ,(myron-get :faded)
            :weight normal))
        "Face for line truncation indicator.")

      (set-display-table-slot standard-display-table 'truncation
        (make-glyph-code ?$ 'my-truncation-face)))

    (llet [cache-dir (~ ".cache/rice/")]
      (f-mkdir-full-path cache-dir)
      (spit (ns/str cache-dir "emacs-theme-name")
        (ns/str (first custom-enabled-themes)))
      (spit (ns/str cache-dir "emacs-theme-cache")
        (ns/emacs-to-theme)))


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

(ns/use (stillness-mode :host github :repo "neeasade/stillness-mode.el")
  (stillness-mode))

(ns/use rainbow-mode
  (setq rainbow-html-colors nil
    rainbow-x-colors nil)
  (ns/bind "tc" 'rainbow-mode))
