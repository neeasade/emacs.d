;; -*- lexical-binding: t; -*-
;; nb: main entrypoint here is ns/load-theme

(setq-default indicate-empty-lines nil)

;; compat https://git.savannah.gnu.org/cgit/emacs.git/commit/lisp/color.el?id=c5e5940ba40b801270bbe02b92576eac36f73222
(when-not (functionp 'color-oklab-to-xyz)
  (defun color-oklab-to-xyz (l a b)
    "Convert the OkLab color represented by L A B to CIE XYZ.
Oklab is a perceptual color space created by Björn Ottosson
<https://bottosson.github.io/posts/oklab/>. It has the property that
changes in the hue and saturation of a color can be made while maintaining
the same perceived lightness."
    (let ((ll (expt (+ (* 1.0 l) (* 0.39633779 a) (* 0.21580376 b)) 3))
           (mm (expt (+ (* 1.00000001 l) (* -0.10556134 a) (* -0.06385417 b)) 3))
           (ss (expt (+ (* 1.00000005 l) (* -0.08948418 a) (* -1.29148554 b)) 3)))
      (list (+ (* ll 1.22701385) (* mm -0.55779998) (* ss 0.28125615))
        (+ (* ll -0.04058018) (* mm 1.11225687) (* ss -0.07167668))
        (+ (* ll -0.07638128) (* mm -0.42148198) (* ss 1.58616322)))))

  (defun color-xyz-to-oklab (x y z)
    "Convert the CIE XYZ color represented by X Y Z to Oklab."
    (let ((ll (+ (* x 0.8189330101) (* y 0.3618667424) (* z -0.1288597137)))
           (mm (+ (* x 0.0329845436) (* y 0.9293118715) (* z 0.0361456387)))
           (ss (+ (* x 0.0482003018) (* y 0.2643662691) (* z 0.6338517070))))
      (let*
        ((cube-root (lambda (f)
                      (if (< f 0)
	                      (- (expt (- f) (/ 1.0 3.0)))
                        (expt f (/ 1.0 3.0)))))
          (lll (funcall cube-root ll))
          (mmm (funcall cube-root mm))
          (sss (funcall cube-root ss)))
        (list (+ (* lll 0.2104542553) (* mmm 0.7936177850) (* sss -0.0040720468))
          (+ (* lll 1.9779984951) (* mmm -2.4285922050) (* sss 0.4505937099))
          (+ (* lll 0.0259040371) (* mmm 0.7827717662) (* sss -0.8086757660))))))

  (defun color-oklab-to-srgb (l a b)
    "Convert the Oklab color represented by L A B to sRGB."
    (apply #'color-xyz-to-srgb (color-oklab-to-xyz l a b)))

  (defun color-srgb-to-oklab (r g b)
    "Convert the sRGB color R G B to Oklab."
    (apply #'color-xyz-to-oklab (color-srgb-to-xyz r g b))))

(ns/use doom-modeline)
(ns/use (myron-themes :host github :repo "neeasade/myron-themes" :files ("*.el" "themes/*.el"))
  (setq base16-theme-256-color-source 'colors))

(when ns/enable-home-p (setq myron-use-cache nil))

(setq myron-use-cache t)

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
    (ns/parse-font (get-resource "font.variable.spec"))))

(defun ns/set-faces-monospace (faces)
  (apply 'ns/face faces
    (ns/parse-font (get-resource "font.mono.spec"))))

(defun ns/set-buffers-face-variable (buffers)
  (llet [font (ns/parse-font (get-resource "font.variable.spec"))]
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
  (when-not window-system
    ;; bold monospace headings in org look gross
    (->> (face-list)
      (--filter (s-starts-with-p "org" (ns/str it)))
      (--map (ns/face it :weight 'normal)))

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

  ;; todo: these should probably move into the theme
  (ns/face '(region evil-ex-search isearch lazy-highlight evil-ex-lazy-highlight) :weight 'unspecified)

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

  (ns/conf-doomline)

  (ns/face '(vertical-border window-divider window-divider-first-pixel window-divider-last-pixel)
    :background (face-attribute 'mode-line-inactive :background)
    :foreground (face-attribute 'mode-line-inactive :background))

  (-map (fn (when (fboundp <>)
              (message (pr-str <>))
              (funcall-interactively <>)))
    '(ns/style-circe ns/style-org ns/style-markdown ns/style-adoc ns/style-terminal))

  (f-mkdir-full-path (~ ".cache/rice/"))
  (spit (~ ".cache/rice/emacs-theme-cache")
    (ns/emacs-to-theme))

  (when (and (called-interactively-p 'any)
          ns/enable-home-p)
    (sh-toss "kitty ltheme wm qutebrowser")
    ;; (load-file (which "awp"))
    ;; (start-process "bgtint" nil "bgtint")
    )

  ;; (when ns/enable-blog-p
  ;;   ;; this takes a bit
  ;;   (make-thread
  ;;     (fn (ns/blog-set-htmlize-colors))))
  t)
