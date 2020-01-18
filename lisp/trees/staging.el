(use-package indent-guide
  :config

  (set-face-foreground 'indent-guide-face
    (face-attribute 'font-lock-keyword-face :foreground))

  ;; (set-face-foreground 'indent-guide-face (ns/color-tone (face-attribute 'default :background) 15 15))

  (setq indent-guide-char "|")
  (indent-guide-global-mode 0)
  )

(use-package link-hint)

;; file:/home/nathan/.vimrc
;; /home/nathan/.vimrc
;; todo: use noctuid's link package here to take advantage of different kinds of links.
(defun! ns/follow()
  (or
    ;; first try to open with org handling (includes urls)
    (not (eq 'fail (condition-case nil (org-open-at-point) (error 'fail))))

    ;; then, see if it's a file by ffap, and handle line numbers as :<#> by converting it into an org file link.
    (let
      ((file-name (nth 0 (s-split ":" (ffap-string-at-point))))
        (file-line (nth 1 (s-split ":" (ffap-string-at-point))))
        (file-char (nth 2 (s-split ":" (ffap-string-at-point)))))
      (when (f-exists-p file-name)
        (org-open-link-from-string
          (format "file:%s%s" file-name
            (if file-line
              ;; this is done to coerce non-numbers (EG grep results with file name appended) to 0
              (format "::%s" (string-to-number file-line))
              ""
              )))

        (when file-char
          (move-beginning-of-line nil)
          (move-to-column file-char))))

    ;; fall back to definitions with smart jump
    (shut-up (smart-jump-go))
    ))

(ns/bind "nn" 'ns/follow)

(defmacro ns/make-char-table (name upper lower)
  "Make a char table for a certain kind of character"
  `(defvar ,name
     (let ((str (make-string 127 0)))
       (dotimes (i 127)
         (aset str i i))
       (dotimes (i 26)
         (aset str (+ i ?A) (+ i ,upper))
         (aset str (+ i ?a) (+ i ,lower)))
       str)))

(ns/make-char-table ns/monospace-table ?𝙰 ?𝚊)
(ns/make-char-table ns/widechar-table ?Ａ ?ａ)
(ns/make-char-table ns/gothic-table ?𝔄 ?𝔞)
(ns/make-char-table ns/cursive-table ?𝓐 ?𝓪)


;; todo: make all these circe functions
(defun ns/text-to-cursive (beg end) (interactive "r")
  (translate-region beg end ns/cursive-table))

(defun ns/text-to-monospace (beg end) (interactive "r")
  (translate-region beg end ns/monospace-table))

(defun ns/text-to-gothic (beg end) (interactive "r")
  (translate-region beg end ns/gothic-table))

(defun ns/text-to-widechar (beg end) (interactive "r")
  (translate-region beg end ns/widechar-table))

;; todo: idea: turn all the above into circe commands
;; or matrix.el commands
;; also: add a clap👏text function (lol)

;; (use-package string-inflection
;;   (defun! ns/string-inflection-auto
;;     "switching by major-mode"
;;     (cond
;;       ((eq major-mode 'emacs-lisp-mode)
;;         (string-inflection-all-cycle))
;;       ((eq major-mode 'python-mode)
;;         (string-inflection-python-style-cycle))
;;       ((eq major-mode 'java-mode)
;;         (string-inflection-java-style-cycle))
;;       (t (string-inflection-ruby-style-cycle)))))

;; hack: todo: not create an entry on getting content

(defun ns/urlnote-get-content (&optional url)
  (save-window-excursion
    (ns/urlnote-jump url)
    (s-clean (org-get-entry))))

(defun ns/urlnote-get-or-add (url)
  "return a marker on the note entry"
  (defun ns/find-url-heading (path)
    (catch 'error
      (condition-case msg
        (org-find-olp `(,(concat org-directory "/projects/url.org")
                         "URL notes"
                         ,@path))
        (error
          ;; (nth 1 msg)
          nil
          ))))

  ;; note: only handles one level currently
  (defun ns/add-url-heading (path)
    (let ((parent (-remove-last (fn t) path))
           (child (-last (fn t) path)))
      ;; todo: this with a full file path
      (with-current-buffer "url.org"
        (when
          (and (not (ns/find-url-heading path))
            (ns/find-url-heading parent))
          (progn
            (goto-char (ns/find-url-heading parent))
            (org-insert-heading-after-current)
            (insert child)
            (org-do-demote))))))

  (let ((parent (-> url url-generic-parse-url url-host))
         (child url))

    (when (not (ns/find-url-heading (list parent)))
      (ns/add-url-heading (list parent)))

    (when (not (ns/find-url-heading (list parent child)))
      (ns/add-url-heading (list parent child)))

    (ns/find-url-heading (list parent child))))

(defun! ns/urlnote-jump (&optional url)
  (let ((target (or url
                  (->> (simpleclip-get-contents) s-trim s-clean))))
    (when (ffap-url-p target)

      (let ((marker (ns/urlnote-get-or-add target)))
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker)))

      (org-show-context)
      (org-show-subtree)
      (ns/focus-line))))

(use-package eval-in-repl
  :config
  (require 'eval-in-repl)
  (require 'eval-in-repl-python)

  (setq eir-jump-after-eval nil)
  (setq eir-always-split-script-window nil)
  (setq eir-delete-other-windows nil)
  (setq eir-repl-placement 'left)

  ;; run this first to start the repl
  ;; (eir-run-python)
  (ns/bind-mode 'python "e" 'eir-eval-in-python)
  )

;; optionally transform #<12> to #<6>
(defun ns/shorten-color (color)
  (if (= (length color) 7)
    color
    (-as-> color C
      (color-name-to-rgb C)
      `(color-rgb-to-hex ,@C 2)
      (eval C))))

(ns/shorten-color "#cccccc")

(ns/bind
  "nt" (fn! (find-file (~ ".wm_theme")))
  "id" (fn! (org-time-stamp nil))
  )

;; https://github.com/szermatt/emacs-bash-completion
;; comprehensive bash completion in emacs
;; testing out [Fri Dec 20 15:13:58 2019]
(use-package bash-completion)
(bash-completion-setup)

;; colors!
;; ideas:
;; - move color towards bg/fg/<n color>
;; - make color a minimum distance from a set color
;; - make percent functions a global intensity setting
;; - a better visual method than xah's inline face bg

(use-package rainbow-mode
  :config
  (ns/bind "tc" 'rainbow-mode))

(use-package kurecolor
  :config
  (setq-ns kurecolor-color-adjust
    ;; these are in percent
    brightness-step 2
    saturation-step 2
    hue-step 2)

  ;; maybe these should be on the rainbow mode map
  (ns/bind
    "cl" 'kurecolor-increase-brightness-by-step
    "cL" 'kurecolor-decrease-brightness-by-step
    "ch" 'kurecolor-increase-hue-by-step
    "cH" 'kurecolor-decrease-hue-by-step
    "cs" 'kurecolor-increase-saturation-by-step
    "cS" 'kurecolor-decrease-saturation-by-step
    ))
