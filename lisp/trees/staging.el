;; -*- lexical-binding: t; -*-

(use-package indent-guide
  :config
  (set-face-foreground 'indent-guide-face
    ;; (face-attribute 'font-lock-keyword-face :foreground)
    (face-attribute 'font-lock-comment-face :foreground))

  ;; (set-face-foreground 'indent-guide-face (ns/color-tone (face-attribute 'default :background) 15 15))

  (setq indent-guide-char "|")
  (indent-guide-global-mode 0)
  )

(use-package link-hint)

;; /home/neeasade/My_Games/Skyrim/RendererInfo.txt:10
;; file:/home/neeasade/My Games/Skyrim/RendererInfo.txt:10
;; - link-hint-open-link-at-point
;; file:/home/neeasade/.vimrc
;; /home/neeasade/.vimrc

;; $HOME/.vimrc - todo: link-hint-open-link-at-point handles this

;; todo: use noctuid's link package here to take advantage of different kinds of links.
;; todo: if it's a dir and we are in shell-mode, cd to the dir instead in the current shell

;; if would be cool if this were tramp aware
;; update: just check (file-remote-p default-directory)
;; maybe ffap-string-at-point should be let'd -- used almost everywhere

;; idea: rg could search $PATH and also git repo -- idea is jump to script when reading one
;; maybe ns/follow could have a soft/query mode where you just message the filepath/type of thing you are looking at
;; idea: if you have a region selected, the link logic stuff should act on that
;; todo: if you are in org mode, looking at a link

(defun ns/follow-log (msg)
  (message msg))

(defun! ns/follow()
  "This is my home rolled DWIM at point function -- maybe it could be considered to be 'bad hyperbole'
   Tries to integrate a few meta solutions
   org link --> our own peek where we build an org file link --> jump to definition with smart-jump"

  ;; should be region if something is selected
  ;; (let ((candidate (ffap-string-at-point))))

  (or
    ;; (when (string-empty-p )
    ;;   (message "not looking at anything! (ffap-string-at-point)")
    ;;   t
    ;;   )

    ;; (when (and (eq major-mode 'shell-mode)
    ;;         (f-directory-p (ffap-string-at-point))
    ;;         )
    ;;   (goto-char (point-max))
    ;;   (insert (concat "cd \"" (ffap-string-at-point) "\""))
    ;;   (comint-send-input)
    ;;   (ns/follow-log "ns/follow: resolved with shell-mode cd")
    ;;   t
    ;;   )

    ;; nahh: maybe dired @ directory in current window rather than comint-send-input

    ;; first try to open with org handling (includes urls)
    ;; todo: see how org-open-at-point handles spacing
    (not (eq 'fail (condition-case nil (org-open-at-point) (error 'fail))))

    ;; this handles org links as well

    ;; then, see if it's a file by ffap, and handle line numbers as :<#> by converting it into an org file link.
    (let* ((file-name
             ;; this split breaks because it will be ssh in the below/tramp'd:
             ;; /ssh:neeasade@<ip>:/home/neeasade/
             (nth 0 (s-split ":" (f-full (ffap-string-at-point)))))
            (file-name-optimistic
              (if (f-exists-p file-name)
                file-name
                ;; peek ahead of the point a space or two and see if that's a valid path
                ;; this is so that paths with a single space in them can be used
                ;; hmmmmmmm
                )
              )

            (file-line (nth 1 (s-split ":" (ffap-string-at-point))))
            (file-char (nth 2 (s-split ":" (ffap-string-at-point))))

            (full-file-name
              (format "file:%s%s" file-name
                (if file-line
                  ;; the string-to-number is done to coerce non-numbers (EG grep results with file name appended) to 0
                  (format "::%s" (string-to-number file-line)) "")
                )
              )
            )

      (if (f-exists-p file-name)
        (progn
          (org-open-link-from-string full-file-name)

          (when file-char
            (move-beginning-of-line nil)
            (move-to-column file-char))

          (ns/follow-log (format "ns/follow: resolved with org link after building: %s" full-file-name))
          t)

        (let* ((rg-initial-result (ns/shell-exec (format "rg --files -g '%s'" file-name)))
                (rg-result (if (s-contains-p "\n" rg-initial-result)
                             (ivy-read "pickem: " (s-split "\n" rg-initial-result))
                             rg-initial-result)))
          (when (and (not (s-blank-p rg-result))
                  (f-exists-p (or rg-result "nil doesn't exist don't  use me")))
            (org-open-link-from-string
              (format "file:%s%s" rg-result
                (if file-line
                  ;; the string-to-number is done to coerce non-numbers (EG grep results with file name appended) to 0
                  (format "::%s" (string-to-number file-line)) "")))

            (ns/follow-log "ns/follow: resolved with ripgrep")
            t
            ))))

    ;; fall back to definitions with smart jump
    (progn
      (ns/follow-log "ns/follow: resolving with smart-jump-go")
      (shut-up (smart-jump-go)))))


;; todo: handle the bash/shell line number format:
;; /home/neeasade/.wm_theme: line 155:
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

;; todo: this is broken/getting is impure -- maybe try and use org capture for the same purpose with a pure getter w/ orgql
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
  "id" (fn! (org-time-stamp t))
  "iD" (fn! (org-time-stamp nil))
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

;; M-x direnv-update-environment
;; sync from the pov of the current file
;; using in combination with lorri
(use-package direnv)

(defun ns/color-format (color)
  (format "#%s" (substring color -6 nil)))

(defun ns/color-greaten (percent color)
  (ns/shorten-color
    (if (ns/color-is-light-p color)
      (color-lighten-name color percent)
      (color-darken-name color percent))))

(defun ns/color-lessen (percent color)
  (ns/shorten-color
    (if (ns/color-is-light-p color)
      (color-darken-name color percent)
      (color-lighten-name color percent))))

;; run garbage collection every ~5 minutes when we've been away for longer than 5 minutes.
;; this means you won't have to garbage collect for literal minutes when we leave emacs running
;; all night long

(named-timer-run :maybe-garbage-collect
  ;; run the first time in 30 seconds
  ;; relative times are.. strings? cf https://www.gnu.org/software/emacs/manual/html_node/elisp/Timers.html
  "30 sec"
  (* 5 60)
  (fn (when (> ;; PUNS
              (second (current-idle-time))
              (* 5 60))
        (garbage-collect)
        ;; save everyone:
        )))

(ns/bind
  "nd"
  (fn!
    (ivy-read "directory: "
      (->> ns/cd-dirs
        (-uniq)
        (-filter (fn (s-equals-p (file-remote-p <>)
                       (file-remote-p default-directory)))))

      :action
      (lambda (dir)
        (cond
          ((eq major-mode 'dired-mode) (dired dir))
          ((eq major-mode 'shell-mode)
            (goto-char (point-max))
            (insert (concat "cd \""
                      (s-replace
                        (or (file-remote-p dir) "")
                        ""
                        dir
                        )
                      "\""))
            (comint-send-input))
          (t (insert dir))))

      )))

(defun ns/emacs-to-theme ()
  (use-package theme-magic)

  (s-join "\n"
    (append (seq-map-indexed
              (fn (format "color%s=%s" (number-to-string <2>)
                    (s-replace "#" "" <1>)))
              (theme-magic--auto-extract-16-colors))

      ;; (face-attribute 'mode-line :background)
      ;; (face-attribute 'mode-line-inactive :background)

      (list
        (format "foreground=%s" (s-replace "#" "" (face-attribute 'default :foreground)))
        (format "background=%s" (s-replace "#" "" (face-attribute 'default :background)))
        (format "cursorColor=%s" (s-replace "#" "" (first evil-insert-state-cursor)))))))


(use-package git-link
  :config
  (setq git-link-open-in-browser t))

;; todo: keybind to bring up spelling menu and correct, also allow underlines at point

;; spelling
(use-package flyspell-correct-avy-menu
  :config
  (require 'flyspell-correct-avy-menu)
  (setq flyspell-correct-interface #'flyspell-correct-avy-menu))

(define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-wrapper)

(named-timer-run :show-periodic-reminder
  t
  ;; every 3 hours
  (* 60 60 3)
  (fn
    ;; todo: NOT IMPLEMENTED
    ;; query notes file and notify-send it
    ))
