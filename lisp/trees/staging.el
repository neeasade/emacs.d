;; -*- lexical-binding: t; -*-

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

(defun ns/text-to-cursive (beg end) (interactive "r")
  (translate-region beg end ns/cursive-table))

(defun ns/text-to-monospace (beg end) (interactive "r")
  (translate-region beg end ns/monospace-table))

(defun ns/text-to-gothic (beg end) (interactive "r")
  (translate-region beg end ns/gothic-table))

(defun ns/text-to-widechar (beg end) (interactive "r")
  (translate-region beg end ns/widechar-table))

;; todo: idea: turn all the above into circe commands
;; also: add a clap👏text function (lol)

(defun ns/make-urlnote-funcs ()
  (defun ns/urlnote-get-point (url)
    (let ((url-plain
            (when url
              (if (s-contains-p "?" url)
                (first (s-split "?" url)) url))))

      (catch 'error
        (condition-case msg
          (marker-position
            (org-find-olp
              (if url-plain
                (list org-default-notes-file "url notes" url-plain)
                (list org-default-notes-file "url notes"))))
          (error
            ;; show what went wrong:
            ;; (nth 1 msg)
            nil)))))

  (defun ns/urlnote-get-content (url)
    (let ((url-point (ns/urlnote-get-point url)))
      (when url-point
        (with-current-buffer
          (get-file-buffer org-default-notes-file)
          (->> url-point org-ml-parse-subtree-at)))))

  (defun ns/urlnote-jump (url)
    (let ((url-point (ns/urlnote-get-point url)))
      (when url-point
        (find-file org-default-notes-file)
        (goto-char (ns/urlnote-get-point url)))))

  (defun ns/urlnote-make-and-jump (url)
    (find-file org-default-notes-file)
    (goto-char (ns/urlnote-get-point nil))
    (next-line)
    ;; (org-insert-subheading nil)
    ;; (org-insert-heading-after-current)
    (if (s-contains-p "?" url)
      (first (s-split "?" url)) url)
    (insert url)
    (org-do-demote)
    (newline))
  )

(ns/make-urlnote-funcs)

(ns/bind
  "nt" (fn! (find-file (~ ".wm_theme")))
  "id" (fn! (org-time-stamp t))
  "iD" (fn! (org-time-stamp nil))
  )

;; https://github.com/szermatt/emacs-bash-completion
;; comprehensive bash completion in emacs
;; testing out [Fri Dec 20 15:13:58 2019]
;; todo: this is broken, just freezes the shell
;; (use-package bash-completion)
;; (bash-completion-setup)

;; colors!
;; ideas:
;; - move color towards bg/fg/<n color>
;; - make color a minimum distance from a set color
;; - make percent functions a global intensity setting
(setq rainbow-html-colors nil)
(setq rainbow-x-colors nil)

(use-package rainbow-mode
  :config (ns/bind "tc" 'rainbow-mode))

;; M-x direnv-update-environment
;; sync from the pov of the current file
(use-package direnv)

(defun ns/org-media-playing ()
  (defun ns/sh-has-content-p (cmd)
    (-> cmd ns/shell-exec s-blank-p not))

  (or
    (ns/sh-has-content-p "playerctl metadata 2>/dev/null | grep -i netflix")
    (ns/sh-has-content-p "pgrep mpv")
    (ns/sh-has-content-p "pgrep vlc")

    ;; adhoc hack (uncomment this when viewing something in an unaccounted for medium)
    ;; t
    ))

(named-timer-run :maybe-garbage-collect
  ;; run garbage collection every ~5 minutes when we've been away for longer than 5 minutes.
  ;; this means you won't have to garbage collect for literal minutes when we leave emacs running
  ;; all night long

  ;; run the first time in 30 seconds
  ;; relative times are.. strings? cf https://www.gnu.org/software/emacs/manual/html_node/elisp/Timers.html
  "30 sec"
  (* 5 60)
  (fn (when (> (org-user-idle-seconds)
              (* 5 60))
        (garbage-collect)

        (when (not (ns/org-media-playing))
          (ns/org-clock-out))

        ;; auto revert any who have changed on disk
        (auto-revert-buffers)

        ;; save everyone
        (save-some-buffers t))))

(named-timer-run :auto-clock-out
  "30 sec"
  30
  (fn (when (> (org-user-idle-seconds)
              (* 3 60))

        (when (not (ns/org-media-playing))
          (ns/org-clock-out)))))

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
          ;; (t (insert dir))
          (t (dired dir))
          )))))

(use-package theme-magic)
(defun ns/emacs-to-theme ()
  (s-join "\n"
    (append (seq-map-indexed
              (fn (format "color%s=%s" (number-to-string <2>)
                    (s-replace "#" "" <1>)))
              (theme-magic--auto-extract-16-colors))

      (-map
        (fn (format "%s=%s" (car <>)
              (s-replace "#" "" (ns/color-shorten (cadr <>)))))
        (-partition 2
          (list
            "foreground" (face-attribute 'default :foreground)
            "background" (face-attribute 'default :background)
            "cursorColor" (first evil-insert-state-cursor)))))))

(use-package git-link
  :config
  (setq git-link-open-in-browser t))

;; this seems to be a little nicer:
;; (use-package browse-at-remote)


;; (named-timer-run :show-periodic-reminder
;;   t
;;   (* 60 60 2)
;;   (fn
;;     (when (< (second (current-idle-time)) 120)
;;       (alert (let ((reminders
;;                      (org-ql-select org-default-notes-file
;;                        '(tags "reminders")
;;                        :action '(s-clean (org-get-heading t t)))
;;                      ))
;;                (nth (random (length reminders)) reminders))
;;         :severity 'normal
;;         :title "*Reminder*"
;;         ))))

;; automatic detection of indent settings (vim-sleuth)
;; todo: doom does a thing where they blend the major mode w/ editor config
;;       so for example sh-mode files if a *.sh rule is present, editorconfig takes precedence over this
(use-package dtrt-indent :config (dtrt-indent-global-mode 1))


;; whether or not to rely on notifications from the fs that files have changed
;; when set to nil, checks every 5 seconds
(setq auto-revert-use-notify nil)

;; has a nice url regexp
(require 'rcirc)

;; jump to url in current window text:
(defun! ns/ivy-url-jump ()
  (let* ((window-text (s-clean (buffer-substring (window-start) (window-end))))
          (urls (s-match-strings-all rcirc-url-regexp window-text)))
    (if urls
      (ivy-read "url: "
        (->> urls (-map 'car))
        :action 'browse-url)
      (message "no urls!"))))

(ns/bind "nu" 'ns/ivy-url-jump)

(defun ns/org-scheduled-today (heading)
  "Get headings scheduled from <now - 2hrs> ==> end of day"
  (let ((scheduled (plist-get (cadr (org-ml-headline-get-planning heading)) :scheduled)))
    (when scheduled
      (let (scheduled-value)
        (ts-in
          (ts-adjust 'hour -2 (ts-now))
          (ts-apply :hour 23 :minute 59 :second 59 (ts-now))
          (ts-parse-org (plist-get (cadr scheduled) :raw-value)))))))

(defun ns/org-scheduled-past-todo (heading)
  "Get TODO items that are scheduled in the past. incidentally, this will also get out of date habits."
  (llet [scheduled (plist-get (cadr (org-ml-headline-get-planning heading)) :scheduled)
          todo-state (org-ml--get-property-nocheck :todo-keyword heading)]
    (when (string= todo-state "TODO")
      (when scheduled
        (ts> (ts-now)
          (ts-parse-org (plist-get (cadr scheduled) :raw-value)))))))

(defun ns/org-scheduled-future (heading)
  (let ((scheduled (plist-get (cadr (org-ml-headline-get-planning heading)) :scheduled)))
    (when scheduled
      (ts< (ts-now)
        (ts-parse-org (plist-get (cadr scheduled) :raw-value))))))

;; track headlines to notification status
(when (not (boundp 'ns/org-notify-ht))
  (setq ns/org-notify-ht (ht)))

;; <2020-09-26 Sat 13:22> this will probably not scale well.
;; seeing ~0.5 secs on a relatively small org notes file (~2k lines, 300 headings)
;; the dependency is all ~org-ml-get-subtrees~
;; this is similar a manual version of the package org-wild-notifier
;; the main difference is instead of leveraging the agenda we do it ourselves
(defun ns/org-notify ()
  (ns/with-notes
    (->> (org-ml-get-subtrees)
	    (org-ml-match '(:any * (:pred ns/org-scheduled-today)))
      ;; map headline text to scheduled timestamps
      (-map (fn (list (-> <> cadr cadr)
                  (--> <>
                    (org-ml-headline-get-planning it)
                    (cadr it)
                    (plist-get it :scheduled)
                    (cadr it)
                    (plist-get it :raw-value)
                    (ts-parse-org it)))))
      ;; process notifications
      (-map
        (lambda (pair)
          (cl-destructuring-bind (headline timestamp) pair
            ;; ensure we're tracking the headline
            (when (not (ht-contains? ns/org-notify-ht headline))
              (ht-set! ns/org-notify-ht headline nil))

            (when (and
                    (not (ht-get ns/org-notify-ht headline))
                    (ts> (ts-now)
                      ;; get notified in advance
                      (ts-adjust 'minute -3 timestamp)))
              (ns/shell-exec "notify-send DUNST_COMMAND_RESUME")
              (alert! headline
                :severity 'normal
                :title (ts-format "%l:%M %p" timestamp))
              (ht-set! ns/org-notify-ht headline t))))))))

(ns/comment
  (setq ns/org-notify-ht (ht))
  (ns/org-notify))

(named-timer-run :org-notify-scheduled t 60 'ns/org-notify)

;; lazy
(defun ns/org-notify-reset () (setq ns/org-notify-ht (ht)))
(named-timer-run :org-notify-scheduled-reset t (* 60 60 24) 'ns/org-notify-reset)

(ns/comment
  (with-current-buffer (find-file-noselect org-default-notes-file)
    (-map 'ns/org-is-scheduled (org-ml-get-subtrees)))

  (with-current-buffer (find-file-noselect org-default-notes-file)
    (-map 'ns/org-scheduled-today (org-ml-get-subtrees)))
  )

(defun ns/export-scheduled-org-headings-past ()
  (let ((count
          (ns/with-notes
            (->> (org-ml-get-subtrees)
	            (org-ml-match '(:any * (:pred ns/org-scheduled-past-todo)))
              (length)))))
    (if (> count 0)
      (format "outdated: %s" count)
      "")))

(defun! ns/org-jump-to-old-org-heading ()
  (find-file org-default-notes-file)
  (->> (org-ml-get-subtrees)
	  (org-ml-match '(:any * (:pred ns/org-scheduled-past-todo)))
    (first)
    (org-ml-get-property :begin)
    (goto-char))

  (ns/org-jump-to-element-content))

(defun! ns/org-jump-to-old-org-heading ()
  (find-file org-default-notes-file)
  (->> (org-ml-get-subtrees)
	  (org-ml-match '(:any * (:pred ns/org-scheduled-past-todo)))
    (first)
    (org-ml-get-property :begin)
    (goto-char)
    )

  (ns/org-jump-to-element-content))

(ns/bind
  "oq" 'ns/org-jump-to-old-org-heading
  )

(defun ns/org-get-current-clock-time ()
  "return minutes on the current clock"
  (if (org-clocking-p)
    (floor (org-time-convert-to-integer
		         (org-time-since org-clock-start-time)) 60)
    0
    ))

(defun ns/export-scheduled-org-headings ()
  (ns/with-notes
    (->> (org-ml-get-subtrees)
	  (org-ml-match '(:any * (:pred ns/org-scheduled-future)))
      (-map 'org-ml-to-string)
      (s-join "\n"))))

(defun ns/org-clock-sum-week ()
  (let* ((start-of-week
           (let ((now (ts-now)))
             (->> now
               (ts-adjust 'day (- (ts-dow now)))
               (ts-apply :hour 0 :minute 0 :second 0)
               (ts-unix)))))
    (org-clock-sum-current-entry-only start-of-week)))

;; this is measured in minutes
(setq ns/org-casual-timelimit (* 60 5))

(defun ns/org-check-casual-time-today ()
  ;; this function assumes you are clocked into a casual setting already, and
  ;; executing within the notes file
  (org-find-property "casual")
  (let ((time-clocked
          (+
            (org-clock-sum-today)
            ;; todo: only add current clock if it's under casual time
            (ns/org-get-current-clock-time)

            )
          ))
    (when (> time-clocked ns/org-casual-timelimit)
      (progn
        (ns/shell-exec "notify-send DUNST_COMMAND_RESUME")
        (alert! (format "You are out of casual time for today."
                  ;; (/ time-clocked (float ns/org-casual-timelimit))
                  )
          :severity 'normal
          :title "TIME"))
      )

    time-clocked
    ))

(ns/comment

  (float (ns/with-notes (ns/org-check-casual-time-today)))

  (/ (float (ns/with-notes (ns/org-check-casual-time-today)))
    ns/org-casual-timelimit)

  (- ns/org-casual-timelimit (float (ns/with-notes (ns/org-check-casual-time-today))))

  )

(named-timer-run :harass-myself
  t
  20
  (fn
    ;; when you're not idle
    (when (< (org-user-idle-seconds) 20)
      ;; not clocked into anything or on a break
      (if (and (not (org-clocking-p))
            (not (-contains-p '(:short-break :long-break) org-pomodoro-state)))
        ;; llet [current-task-text (with-current-buffer (find-file-noselect org-default-notes-file) (save-excursion (org-ml-parse-headline-at (ns/org-get-active-point))))]
        (alert! (format "Hey! you should be clocked into something. %s"
                  (random))
          :severity 'normal
          :title "TIME"
          ))

      ;; we are clocked into something, check if it's casual and check limit
      (ns/with-notes
        (goto-char (ns/org-get-active-point))
        (when (string= (first (org-get-outline-path)) "casual")
          (ns/org-check-casual-time-today))))))

;; (ns/with-notes nil)

;; cf "track time" @ https://pages.sachachua.com/.emacs.d/Sacha.html
(setq org-clock-idle-time nil)
