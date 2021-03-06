;; PIM = personal information management
;; managing my information with org in a really big notes file
;; includes org notifications, timeout, pim stuff, exporting to some formats

(named-timer-run :auto-clock-out
  "30 sec"
  30
  (fn (when (> (org-user-idle-seconds)
              (* 3 60))

        (when (not (ns/media-playing-p))
          (ns/org-clock-out)))))

(defun ns/get-notes-nodes (&optional filter-pred)
  "Retrieve nodes from notes file for read-only operations."
  (ns/with-notes
    (if filter-pred
	    (org-ml-match `(:any * (:pred ,filter-pred)) (org-ml-get-subtrees))
      (org-ml-get-subtrees))))

(defun ns/org-scheduled-today (heading)
  "Get headings scheduled from <now - 2hrs> --> end of day"
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
    (when (and
            (string= todo-state "TODO")
            scheduled)
      (ts> (ts-now)
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
  (->> (ns/get-notes-nodes 'ns/org-scheduled-today)
    ;; map headline text to scheduled timestamps
    (-map (fn (list
                ;; FIXME: code smell -- there should be a plist thing to get the title  here
                ;; (this same smell is in node-to-note export)
                (-> <> cadr cadr)
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
        (seq-let (headline timestamp) pair
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
            (ht-set! ns/org-notify-ht headline t)))))))

(ns/comment
  (setq ns/org-notify-ht (ht))
  (ns/org-notify))

(named-timer-run :org-notify-scheduled t 60 'ns/org-notify)

;; lazy
(defun ns/org-notify-reset () (setq ns/org-notify-ht (ht)))
(named-timer-run :org-notify-scheduled-reset t (* 60 60 24) 'ns/org-notify-reset)

(defun ns/export-scheduled-org-headings-past ()
  (let ((count (length (ns/get-notes-nodes 'ns/org-scheduled-past-todo))))
    (if (> count 0)
      (format "outdated: %s" count)
      "")))

(defun! ns/org-jump-to-old-org-heading ()
  (-if-let (out-of-date (ns/get-notes-nodes 'ns/org-scheduled-past-todo))
    (progn
      (find-file org-default-notes-file)
      (->> (first out-of-date)
        (org-ml-get-property :begin)
        (goto-char))
      (ns/org-jump-to-element-content))
    (message "no out of date notes!")))

(ns/bind "oq" 'ns/org-jump-to-old-org-heading)

(defun ns/org-get-current-clock-time ()
  "Return minutes on the current org clock."
  (if (org-clocking-p)
    (floor (org-time-convert-to-integer
		         (org-time-since org-clock-start-time)) 60)
    0))

(defun ns/export-scheduled-org-headings ()
  (defun ns/org-scheduled-future (heading)
    (let ((scheduled (plist-get (cadr (org-ml-headline-get-planning heading)) :scheduled)))
      (when scheduled
        (ts< (ts-now)
          (ts-parse-org (plist-get (cadr scheduled) :raw-value))))))

  (->> (ns/get-notes-nodes 'ns/org-scheduled-future)
    (-map 'org-ml-to-string)
    (s-join "\n")))

(defun ns/write-node-to-post (node)
  "Org headline node to blog post. assumes the presence of blog_slug."
  (let*
    ((slug (org-ml-headline-get-node-property "blog_slug" node))
      (dest (ns/blog-path (format "notes/%s.org" slug)))
      (exists? (f-exists-p dest))
      (old-content (if exists? (f-read dest) "")))

    (f-mkdir (f-dirname dest))
    (when exists? (f-delete dest))
    (f-write
      (format "
#+title: %s
#+title_extra: %s
#+filetags: %s
#+pubdate: %s
#+post_type: note
%s"
        (or (ns/blog-get-prop "title" old-content) (-> node cadr cadr))
        (or (ns/blog-get-prop "title_extra" old-content) "")
        (or (ns/blog-get-prop "filetags" old-content) "")
        (or (ns/blog-get-prop "pubdate" old-content) (ns/shell-exec "date '+<%Y-%m-%d>'"))
        (->> node
          (org-ml-headline-map-node-properties (lambda (_) nil))
          (org-ml-to-trimmed-string)

          ;; remove through the end of the PROPERTIES drawer:
          (s-split "\n" )
          (cdr)
          (s-join "\n")))

      'utf-8
      dest)
    ;; return the path:
    dest))

(defun ns/org-normalize (node)
  "Move an org headline to be top level"
  (let ((difference (- 1 (org-ml-get-property :level node))))
    (->> node
      (org-ml-map-children*
        (if (eq (org-ml-get-type it) 'headline)
          (org-ml-shift-property :level difference it)
          it))
      (org-ml-set-property :level 1))))

(defun! ns/org-export-shared ()
  (defun ns/org-note-share (heading)
    (org-ml-headline-get-node-property "share" heading))

  (let ((content (->>
                   (ns/get-notes-nodes 'ns/org-note-share)
                   (-map 'ns/org-normalize)
                   (-map 'org-ml-to-string)
                   (s-join "\n")))
         (labs-folder (pass "labs-folder")))
    (when (f-exists-p labs-folder)
      (f-write (format "Exported on: %s\n\n %s" (ts-format (ts-now)) content)
        'utf-8
        (format "%s/notes.org" labs-folder)))))

(defun! ns/export-blog-note-targets ()
  (defun ns/org-blog-note (heading)
    (org-ml-headline-get-node-property "blog_slug" heading))

  (->> (ns/get-notes-nodes 'ns/org-blog-note)
    (-map 'ns/org-normalize)
    (-map 'ns/write-node-to-post)
    ((lambda (valid)
       (dolist (file (ns/blog-get-org "notes"))
         (when (not (-contains? valid file))
           (f-delete file)))))))

(defun ns/org-clock-sum-week ()
  ;; get time clocked under an item and it's children for this week
  (org-clock-sum-current-item
    (let ((now (ts-now)))
      (->> now
        (ts-adjust 'day (- (ts-dow now)))
        (ts-apply :hour 0 :minute 0 :second 0)
        (ts-unix)))))

(defun ns/org-clock-sum-day ()
  ;; get time clocked under an item and it's children for today
  (org-clock-sum-current-item
    (->> (ts-now)
      (ts-apply :hour 0 :minute 0 :second 0)
      (ts-unix))))

;; this is measured in minutes
(setq ns/org-casual-timelimit (* 60 5))

(defun ns/org-check-casual-time-today (&optional notify)
  ;; returns remaining casual minutes
  ;; optionally notifies if you are out of them
  ;; accounts for current clock if it is under a casual heading
  (ns/with-notes
    (goto-char (ns/org-get-active-point))
    (let* ((clocked-casual-p (string= (first (org-get-outline-path)) "casual"))
            (current-clock-time
              (if clocked-casual-p (ns/org-get-current-clock-time) 0))
            (casual-clocked-time
              (progn
                (goto-char (org-find-property "casual"))
                (ns/org-clock-sum-day))))

      (when (and
              (or notify nil)
              clocked-casual-p
              (> (+ casual-clocked-time current-clock-time)
                ns/org-casual-timelimit))
        (ns/shell-exec "notify-send DUNST_COMMAND_RESUME")
        (alert! (format "You are out of casual time for today.")
          :severity 'normal
          :title "TIME"))
      (- ns/org-casual-timelimit
        (+ casual-clocked-time current-clock-time))
      )))

(ns/comment
  (ns/org-check-casual-time-today)

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
        (alert! (format "Hey! you should be clocked into something. %s"
                  (random))
          :severity 'normal
          :title "TIME"
          ))
      (ns/org-check-casual-time-today t))))

;; cf "track time" @ https://pages.sachachua.com/.emacs.d/Sacha.html
(setq org-clock-idle-time nil)

(defun! ns/org-clock-into-misc ()
  (llet [position (->> `(,org-default-notes-file "casual" "misc")
                    ns/org-find-olp
                    marker-position)]
    (ns/with-notes
      (goto-char position)
      (org-clock-in))))

