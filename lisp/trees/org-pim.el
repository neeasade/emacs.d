;; PIM = personal information management

(named-timer-run :auto-clock-out
  "30 sec"
  60
  (fn (when (> (org-user-idle-seconds)
              (* 10 60))
        (when (not (ns/media-playing-p))
          (ns/org-clock-out))

        ;; assume we are on a hanging clock
        (when (> (org-user-idle-seconds)
                (* 60 60 5))
          (ns/org-clock-out)))))

(ns/comment
  (ns/get-notes-nodes-fn
    (lambda (heading)
      (-when-let (scheduled (ns/headline-date heading))
        (ts-in
          (ts-adjust 'hour -2 (ts-now))
          (ts-apply :hour 23 :minute 59 :second 59 (ts-now))
          (ts-parse-org scheduled)))))

  (defun ns/get-notes-nodes-fn (fn)
    (ns/get-notes-nodes
      '(lambda (heading)
         (-when-let (scheduled (ns/headline-date heading))
           (ts-in
             (ts-adjust 'hour -2 (ts-now))
             (ts-apply :hour 23 :minute 59 :second 59 (ts-now))
             (ts-parse-org scheduled))))
      )

    )
  )

(defun ns/get-notes-nodes (&rest filters)
  "Retrieve headline nodes from notes file for read-only operations."
  (llet [all-nodes (ns/with-notes (org-ml-parse-headlines 'all))]
    (if-not filters
      all-nodes
      (org-ml-match `((:or ,@(-map (lambda (f)
                                     `(:pred ,f))
                               filters)))
        all-nodes))))

(defun ns/org-scheduled-today (heading)
  "Get headings scheduled from <now - 2hrs> --> end of day"
  (-when-let (scheduled (ns/headline-date heading))
    (ts-in
      (ts-adjust 'hour -2 (ts-now))
      (ts-apply :hour 23 :minute 59 :second 59 (ts-now))
      (ts-parse-org scheduled))))

(defun ns/org-scheduled-past-todo (heading)
  "Get TODO items that are scheduled in the past. incidentally, this will also get out of date habits."
  (llet [scheduled (ns/headline-date heading)
          todo-state (org-ml-get-property :todo-keyword heading)]
    (when (and scheduled
            (string= todo-state "TODO"))
      (ts> (ts-now) (ts-parse-org scheduled)))))

;; track headlines to notification status
(when (not (boundp 'ns/org-notify-ht))
  (setq ns/org-notify-ht (ht)))

;; <2020-09-26 Sat 13:22> this will probably not scale well.
;; seeing ~0.5 secs on a relatively small org notes file (~2k lines, 300 headings)
;; the dependency is all ~org-ml-get-subtrees~
;; this is similar a manual version of the package org-wild-notifier
;; the main difference is instead of leveraging the agenda we do it ourselves
;; <2021-08-17 Tue 10:48> I have since decoupled getting the nodes and processing headlines and the
;; lag on the notes buffer is greatly reduced (~0.2s)
(defun ns/org-notify ()
  (->> (ns/get-notes-nodes 'ns/org-scheduled-today)

    ;; map headline text to scheduled timestamps
    (-map (fn (list
                (-> <> org-ml-headline-get-path -last-item)
                (ts-parse-org (ns/headline-date <>)))))

    ;; process notifications
    (-map (-lambda ((headline timestamp))
            ;; ensure we're tracking the headline
            (when-not (ht-contains? ns/org-notify-ht headline)
              (ht-set! ns/org-notify-ht headline nil))

            (when (and (not (ht-get ns/org-notify-ht headline))
                    (ts> (ts-now)
                      ;; get notified in advance
                      (ts-adjust 'minute -3 timestamp)))
              (ns/shell-exec "dunstctl set-paused false")
              (alert! headline
                :severity 'normal
                :title (ts-format "%l:%M %p" timestamp))
              (ht-set! ns/org-notify-ht headline t))))))

(ns/comment
  (setq ns/org-notify-ht (ht))
  (ns/org-notify))

(named-timer-run :org-notify-scheduled t 60 'ns/org-notify)
(named-timer-cancel :org-notify-scheduled)

;; lazy
(defun ns/org-notify-reset () (setq ns/org-notify-ht (ht)))
(named-timer-run :org-notify-scheduled-reset t (* 60 60 24) 'ns/org-notify-reset)

(defun ns/org-status-outdated ()
  (when-not (and (boundp 'org-pomodoro-state)
              (eq org-pomodoro-state :pomodoro))
    (llet [outdated (ns/get-notes-nodes 'ns/org-scheduled-past-todo)
            outdated-next (-> outdated first org-ml-headline-get-path -last-item)
            count (length outdated)]
      (when (> count 0)
        (if (= count 1)
          (format "OUTDATED: %s" outdated-next)
          (format "OUTDATED: %s (next: %s)" count outdated-next))))))

(defun ns/org-rotate (points)
  "Rotate through org headings by points. Assumes you are already in an org file with said headings"
  (if-not points
    (message "Nothing to jump to!")
    (let* ((points (-uniq points))
            (points (-snoc points (first points)))
            (current-headline-point (-if-let (current-headline (org-ml-parse-headline-at (point)))
                                      (-> current-headline second (plist-get :begin))
                                      0)))
      (-if-let (current-match (-find-index (-partial '= current-headline-point) points))
        (goto-char (nth (+ 1 current-match) points))
        (goto-char (or (first (-filter (-partial '< (point)) points))
                     (first points))))
      (ns/org-jump-to-element-content))))

(defun ns/headline-date (headline-node)
  (-some->> headline-node
    (org-ml-headline-get-planning)
    (org-ml-get-property :scheduled)
    (org-ml-get-property :raw-value)))

(defun! ns/org-rotate-outdated ()
  (ns/find-or-open org-default-notes-file)

  (defun ns/org-outdated-sort-node (node1 node2)
    ;; ideas
    ;; anything for TODAY should come first
    ;; then priority -> oldest
    ;; also repeaters?

    ;; (llet
    ;;   [
    ;;     today? (fn )
    ;;     ]
    ;;   )

    ;; (-group-by 'identity '(1 1 2 ) )
    ;; (org-ml-get-pr)

    ;; everything that comes here WILL have a date
    ;; t
    nil
    )

  (ns/org-rotate
    (->> (ns/get-notes-nodes 'ns/org-scheduled-past-todo 'ns/org-scheduled-today)
      (-sort 'ns/org-outdated-sort-node)
      (-map (-partial 'org-ml-get-property :begin)))))

(defun! ns/org-rotate-captures ()
  (defun ns/org-captures-review (headline)
    "Get TODO items that are scheduled in the past. incidentally, this will also get out of date habits."
    (when (eq 'headline (org-ml-get-type headline))
      (and
        (let ((path (org-ml-headline-get-path headline)))
          (and
            (string= "projects" (nth 0 path))
            (string= "captures" (nth 2 path))
            (not (string= (-last-item path) "captures"))))
        (-if-let (capture-date (org-ml-headline-get-node-property "captured" headline))
          (ts< (ts-parse-org capture-date)
            (ts-adjust 'day (- 14) (ts-now)))
          t))))

  (ns/find-or-open org-default-notes-file)
  (ns/org-rotate
    (-map (-partial 'org-ml-get-property :begin)
      (ns/get-notes-nodes 'ns/org-captures-review))))

(ns/bind "oq" 'ns/org-rotate-outdated)

(ns/comment
  (ns/bind "oq" 'ns/org-rotate-captures)

  )

(defun ns/org-get-current-clock-time ()
  "Return minutes on the current org clock."
  (if (org-clocking-p)
    (floor (org-time-convert-to-integer
		         (org-time-since org-clock-start-time)) 60)
    0))

(defun ns/export-scheduled-org-headings ()
  (defun ns/org-scheduled-future (heading)
    (-when-let (scheduled (ns/headline-date heading))
      (ts< (ts-now) (ts-parse-org scheduled))))

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
  (let ((content (->> (ns/get-notes-nodes 'ns/org-note-share)
                   (-map 'ns/org-normalize)
                   (-map 'org-ml-to-string)
                   (s-join "\n")))
         (labs-folder (pass "labs-folder")))
    (when (f-exists-p labs-folder)
      (f-write (format "Exported on: %s\n\n %s" (ts-format (ts-now)) content)
        'utf-8
        (format "%s/notes.org" labs-folder)))))

(defun! ns/export-blog-note-targets ()
  (->> (ns/get-notes-nodes (-partial 'org-ml-headline-get-node-property "blog_slug"))
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
(setq ns/org-casual-timelimit (* 60 30))

(defun ns/org-get-path (&optional point)
  "Get the FULL path to an outline at a point within notes"
  (when point
    (goto-char point))
  (-snoc
    (org-get-outline-path)
    (s-clean (org-get-heading))))

(defun ns/org-check-casual-time-today (&optional notify)
  ;; returns remaining casual minutes
  ;; optionally notifies if you are out of them
  ;; accounts for current clock if it is under a casual heading
  (ns/with-notes
    (goto-char (ns/org-get-active-point))
    (let* ((clocked-casual-p (string= (first (ns/org-get-path)) "casual"))
            (current-clock-time
              (if clocked-casual-p (ns/org-get-current-clock-time) 0))
            (casual-clocked-time
              (progn
                (goto-char (org-find-property "casual"))
                (ns/org-clock-sum-day))))

      (when (and (or notify nil)
              clocked-casual-p
              (> (+ casual-clocked-time current-clock-time)
                ns/org-casual-timelimit))
        (ns/shell-exec "dunstctl set-paused false")
        (alert! (format "You are out of casual time for today.")
          :severity 'normal
          :title "TIME"))
      (- ns/org-casual-timelimit
        (+ casual-clocked-time current-clock-time)))))

(named-timer-run :harass-myself
  t
  20
  (fn (llet [pomo-break? (-contains-p '(:short-break :long-break) org-pomodoro-state)
              org-recently-clocked-out? (< (- (ts-unix (ts-now))
                                             (if org-clock-out-time
                                               (time-to-seconds org-clock-out-time)
                                               10000))
                                          120)
              idle? (> (org-user-idle-seconds) 20)
              wandering? (-all-p 'null (list idle? pomo-break? org-recently-clocked-out? (org-clocking-p)))]
        (when wandering?
          (alert! (format "Hey! you should be clocked into something. %s"
                    (if ns/enable-linux-p (random) ""))
            :severity 'normal
            :title "BE INTENTIONAL")))))

;; don't prompt when idle more than x minutes -- we auto-clock out
(setq org-clock-idle-time nil)

;; todo: does this work?
(defun! ns/org-clock-into-misc ()
  (llet [position (->> `(,org-default-notes-file "misc")
                    ns/org-find-olp
                    marker-position)]
    (ns/with-notes
      (goto-char position)
      (ns/org-clock-in))))
