;; PIM = personal information management

(ns/use org-ql)

(named-timer-run :auto-clock-out
  "30 sec"
  60
  (fn (when (> (org-user-idle-seconds)
              ;; the long time is because we'll use this for measuring pomodoros on another computer/kvm style
              (ns/t 1h))
        (when (not (ns/media-playing-p))
          (ns/org-clock-out))

        ;; assume we are on a hanging clock
        (when (> (org-user-idle-seconds)
                (ns/t 5h))
          (ns/org-clock-out)))))

(defun ns/parse-headline-at-point ()
  (-when-let (headline (org-ml-parse-this-headline))
    (org-ml-headline-set-node-property
      "internal_filepath" (buffer-file-name)
      headline)))

(defun ns/headline-marker (headline)
  (when headline
    (set-marker (make-marker)
      (org-ml-get-property :begin headline)
      (get-file-buffer (org-ml-headline-get-node-property "internal_filepath" headline)))))

(defun ns/get-notes-nodes (org-ql-query &optional paths)
  "get headlines with org-ql. returns parse results of org-ml"
  (org-ql-select (or paths org-agenda-files)
    org-ql-query :action 'ns/parse-headline-at-point))

(defun ns/org-ml-filter (filters headlines)
  "filter headlines by named filter functions"
  (org-ml-match `((:or ,@(-map (lambda (f)
                                 `(:pred ,f))
                           (-list filters))))
    headlines))

(defun ns/headline-date (headline-node)
  (-some->> headline-node
    (org-ml-headline-get-planning)
    (org-ml-get-property :scheduled)
    (org-ml-get-property :raw-value)
    (ts-parse-org)))

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
  (->> (ns/get-notes-nodes '(scheduled :to today))

    ;; map headline text to scheduled timestamps
    (-map (fn (list
                (-> <> org-ml-headline-get-path -last-item)
                (ns/headline-date <>))))

    ;; process notifications
    (-map (-lambda ((headline timestamp))
            ;; ensure we're tracking the headline
            (when-not (ht-contains? ns/org-notify-ht headline)
              (ht-set! ns/org-notify-ht headline nil))

            (when (and (not (ht-get ns/org-notify-ht headline))
                    (ts> (ts-now)
                      ;; get notified in advance
                      (ts-adjust 'minute -3 timestamp)))
              (sh "dunstctl set-paused false")
              (alert! headline
                :severity 'normal
                :title (ts-format "%l:%M %p" timestamp))
              (ht-set! ns/org-notify-ht headline t))))))

(ns/comment
  (setq ns/org-notify-ht (ht))
  (ns/org-notify))

(named-timer-run :org-notify-scheduled t 60 'ns/org-notify)

(named-timer-cancel :org-notify-scheduled)

;; (named-timer-cancel :org-notify-scheduled)

;; lazy
(defun ns/org-notify-reset () (setq ns/org-notify-ht (ht)))
(named-timer-run :org-notify-scheduled-reset t (ns/t 1d) 'ns/org-notify-reset)

(defun ns/org-status-outdated ()
  (when-not (and (boundp 'org-pomodoro-state)
              (eq org-pomodoro-state :pomodoro))
    (llet [outdated (-sort 'ns/org-outdated-sort-node
                      (ns/get-notes-nodes `(and (scheduled :to ,(ts-now)) (not (todo "DONE")))))
            outdated-next (-> outdated first org-ml-headline-get-path -last-item)
            count (length outdated)]
      (when (> count 0)
        (if (= count 1)
          (format "on deck: %s" outdated-next)
          (format "[%s]on deck: %s" (make-string count ?@) outdated-next))))))

(defun ns/org-status-scheduled ()
  (llet [nodes (ns/get-notes-nodes `(scheduled :from ,(ts-now) :to today)) ; now through eod
          ;; nodes (-sort 'ts> (-map 'ns/headline-date nodes))
          next-headline (first nodes)]
    (-when-let (next-ts (ns/headline-date next-headline))
      (when (ts-in (ts-now) (ts-adjust 'hour +1 (ts-now)) next-ts)
        (format "%s in %im"
          (-> next-headline org-ml-headline-get-path -last-item)
          (/ (- (ts-unix next-ts)
               (ts-unix (ts-now)))
            60))))))

(defun ns/goto-marker (marker)
  (ns/switch-to-buffer-or-window (marker-buffer marker))
  (goto-char (marker-position marker)))

(defun ns/org-rotate (headlines)
  "Rotate through org headings by markers."
  (if-not headlines
    (message "Nothing to jump to!")
    (llet [headlines (-sort 'ns/org-outdated-sort-node headlines)
            markers (-uniq (-map 'ns/headline-marker headlines))
            markers (-snoc markers (first markers))
            target (-when-let (marker (ns/headline-marker (ns/parse-headline-at-point)))
                     ;; we're looking at a headline, is it in the list?
                     (-if-let (index (-find-index (-partial '= marker) markers))
                       (nth (+ 1 index) markers)
                       ;; skip previous markers
                       (--first (> it marker) markers)))]
      (ns/goto-marker (or target (first markers)))
      (ns/org-jump-to-element-content))))

(defun ns/org-scheduled-today (headline)
  (-some->> (ns/headline-date headline)
    (ts-in
      (ts-apply :hour 0 :minute 0 :second 0 (ts-now))
      (ts-apply :hour 23 :minute 59 :second 59 (ts-now)))))

(defun ns/org-outdated-sort-node (&rest headlines)
  (llet [(h1 h2) headlines
          (p1 p2) (--map (or (org-ml-get-property :priority it) 1000) headlines)
          (d1 d2) (-map 'ns/headline-date headlines)
          ;; is it a habit?
          (h1 h2) (--map (-some->> it
                           (org-ml-headline-get-planning)
                           (org-ml-get-property :scheduled)
                           (org-ml-get-property :raw-value)
                           (s-contains-p "+"))
                    headlines)]
    (cond
      ((not (= p1 p2)) (if (< p1 p2) t nil))
      ((and h1 (not h2)) nil)
      ((and h2 (not h1)) t)
      ((-any 'ns/org-scheduled-today headlines)
        (if (ts> d1 d2) t nil))
      (t (if (ts> d1 d2) nil t)))))

(defun! ns/org-rotate-outdated ()
  (ns/org-rotate
    (->> (ns/get-notes-nodes '(and (scheduled :to today) (not (todo "DONE"))))
      ;; (-sort 'ns/org-outdated-sort-node)
      )))

(defun! ns/org-rotate-captures ()
  (ns/find-or-open org-default-notes-file)
  (ns/org-rotate
    (-map (-partial 'org-ml-get-property :begin)
      (ns/get-notes-nodes '(and (outline-path-segment "captures")
                             (not (regexp "captures")))))))

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

  (->>
    (length
      (ns/get-notes-nodes
        ;; todo: not quite the same
        ;; '(scheduled :from today)
        'ns/org-scheduled-future
        ))

    (-map 'org-ml-to-string)
    (s-join "\n")))

(defun! ns/org-clock-sum-week ()
  "get time clocked under an item and it's children for this week"
  (org-clock-sum-current-item
    (->> (ts-now)
      (ts-adjust 'day (- (ts-dow (ts-now))))
      (ts-apply :hour 0 :minute 0 :second 0)
      (ts-unix))))

(defun! ns/org-clock-sum-day ()
  "get time clocked under an item and it's children for today"
  (org-clock-sum-current-item
    (->> (ts-now)
      (ts-apply :hour 0 :minute 0 :second 0)
      (ts-unix))))

(defun ns/org-get-path (&optional point)
  "Get the FULL path to an outline at a point within notes"
  (when point (goto-char point))
  (-snoc
    (org-get-outline-path)
    (s-clean (org-get-heading))))

(defun ns/org-pim-wandering? ()
  (llet [pomo-break? (-contains-p '(:short-break :long-break) org-pomodoro-state)
          org-recently-clocked-out? (< (- (ts-unix (ts-now))
                                         (if org-clock-out-time
                                           (time-to-seconds org-clock-out-time)
                                           10000))
                                      120)
          idle? (> (org-user-idle-seconds) 20)]
    (-all-p 'null (list idle? pomo-break? org-recently-clocked-out? (org-clocking-p)))))

(named-timer-run :harass-myself
  t 20
  (fn (when (ns/org-pim-wandering?)
        (alert! (format "Hey! you should be clocked into something. %s"
                  (if ns/enable-linux-p (random) ""))
          :severity 'normal
          :title "BE INTENTIONAL"))))

(named-timer-cancel :harass-myself)

(ns/comment
  (named-timer-run :outdated-task-nudge
    t
    (ns/t 10m)
    (fn (when (ns/org-pim-wandering?)
          (--when-let (ns/org-status-outdated)
            (alert! it
              :severity 'normal
              :title "Outstanding tasks")))))

  (named-timer-cancel :outdated-task-nudge))

;; don't prompt when idle more than x minutes -- we auto-clock out
(setq org-clock-idle-time nil)

(defun! ns/org-clock-into (&rest path)
  "Clock into a heading in the notes file. defaults to clock->misc"
  (llet [headline-path (or path '("clock" "misc"))
          position (->> `(,org-default-notes-file ,@headline-path)
                     ns/org-find-olp
                     marker-position)]
    (if-not position
      (message "ns/org-clock-into: headline path not found: %s" path)
      (ns/with-notes
        (goto-char position)
        (ns/org-clock-in)))))

(defun ns/org-headline-to-progress (headline)
  "Convert headline completion to percentage"
  (-when-let (status (-some->> headline
                       (org-ml-headline-get-path)
                       (-last-item)
                       (s-match (rx "["
                                  (group (+ digit)) "/"
                                  (group (+ digit))
                                  "]" eol))))
    ;; now we have EG ("[0/1]" "0" "1")
    (llet [(_ progress total) status
            (progress total) (-map (-compose 'float 'string-to-number)
                               (list progress total))]
      (floor (* 100 (/ progress total))))))
