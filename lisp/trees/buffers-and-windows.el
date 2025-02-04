;; buffers and windows

(defun ns/buffers-by-mode (&rest modes)
  (--filter (-contains-p modes (buffer-local-value 'major-mode it))
    (buffer-list)))

(defun! ns/kill-other-buffers ()
  "Kill all other buffers."
  (->> (buffer-list)
    (-remove (lambda (b) (eq b (current-buffer))))
    (-map 'kill-buffer)))

(defun! ns/kill-buffers-no-file ()
  "Kill buffers pointing to a file when that file doesn't exist"
  (-map 'kill-buffer
    (-filter (fn (let ((file (buffer-file-name <>)))
                   ;; don't worry about remote files (tramp password check)
                   (if file (when (not (file-remote-p file))
                              (not (f-exists-p file)))
                     nil)))
      (buffer-list))))

(defun! ns/kill-buffers-by-mode ()
  (->> (buffer-list)
    (-map (-partial 'buffer-local-value 'major-mode))
    (-uniq)
    (ns/pick "mode to kill")
    (intern)
    (ns/buffers-by-mode)
    (-map #'kill-buffer)))

(winner-mode 1)

(ns/bind
  "w" '(:ignore t :which-key "Windows")
  "wh" 'evil-window-left
  "wn" 'evil-window-down
  "we" 'evil-window-up
  "wl" 'evil-window-right
  "wd" 'evil-window-delete
  "ww" 'other-window
  "wb" 'balance-windows-area

  "ws" (fn!! (split-window-horizontally)
         (evil-window-right 1))

  "wS" (fn!! (split-window-vertically)
         (evil-window-down 1))

  "wf" (fn!! (follow-mode)
         (delete-other-windows)
         (evil-window-vsplit))

  "wm" 'delete-other-windows ;; "window max"

  "wo" 'winner-undo
  "wi" 'winner-redo

  "b" '(:ignore t :which-key "Buffers")

  "bb" (fn!! surf-buffers
         (->> (ns/jump-file-candidates :buffers-without-files)
           (ns/pick "buffer")
           (ns/find-or-open)))

  ;; idea: find shell buffers running something - might also be nice to show pname
  "bM" (fn!! surf-shells-running-something
         (-some->> (ns/buffers-by-mode 'shell-mode)
           (-keep
             (lambda (b)
               (when (get-buffer-process b)
                 (llet [pid (process-id (get-buffer-process b))
                         ;; -P works on macos and linux
                         children (sh (format "pgrep -P %s" pid))]
                   (when (not (s-blank? children))
                     (ns/str (buffer-name b)
                       "\t"
                       (first (s-split) children)
                       (process-name (get-buffer-process b))))))))
           (ns/pick "buffer")
           (s-split "\t")
           (first)
           (ns/find-or-open)))

  "bm" (fn!! surf-buffers-mode
         (->> (ns/buffers-by-mode major-mode)
           (-map 'buffer-name)
           (ns/pick "buffer")
           (ns/find-or-open)))

  "br" 'revert-buffer

  "bd" (fn!! (kill-buffer nil))
  "bn" (fn!! buffer-same-name
         (let ((current-filename (f-filename (buffer-file-name (current-buffer)))))
           (->> (buffer-list)
             (-map 'buffer-name)
             (--filter (s-starts-with-p current-filename it))
             (ns/pick)
             (ns/find-or-open))))
  ;; "bK" 'ns/kill-other-buffers
  ;; "bk" 'kill-matching-buffers
  ;; "bm" 'ns/kill-buffers-by-mode
  )
