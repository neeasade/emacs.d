;; -*- lexical-binding: t; -*-

(defun ns/eww-browse-existing-or-new (url)
  "If eww is displayed, use that for URL, else open here."
  (when (get-buffer-window "*eww*" 'visible)
    (select-window (get-buffer-window "*eww*" 'visible)))
  (eww url)
  (ns/zz-scroll))

(defun ns/what-face (&optional point)
  (interactive)
  (let* ((point (or point (point)))
          (face (or (get-char-property point 'read-face-name)
                  (get-char-property point 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" point))))

(defun! ns/what-minor-modes ()
  "Show enabled minor modes"
  (->> minor-mode-alist
    (--keep (when-let (enabled (symbol-value (first it)))
              (first it)))
    (-map 'ns/str)
    (s-join " ")
    (message)))

(defun ns/get-functions ()
  "Get all the defconfig entries in the forest."
  (->> (~e "lisp/forest.el")
    f-read
    (s-match-strings-all  "^(ns/defconfig [^ \(\)]+")
    (mapcar (fn (->> (car <>) (s-chop-prefix "(ns/defconfig ") (s-chomp))))
    (append '("dirt" "init" "forest"))))

(defun! ns/check-for-orphans ()
  "Check to see if any defconfigs are missing from init."
  (let ((initfile (f-read (~e "init.el"))))
    (-map
      (lambda (conf)
        (when (not (s-contains? conf initfile))
          (message (concat "orphaned function!: " conf))))
      (ns/get-functions))))

(defun! ns/jump-config ()
  (llet [f (ns/pick "config" (ns/get-functions))]
    (cond
      ((string= "dirt" f) (ns/find-or-open (~e "lisp/dirt.el")))
      ((string= "forest" f) (ns/find-or-open (~e "lisp/forest.el")))
      ((string= "init" f) (ns/find-or-open (~e "init.el")))
      ((string= "follow-dwim" f) (ns/find-or-open (~e "lisp/trees/follow.el")))
      ((f-exists-p (format (~e "lisp/trees/%s.el") f))
        (ns/find-or-open (format (~e "lisp/trees/%s.el") f)))
      (t
        (ns/find-or-open (~e "lisp/forest.el"))
        (goto-char (point-min))
        (re-search-forward (format "defconfig %s" f)))))
  (recenter))

(defun! ns/toggle-bloat()
  "toggle bloat in the current buffer"
  (if (not (bound-and-true-p company-mode))
    (progn
      (message "bloat-local: enabled")
      (company-mode)
      (flycheck-mode)
      (font-lock-mode)
      (git-gutter-mode))
    (progn
      (message "bloat-local: disabled")
      (company-mode -1)
      (flycheck-mode -1)
      (font-lock-mode 0)
      (git-gutter-mode 0))))

(defun! ns/toggle-bloat-global (&optional toggle)
  "Toggle global bloat"
  (if (or toggle (not global-company-mode))
    (progn
      (message "bloat-global: enabled")
      (global-company-mode)
      (global-flycheck-mode)
      (global-font-lock-mode)
      (global-git-gutter-mode (if ns/enable-windows-p 0 t)))

    (progn
      (message "bloat-global: disabled")
      (global-company-mode -1)
      (global-flycheck-mode -1)
      (global-font-lock-mode 0)
      (global-git-gutter-mode 0))))

(ns/use simpleclip)

(defun! ns/paste-from-clipboard-url ()
  "GET the clipboard contents into current point"

  ;; pls seems iffy
  ;; see https://github.com/alphapapa/plz.el/issues/3
  ;; (insert (plz 'get (simpleclip-get-contents)))

  (request
    :type "GET"
    :parser 'buffer-string
    :success
    (function*
      (lambda (&key data &allow-other-keys)
        (interactive)
        (insert data)))))

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

(defun ns/set-buffers-face-monospace (buffers)
  (llet [font (ns/parse-font (get-resource "font.mono.spec"))]
    (--map (with-current-buffer it
             (setq-local buffer-face-mode-face font)
             (buffer-face-mode t))
      buffers)))

(defun! ns/set-buffer-face-variable (&optional buffer)
  (ns/set-buffers-face-variable (list (or buffer (current-buffer)))))

(defun! ns/set-buffer-face-monospace (&optional buffer)
  (ns/set-buffers-face-monospace (list (or buffer (current-buffer)))))

(defun ns/make-lines (list)
  "Transform a LIST of things into something that can be newline iterated by a shell script."
  (->> list
    (-map 'ns/str)
    (-map 's-clean)
    (s-join "\n")))

(defun ns/buffers-by-mode (&rest modes)
  (--filter (-contains-p modes (buffer-local-value 'major-mode it))
    (buffer-list)))

(defun! ns/insert-history ()
  (let ((shell-name
          (if (eq major-mode 'shell-mode)
            (file-name-nondirectory (car (process-command (get-buffer-process (current-buffer)))))
            "bash"))

         (vertico-prescient-enable-sorting nil))

    (llet [history-item (ns/pick "history"
                          (->>
                            (append
                              ;; current history across all open shells:
                              (-mapcat
                                (fn (with-current-buffer <>
                                      (when (boundp 'comint-input-ring)
                                        (when (> (ring-size comint-input-ring) 0)
                                          (mapc 's-clean (ring-elements comint-input-ring)
                                            )))))
                                (ns/buffers-by-mode 'shell-mode))

                              (->>
                                (~ (format ".%s_history" shell-name))
                                f-read
                                (s-split "\n")
                                reverse
                                (-map
                                  (fn ;; shared history format: ': 1556747685:0;cmd'
                                    (if (s-starts-with-p ":" <>)
                                      (s-replace-regexp (pcre-to-elisp "^:[^;]*;") "" <>)
                                      <>)))))
                            (-uniq)
                            (-remove (-partial #'s-starts-with-p " "))))]
      (when (eq major-mode 'shell-mode)
        (goto-char (point-max)))
      (insert history-item))))

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

  (eval `(setq-default ,symbol ,value))
  )

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

(defun! ns/kill-buffers-no-file ()
  "Kill buffers pointing to a file when that file doesn't exist"
  (-map 'kill-buffer
    (-filter (fn (let ((file (buffer-file-name <>)))
                   (if file (not (f-exists-p file)) nil)))
      (buffer-list))))

(defmacro measure-time (&rest body)
  "Measure the time (in seconds) it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defmacro ns/install-dashdoc (docset mode-hook)
  "Install dash DOCSET if dashdocs enabled, add mode hook to narrow dash search targets."
  `(when (bound-and-true-p ns/enable-dashdocs-p)

     (when nil
       (message (format "Installing %s docset..." ,docset))
       (counsel-dash-install-docset (subst-char-in-string ?\s ?_ ,docset)))
     (add-hook ,mode-hook (fn (setq-local counsel-dash-docsets '(,docset))))))

;; using this package only for a tramp aware 'open file as root' function
;; initially went to steal but turned out to be many functions to steal
(ns/use crux (crux-reopen-as-root-mode t))

(defun! ns/insert-qute-url (&optional description-in)
  (llet [url (sh "qb_active_url")]
    (if (s-blank-p url)
      (message "failed to get url!")
      (llet [desc (if description-in description-in
                    (when (-contains-p '(org-mode adoc-mode markdown-mode) major-mode)
                      (if (region-active-p)
                        (let ((result (buffer-substring (region-beginning) (region-end))))
                          (delete-region (region-beginning) (region-end))
                          result)
                        (read-string (format  "link description for %s (blank for none): " url)))))
              desc (if (and (stringp desc) (s-blank-p desc))
                     nil
                     desc)]

        (insert
          (cond
            ((not desc) url)
            ((eq major-mode 'org-mode) (format "[[%s][%s]]" url desc))
            ((eq major-mode 'adoc-mode) (format "%s[%s]" url desc))
            ((eq major-mode 'markdown-mode) (format "[%s](%s)" desc url))
            (t url)))))))

(defun! ns/insert-qute-url-title ()
  (ns/insert-qute-url (sh "qb_active_url .title")))

(ns/bind
  "qf" 'ns/what-face
  "qc" 'describe-char
  "qm" (fn!! what-major-mode (message "%s" major-mode))
  "qi" 'ns/what-minor-modes

  "fE" 'crux-sudo-edit
  "nc" 'ns/jump-config
  "tb" 'ns/toggle-bloat

  "iu" 'ns/insert-qute-url
  "iU" 'ns/insert-qute-url-title

  "ih" 'ns/insert-history)
