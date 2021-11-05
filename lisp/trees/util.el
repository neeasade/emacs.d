;; -*- lexical-binding: t; -*-
(defun! ns/what-line ()
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

;; todo: this does not work with anchors?
(defun ns/eww-browse-existing-or-new (url)
  "If eww is displayed, use that for URL, else open here."
  (if (get-buffer-window "*eww*" 'visible)
    (save-excursion
      (select-window (get-buffer-window "*eww*" 'visible))
      (ns/zz-scroll)
      (eww url))
    (eww url)))

(defun ns/what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun! ns/what-major-mode ()
  "Reveal current major mode."
  (message "%s" major-mode))

(defun! ns/what-minor-modes ()
  (message
    (format "%s"
      (delq nil
        (mapcar
          (lambda (x)
            (let ((car-x (car x)))
              (when (and (symbolp car-x) (symbol-value car-x))
                x)))
          minor-mode-alist))))
  (ns/look-at-last-message))

(defun ns/get-functions ()
  "Get all the defconfig entries in the forest."
  (->> (~e "lisp/forest.el")
    f-read
    (s-match-strings-all  "^(defconfig [^ \(\)]+")
    (mapcar (fn (->> (car <>) (s-chop-prefix "(defconfig ") (s-chomp))))
    (append '("style" "dirt" "init" "theme" "forest"))))

(defun! ns/check-for-orphans ()
  "Check to see if any defconfigs are missing from init."
  (let ((initfile (f-read (~e "init.el"))))
    (mapcar
      (lambda(conf)
        (when (not (s-contains? conf initfile))
          (message (concat "orphaned function!: " conf))))
      (ns/get-functions))))

(defun! ns/jump-config ()
  (ivy-read "config: " (ns/get-functions)
    :action
    (fn (interactive)
      (cond
        ((string= "dirt" <>) (ns/find-or-open (~e "lisp/dirt.el")))
        ((string= "forest" <>) (ns/find-or-open (~e "lisp/forest.el")))
        ((string= "theme" <>) (ns/find-or-open (~e "lisp/themes/neea-theme.el")))
        ((string= "init" <>) (ns/find-or-open (~e "init.el")))
        ((string= "follow-dwim" <>) (ns/find-or-open (~e "lisp/trees/follow.el")))
        ((f-exists-p (format (~e "lisp/trees/%s.el") <>))
          (ns/find-or-open (format (~e "lisp/trees/%s.el") <>)))
        (t
          (ns/find-or-open (~e "lisp/forest.el"))
          (goto-char (point-min))
          (re-search-forward (format "defconfig %s\n" <>))))
      (recenter))))

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
  "toggle global bloat"
  (if (or toggle (not global-company-mode))
    (progn
      (message "bloat-global: enabled")
      (global-company-mode)
      (global-flycheck-mode)
      (global-font-lock-mode)
      (when (not ns/enable-windows-p)
        (add-hook 'prog-mode-hook 'git-gutter-mode)))

    (progn
      (message "bloat-global: disabled")
      (global-company-mode -1)
      (global-flycheck-mode -1)
      (global-font-lock-mode 0)
      (remove-hook 'prog-mode-hook 'git-gutter-mode))))

(use-package simpleclip)

(defun! ns/paste-from-clipboard-url ()
  "GET the clipboard contents into current point"

  (request
    (simpleclip-get-contents)

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
  (dolist (face faces)
    (apply 'set-face-attribute face nil (ns/parse-font (get-resource "st.font_variable")))))

(defun ns/set-faces-monospace (faces)
  (dolist (face faces)
    (apply 'set-face-attribute face nil (ns/parse-font (get-resource "st.font")))))

(defun! ns/set-buffer-face-variable (&optional b)
  (with-current-buffer (or b (current-buffer))
    (setq-local buffer-face-mode-face (ns/parse-font (get-resource "st.font_variable")))
    (buffer-face-mode t)))

(defun! ns/set-buffer-face-monospace (&optional b)
  (with-current-buffer (or b (current-buffer))
    (setq-local buffer-face-mode-face (ns/parse-font (get-resource "st.font")))
    (buffer-face-mode t)))

(defun ns/make-lines (list)
  "Transform a LIST of things into something that can be newline iterated by a shell script."
  (->> list
    (-map (fn (if (stringp <>) <>
                (prin1-to-string <>))))
    (-map 's-clean)
    (s-join "\n")))

;; todo: maybe ensure a same order here - buffer-list order can vary
(defun ns/buffers-by-mode (&rest modes)
  (cl-remove-if-not
    (fn (-contains-p modes (buffer-local-value 'major-mode <>)))
    (buffer-list)))

(defun! ns/insert-history ()
  (let ((shell-name
          (if (eq major-mode 'shell-mode)
            (file-name-nondirectory (car (process-command (get-buffer-process (current-buffer)))))
            "bash"))

         (ivy-prescient-enable-sorting nil))

    (ivy-read "history: "
      (-uniq
        (append
          ;; current history across all open shells:
          ;; todo here: don't include commands starting with a space
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
                  <>))))))

      :action (fn (when (eq major-mode 'shell-mode)
                    (goto-char (point-max)))
                (insert <>)
                ))))

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

  (add-to-list 'default-frame-alist `(,key . ,value))
  )

(defun! ns/kill-buffers-no-file ()
  "Kill buffers pointing to a file when that file doesn't exist"
  (mapcar 'kill-buffer
    (-filter (fn (let ((file (buffer-file-name <>)))
                   (if file (not (f-exists-p file)) nil)))
      (buffer-list))))

(defmacro measure-time (&rest body)
  "Measure the time (in seconds) it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

;; using this package only for a tramp aware 'open file as root' function
;; initially went to steal but turned out to be many functions to steal
(use-package crux)

;; todo: insert qute selected text would be nice
(defun! ns/insert-qute-url ()
  (llet [url (ns/shell-exec "qb_active_url")]
    (if (s-blank-p url)
      (message "failed to get url!")
      (llet [desc (when (-contains-p '(org-mode adoc-mode markdown-mode) major-mode)
                    (if (region-active-p)
                      (buffer-substring (region-beginning) (region-end))
                      (read-string (format  "link description for %s (blank for none): " url))))
              desc (if (and
                         (stringp desc)
                         (s-blank-p desc))
                     nil
                     desc)]
        (when (region-active-p)
          (delete-region (region-beginning) (region-end)))
        (insert
          (cond
            ((not desc) url)
            ((eq major-mode 'org-mode) (format "[[%s][%s]]" url desc))
            ((eq major-mode 'adoc-mode) (format "%s[%s]" url desc))
            ((eq major-mode 'markdown-mode) (format "[%s](%s)" desc url))
            (t url)))))))

(ns/bind
  "qf" 'ns/what-face
  "qc" 'describe-char
  "qm" 'ns/what-major-mode
  "qi" 'ns/what-minor-modes
  "qq" 'ns/look-at-last-message

  ;; "qh" 'ns/insert-history

  "fE" 'crux-sudo-edit
  "nc" 'ns/jump-config
  "tb" 'ns/toggle-bloat

  ;; todo: title/auto link with title, insert region (would need a qute script?)
  "iu" 'ns/insert-qute-url
  ;; "iU" 'ns/insert-qute-url-title

  "ih" 'ns/insert-history
  )

