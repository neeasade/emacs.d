;; -*- lexical-binding: t; -*-
(use-package pcre2el)

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

(defun ns/color-is-light-p (name)
  (let*
    ((rgb (color-name-to-rgb name))
      (red (first rgb))
      (green (second rgb))
      (blue (third rgb))
      ;; cf https://en.wikipedia.org/wiki/YIQ#From_RGB_to_YIQ
      (yiq (+ (* red .299) (* green .587) (* blue .114))))
    (>= yiq 0.5)
    ))

(defun ns/color-tone (name light dark)
  "tone name a percent based on if light or dark - generally want softer value for dark."
  (if (ns/color-is-light-p name)
    (color-darken-name name light)
    (color-lighten-name name dark)))

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
  (->> (~ ".emacs.d/lisp/forest.el")
    f-read
    (s-match-strings-all  "^(defconfig [^ \(\)]+")
    (mapcar (fn (->> (car <>) (s-chop-prefix "(defconfig ") (s-chomp))))
    (cons "style")
    (cons "dirt")
    ))

(defun! ns/check-for-orphans ()
  "Check to see if any defconfigs are missing from init."
  (let ((initfile (f-read (~ ".emacs.d/init.el"))))
    (mapcar
      (lambda(conf)
        (when (not (s-contains? conf initfile))
          (message (concat "orphaned function! " conf))))
      (ns/get-functions))))

(defun! ns/jump-config ()
  (ivy-read "config: " (ns/get-functions)
    :action
    (fn (interactive)
      (cond
        ((string= "dirt" <>)
          (ns/find-or-open (~ ".emacs.d/lisp/dirt.el")))
        ((f-exists-p (format (~ ".emacs.d/lisp/trees/%s.el") <>))
          (ns/find-or-open (format (~ ".emacs.d/lisp/trees/%s.el") <>)))
        (t
          (ns/find-or-open (~ ".emacs.d/lisp/forest.el"))
          (goto-char (point-min))
          (re-search-forward (format "defconfig %s\n" <>))))
      (ns/focus-line))))

(defun! ns/toggle-bloat()
  "toggle bloat in the current buffer"
  (if (not (bound-and-true-p company-mode))
    (progn
      (company-mode)
      (flycheck-mode)
      (font-lock-mode)
      (git-gutter-mode))
    (progn
      (company-mode -1)
      (flycheck-mode -1)
      (font-lock-mode 0)
      (git-gutter-mode 0))))

(defun ns/toggle-bloat-global (toggle)
  "toggle global bloat"
  (if toggle
    (progn
      (global-company-mode)
      (global-flycheck-mode)
      (global-font-lock-mode)
      (when (and ns/enable-git-p
              (not ns/enable-windows-p))
        (global-git-gutter-mode t)))

    (progn
      (global-company-mode -1)
      (global-flycheck-mode -1)
      (global-font-lock-mode 0)
      (global-git-gutter-mode 0))))

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

(defun! ns/focus-line (&rest ignore)
  (evil-scroll-line-to-center (ns/what-line)))

(defun ns/get-last-message()
  (with-current-buffer (get-buffer "*Messages*")
    (goto-char (point-max))
    (previous-line 1)
    (let ((beg (line-beginning-position 1))
           (end (line-beginning-position 2)))
      (buffer-substring beg end))))

(defun! ns/look-at-last-message()
  (ns/find-or-open (~ ".emacs.d/lisp/scratch.el"))
  (goto-char (point-max))
  (insert "\n")
  (insert (ns/get-last-message))
  (previous-line 1))

(defun ns/parse-font (font)
  (let* ((parts-in (s-split "-" font))
          ;; if there is no -, assume it is something like 'Monospace 11', just replace all spaces with dashes
          (parts (if (< (length parts-in) 2)
                   (s-split "-" (s-replace " " "-" font))
                   parts-in))
          (family (first parts))
          (size (string-to-number (second parts))))
    ;; height is 10x pt
    `(:family ,family :height ,(* 10 size))))

(defun ns/set-faces-variable (faces)
  (dolist (face faces)
    (apply 'set-face-attribute face nil (ns/parse-font (get-resource "st.font_variable")))))

(defun ns/set-faces-monospace (faces)
  (dolist (face faces)
    (apply 'set-face-attribute face nil (ns/parse-font (get-resource "st.font")))))

(defun! ns/set-buffer-face-variable ()
  (setq buffer-face-mode-face (ns/parse-font (get-resource "st.font_variable")))
  (buffer-face-mode t))

(defun! ns/set-buffer-face-monospace ()
  (setq buffer-face-mode-face (ns/parse-font (get-resource "st.font")))
  (buffer-face-mode t))

(defun ns/make-lines (list)
  "Transform a LIST of things into something that can be newline iterated by a shell script."
  (->> list
    (mapcar (fn (if (stringp <>) <>
                  (prin1-to-string <>))))
    (s-join "\n")))

;; todo: maybe ensure a same order here - buffer-list order can vary
(defun ns/buffers-by-mode (&rest modes)
  (remove-if-not
    (fn (-contains-p modes (buffer-local-value 'major-mode <>)))
    (buffer-list)))

(defun! ns/insert-history ()
  (let ((shell-name
          (if (eq major-mode 'shell-mode)
            (file-name-nondirectory (car (process-command (get-buffer-process (current-buffer)))))
            "bash"))

         (ivy-prescient-enable-sorting nil)
         )

    (ivy-read "history: "
      (-uniq
        (append
          ;; current history across all open shells:
          (-flatten
            (mapcar
              (fn (with-current-buffer <>
                    (when (boundp 'comint-input-ring)
                      (when (> (ring-size comint-input-ring) 0)
                        (mapc 's-clean (ring-elements comint-input-ring)
                          )))))
              (ns/buffers-by-mode 'shell-mode)))

          (mapcar
            (fn ;; shared history format: ': 1556747685:0;cmd'
              (if (s-starts-with-p ":" <>)
                (s-replace-regexp (pcre-to-elisp "^:[^;]*;") "" <>)
                                                      <>))
                                   (reverse (s-split "\n" (f-read (~ (format ".%s_history" shell-name))))))))

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
          (eval `(setq-local ,symbol ,value)))
        (eval `(setq-local ,symbol ,value)))))

  (eval `(setq-default ,symbol ,value)))

;; callback on all open frames
(defun! ns/apply-frames (action)
  (mapc (lambda (frame)
          (funcall action frame)
          (redraw-frame frame))
    (frame-list)))

(defun! ns/kill-buffers-no-file ()
  "Kill buffers pointing to a file when that file doesn't exist"
  (mapcar 'kill-buffer
    (-filter (fn (let ((file (buffer-file-name <>)))
                   (if file (not (f-exists-p file)) nil)))
      (buffer-list))))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

;; using this package only for a tramp aware 'open file as root' function
;; initially went to steal but turned out to be many functions to steal
(use-package crux)

(ns/bind
  "qf" 'ns/what-face
  "qm" 'ns/what-major-mode
  "qi" 'ns/what-minor-modes
  "qq" 'ns/look-at-last-message

  ;; "qh" 'ns/insert-history

  "fE" 'crux-sudo-edit
  "nc" 'ns/jump-config
  "tb" 'ns/toggle-bloat

  "iu" 'ns/buffercurl
  "ih" 'ns/insert-history
  )
