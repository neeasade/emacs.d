(use-package pcre2el)

(defun s-clean (s)
  "remove text properies from S."
  (set-text-properties 0 (length s) nil s) s)

(defun ns/get-current-line ()
  (save-excursion
    (goto-char (point-at-bol))
    (let ((b (point))
           (e (progn (end-of-line) (point))))
      (buffer-substring-no-properties b e))))

(defcommand what-line ()
  "Print the current line number (in the buffer) of point."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

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

(defcommand what-major-mode ()
  "Reveal current major mode."
  (message "%s" major-mode))

(defcommand what-minor-modes ()
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

;; todo: this smart over tramp
;; https://www.emacswiki.org/emacs/TrampMode#toc20
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
    (find-file (concat "/sudo:root@localhost:"
                 (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun ns/get-functions()
  (cons "style"
    (mapcar*
      (lambda(item)
        (s-chomp (s-chop-prefix "(defconfig " (car item))))
      (s-match-strings-all
        "^(defconfig [^ \(\)]+"
        (f-read (~ ".emacs.d/lisp/forest.el"))))))

(defun ns/check-for-orphans()
  "Check to see if any defconfigs are missing from init."
  (let ((initfile (f-read (~ ".emacs.d/init.el"))))
    (mapcar
      (lambda(conf)
        (when (not (s-contains? conf initfile))
          (message (concat "orphaned function! " conf))))
      (ns/get-functions))))

(defcommand jump-config()
  (ivy-read "config: " (ns/get-functions)
    :action
    (lambda (option)
      (interactive)
      (if (f-exists-p (concat "~/.emacs.d/lisp/trees/" option ".el"))
        (ns/find-or-open (concat "~/.emacs.d/lisp/trees/" option ".el"))
        (progn
          (ns/find-or-open (~ ".emacs.d/lisp/forest.el"))
          (goto-char (point-min))
          (re-search-forward (concat "defconfig " option "\n"))))
      (ns/focus-line))))

(defcommand toggle-bloat()
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

(defun ns/toggle-bloat-global(toggle)
  "toggle global bloat - must be called on it's own"
  (if toggle
    (progn
      (global-company-mode)
      (global-flycheck-mode)
      (global-font-lock-mode)
      (when (not ns/enable-windows-p)
        (global-git-gutter-mode t)))
    (progn
      (global-company-mode -1)
      (global-flycheck-mode -1)
      (global-font-lock-mode 0)
      (global-git-gutter-mode 0))))

(use-package simpleclip)

(defcommand paste-from-clipboard-url ()
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

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defcommand focus-line (&rest ignore)
  (evil-scroll-line-to-center (ns/what-line)))

(defun ns/get-last-message()
  (with-current-buffer (get-buffer "*Messages*")
    (goto-char (point-max))
    (previous-line 1)
    (let ((beg (line-beginning-position 1))
           (end (line-beginning-position 2)))
      (buffer-substring beg end))))

(defun ns/look-at-last-message()
  (interactive)
  (ns/find-or-open (~ ".emacs.d/lisp/scratch.el"))
  (goto-char (point-max))
  (insert "\n")
  (insert (ns/get-last-message))
  (previous-line 1)
  )

(defun ns/parse-font (font)
  (let* ((parts-in (s-split "-" font))
          ;; if there is no -, assume it is something like 'Monospace 11', just replace all spaces with dashes
          (parts (if (< (length parts-in) 2)
                   (s-split "-" (s-replace " " "-" font))
                   parts-in))
          (family (first parts))
          (size (string-to-number (second parts))))
    ;; height is in 1/10th of pt
    `(:family ,family :height ,(* 10 size))))

(defun ns/set-faces-variable (faces)
  (dolist (face faces)
    (apply 'set-face-attribute face nil (ns/parse-font (get-resource "st.font_variable")))))

(defun ns/set-faces-monospace (faces)
  (dolist (face faces)
    (apply 'set-face-attribute face nil (ns/parse-font (get-resource "st.font")))))

(defcommand set-buffer-face-variable ()
  (setq buffer-face-mode-face (ns/parse-font (get-resource "st.font_variable")))
  (buffer-face-mode t))

(defcommand set-buffer-face-monospace ()
  (setq buffer-face-mode-face (ns/parse-font (get-resource "st.font")))
  (buffer-face-mode t))

(defun ns/make-lines(list)
  (s-join "\n"
    (mapcar
      (lambda (item)
        (if (stringp item) item
          (prin1-to-string item)))
      list)))

(defun ns/buffers-by-mode (&rest modes)
  (remove-if-not
    (fn (-contains-p modes (buffer-local-value 'major-mode <>)))
    (buffer-list)))

(defun ns/insert-history ()
  (interactive)
  (let ((shell-name
          (if (eq major-mode 'shell-mode)
            (file-name-nondirectory (car (process-command (get-buffer-process (current-buffer)))))
            "zsh")))

    (ivy-read "history: "
      (mapcar
        (fn ;; shared history format: ': 1556747685:0;cmd'
          (if (s-starts-with-p ":" <>)
            (s-replace-regexp (pcre-to-elisp "^:[^;]*;") "" <>)
            <>))
        (s-split "\n" (f-read (~ (format ".%s_history" shell-name)))))

      :action (fn (goto-char (point-max)) (insert <>)))))

(ns/bind
  ;; reconsider these, moved from w -> q for query
  "qf" 'ns/what-face
  "qm" 'ns/what-major-mode
  "qi" 'ns/what-minor-modes
  "qq" 'ns/look-at-last-message

  ;; this should maybe be more generic ie mx history when not in shell
  "qh" 'ns/insert-history

  "fE" 'sudo-edit
  "nc" 'ns/jump-config
  "tb" 'ns/toggle-bloat
  "iu" 'ns/buffercurl
  )

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
(defun ns/apply-frames (action)
  (mapc (lambda(frame)
          (interactive)
          (funcall action frame)
          (redraw-frame frame))
    (frame-list)))

(defun ns/kill-buffers-no-file ()
  "Kill buffers pointing to a file when that file doesn't exist"
  (mapcar 'kill-buffer
    (-filter (fn (let ((file (buffer-file-name <>)))
                   (if file (not (f-exists-p file)) nil)))
      (buffer-list))))
