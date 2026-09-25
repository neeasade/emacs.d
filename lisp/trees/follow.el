;; -*- lexical-binding: t; -*-
;; follow <thing> at point
;; <thing> can be a file location, one of many kinds of emacs links, a code definition, whatever.

;; layer on top of dumb-jump
(ns/use smart-jump
  (setq smart-jump-find-references-fallback-function
    (lambda ()
      (interactive)
      (llet [search (cond ((use-region-p)
                            (buffer-substring-no-properties (region-beginning)
                              (region-end)))
                      ((symbol-at-point)
                        (substring-no-properties
                          (symbol-name (symbol-at-point)))))]
        (deadgrep search))))
  (setq dumb-jump-force-searcher 'rg)
  (smart-jump-setup-default-registers))

(ns/use hyperbole
  (hyperbole-mode t))

(defun ns/hyperbole-file-location--parse (candidate)
  "Parse CANDIDATE as an existing file with optional line and column."
  (let ((home (file-name-as-directory (getenv "HOME"))))
    (cl-labels
      ((expand-path
         (path)
         (cond
           ((string-prefix-p "$HOME/" path)
             (concat home (substring path 6)))
           ((string= path "$HOME") (directory-file-name home))
           (t (expand-file-name path))))
        (location
          (path line column)
          (let ((expanded (expand-path path)))
            (when (file-exists-p expanded)
              (list :file expanded :line line :column column)))))
      (or
        (when (string-match "\\`\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\\'" candidate)
          (location
            (match-string-no-properties 1 candidate)
            (string-to-number (match-string-no-properties 2 candidate))
            (string-to-number (match-string-no-properties 3 candidate))))
        (when (string-match "\\`\\(.*\\):\\([0-9]+\\)\\'" candidate)
          (location
            (match-string-no-properties 1 candidate)
            (string-to-number (match-string-no-properties 2 candidate))
            nil))
        (location candidate nil nil)))))

(defun ns/hyperbole-file-location-at-point ()
  "bot: Return an existing file location on the current line at point.
Unlike Hyperbole's standard pathname buttons, this recognizes unquoted
pathnames containing spaces.  Locations may end in :LINE or :LINE:COLUMN,
or use the form [at PATH, line LINE, column COLUMN]."
  (let ((origin (point))
         (line-end (line-end-position))
         matches)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (re-search-forward
               "\\[at \\(.+?\\), line \\([0-9]+\\), column \\([0-9]+\\)\\]"
               line-end t)
        (let ((start (match-beginning 0))
               (end (match-end 0))
               (label (match-string-no-properties 0))
               (path (match-string-no-properties 1))
               (line (match-string-no-properties 2))
               (column (match-string-no-properties 3)))
          (when (and (<= start origin) (< origin end))
            (let ((location
                    (ns/hyperbole-file-location--parse
                      (format "%s:%s:%s"
                        path line column))))
              (when location
                (push
                  (append
                    (list
                      :label label
                      :start start
                      :end end)
                    location)
                  matches))))))
      (goto-char (line-beginning-position))
      (while (re-search-forward "\\(?:\\$HOME\\|~\\|/\\)" line-end t)
        (let ((start (match-beginning 0)))
          (when (<= start origin)
            (let ((end line-end)
                   location)
              (while (and (> end origin) (not location))
                (let* ((raw (buffer-substring-no-properties start end))
                        (candidate (string-trim-right raw "[ \t\"'`’)=}>]+"))
                        (candidate-end (+ start (length candidate))))
                  (when (> candidate-end origin)
                    (setq location
                      (ns/hyperbole-file-location--parse candidate)))
                  (if location
                    (push
                      (append
                        (list :label candidate :start start :end candidate-end)
                        location)
                      matches)
                    (setq end (1- end))))))))))
    (car (sort matches
           (lambda (left right)
             (> (- (plist-get left :end) (plist-get left :start))
               (- (plist-get right :end) (plist-get right :start))))))))

;; to delete a button:
;; (ibtype:delete 'ns-file-location)

(defib ns-file-location ()
  "bot: Open an existing path at point, optionally at its line and column.
This extends Hyperbole pathname handling to unquoted paths containing spaces."
  (let ((location (ns/hyperbole-file-location-at-point)))
    (when location
      (ibut:label-set
        (plist-get location :label)
        (plist-get location :start)
        (plist-get location :end))
      (let ((file (plist-get location :file))
             (line (plist-get location :line))
             (column (plist-get location :column)))
        (cond
          (column (hact 'link-to-file-line-and-column file line column))
          (line (hact 'link-to-file-line file line))
          (t (hact 'link-to-file file)))))))

;; thanks @noctuid
(defun noct-open ()
  "Open the thing at point.
Try with lsp or smart jump (if in a prog-mode buffer) then with hyperbole."
  (interactive)
  (or (when (derived-mode-p 'prog-mode)
        (cond ((bound-and-true-p lsp-mode)
                (not (stringp (lsp-find-definition))))
          ((fboundp 'smart-jump-go)
            (when
              (cl-letf (((symbol-function 'xref--prompt-p) #'ignore))
                (smart-jump-go))
              (recenter)
              t)

            )))
    ;; hyperbole
    (action-key)))

;; (ns/bind "nn" 'ns/follow)
;; (ns/bind "nn" 'smart-jump-go)


;; handles many kinds of links
(ns/use link-hint)
;; todo: bind "S" in normal mode to link
;; (link-hint-open-link)

(ns/bind
  "n" '(:ignore t :which-key "Jump")
  ;; "ng" 'smart-jump-go
  "nb" 'smart-jump-back
  "nr" 'smart-jump-references
  "nn" 'noct-open
  ;; (kbd "M-<return>") 'noct-open
  ;; (kbd "M-RET") 'noct-open

  )

;; todo: :style paths for git root relative?
;; bug: follow is not using smart-jump-go correctly

;; (ns/bind "nn" (fn!! jump-feedback (smart-jump-go)))
