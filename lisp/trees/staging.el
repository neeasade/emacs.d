(global-set-key (kbd "C-e") 'previous-line)

(use-package indent-guide
  :config

  (set-face-foreground 'indent-guide-face
    (face-attribute 'font-lock-keyword-face :foreground))

  ;; (set-face-foreground 'indent-guide-face (ns/color-tone (face-attribute 'default :background) 15 15))

  (setq indent-guide-char "|")
  (indent-guide-global-mode 0)
  )

(defun xah-syntax-color-hex (toggle)
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2017-03-12"
  (interactive)
  (eval
    (cons
      (if toggle 'font-lock-add-keywords 'font-lock-remove-keywords)
      '(nil
         '(("#[[:xdigit:]]\\{3\\}"
             (0 (put-text-property
                  (match-beginning 0)
                  (match-end 0)
                  'face (list :background
                          (let* (
                                  (ms (match-string-no-properties 0))
                                  (r (substring ms 1 2))
                                  (g (substring ms 2 3))
                                  (b (substring ms 3 4)))
                            (concat "#" r r g g b b))))))
            ("#[[:xdigit:]]\\{6\\}"
              (0 (put-text-property
                   (match-beginning 0)
                   (match-end 0)
                   'face (list :background (match-string-no-properties 0))))))))
    (font-lock-flush))
  )

(defcommand toggle-colors ()
  (if (not (boundp 'ns/show-colors))
    (setq-local ns/show-colors nil))

  (setq-local ns/show-colors (not ns/show-colors))
  (xah-syntax-color-hex ns/show-colors))

(ns/bind "tc" 'ns/toggle-colors)

(defcommand follow()
  (when
    ;; first try to open with org handling (includes urls)
    (eq 'fail (condition-case nil (org-open-at-point) (error 'fail)))

    ;; then, see if it's a file by ffap, and handle line numbers as :<#> by converting it into an org file link.
    (if (f-exists-p (nth 0 (s-split ":" (ffap-string-at-point))))
      (org-open-link-from-string
        (format "file:%s" (s-replace ":" "::" (ffap-string-at-point))))

      ;; if that failed, fallback to smart jump
      (smart-jump-go))))

(ns/bind "nn" 'ns/follow)

(defun ns/gradient (start end steps)
  (mapcar (lambda (c) (eval `(color-rgb-to-hex ,@c 2)))
    (color-gradient (color-name-to-rgb start)
      (color-name-to-rgb end)
      steps)))


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
