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

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2018-02-21"
  (interactive)
  (let* (($inputStr (if (use-region-p)
                      (buffer-substring-no-properties (region-beginning) (region-end))
                      (let ($p0 $p1 $p2
                             ;; chars that are likely to be delimiters of file path or url, e.g. space, tabs, brakets. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
                             ($pathStops "^  \t\n\"`'‘’“”|()[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭·。\\"))
                        (setq $p0 (point))
                        (skip-chars-backward $pathStops)
                        (setq $p1 (point))
                        (goto-char $p0)
                        (skip-chars-forward $pathStops)
                        (setq $p2 (point))
                        (goto-char $p0)
                        (buffer-substring-no-properties $p1 $p2))))
          ($path
            (replace-regexp-in-string
              "^file:///" "/"
              (replace-regexp-in-string
                ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
      (if (fboundp 'xahsite-url-to-filepath)
        (let (($x (xahsite-url-to-filepath $path)))
          (if (string-match "^http" $x )
            (progn (browse-url $x) t)
            (find-file $x)))
        (progn (browse-url $path)))
      (if ; not starting “http://”
        (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" $path)
        (let (
               ($fpath (match-string 1 $path))
               ($line-num (string-to-number (match-string 2 $path))))
          (if (file-exists-p $fpath)
            (progn
              (find-file $fpath)
              (goto-char 1)
              (forward-line (1- $line-num)))
            ;; (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath)) (find-file $fpath))
            nil
            ))
        (if (file-exists-p $path)
          (progn ; open f.ts instead of f.js
            (let (($ext (file-name-extension $path))
                   ($fnamecore (file-name-sans-extension $path)))
              (if (and (string-equal $ext "js")
                    (file-exists-p (concat $fnamecore ".ts")))
                (find-file (concat $fnamecore ".ts"))
                (find-file $path))))
          (if (file-exists-p (concat $path ".el"))
            (find-file (concat $path ".el"))
            ;; (when (y-or-n-p (format "file no exist: 「%s」. Create?" $path)) (find-file $path))
            nil
            ))))))
;; todo: make this search back for url? ^

(defcommand follow ()
  (if (not (xah-open-file-at-cursor))
    (if (string= major-mode "org-mode")
      (org-open-at-point)
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
