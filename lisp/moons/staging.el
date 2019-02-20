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

(defun shell-sync-dir-with-prompt (string)
  "A preoutput filter function (see `comint-preoutput-filter-functions')
which sets the shell buffer's path to the path embedded in a prompt string.
This is a more reliable way of keeping the shell buffer's path in sync
with the shell, without trying to pattern match against all
potential directory-changing commands, ala `shell-dirtrack-mode'.

In order to work, your shell must be configured to embed its current
working directory into the prompt.  Here is an example .zshrc
snippet which turns this behavior on when running as an inferior Emacs shell:

  if [ $EMACS ]; then
     prompt='|Pr0mPT|%~|[%n@%m]%~%# '
  fi

The part that Emacs cares about is the '|Pr0mPT|%~|'
Everything past that can be tailored to your liking.
"
  (if (string-match "|Pr0mPT|\\([^|]*\\)|" string)
    (let ((cwd (match-string 1 string)))
      (setq default-directory
        (if (string-equal "/" (substring cwd -1))
          cwd
          (setq cwd (concat cwd "/"))))
      (replace-match "" t t string 0))
    string))

(defun ns/shell-track ()
  (shell-dirtrack-mode nil)
  (add-hook 'comint-preoutput-filter-functions 'shell-sync-dir-with-prompt nil t))

(add-hook 'shell-mode-hook 'ns/shell-track)
