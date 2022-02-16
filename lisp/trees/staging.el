;; -*- lexical-binding: t; -*-

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

(defun ns/make-urlnote-funcs ()
  (defun ns/urlnote-get-point (url)
    (let ((url-plain
            (when url
              (if (s-contains-p "?" url)
                (first (s-split "?" url)) url))))

      (catch 'error
        (condition-case msg
          (marker-position
            (org-find-olp
              (if url-plain
                (list org-default-notes-file "url notes" url-plain)
                (list org-default-notes-file "url notes"))))
          (error
            ;; show what went wrong:
            ;; (nth 1 msg)
            nil)))))

  (defun ns/urlnote-get-content (url)
    (let ((url-point (ns/urlnote-get-point url)))
      (when url-point
        (with-current-buffer
          (get-file-buffer org-default-notes-file)
          (->> url-point org-ml-parse-subtree-at)))))

  (defun ns/urlnote-jump (url)
    (let ((url-point (ns/urlnote-get-point url)))
      (when url-point
        (find-file org-default-notes-file)
        (goto-char (ns/urlnote-get-point url)))))

  (defun ns/urlnote-make-and-jump (url)
    (find-file org-default-notes-file)
    (goto-char (ns/urlnote-get-point nil))
    (next-line)
    ;; (org-insert-subheading nil)
    ;; (org-insert-heading-after-current)
    (if (s-contains-p "?" url)
      (first (s-split "?" url)) url)
    (insert url)
    (org-do-demote)
    (newline)))

(ns/bind "nt" (fn! (find-file
                     (if ns/enable-mac-p
                       (~ ".dotfiles/theming/scratch_mac.toml")
                       (~ ".dotfiles/theming/scratch.toml")))))

;; https://github.com/szermatt/emacs-bash-completion
;; comprehensive bash completion in emacs
;; testing out [Fri Dec 20 15:13:58 2019]
;; todo: this is broken, just freezes the shell
;; (use-package bash-completion)
;; (bash-completion-setup)

(use-package rainbow-mode
  :config
  (setq
    ;; don't preview non-hex codes
    rainbow-html-colors nil
    rainbow-x-colors nil)

  (ns/bind "tc" 'rainbow-mode))

;; M-x direnv-update-environment
;; sync from the pov of the current file
(use-package direnv)



(use-package git-link
  :config
  (setq git-link-open-in-browser t))

;; this seems to be a little nicer:
;; (use-package browse-at-remote)

;; (named-timer-run :show-periodic-reminder
;;   t
;;   (* 60 60 2)
;;   (fn
;;     (when (< (second (current-idle-time)) 120)
;;       (alert (let ((reminders
;;                      (org-ql-select org-default-notes-file
;;                        '(tags "reminders")
;;                        :action '(s-clean (org-get-heading t t)))
;;                      ))
;;                (nth (random (length reminders)) reminders))
;;         :severity 'normal
;;         :title "*Reminder*"
;;         ))))

;; automatic detection of indent settings (vim-sleuth)
;; todo: doom does a thing where they blend the major mode w/ editor config
;;       so for example sh-mode files if a *.sh rule is present, editorconfig takes precedence over this
(use-package dtrt-indent :config (dtrt-indent-global-mode 1))

;; whether or not to rely on notifications from the fs that files have changed
;; when set to nil, checks every 5 seconds
(setq auto-revert-use-notify nil)

;; has a nice url regexp
(require 'rcirc)

;; jump to url in current window text:
(defun! ns/ivy-url-jump ()
  (let* ((window-text (s-clean (buffer-substring (window-start) (window-end))))
          (urls (s-match-strings-all rcirc-url-regexp window-text)))
    (if urls
      (ivy-read "url: "
        (->> urls (-map 'car))
        :action 'browse-url)
      (message "no urls!"))))

(ns/bind "nu" 'ns/ivy-url-jump)

(use-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode)
          ("\\.asciidoc\\'" . adoc-mode)))
(use-package ox-asciidoc)


(when ns/enable-mac-p
  (ns/frame-set-parameter 'inhibit-double-buffering t)

  ;; adding the (t . emacs) so we don't open in textedit and stuff when using ns/follow
  (setq org-file-apps
    '(
       (auto-mode . emacs)
       (directory . emacs)
       ("\\.mm\\'" . default)
       ("\\.x?html?\\'" . default)
       ("\\.pdf\\'" . default)
       (t . emacs))))

(use-package yaml-mode)
(use-package org-ql)

;; (let ((org-super-agenda-groups
;;         '((:auto-group t))))
;;   (org-agenda-list))

;; (use-package 4clojure)

(use-package ag)

(defun org-clocking-buffer ()
  "Return the clocking buffer if we are currently clocking a task or nil."
  (marker-buffer org-clock-marker))

(use-package go-mode)

(ns/bind "it"
  (fn!
    (ivy-read "theme key: "
      (s-split "\n" (ns/shell-exec "theme -o keys"))
      :action #'insert)))

(use-package paren-face)

(defalias 'evil-window-north 'evil-window-up)
(defalias 'evil-window-south 'evil-window-down)
(defalias 'evil-window-east 'evil-window-right)
(defalias 'evil-window-west 'evil-window-left)
