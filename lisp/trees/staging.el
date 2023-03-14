;; -*- lexical-binding: t; -*-

(defun ns/make-char-table (name upper lower)
  "Make a char table for a certain kind of character"
  (set name
    (let ((str (make-string 127 0)))
      (dotimes (i 127)
        (aset str i i))
      (dotimes (i 26)
        (aset str (+ i ?A) (+ i upper))
        (aset str (+ i ?a) (+ i lower)))
      str)))

(->> '((?𝙰 ?𝚊 monospace)
        (?Ａ ?ａ widechar)
        (?𝔄 ?𝔞 gothic)
        (?𝓐 ?𝓪 cursive))
  (-map
    (-lambda ((upper lower label))
      (llet [char-table-name (intern (format "ns/%s-char-table" label))
              fn-name (intern (format "ns/text-to-%s" label))]
        (ns/make-char-table char-table-name upper lower)
        (eval
          `(defun ,fn-name (beg end) (interactive "r")
             (translate-region beg end ,char-table-name)))))))

(defun ns/cleanup-shells ()
  "Clean up shell-mode buffers that have no children"
  (interactive)
  (->> (ns/buffers-by-mode 'shell-mode)
    (-filter
      (lambda (b)
        (llet [pid (process-id (get-buffer-process b))
                children (sh (format "pgrep -P %s" pid))
                visible? (get-buffer-window b)]
          (and (s-blank? children)
            (not visible?)))))
    (-map 'kill-buffer)))

(named-timer-run :maybe-cleanup-shells
  t                                     ; do not run initially
  ;; once a day I suppose?
  (* 60 60 24)
  (fn (when (> (org-user-idle-seconds)
              (* 5 60))
        (ns/cleanup-shells))))

(ns/bind "nt" 'projectile-toggle-between-implementation-and-test)

(ns/bind "nk" (fn!! goto-theme (find-file (executable-find "theme"))
                (goto-line 0)
                (re-search-forward (if ns/enable-work-p "work-theme" "home-theme"))
                (recenter)))

;; https://github.com/szermatt/emacs-bash-completion
;; comprehensive bash completion in emacs
;; testing out [Fri Dec 20 15:13:58 2019]
;; todo: this is broken, just freezes the shell
;; (ns/use bash-completion)
;; (bash-completion-setup)

(ns/use rainbow-mode
  (setq rainbow-html-colors nil
    rainbow-x-colors nil)

  (ns/bind "tc" 'rainbow-mode))

;; M-x direnv-update-environment
;; sync from the pov of the current file
(ns/use direnv)

(ns/use git-link (setq git-link-open-in-browser t))

;; this seems to be a little nicer:
;; (ns/use browse-at-remote)

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
(ns/use dtrt-indent  (dtrt-indent-global-mode 1))

;; whether or not to rely on notifications from the fs that files have changed
;; when set to nil, checks every 5 seconds
(setq auto-revert-use-notify nil)

;; has a nice url regexp
(require 'rcirc)

;; jump to url in current window text:
(defun! ns/ivy-url-jump ()
  (let* ((window-text (s-clean (buffer-substring (window-start) (window-end))))
          (urls (s-match-strings-all rcirc-url-regexp window-text))
          (urls (-map 'car urls)))
    (if urls
      (browse-url (ns/pick urls))
      (message "no urls!"))))

(ns/bind "nu" 'ns/ivy-url-jump)

(ns/comment
  (ns/use adoc-mode
    (ns/file-mode "adoc" 'adoc-mode)
    (ns/file-mode "asciidoc" 'adoc-mode)))

(ns/use ox-asciidoc)

(when ns/enable-mac-p
  (ns/frame-set-parameter 'inhibit-double-buffering t)

  ;; adding the (t . emacs) so we don't open in textedit and stuff when using ns/follow
  (setq org-file-apps
    '((auto-mode . emacs)
       (directory . emacs)
       ("\\.mm\\'" . default)
       ("\\.x?html?\\'" . default)
       ("\\.pdf\\'" . default)
       (t . emacs))))

(ns/use yaml-mode)

(ns/use org-ql)

;; (let ((org-super-agenda-groups
;;         '((:auto-group t))))
;;   (org-agenda-list))

(ns/use ag)

(defun org-clocking-buffer ()
  "Return the clocking buffer if we are currently clocking a task or nil."
  (marker-buffer org-clock-marker))

(ns/bind "it"
  (fn!! insert-theme-key
    (->> (ns/shell-exec "theme -k")
      (s-split "\n")
      (ns/pick)
      (insert))))

(ns/use paren-face)

;; used in window move scripts
(defalias 'evil-window-north 'evil-window-up)
(defalias 'evil-window-south 'evil-window-down)
(defalias 'evil-window-east 'evil-window-right)
(defalias 'evil-window-west 'evil-window-left)

;; move to style?
(defun ns/make-border-color (c)
  "pass in myron theme label to get a border-style version of it"
  (--> (myron-get c)
    (ct-iterate it 'ct-pastel
      (lambda (c)
        (> (ct-distance it c) 20)))
    (ct-iterate it 'ct-edit-lab-l-inc
      (lambda (c) (ct-is-light-p c 75)))))

(when ns/enable-work-p
  ;; somehow initialize is broken in macos at the moment
  (setq exec-path
    (->> (exec-path-from-shell-initialize)
      (second)
      (cdr)
      (s-split ":")
      (cons (~ ".nix-profile/bin/")))))

;; (ns/use frog-jump-buffer
;;   
;;   (ns/bind "b" 'frog-jump-buffer)
;;   (ns/bind "B" 'frog-jump-buffer-other-window)

;;   (setq frog-jump-buffer-default-filter
;;     ;; 'frog-jump-buffer-filter-file-buffers
;;     ;; 'frog-jump-buffer-filter-same-project
;;     'frog-jump-buffer-filter-recentf
;;     ;; 'ns/jump-file-candidates
;;     )

;;   ;; (setq frog-menu-avy-padding)
;;   (setq frog-menu-avy-keys '(?a ?r ?s ?t ?g ?k ?n ?e ?i ?o))
;;   (setq frog-jump-buffer-max-buffers (length frog-menu-avy-keys))
;;   (setq frog-jump-buffer-include-current-buffer nil)
;;   (setq frog-jump-buffer-posframe-parameters
;;     `(;; cf https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-and-Color-Parameters.html
;;        (background-color . ,(tarp/get :background :weak))

;;        (foreground-color . ,(tarp/get :foreground :weak))
;;        (left . 0.0)
;;        ))

;;   ;; (set-face-attribute 'avy-lead-face nil :box (tarp/get :faded))
;;   (set-face-attribute 'avy-lead-face nil :box nil))

(defun frog-menu-type ()
  "Return variable `frog-menu-type' to use."
  (cond ((display-graphic-p)
          'avy-posframe)
    (t
      'avy-side-window)))

;; (frog-menu-type)

;; todo: spc n g -> browse git repo
;; want a shortcut to open:
;; associated PR
;; or just git repo generally

;; clean-buffer-list-delay-general
;; clean-buffer-list-delay-special
;; clean-buffer-list-kill-buffer-names
;; clean-buffer-list-kill-never-buffer-names
;; clean-buffer-list-kill-regexps
;; clean-buffer-list-kill-never-regexps

(ns/comment
  (defun ns/within (value tolerance anchor)
    "return if a value is within a tolerance"
    (>= (+ anchor tolerance)
      value
      (- anchor tolerance)))

  (defun amp-value (v fn arg1 arg1-amp tolerance-fn)
    (let ((next (funcall fn v arg1)))
      (if (not (string= next v))
        ;; we changed it!
        next
        (if (funcall tolerance-fn arg1)
          (amp-value v fn (funcall arg1-amp arg1) arg1-amp tolerance-fn)
          ;; we're out of tolerance, give up
          v))))

  (amp-value
    "#cccccc"
    (lambda (color amount)
      (ct-edit-hsl-l color (-partial '+ amount)))
    0.01
    (-partial '+ 0.01)
    (fn (ns/within <> 2 0.01)))


  )

;; consider the current day to end at 3AM
;; (setq org-extend-today-until 0)

;; make timestamp processing functions aware of this
;; (setq org-use-effective-time nil)

;; todo: I'm not sure why we set this
(setq org-duration-format
  (quote h:mm))

(defun! ns/babashka-default-connect ()
  (cider-connect-clj '(:host "localhost" :port 1667)))

(defun ns/re-search-forward (search-term)
  "A version of re-search-forward that sets point to the beginning of the match, not the end"
  ;; todo: try this out
  (re-search-forward search-term)
  (backward-char (count search-term)))

;; ugh
(setq mac-control-modifier 'super mac-command-modifier 'control)


(defun! ns/toggle-modeline ()
  "toggle the modeline in the current buffer"
  (make-local-variable 'ns/modeline)
  (if mode-line-format
    (progn
      (setq ns/modeline mode-line-format)
      (setq mode-line-format nil))
    (setq mode-line-format '("%e" (:eval (doom-modeline-format--neeasade-doomline)))))
  (redraw-frame))

(ns/bind "tm" 'ns/toggle-modeline)

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

(defun ns/org-headline-to-progress (headline)
  "Convert headline completion to percentage"
  (-when-let (status (-some->> headline
                       (org-ml-headline-get-path)
                       (-last-item)
                       (s-match (rx "["
                                  (group (+ digit)) "/"
                                  (group (+ digit))
                                  "]" eol))))
    ;; now we have EG ("[0/1]" "0" "1")
    (llet [(_ progress total) status
            (progress total) (-map (-compose 'float 'string-to-number)
                               (list progress total))]
      (floor (* 100 (/ progress total))))))

(ns/conf-style)
(load-file (~e "straight/repos/myron-themes/myron.el"))

(setq undo-tree-enable-undo-in-region t)

(ns/bind "iq" (fn! (sh "qb_userscript paste_selected")))


(ns/use dockerfile-mode)

;; attempting to fix wandering undo paths
(setq undo-tree-enable-undo-in-region nil)


(provide 'staging)
;;; staging.el ends here
