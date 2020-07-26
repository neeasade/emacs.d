;; -*- lexical-binding: t; -*-

;; examples of kinds handled:
;; /home/neeasade/My Games/Skyrim/RendererInfo.txt:10:2
;; /home/neeasade/.vimrc:50
;; /home/neeasade/:10
;; =~/.local/share/fonts/=
;; org links (so you can use org link types here)
;; [[file:/home/neeasade/.vimrc::50]]

;; todo:
;; could add the handling for this in handle-potential-file-link
;; clojure.lang.ExceptionInfo: Cannot call  with 2 arguments [at /home/neeasade/.dotfiles/bin/bin/btags, line 134, column 3]

;; todo: handle the bash/shell line number format:
;; /home/neeasade/.wm_theme: line 155:

;; give me org-open-link-from-string
(require 'org)

;; handles many kinds of links
(use-package link-hint)

(defun ns/handle-potential-file-link (file)
  "Jump to a file with org if it exists - handles <filename>[:<row>][:<col>]
  return nil if FILE doesn't exist"
  ;; untested on tramp wrt speed

  ;; (message (concat "trying file: " file))
  (let ((file (->> file
                (s-replace "$HOME" (getenv "HOME"))
                (s-replace "~" (getenv "HOME")))))
    (cond
      ((s-blank-p file) nil)
      ;; ((not (f-exists-p file)) nil)
      ((s-matches-p (pcre-to-elisp ".*:[0-9]+:[0-9]+") file)
        (let* ((parts (s-split ":" file))
                (filepath (s-join ":" (-remove-at-indices (list (- (length parts) 2) (- (length parts) 1)) parts))))
          (when (f-exists-p filepath)
            (org-open-link-from-string
              (format "[[file:%s::%s]]" filepath (cadr (reverse parts))))
            (move-to-column (string-to-number (car (last parts))))
            t)))

      ((s-matches-p (pcre-to-elisp ".*:[0-9]+") file)
        (let* ((parts (s-split ":" file))
                (filepath (s-join ":" (-remove-at-indices (list (- (length parts) 1)) parts))))
          (when (f-exists-p filepath)
            (org-open-link-from-string
              (format "[[file:%s::%s]]" filepath (car (last parts))))
            t)))

      (t (when (f-exists-p file)
           (org-open-link-from-string
             (format "[[file:%s]]" file))
           t)))))

;; (ns/handle-potential-file-link (~ ".vimrrc"))
;; (ns/make-org-link (~ ".vimrc:50:2"))
;; (org-link-open-from-string )

(defun ns/follow-log (msg)
  (message msg))

(defun! ns/follow()
  "This is my home rolled DWIM at point function -- maybe it could be considered to be 'bad hyperbole'
   Tries to integrate a few meta solutions
   org link --> our own peek where we build an org file link --> jump to definition with smart-jump"

  (or
    ;; first try to open with org handling (includes urls)
    (not (eq 'fail (condition-case nil (org-open-at-point) (error 'fail))))

    ;; note: ffap-string-at-point is region if one is selected
    (let* ((candidate (ffap-string-at-point))
            (fallback-candidates
              ;; line to end
              (->>
                (buffer-substring
                  (car ffap-string-at-point-region)
                  (save-excursion
                    (goto-char (car ffap-string-at-point-region))
                    (end-of-line) (point)))
                (s-clean)

                ;; assemble potential spaced out files
                ((lambda (line-to-end)
                   (let ((parts (s-split " " line-to-end)))
                     (reverse
                       (-map
                         (fn (s-join " "
                               (-remove-at-indices
                                 (-map (lambda (i) (- (length parts) i)) (range <>))
                                 parts)))
                         (range 1 (length parts)))))))))

            (candidates
              (-concat
                ;; regions get priority
                (when (region-active-p) (list (buffer-substring (region-beginning) (region-end))))

                (list
                  candidate
                  ;; handling case: '/path/to/file:some content'
                  ;; doesn't handle spaces
                  (let ((parts (s-split ":" candidate)))
                    (s-join ":" (-remove-at-indices (list (- (length parts) 1)) parts))))
                fallback-candidates)
              ))

      (let ((match (-first 'ns/handle-potential-file-link candidates)))
        (when match
          (ns/follow-log (format "ns/follow: resolved with org link after building: %s" match))))

      ;; fun, but let's not do this for now:
      ;; (let* ((rg-initial-result (ns/shell-exec (format "rg --files -g '%s'" file-name)))
      ;;         (rg-result (if (s-contains-p "\n" rg-initial-result)
      ;;                      (ivy-read "pickem: " (s-split "\n" rg-initial-result))
      ;;                      rg-initial-result)))
      ;;   (when (and (not (s-blank-p rg-result))
      ;;           (f-exists-p (or rg-result "nil doesn't exist don't  use me")))
      ;;     (org-open-link-from-string
      ;;       (format "file:%s%s" rg-result
      ;;         (if file-line
      ;;           ;; the string-to-number is done to coerce non-numbers (EG grep results with file name appended) to 0
      ;;           (format "::%s" (string-to-number file-line)) "")))

      ;;     (ns/follow-log "ns/follow: resolved with ripgrep")
      ;;     t
      ;;     ))
      )

    (not (string= (link-hint-open-link-at-point) "There is no link supporting the :open action at the point."))

    ;; fall back to definitions with smart jump
    (progn
      (ns/follow-log "ns/follow: resolving with smart-jump-go")
      (shut-up (smart-jump-go)))))


(ns/bind "nn" 'ns/follow)

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


;; todo: make all these circe functions
(defun ns/text-to-cursive (beg end) (interactive "r")
  (translate-region beg end ns/cursive-table))

(defun ns/text-to-monospace (beg end) (interactive "r")
  (translate-region beg end ns/monospace-table))

(defun ns/text-to-gothic (beg end) (interactive "r")
  (translate-region beg end ns/gothic-table))

(defun ns/text-to-widechar (beg end) (interactive "r")
  (translate-region beg end ns/widechar-table))

;; todo: idea: turn all the above into circe commands
;; or matrix.el commands
;; also: add a clap👏text function (lol)

;; (use-package string-inflection
;;   (defun! ns/string-inflection-auto
;;     "switching by major-mode"
;;     (cond
;;       ((eq major-mode 'emacs-lisp-mode)
;;         (string-inflection-all-cycle))
;;       ((eq major-mode 'python-mode)
;;         (string-inflection-python-style-cycle))
;;       ((eq major-mode 'java-mode)
;;         (string-inflection-java-style-cycle))
;;       (t (string-inflection-ruby-style-cycle)))))

;; todo: idea: org-capture for current qutebrowser url

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
              (list org-default-notes-file "url notes")
              )))
        (error
          ;; show what went wrong:
          ;; (nth 1 msg)
          nil)))))

(defun ns/urlnote-get-content (url)
  (let ((url-point (ns/urlnote-get-point url)))
    (when url-point
      (with-current-buffer
        (get-file-buffer org-default-notes-file)
        (->> url-point om-parse-subtree-at)))))

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
  (newline))

(ns/bind
  "nt" (fn! (find-file (~ ".wm_theme")))
  "id" (fn! (org-time-stamp t))
  "iD" (fn! (org-time-stamp nil))
  )

;; https://github.com/szermatt/emacs-bash-completion
;; comprehensive bash completion in emacs
;; testing out [Fri Dec 20 15:13:58 2019]
(use-package bash-completion)
(bash-completion-setup)

;; colors!
;; ideas:
;; - move color towards bg/fg/<n color>
;; - make color a minimum distance from a set color
;; - make percent functions a global intensity setting
(setq rainbow-html-colors nil)
(setq rainbow-x-colors nil)

(use-package rainbow-mode
  :config (ns/bind "tc" 'rainbow-mode))

;; M-x direnv-update-environment
;; sync from the pov of the current file
;; using in combination with lorri
(use-package direnv)


;; run garbage collection every ~5 minutes when we've been away for longer than 5 minutes.
;; this means you won't have to garbage collect for literal minutes when we leave emacs running
;; all night long

(named-timer-run :maybe-garbage-collect
  ;; run the first time in 30 seconds
  ;; relative times are.. strings? cf https://www.gnu.org/software/emacs/manual/html_node/elisp/Timers.html
  "30 sec"
  (* 5 60)
  (fn (when (> ;; PUNS
              (second (current-idle-time))
              (* 5 60))
        (garbage-collect)

        ;; auto revert any who have changed on disk
        (auto-revert-buffers)

        ;; save everyone
        (save-some-buffers t)

        ;; todo here: if on pinebook, suspend
        )))

(ns/bind
  "nd"
  (fn!
    (ivy-read "directory: "
      (->> ns/cd-dirs
        (-uniq)
        (-filter (fn (s-equals-p (file-remote-p <>)
                       (file-remote-p default-directory)))))

      :action
      (lambda (dir)
        (cond
          ((eq major-mode 'dired-mode) (dired dir))
          ((eq major-mode 'shell-mode)
            (goto-char (point-max))
            (insert (concat "cd \""
                      (s-replace
                        (or (file-remote-p dir) "")
                        ""
                        dir
                        )
                      "\""))
            (comint-send-input))
          (t (insert dir)))))))

(defun ns/emacs-to-theme ()
  (use-package theme-magic)
  (s-join "\n"
    (append (seq-map-indexed
              (fn (format "color%s=%s" (number-to-string <2>)
                    (s-replace "#" "" <1>)))
              (theme-magic--auto-extract-16-colors))

      (-map
        (fn (format "%s=%s" (car <>)
              (s-replace "#" "" (ns/color-shorten (cadr <>)))))
        (-partition 2
          (list
            "foreground" (face-attribute 'default :foreground)
            "background" (face-attribute 'default :background)
            "cursorColor" (first evil-insert-state-cursor)))))))

(use-package git-link
  :config
  (setq git-link-open-in-browser t))

;; spelling
(use-package flyspell-correct-avy-menu
  :config
  (require 'flyspell-correct-avy-menu)
  (setq flyspell-correct-interface #'flyspell-correct-avy-menu))

(define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-wrapper)

(named-timer-run :show-periodic-reminder
  t
  (* 60 60 2)
  (fn
    (when (< (second (current-idle-time)) 120)
      (alert (let ((reminders
                     (org-ql-select org-default-notes-file
                       '(tags "reminders")
                       :action '(s-clean (org-get-heading t t)))
                     ))
               (nth (random (length reminders)) reminders))
        :severity 'normal
        :title "*Reminder*"
        ))))


;; todo: a timer that checks that you are not in pomodoro mode and alerts every once in awhile

;; (named-timer-run :org-alert-scheduled
;;   ;; ugghhh
;;   t
;;   20
;;   (fn
;;     (when (not (get-file-buffer org-default-notes-file))
;;       (find-file org-default-notes-file))

;;     (with-current-buffer (get-file-buffer org-default-notes-file)
;;       (->> (om-get-headlines)
;;         ;; parse scheduled time, see if any are ahead of NOW,
;;         ;; alert if we haven't for this heading
;;         ;; org-ql is probably a good candidate here
;;         ))))


;; takes awhile -- doesn't handle noto color emoji?
;; (use-package unicode-fonts
;;   :config
;;   (require 'unicode-fonts)
;;   (unicode-fonts-setup))

;; (use-package persist)

;; automatic detection of indent settings (vim-sleuth)
;; todo: doom does a thing where they blend the major mode w/ editor config
;;       so for example sh-mode files if a *.sh rule is present, editorconfig takes precedence over this
;;
(use-package dtrt-indent :config (dtrt-indent-global-mode 1))

;; (use-package org-doct)
(ns/use-package org-doct "progfolio/doct"
  :config
  (require 'doct))

;; (use-package ts)    ; timestamps
(ns/use-package ts "alphapapa/ts.el")    ; timestamps

(ns/use-package org-super-agenda "alphapapa/org-super-agenda")
(require 'org-super-agenda)

(defun ns/make-project-capture (project &optional template-override key)
  `(,project
     :keys ,(or key (-> project string-to-list first char-to-string))
     :file ,org-default-notes-file
     ;; todo: maybe want: a way to override the olp path and file? (eg, project level notes)
     ;; alternatively, just import the notes into your main ones
     :children (("task" :keys "t" :todo-state "TODO"
                  :immediate-finish t
                  :template ,(or template-override (list "* %{todo-state} %^{Description}" "%?"))
                  :olp ("projects" ,project "tasks"))
                 ("capture" :keys "c" :todo-state "TODO"
                   :immediate-finish t
                   :template ,(or template-override (list "* %{todo-state} %^{Description}" "%?"))
                   :olp ("projects" ,project "captures"))
                 ("note" :keys "n"
                   :immediate-finish t
                   :template ,(or template-override (list "* %^{Description}" "%?"))
                   :olp ("projects" ,project "notes"))

                 ("task" :keys "T" :todo-state "TODO"
                   :template ,(or template-override (list "* %{todo-state} %{Description}" "%?"))
                   :olp ("projects" ,project "tasks"))
                 ("capture" :keys "C" :todo-state "TODO"
                   :template ,(or template-override (list "* %{todo-state} %{Description}" "%?"))
                   :olp ("projects" ,project "captures"))
                 ("note" :keys "N"
                   :template ,(or template-override (list "* %{Description}" "%?"))
                   :olp ("projects" ,project "notes"))
                 )))

;; in the future if we want to nest projects under a heading:
;; ("Projects" :keys "p"
;;   :children
;;   (;; projects:
;;     ,(ns/make-project-capture "other")
;;     ))

(setq ns/org-capture-project-list
  (if (f-exists-p org-default-notes-file)
    (with-current-buffer (find-file-noselect org-default-notes-file)
      (->> (org-find-property "projects")
        (om-parse-subtree-at)
        ;; (om-parse-headline-at )
	      (om-get-children)
        cdr
        (-map 'om-headline-get-path)
        (-map 'last)
        -flatten))
    "no notes file here"))

(ns/use-package linkmarks "dustinlacewell/linkmarks"
  :config
  (setq linkmarks-file (concat org-directory "/linkmarks.org")))

(setq ns/org-capture-project-templates
  (doct
    `(
       ;; ,(ns/make-project-capture "meta" nil "c")
       ,@(-map 'ns/make-project-capture ns/org-capture-project-list)

       ;; ("Reminder" :keys "r"
       ;;   :template "* %?\n%U\n"
       ;;   )

       ("LinkMark" :keys "l"
         :file ,linkmarks-file
         :template ("* %^{Title}"
                     ":PROPERTIES:"
                     ":Created: %U"
                     ":END:"
                     "[[%?]]"
                     ))

       ("Journal" :keys "j"
         :template "* %?\n%U\n"
         :clock-in t :clock-resume t
         :datetree t :file ,org-default-diary-file
         ))))

(setq ns/org-capture-region-templates
  (doct
    `(
       ;; ,(ns/make-project-capture "meta")
       ;; this is wrapped in a progn so the lines after the first don't get made into headings
       ;; I hate org mode
       ,@(-map (fn (ns/make-project-capture <> "* %(progn \"%i\")"))
           ns/org-capture-project-list)

       )))

(setq org-capture-templates ns/org-capture-project-templates)

(defun! ns/capture-current-subtree ()
  (let ((ns/org-points
          (save-excursion
            (list
              (progn (org-back-to-heading) (point))
              (progn (org-back-to-heading) (evil-forward-word-begin) (point))
              (progn (org-end-of-subtree) (point))))))

    (set-mark (second ns/org-points))
    (goto-char (third ns/org-points))
    (activate-mark)

    (ns/capture-current-region)
    ;; kill the leftover headline
    (kill-whole-line)
    (when (s-blank-str-p (thing-at-point 'line)) (kill-whole-line))))

(defun! ns/capture-current-region ()
  ;; keep the initial region
  (let ((ns/region-points (list (region-beginning) (region-end))))
    (setq org-capture-templates ns/org-capture-region-templates)
    (when (org-capture)
      ;; assume we succeeded

      ;; if re captured to somewhere in the current buffer, our point might have changed
      (when (not (= (region-beginning) (first ns/region-points)))
        (setq ns/region-points
          (llet
            [start (first ns/region-points)
              end (second ns/region-points)
              new-start (if (> (region-beginning) start)
                          (+ start (- (region-beginning) start))
                          (- start (- start (region-beginning))))
              new-end (if (> (region-beginning) start)
                        (+ end (- (region-beginning) start))
                        (- end (- start (region-beginning))))]
            (list new-start new-end))))

      (apply 'kill-region ns/region-points)

      (when (s-blank-str-p (thing-at-point 'line))
        (kill-line))))

  ;; todo: catch quit for revert as well
  (setq org-capture-templates ns/org-capture-project-templates))

(ns/bind-mode 'org
  "or" (fn! (if (use-region-p)
              (ns/capture-current-region)
              (ns/capture-current-subtree)))

  "oc"
  (fn! (if (use-region-p)
         (ns/capture-current-region)
         (org-capture)))
  ;; 'org-capture
  ;; "org move"
  "om" 'org-refile
  )

;; allow shell blocks in org mode to be executed:
(org-babel-do-load-languages 'org-babel-load-languages
  '((shell . t)))

;; EG, call with org-babel-execute-src-block on:
;; the ':results output silent' means don't insert into the buffer
;; #+begin_src sh :results output silent
;; <code to execute>
;; #+end_src
(ns/bind-mode 'org "e"
  (fn! (when (org-in-src-block-p)
         ;; living dangerously
         (let ((org-confirm-babel-evaluate (fn nil)))
           (org-babel-execute-src-block)))))

;; (setq org-capture-templates)

(defun ns/notes-current-standup-headline ()
  "get the current standup heading as matched from notes"
  ;; note to self: standup is the wrong word probably -- it's the heading we track all of 'today'
  ;; changing the name would make more sense for slamming it on project headings as well
  (with-current-buffer (find-file-noselect org-default-notes-file)
    (om-parse-headline-at (org-find-property "focus"))))


(defun ns/notes-current-standup-task (parent-headline)
  "get the current standup heading as matched from notes"
  (let ((standup-point
	        (->> parent-headline
	          cdr car
	          ((lambda (props) (plist-get props :begin))))))

    ;; standup-point
    (or
      (with-current-buffer (get-file-buffer org-default-notes-file)
        (->> (om-parse-element-at standup-point)
	        (om-get-children)
	        ;; what we want:
	        ;; next headline that has TODO blank or TODO, with no scheduled time
	        ((lambda (children)
	           (append
	             ;; TODO: can't find out how to query headlines with no todo keyword
	             ;; idea: map headlines, set todo keyword to 'unset'
	             ;; (om-match '((:todo-keyword "")) children)
	             (om-match '((:todo-keyword "TODO")) children)
	             )))
	        first
          ))
      parent-headline
      )))

;; todo: timer to check if you have an active intent
;; (named-timer-run :harass-myself
;;   t
;;   (* 3 60)
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

;; whether or not to rely on notifications from the fs that files have changed
;; when set to nil, checks every 5 seconds
(setq auto-revert-use-notify nil)

;; refile the current subtree to capture targets

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



;; to consider later: org drill -- noting a fix here for now
;; something else: keybinds don's work in org-drill, even after:
;; (add-to-list 'evil-emacs-state-modes 'org-drill-mode)
;; (require 'org-drill)

;; ;; cf https://emacs.stackexchange.com/questions/46916/org-drill-invalid-match-tag
;; (defun org-drill-hide-subheadings-if (test)
;;   "TEST is a function taking no arguments. TEST will be called for each
;; of the immediate subheadings of the current drill item, with the point
;; on the relevant subheading. TEST should return nil if the subheading is
;; to be revealed, non-nil if it is to be hidden.
;; Returns a list containing the position of each immediate subheading of
;; the current topic."
;;   (let ((drill-entry-level (org-current-level))
;;          (drill-sections nil))
;;     (org-show-subtree)
;;     (save-excursion
;;       (org-map-entries
;;         (lambda ()
;;           (when (and (not (org-invisible-p))
;;                   (> (org-current-level) drill-entry-level))
;;             (when (or (/= (org-current-level) (1+ drill-entry-level))
;;                     (funcall test))
;;               (hide-subtree))
;;             (push (point) drill-sections)))
;;         nil 'tree))
;;     (reverse drill-sections)))

(defun ns/org-is-scheduled (heading)
  (let ((scheduled (plist-get (cadr (om-headline-get-planning heading)) :scheduled)))
    (when scheduled
      (ts<
        (ts-now)
        (ts-parse-org (plist-get (cadr scheduled) :raw-value))
        ))))

(ns/comment
  (ns/org-is-scheduled
    (with-current-buffer (find-file-noselect org-default-notes-file)
      (->> (om-get-subtrees)
        (first)))))

(defun ns/export-scheduled-org-headings ()
  (with-current-buffer (find-file-noselect org-default-notes-file)
    (->> (om-get-subtrees)
      ;; something better than 'TODO' might be "scheduled for later than today"
      ;; I think orgql has that /exact/ example in their readme..
	    ;; (om-match '((:todo-keyword "TODO")))
	    (om-match '(:many (:pred ns/org-is-scheduled)))
      (-map 'om-to-string)
      (s-join "\n"))))
