;; -*- lexical-binding: t; -*-

(use-package org
  :straight (:host github
              :repo "emacsmirror/org"
              :files ("lisp/*.el" "contrib/lisp/*.el")))

(require 'org-habit)

;; give us easy templates/tab completion like yasnippet and the like
;; form is '<<key><tab>', eg <s<tab> expands to src block
;; todo: reference what all this gives us: https://orgmode.org/manual/Easy-templates.html
(require 'org-tempo)

;; nice functional org library
(ns/use-package org-ml "ndwarshuis/org-ml" :config (require 'org-ml))

(when ns/enable-evil-p
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle))

;; config
(setq-ns org
  directory (if (f-exists-p (~ "sync/main/notes"))
              (~ "sync/main/notes")
              (~ "notes"))

  default-notes-file  (concat org-directory "/notes.org")
  default-diary-file  (concat org-directory "/journal.org")
  default-habits-file  (concat org-directory "/habits.org")

  ;; one really big notes file
  agenda-files (list org-default-notes-file)

  ;; weeks start on mondays
  agenda-start-on-weekday 1

  ;; ellipsis "---"
  ellipsis "*"

  adapt-indentation nil
  startup-indented nil
  startup-folded t
  startup-align-all-tables t

  src-fontify-natively t
  src-tag-acts-natively t
  src-preserve-indentation t

  ;; export options
  html-checkbox-type 'html
  html-postamble nil
  export-with-section-numbers nil
  export-with-toc nil

  ;; days before expiration where a deadline becomes active
  deadline-warn-days 14
  todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "INACTIVE(i@/!)" "|" "CANCELLED(c@/!)" "MEETING"))

  blank-before-new-entry '((heading . t) (plainlist-item . nil))
  tag-alist '(
               ("work" . ?w)
               ("personal" . ?P)
               ("emacs" . ?e)
               ("dotfiles" . ?d)
               ("project" . ?p)
               ("active" . ?a)
               ("inactive" . ?i)
               )

  ;; clock
  clock-x11idle-program-name "x11idle"
  clock-idle-time 5
  clock-sound nil

  pomodoro-play-sounds nil
  pomodoro-keep-killed-pomodoro-time t
  pomodoro-ask-upon-killing nil

  ;; todo: consider note option here.
  log-done 'time

  ;; log state into drawer instead of inserting a list under the heading
  log-into-drawer t

  ;; current file or any of the agenda-files, max 9 levels deep
  refile-targets '((nil :maxlevel . 9)
                    (org-agenda-files :maxlevel . 9))

  outline-path-complete-in-steps nil    ; Refile in a single go
  refile-use-outline-path t             ; Show full paths for refiling

  ;; this way we don't haev to create annoying outlines
  refile-allow-creating-parent-nodes 'confirm

  ;; save target buffer after archiving
  archive-subtree-save-file-p t
  )

;; todo: maybe don't be redundant with nested with-notes calls -- check current buffer at outset, maybe set a variable or something
(defmacro ns/with-notes (&rest body)
  `(with-current-buffer (find-file-noselect org-default-notes-file)
     (save-excursion
       ,@body)))

(defun! ns/org-set-unique-property (&optional property value)
  (let ((prop (or property "focus")))
    (org-delete-property-globally prop)
    (org-set-property prop (or value "t"))))


(defun ns/org-ml-not-done (node)
  (and (eq (org-ml-get-type node) 'headline)
    (not (org-ml-headline-is-done node))))

(defun ns/notes-current-standup-task (parent-headline)
  "Get a TODO underneath a headline that is passed in."
  (let ((standup-point
          (->> parent-headline
            cdr car
            ((lambda (props) (plist-get props :begin))))))
    (or
      (with-current-buffer (get-file-buffer org-default-notes-file)
        (->> (org-ml-parse-element-at standup-point)
          (org-ml-get-children)
          (org-ml-match '(:any * (:pred ns/org-ml-not-done)))
          first))

      parent-headline)))

(defun! ns/org-get-next-review-point (&optional property)
  "Return the next captured thing to review"
  ;; something to consider: organize by date,
  ;; since we mark that with capture targets
  ;; then just treat dateless as priority above dated
  )

(defun ns/org-get-clock-marker ()
  ;; sniped from org-clock-goto
  (cond
    ((org-clocking-p) org-clock-marker)
    ((and org-clock-goto-may-find-recent-task
       (car org-clock-history)
       (marker-buffer (car org-clock-history)))
      (setq recent t)
      (car org-clock-history))
    (t (user-error "No active or recent clock task"))))

(defun! ns/org-get-active-point (&optional property)
  "Resolves to a point in my big notes files to either:
- currently clocked in org headline
- the first TODO under the headline with a 'focus' property
- if no TODO is found, just go to the headline with a 'focus' property directly
(nb: the 'focus' property target may be overridden with an argument)
"
  (ns/with-notes
    (save-window-excursion
      (if org-clock-current-task
        ;; org clock goto does handle some niceties for us
        (let ((m (ns/org-get-clock-marker)))
          (with-current-buffer (marker-buffer m)
            (goto-char (marker-position m))))
        (->> (org-find-property (or property "focus"))
          (org-ml-parse-headline-at)
          (ns/notes-current-standup-task)
          cadr
          ((lambda (props)
             (or (plist-get props :contents-begin)
               (plist-get props :begin))))
          (goto-char)))
      (point))))

(defun! ns/org-goto-active (&optional property)
  (find-file org-default-notes-file)
  (goto-char (ns/org-get-active-point property))
  (ns/org-jump-to-element-content))

(use-package org-pomodoro
  ;; pomodoro, tied to music playing status
  :config
  (defun ns/toggle-music (action)
    (let ((target (or (executable-find "player.sh") "mpc")))
      (ns/shell-exec-dontcare (concat target " " action))))

  ;; named functions so we don't append lambdas on init reload
  (defun ns/toggle-music-play () (ns/toggle-music "play"))
  (defun ns/toggle-music-pause () (ns/toggle-music "pause"))

  (defun! ns/focus-mode-enter ()
    (ns/toggle-music-play)
    (ns/shell-exec-dontcare "notify-send DUNST_COMMAND_PAUSE")
    ;; turn off distracting websites
    ;; list stolen from https://raw.githubusercontent.com/viccherubini/get-shit-done/master/sites.ini
    ;; and extended a little bit
    (->>
      "mastodon.social, aurora.web.telegram.org, lobste.rs, nixers.net, old.reddit.com, reddit.com, www.reddit.com, web.telegram.org, forums.somethingawful.com, somethingawful.com, digg.com, break.com, news.ycombinator.com, infoq.com, bebo.com, api.twitter.com, zulipchat.com, twitter.com, facebook.com, blip.com, youtube.com, vimeo.com, delicious.com, flickr.com, friendster.com, hi5.com, linkedin.com, livejournal.com, meetup.com, myspace.com, plurk.com, stickam.com, stumbleupon.com, yelp.com, slashdot.org, plus.google.com, hckrnews.com, kongregate.com, newgrounds.com, addictinggames.com, hulu.com"
      (s-replace ", " "\n")
      ((lambda (content)
         (f-write content 'utf-8 (~ ".config/qutebrowser/adblock.txt")))))
    (ns/shell-exec-dontcare "qb_command :adblock-update")
    (ns/shell-exec-dontcare "panelt false"))

  (defun! ns/focus-mode-quit ()
    (ns/toggle-music-pause)
    (ns/shell-exec-dontcare "notify-send DUNST_COMMAND_RESUME")
    (f-write "" 'utf-8 (~ ".config/qutebrowser/adblock.txt"))
    (ns/shell-exec-dontcare "qb_command :adblock-update")
    (ns/shell-exec-dontcare "panelt true"))

  (add-hook 'org-pomodoro-extend-last-clock 'ns/focus-mode-enter)
  (add-hook 'org-pomodoro-started-hook 'ns/focus-mode-enter)
  (add-hook 'org-pomodoro-finished-hook 'ns/focus-mode-quit)
  (add-hook 'org-pomodoro-killed-hook 'ns/focus-mode-quit)

  (add-hook 'org-pomodoro-break-finished-hook 'ns/toggle-music-play))

;; todo: there's a bug in this -- if a heading is the last line of a file, we should insert a newline
(defun ns/org-jump-to-element-content ()
  "Jump from a anywhere in a headline to the start of it's content"
  ;; org mode is cursed

  ;; (org-show-siblings)
  ;; (org-show-all)
  ;; (org-reveal)

  (org-show-set-visibility 'canonical)
  ;; (org-show-context)
  (org-show-entry)
  (org-show-children)
  ;; (org-show-subtree)

  (let* ((props (cadr (org-ml-parse-this-headline)))
          (contents-begin (plist-get props :contents-begin))
          (begin (plist-get props :begin)))
    (goto-char (or contents-begin begin))

    (if contents-begin
      (progn
        (let ((first-element (org-element-at-point)))
          (when (eq 'property-drawer (car first-element))
            (goto-char (org-element-property :end first-element))))

        (let ((first-element (org-element-at-point)))
          (when (eq 'drawer (car first-element))
            (goto-char (org-element-property :end first-element))))

        ;; if we're looking at a headline, we went too far
        ;; (easily possible with blank headlines)
        (when (s-starts-with-p "*" (thing-at-point 'line))
          (evil-previous-line)))

      ;; empty headline
      (when (s-starts-with-p "*" (thing-at-point 'line))
        (evil-next-line)
        (when (s-starts-with-p "*" (thing-at-point 'line))
          (evil-open-above 1)
          (evil-normal-state)))))

  ;; todo: consider ensuring drawers are collapsed after this
  (recenter)
  )

(defun! ns/make-org-link-to-here ()
  "Insert a link to the current cursor location in the active headline"
  (let* ((line-content (s-trim (thing-at-point 'line)))
          (line (number-to-string (line-number-at-pos)))
          (link (format "file:%s::%s" (buffer-file-name) line))
          ;; (label (format "%s:%s" (buffer-name) line))
          (filepath (s-replace (s-replace "\\" "/" (~ "")) "~/" (buffer-file-name)))
          (label (format "%s:%s" filepath line))
          (org-link (format "[[%s][%s]]" link label)))

    (ns/with-notes
      (ns/org-goto-active)
      (insert
        (format "\n%s > ~%s~\n"
          (s-pad-right 20 " " org-link)
          line-content)))
    (message "added link!")))

;; putting in this file to make sure it's after org mode
(when ns/enable-evil-p
  (ns/use-package evil-org "Somelauw/evil-org-mode")
  (require 'evil-org-agenda)
  (require 'evil-org)
  (add-hook 'org-mode-hook 'evil-org-mode)

  (setq evil-org-movement-bindings
    '((up . "e")
       (down . "n")
       (left . "h")
       (right . "l")))

  ;; cf https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
  ;; todo: review textobjects https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org#text-objects
  (setq org-special-ctrl-a/e t)
  (evil-org-set-key-theme '(textobjects navigation))

  ;; todo: review these https://github.com/Somelauw/evil-org-mode/blob/master/evil-org-agenda.el
  (evil-org-agenda-set-keys)
  (evil-define-key 'motion org-agenda-mode-map
    "n" 'org-agenda-next-line
    "e" 'org-agenda-previous-line
    (kbd "C-n") 'org-agenda-next-item
    (kbd "C-e") 'org-agenda-previous-item
    "N" 'org-agenda-priority-down
    "E" 'org-agenda-priority-up
    )

  (general-define-key
    :states '(normal insert)
    :keymaps 'org-mode-map
    ;; should these be switched? I like carrying trees by default I think
    (kbd "C-t") 'org-shiftmetaright
    (kbd "C-d") 'org-shiftmetaleft
    (kbd "C-S-T") 'org-metaright
    (kbd "C-S-D") 'org-metaleft
    (kbd "M-e") 'org-metaup
    (kbd "M-n") 'org-metadown
    )

  (general-define-key
    :states 'normal
    :keymaps 'org-mode-map
    (kbd "E") 'org-toggle-heading))

(use-package org-present
  :config
  (defun ns/org-present-init ()
    (org-display-inline-images)
    (org-present-hide-cursor)
    (org-present-read-only)

    ;; remove the height tweaking we do in (ns/style) so that scaling works right
    (dolist (face '(org-level-1
                     org-level-2
                     org-level-3
                     org-level-4
                     org-level-5
                     org-level-6
                     org-block
                     org-code
                     org-table
                     org-verbatim
                     company-tooltip
                     company-tooltip-common
                     company-tooltip-selection
                     org-block-begin-line
                     org-block-end-line
                     ))
      (set-face-attribute face nil :height 1.0))

    (set-face-attribute 'org-block nil :height 0.7)
    (org-present-big))

  (setq org-present-text-scale 5)

  ;; todo: maybe here presentation mode -- gg for start, G for end
  ;; a 'temp cursor' mode? to click links on slides and stuff
  ;; line wrap should be on/and indent
  (ns/inmap 'org-present-mode-keymap
    "q" 'org-present-quit
    ">" 'org-present-next
    "<" 'org-present-prev
    "n" 'org-present-next
    "e" 'org-present-prev
    "l" 'org-present-next
    "h" 'org-present-prev
    )

  (general-imap :keymaps 'org-present-mode-keymap
    "q" 'org-present-quit)

  ;; todo: a different binding
  ;; (ns/bind "op" 'org-present)

  (defun ns/org-present-quit ()
    (org-present-small)
    (org-remove-inline-images)
    (org-present-show-cursor)
    (org-present-read-write)
    ;; restore our org mode font tweaks
    (ns/style-org))

  (add-hook 'org-present-mode-hook 'ns/org-present-init)
  (add-hook 'org-present-mode-quit-hook 'ns/org-present-quit))

;; allow shell blocks in org mode to be executed:
(org-babel-do-load-languages 'org-babel-load-languages
  '((shell . t)))

;; EG, call with org-babel-execute-src-block on:
;; the ':results output silent' means don't insert into the buffer
;; #+begin_src sh :results output silent
;; <code to execute>
;; #+end_src
(ns/bind-mode 'org "e"
  (fn!
    (cond
      ((org-in-clocktable-p)
        (org-clock-report))
      ((org-in-src-block-p)
        ;; living dangerously
        (let ((org-confirm-babel-evaluate (fn nil)))
          (org-babel-execute-src-block)))
      )))

;; writing niceties:
(use-package olivetti
  :config
  (ns/bind "tf" 'olivetti-mode)
  (setq-default fill-column 100)

  ;; The original value is "\f\\|[      ]*$", so we add the bullets (-), (+), and (*).
  ;; There is no need for "^" as the regexp is matched at the beginning of line.
  (setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")


  ;; words are hard (meaning)
  (use-package mw-thesaurus
    :config
    ;; this binding doesn't work -- org has some cycle agenda files meaning
    ;; (global-set-key (kbd "C-'") #'mw-thesaurus-lookup-at-point)
    )

  ;; words are hard (spelling)
  (use-package flyspell-correct-avy-menu
    :config
    (require 'flyspell-correct-avy-menu)
    (setq flyspell-correct-interface #'flyspell-correct-avy-menu)
    ;; (define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-wrapper)
    ;; (global-set-key (kbd "C-;") #'flyspell-correct-at-point)
    (global-set-key (kbd "C-;")
      (lambda ()
        (when
          (not (and (boundp 'flyspell-mode)
                 flyspell-mode))
          (flyspell-mode))
        (flyspell-correct-wrapper)))))

(defun ns/org-mode-hook ()
  (olivetti-mode)
  (git-gutter-mode 0)
  (ns/set-buffer-face-variable)

  (flyspell-mode 0)

  (setq flyspell-generic-check-word-predicate
    (lambda ()
      ;; are we in an org link? don't check it.
      (if (-contains-p (let ((face (get-char-property (point) 'face)))
                         (if (listp face) face (list face)))
            'org-link)
        nil
        ;; not in an org link, do the usual thing:
        (org-mode-flyspell-verify)
        )))

  (setq mode-line-format nil)
  )

(add-hook 'org-mode-hook 'ns/org-mode-hook)

(defun! ns/style-org ()
  (ns/set-faces-monospace '(org-block
                             org-code
                             org-checkbox
                             org-table
                             org-macro
                             org-formula
                             org-verbatim
                             company-tooltip
                             company-tooltip-common
                             company-tooltip-selection
                             org-block-begin-line
                             org-block-end-line
                             ))

  ;; smol -- consider doing this in the tarp themes
  (set-face-attribute 'org-block-begin-line nil :height 65)
  (set-face-attribute 'org-block-end-line nil :height 65)

  (set-face-attribute 'org-ellipsis nil :underline nil)

  (->>
    `(
       (1 2 3 4 5 6)
       (15 10 5 0 0 0)
       ;; ,(-repeat 6 0)
       )
    (apply #'-interleave)
    (-partition 2)
    (-map (-applify
            (lambda (level height-mod)
              `(set-face-attribute
                 ',(intern (format "org-level-%s" level))
                 nil
                 :height ,(+
                            (plist-get (ns/parse-font (get-resource "st.font")) :height)
                            height-mod)
                 :weight 'semi-bold
                 ;; :weight 'normal
                 ;; :underline t
                 :underline nil
                 ))))
    (-map #'eval))

  (-map #'ns/set-buffer-face-variable (ns/buffers-by-mode 'org-mode))
  )

(defun! ns/jump-to-notes-heading (&optional target-buffer handler)
  "jump to org headlines only within selected files"
  ;; extracted from counsel
  (let* ((buffer-list
           (if target-buffer
             (list (find-file-noselect target-buffer))
             (list
               (when (not (string= org-default-notes-file (buffer-file-name)))
                 (current-buffer))

               (find-file-noselect org-default-notes-file))))
          (entries))
    (dolist (b buffer-list)
      (when b
        (with-current-buffer b
          (when (derived-mode-p 'org-mode)
            (setq entries
              (nconc entries
                (counsel-outline-candidates
                  (cdr (assq 'org-mode counsel-outline-settings))
                  (counsel-org-goto-all--outline-path-prefix))))))))
    (ivy-read "org heading: " entries
      :action (or handler
                (lambda (x)
                  (counsel-org-goto-action x)
                  (ns/org-jump-to-element-content)))
      :caller 'counsel-org-goto-all)))

(defun! ns/org-clock-out ()
  "Clock out of pomodoro, or active clock"
  (when (org-clock-is-active)
    (if (org-pomodoro-active-p)
      (org-pomodoro)
      ;; todo: subtract idle time (right not the loss is small, like 3 min)
      ;; something like org-clockout nil t (- (org-current-time) idle-time)
      (org-clock-out))
    (ns/with-notes (save-buffer))))

(ns/bind
  "oo" (fn!  (let* ((buffer-file-name (buffer-file-name))
                     (project-notes (if buffer-file-name
                                      (concat (projectile-root-bottom-up buffer-file-name) "notes.org") org-default-notes-file)))
               ;; todo: this doesn't work if the notes file isn't already open? what?
               (ns/find-or-open (if (and (f-exists-p project-notes)
                                      (not (string= buffer-file-name project-notes)))
                                  project-notes org-default-notes-file))))

  ;; "os" (fn! (ns/org-goto-active "standups"))
  "os" 'org-sort
  "of" 'ns/org-goto-active
  "oF" (fn!
         (ns/org-clock-out)
         (ns/org-goto-active))

  "oc" (fn! (if (use-region-p)
              (ns/capture-current-region)
              (org-capture)))

  ;; "or" 'org-refile
  "ol" 'ns/make-org-link-to-here
  ;; "om" 'ns/insert-mark-org-links
  "ow" (fn! (widen) (recenter))
  "on" 'org-narrow-to-subtree
  "oa" 'org-agenda

  ;; trash? idk
  "ot" 'org-archive-subtree

  ;; this is just a nice homerow roll on my layout

  ;; clock into the current headline, clocking out of what's running
  "oi" (fn!
         (ns/org-clock-out)
         (org-clock-in)
         (save-buffer))

  ;; cancel org-pomodoro or a clocked in task
  "oI" 'ns/org-clock-out

  "no" #'ns/jump-to-notes-heading)


(ns/bind-mode 'org
  "or" (fn! (if (use-region-p)
              (ns/capture-current-region)
              (ns/capture-current-subtree)))

  ;; "org move"
  "om" 'org-refile

  "op" 'org-pomodoro
  )

(ns/bind-leader-mode
  'org
  "," 'org-ctrl-c-ctrl-c
  "t" 'org-todo
  "T" 'org-show-todo-tree
  "v" 'org-mark-element
  "a" 'org-agenda
  ;; todo: want a "jump to parent"
  "f" 'ns/org-set-unique-property

  "F"
  ;; (fn! (ns/org-set-unique-property (read-string "property name: ")))
  (fn! (org-set-property (read-string "property name: ") (read-string "value: ")))

  "b" (fn! (org-set-property "blog_slug" (read-string "slug: ")))

  "s" (fn! (org-set-property "share" "t"))
  )

(add-to-list 'auto-mode-alist '("qutebrowser-editor-" . org-mode))
