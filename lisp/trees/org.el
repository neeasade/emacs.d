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

  agenda-files
  ;; all the files in our org directory
  (f-entries org-directory (fn (s-ends-with-p ".org" <>))  t)

  ;; weeks start on mondays
  agenda-start-on-weekday 1

  ellipsis "--"

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

(defmacro ns/with-notes (&rest body)
  `(with-current-buffer (find-file-noselect org-default-notes-file)
	   (save-window-excursion
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
  ;; something to consider: organize by date, since we mark that with capture targets
  ;; then just treat dateless as priority above dated
  )

(defun! ns/org-get-active-point (&optional property)
  "Resolves to a point in my big notes files to either:
- currently clocked in org headline
- the first TODO under the headline with a 'focus' property
- if no TODO is found, just go to the headline with a 'focus' property directly
(nb: the 'focus' property target may be overridden with an argument)
"
  (ns/with-notes
    (if org-clock-current-task
      (org-clock-goto)
      (->> (org-find-property (or property "focus"))
        (org-ml-parse-headline-at)
        (ns/notes-current-standup-task)
        cadr
        ((lambda (props)
           (or (plist-get props :contents-begin)
             (plist-get props :begin))))
        (goto-char)))
    (point)))

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
      "lobste.rs, nixers.net, old.reddit.com, reddit.com, www.reddit.com, web.telegram.org, forums.somethingawful.com, somethingawful.com, digg.com, break.com, news.ycombinator.com, infoq.com, bebo.com, api.twitter.com, zulipchat.com, twitter.com, facebook.com, blip.com, youtube.com, vimeo.com, delicious.com, flickr.com, friendster.com, hi5.com, linkedin.com, livejournal.com, meetup.com, myspace.com, plurk.com, stickam.com, stumbleupon.com, yelp.com, slashdot.org, plus.google.com, hckrnews.com, kongregate.com, newgrounds.com, addictinggames.com, hulu.com"
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

  ;; (org-show-context)
  ;; (org-show-siblings)
  ;; (org-show-subtree)

  (org-show-all)

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
          line-content)))))

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

;; (unload-feature 'org-wild-notifier)
;; (require 'org-wild-notifier)

;; notify on timestamps (this is broken)
;; (ns/use-package org-wild-notifier "akhramov/org-wild-notifier.el"
;;   :config
;;   ;; org-wild-notifier-en
;;   (setq-ns org-wild-notifier
;;     alert-time '(1) ;; min
;;     notification-title "scheduled notifier"
;;     ;; keyword-whitelist '()
;;     ;; keyword-blacklist '()
;;     ;; tags-whitelist '("testerino")
;;     ;; tags-blacklist nil
;;     ;; alert-times-property "alert_times"
;;     )

;;   (org-wild-notifier-mode)
;;   (org-wild-notifier-check)

;;   )

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
    (global-set-key (kbd "C-;") #'flyspell-correct-wrapper)

    )
  )

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

  ;; smol
  (set-face-attribute 'org-block-begin-line nil :height 65)
  (set-face-attribute 'org-block-end-line nil :height 65)

  (set-face-attribute 'org-link nil :underline t)
  ;; (set-face-attribute 'org-link nil :foreground nil)
  ;; (set-face-attribute 'underline nil :underline t)

  (let ((height (plist-get (ns/parse-font (get-resource "st.font")) :height)))
    (set-face-attribute 'org-level-1 nil :height (+ height 15) :weight 'semi-bold)
    (set-face-attribute 'org-level-2 nil :height (+ height 10) :weight 'semi-bold)
    (set-face-attribute 'org-level-3 nil :height (+ height 5) :weight 'semi-bold)
    (set-face-attribute 'org-level-4 nil :height height :weight 'semi-bold)
    (set-face-attribute 'org-level-5 nil :height height :weight 'semi-bold)
    (set-face-attribute 'org-level-6 nil :height height :weight 'semi-bold)
    )

  (dolist (b (ns/buffers-by-mode 'org-mode))
    (with-current-buffer b
      (ns/set-buffer-face-variable))))

(defun! ns/jump-to-notes-heading ()
  "jump to org headlines only within selected files"
  (let* ((buffer-list
           `(
              ,(when (and
                       (eq major-mode 'org-mode)
                       (not (string= org-default-notes-file (buffer-file-name))))
                 (current-buffer))

              ;; two different MO's
              ,(find-file-noselect org-default-notes-file)
              ;; ,@(ns/buffers-by-mode 'org-mode)
              ))
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
    (ivy-read "Goto: " entries
      :history 'counsel-org-goto-history
      :action #'counsel-org-goto-action
      :caller 'counsel-org-goto-all)))

(ns/bind
  "oo" (fn!  (let* ((buffer-file-name (buffer-file-name))
                     (project-notes (if buffer-file-name
                                      (concat (projectile-root-bottom-up buffer-file-name) "notes.org") org-default-notes-file)))
               ;; todo: this doesn't work if the notes file isn't already open? what?
               (ns/find-or-open (if (and (f-exists-p project-notes)
                                      (not (string= buffer-file-name project-notes)))
                                  project-notes org-default-notes-file))))

  "os" (fn! (ns/org-goto-active "standups"))
  "of" 'ns/org-goto-active
  "oF" (fn!
         (org-clock-out nil t)
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
         (when (org-clock-is-active)
           (if (org-pomodoro-active-p)
             (org-pomodoro)
             (org-clock-out)))
         (org-clock-in))

  ;; cancel org-pomodoro or a clocked in task
  "oI" (fn! (if (org-pomodoro-active-p)
              (org-pomodoro)
              (org-clock-out)))

  "no" (fn!
         (ns/jump-to-notes-heading)
         ;; (counsel-org-goto-all)
         (ns/org-jump-to-element-content)))


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
  "F" (fn! (ns/org-set-unique-property (read-string "property name: ")))
  "s" 'org-schedule
  )

(add-to-list 'auto-mode-alist '("qutebrowser-editor-" . org-mode))
