;; -*- lexical-binding: t; -*-
;; note: org package is loaded in dirt

(require 'org-fold)
(require 'org-habit)
(require 'org-tempo)

(ns/use org-ml)

(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

;; config
(setq-ns org
  directory (if (f-exists-p (~ "sync/main/notes"))
              (~ "sync/main/notes")
              (~ "notes"))

  default-notes-file  (concat org-directory "/notes.org")
  default-diary-file  (concat org-directory "/journal.org")
  default-habits-file (concat org-directory "/habits.org")

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
  todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                   (sequence "WAITING(w@/!)" "INACTIVE(i@/!)" "|" "CANCELLED(c@/!)" "MEETING"))

  blank-before-new-entry '((heading . t) (plainlist-item . nil))
  tag-alist '(("work" . ?w)
               ("personal" . ?P)
               ("emacs" . ?e)
               ("dotfiles" . ?d)
               ("project" . ?p)
               ("active" . ?a)
               ("inactive" . ?i))

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

;; todo -- nested with-notes might be weird/can self-cancel?
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

(defun ns/org-get-clock-marker ()
  ;; snippet extracted from org-clock-goto
  (cond
    ((org-clocking-p) org-clock-marker)
    ((and org-clock-goto-may-find-recent-task
       (car org-clock-history)
       (marker-buffer (car org-clock-history)))
      (setq recent t)
      (car org-clock-history))
    (t (user-error "No active or recent clock task"))))

;; todo: rethink this
(defun! ns/org-get-active-point (&optional property)
  "Resolves to a point in my big notes files to either:
- currently clocked in org headline
- the first TODO under the headline with a 'focus' property
- if no TODO is found, just go to the headline with a 'focus' property directly
(nb: the 'focus' property target may be overridden with an argument)
"
  (ns/with-notes
    (save-window-excursion
      (if (and org-clock-current-task
            (not (string= org-clock-current-task "misc")))
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

(ns/use org-pomodoro
  (setq ns/macos-vol (if ns/enable-mac-p (ns/shell-exec "macos-vol get") 0))
  (defun ns/toggle-music (action)
    (let ((target (or (executable-find "player.sh") "mpc")))
      (ns/shell-exec-dontcare (concat target " " action))))

  ;; named functions so we don't append lambdas on init reload
  (defun ns/toggle-music-play () (ns/toggle-music "play"))
  (defun ns/toggle-music-pause () (ns/toggle-music "pause"))

  (when ns/enable-mac-p
    (defun! ns/toggle-music-play ()
      (ns/shell-exec-dontcare (format "macos-vol setvol %s" ns/macos-vol)))
    (defun! ns/toggle-music-pause ()
      (setq ns/macos-vol (ns/shell-exec "macos-vol get"))
      (ns/shell-exec-dontcare "macos-vol setvol 0")))

  (defun! ns/focus-mode-enter ()
    (ns/toggle-music-play)

    (when ns/enable-home-p
      (ns/shell-exec-dontcare "dunstctl set-paused true")
      (ns/shell-exec-dontcare "panelt stop")

      (f-write (f-read (~ ".config/qutebrowser/adblock_bad.txt"))
        'utf-8 (~ ".config/qutebrowser/adblock.txt"))
      (ns/shell-exec-dontcare "qb_command :adblock-update")))

  (defun! ns/focus-mode-quit ()
    ;; todo: prompt about are you SURE you want to quit
    (ns/toggle-music-pause)

    (when ns/enable-home-p
      (ns/shell-exec-dontcare "dunstctl set-paused false")
      (ns/shell-exec-dontcare "panelt start"))

    (f-write "" 'utf-8 (~ ".config/qutebrowser/adblock.txt"))
    (ns/shell-exec-dontcare "qb_command :adblock-update"))

  (add-hook 'org-pomodoro-extend-last-clock 'ns/focus-mode-enter)
  (add-hook 'org-pomodoro-started-hook 'ns/focus-mode-enter)
  (add-hook 'org-pomodoro-finished-hook 'ns/focus-mode-quit)
  (add-hook 'org-pomodoro-killed-hook 'ns/focus-mode-quit)

  (add-hook 'org-pomodoro-break-finished-hook 'ns/toggle-music-play)
  (add-hook 'org-pomodoro-break-finished-hook 'ns/focus-mode-enter))

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

  (let* ((props (second (org-ml-parse-this-headline)))
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
  (recenter))

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
;; (when ns/enable-evil-p
;;   (ns/use evil-org :straight (:host github :repo "Somelauw/evil-org-mode"))
;;   (require 'evil-org-agenda)
;;   (require 'evil-org)
;;   (add-hook 'org-mode-hook 'evil-org-mode)

;;   (setq evil-org-movement-bindings
;;     '((up . "e")
;;        (down . "n")
;;        (left . "h")
;;        (right . "l")))

;;   ;; cf https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
;;   ;; todo: review textobjects https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org#text-objects
;;   (setq org-special-ctrl-a/e t)
;;   (evil-org-set-key-theme '(textobjects navigation))

;;   ;; todo: review these https://github.com/Somelauw/evil-org-mode/blob/master/evil-org-agenda.el
;;   (evil-org-agenda-set-keys)
;;   (evil-define-key 'motion org-agenda-mode-map
;;     "n" 'org-agenda-next-line
;;     "e" 'org-agenda-previous-line
;;     (kbd "C-n") 'org-agenda-next-item
;;     (kbd "C-e") 'org-agenda-previous-item
;;     "N" 'org-agenda-priority-down
;;     "E" 'org-agenda-priority-up
;;     )

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
  (kbd "E") 'org-toggle-heading)

;; )

(ns/use org-present
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
(ns/use olivetti
  (ns/bind "tf" 'olivetti-mode)
  (setq-default fill-column 80)

  (setq olivetti-body-width 0.4)
  (setq olivetti-body-width nil)

  ;; The original value is "\f\\|[      ]*$", so we add the bullets (-), (+), and (*).
  ;; There is no need for "^" as the regexp is matched at the beginning of line.
  (setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")


  ;; words are hard (meaning)
  (ns/use mw-thesaurus
    ;; this binding doesn't work -- org has some cycle agenda files meaning
    ;; (global-set-key (kbd "C-'") #'mw-thesaurus-lookup-at-point)
    )

  ;; words are hard (spelling)
  (ns/use flyspell-correct-avy-menu
    (require 'flyspell-correct-avy-menu)
    (setq flyspell-correct-interface #'flyspell-correct-avy-menu)

    (defun! ns/spellcheck-at-point ()
      (when-not (and (boundp 'flyspell-mode)
                  flyspell-mode)
        (flyspell-mode 1)
        (flyspell-word))
      (flyspell-correct-at-point))

    ;; todo: this is not bound, something overriding, correct it
    (global-set-key (kbd "C-;") 'ns/spellcheck-at-point)
    (general-nmap "gs" 'ns/spellcheck-at-point)

    ))

(defun ns/org-mode-hook ()
  (olivetti-mode)
  (git-gutter-mode 0)
  (flyspell-mode 0)

  (setq flyspell-generic-check-word-predicate
    (lambda ()
      ;; don't spellcheck links
      (when-not (-contains-p (-list (get-char-property (point) 'face))
                  'org-link)
        (org-mode-flyspell-verify))))

  ;; (setq mode-line-format nil)
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
                            (plist-get (ns/parse-font (get-resource "font.mono.spec")) :height)
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
      (llet [idle-start-time (seconds-to-time
                               (ts-unix
                                 (ts-adjust 'second
                                   (- (org-user-idle-seconds))
                                   (ts-now))))]
        (org-clock-out nil t idle-start-time)))


    (ns/with-notes (save-buffer))))

(defun! ns/org-clock-in ()
  "Clock into headline at point"
  (ns/org-clock-out)
  (org-clock-in)
  (save-buffer))

(ns/bind
  "oo" (fn!  (let* ((buffer-file-name (buffer-file-name))
                     (project-notes (if buffer-file-name
                                      (concat (projectile-root-bottom-up buffer-file-name) "notes.org") org-default-notes-file)))
               ;; todo: this doesn't work if the notes file isn't already open? what?
               (ns/find-or-open (if (and (f-exists-p project-notes)
                                      (not (string= buffer-file-name project-notes)))
                                  project-notes org-default-notes-file))))

  "os" 'org-sort
  "of" 'ns/org-goto-active
  "oF" (fn!
         (ns/org-clock-out)
         (ns/org-goto-active))

  "oc" (fn! (if (use-region-p)
              ;; todo: currently ns/capture-current-region is very opinionated it should maybe allow you
              ;; to SEE what you are capturing instead of defaulting to refile-like behavior
              (ns/capture-current-region)
              (org-capture)))

  "ol" 'ns/make-org-link-to-here
  ;; "om" 'ns/insert-mark-org-links
  "ow" (fn! (widen) (recenter))
  "on" 'org-narrow-to-subtree
  "oa" 'org-agenda

  ;; trash? idk
  "ot" 'org-archive-subtree

  ;; this is just a nice homerow roll on my layout
  "oi" 'ns/org-clock-in
  "oI" 'ns/org-clock-out

  ;; todo: jump to heading general markup as well (markdown, adoc)
  "no" #'ns/jump-to-notes-heading)


(ns/bind-mode 'org
  "or" (fn! (if (use-region-p)
              (ns/capture-current-region)
              (ns/capture-current-subtree)))

  ;; "org move"
  "om" 'org-refile

  "op" 'org-pomodoro

  ;; query org element
  "qo" (fn! (-> (point) org-ml-parse-element-at org-ml-get-type pr-str message)))


(ns/bind-leader-mode
  'org
  "," 'org-ctrl-c-ctrl-c
  "t" 'org-todo
  "T" 'org-show-todo-tree
  "v" 'org-mark-element
  "a" 'org-agenda
  "f" 'ns/org-set-unique-property
  "F" (fn! (org-set-property (read-string "property name: ") (read-string "value: ")))

  "b" (fn! (org-set-property "blog_slug" (read-string "slug: ")))

  "s" (fn! (org-set-property "share" "t"))
  )

(add-to-list 'auto-mode-alist '("qutebrowser-editor-" . org-mode))
