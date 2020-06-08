;; -*- lexical-binding: t; -*-

(use-package org
  :straight (:host github
              :repo "emacsmirror/org"
              :files ("lisp/*.el" "contrib/lisp/*.el")))

(require 'org-habit)

(when ns/enable-evil-p
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle))

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

  ellipsis "_"

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

  ;; todo: a timer that checks that you are not in pomodoro mode and alerts every once in awhile

  ;; new setup outline
  ;; todo: edna
  ;; :TRIGGER:  next-sibling chain!("TRIGGER")  todo!("NEXT") self delete-property!("TRIGGER")
  ;; * Inbox
  ;; ** Tasks
  ;; ** Ideas
  ;; ** Reminders

  ;; cf https://orgmode.org/manual/Capture-templates.html#Capture-templates
  ;; todo: elisp function to capture from qutebrowser inbox url to browse later
  capture-templates
  `(
     ("t" "Todo" entry
       (file+olp ,org-default-notes-file "Inbox" "Tasks")
       "* TODO %^{todo}" :prepend t :immediate-finish t)
     ("T" "Todo with details" entry (file+olp ,org-default-notes-file "Inbox" "Tasks") "* TODO %i%?" :prepend t)
     ("i" "Idea" entry (file+olp ,org-default-notes-file "Inbox" "Ideas") "* %^{idea}" :prepend t :immediate-finish t)
     ("I" "Idea with details" entry (file+olp ,org-default-notes-file "Inbox" "Ideas") "* %i%?" :prepend t)
     ("r" "Reminder" entry (file+olp ,org-default-notes-file "Inbox" "Periodic Reminders") "* %i%? \n %t")
     ("j" "Journal" entry (file+datetree ,org-default-diary-file) "* %?\n%U\n" :clock-in t :clock-resume t)
     ("n" "Note" entry (file+olp ,org-default-notes-file "notes") "* %i%?" :prepend t)
     )

  ;; current file or any of the agenda-files, max 9 levels deep
  refile-targets '((nil :maxlevel . 9)
                    (org-agenda-files :maxlevel . 9))

  outline-path-complete-in-steps nil         ; Refile in a single go
  refile-use-outline-path t                  ; Show full paths for refiling
  )

(ns/bind-leader-mode
  'org
  "," 'org-ctrl-c-ctrl-c
  "t" 'org-todo
  "T" 'org-show-todo-tree
  "v" 'org-mark-element
  "a" 'org-agenda
  "l" 'evil-org-open-links
  ;; "p" 'org-pomodoro
  "f" 'ns/org-set-active
  "b" 'ns/org-open-url
  )

(ns/bind-mode 'org "op" 'org-pomodoro)

;; give us easy templates/tab completion like yasnippet and the like
;; form is '<<key><tab>', eg <s<tab> expands to src block
;; todo: reference what all this gives us: https://orgmode.org/manual/Easy-templates.html
(require 'org-tempo)

(defun! ns/org-open-url() (browse-url (org-entry-get nil "url")))

;; todo: get om.el for some of this
;; https://github.com/ndwarshuis/om.el
(defun! ns/org-set-active()
  (org-delete-property-globally "focus")
  (org-set-property "focus" "t")

  (setq ns/org-active-story (substring-no-properties (org-get-heading t t t t)))
  )

;; for externals to call into
(defun ns/org-get-active()
  (if (not (bound-and-true-p ns/org-active-story))
    (save-window-excursion
      (ns/org-goto-active)
      (ns/org-set-active))
    (s-clean ns/org-active-story)))

(defun! ns/org-goto-active()
  (ns/find-or-open org-default-notes-file)

  (if org-clock-current-task
    (org-clock-goto)
    (goto-char (org-find-property "focus")))

  (org-show-context)
  (org-show-subtree)

  (ns/focus-line)
  )

(use-package org-pomodoro
  :config
  (defun ns/toggle-music (action)
    (shell-command (format "%s %s" (if ns/enable-home-p "player.sh" "mpc") action)))

  (add-hook 'org-pomodoro-started-hook
    (apply-partially #'ns/toggle-music "play"))

  (add-hook 'org-pomodoro-break-finished-hook
    (apply-partially #'ns/toggle-music "play"))

  (add-hook 'org-pomodoro-finished-hook
    (apply-partially #'ns/toggle-music "pause")))

;; insert an org link to the current location on the focused heading in notes.org
(defun! ns/make-org-link-to-here ()
  (let* ((line (number-to-string (line-number-at-pos)))
          (link (format "file:%s::%s" (buffer-file-name) line))
          ;; (label (format "%s:%s" (buffer-name) line))
          (filepath (s-replace (s-replace "\\" "/" (~ "")) "~/" (buffer-file-name)))
          (label (format "%s:%s" filepath line))
          (org-link (format "[[%s][%s]]\n" link label)))

    (with-current-buffer (find-file-noselect org-default-notes-file)
      (save-excursion
        (goto-char (org-find-property "focus"))
        ;; cf https://stackoverflow.com/questions/52121961/emacs-org-mode-insert-text-after-heading-properties
        (goto-char (org-element-property :contents-begin (org-element-at-point)))
        (let ((first-element (org-element-at-point)))
          (when (eq 'property-drawer (car first-element))
            (goto-char (org-element-property :end first-element))))

        (insert org-link)))))

(defun! ns/evil-delete-marks ()
  (evil-delete-marks "ABCDEFGHIJKLMNOPQRSTUPWXYZ"))

;; neat idea, but I never use this
(defun! ns/insert-mark-org-links ()
  (setq ns/markers
    (append (cl-remove-if (lambda (m)
                            (or (evil-global-marker-p (car m))
                              (not (markerp (cdr m)))))
              evil-markers-alist)
      (cl-remove-if (lambda (m)
                      (or (not (evil-global-marker-p (car m)))
                        (not (markerp (cdr m)))))
        (default-value 'evil-markers-alist))))

  ;; remove automatic marks
  (dolist (key '(40 41 94 91 93))
    (setq ns/markers (delq (assoc key ns/markers) ns/markers)))

  (insert (s-join "\n"
            (mapcar (lambda(mark)
                      (let ((file (buffer-file-name (marker-buffer mark)))
                             (linenumber
                               (with-current-buffer (marker-buffer mark)
                                 (line-number-at-pos (marker-position mark)))))
                        (concat "[[file:" file "::" (number-to-string linenumber) "]]")))
              (mapcar 'cdr ns/markers)))))

(ns/bind
  "oo" (fn! (let ((project-notes (concat (projectile-root-bottom-up
                                           (buffer-file-name)) "notes.org")))
              (ns/find-or-open
                (if (and (f-exists-p project-notes)
                      (not (string= (buffer-file-name) project-notes)))
                  project-notes
                  org-default-notes-file))))
  "of" 'ns/org-goto-active
  "oc" 'org-capture
  "or" 'org-refile
  "ol" 'ns/make-org-link-to-here
  "om" 'ns/insert-mark-org-links
  "ow" (fn! (widen) (ns/focus-line))
  "on" 'org-narrow-to-subtree
  "oa" 'org-agenda

  "no" (fn!
         (counsel-org-goto-all)
         (org-show-context)
         (org-show-siblings)
         (org-show-subtree)
         (ns/focus-line))
  )

(add-hook 'org-mode-hook 'ns/set-buffer-face-variable)
(add-hook 'org-mode-hook 'flyspell-mode)

(defun! ns/style-org ()
  (ns/set-faces-monospace '(org-block
                             org-code
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

  (set-face-attribute 'org-block-begin-line nil :height 65)
  (set-face-attribute 'org-block-end-line nil :height 65)

  (let ((height (plist-get (ns/parse-font (get-resource "st.font")) :height)))
    (set-face-attribute 'org-level-1 nil :height (+ height 15) :weight 'semi-bold)
    (set-face-attribute 'org-level-2 nil :height (+ height 10) :weight 'semi-bold)
    (set-face-attribute 'org-level-3 nil :height (+ height 5) :weight 'semi-bold)
    (set-face-attribute 'org-level-4 nil :height height :weight 'semi-bold))

  (dolist (b (ns/buffers-by-mode 'org-mode))
    (with-current-buffer b (ns/set-buffer-face-variable))))

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

  ;; todo: consider mapping: org-insert-todo-heading
  (general-define-key
    :states '(normal insert)
    :keymaps 'org-mode-map
    ;; should these be switched? I like carrying trees by default I think
    (kbd "C-t") 'org-shiftmetaright
    (kbd "C-d") 'org-shiftmetaleft
    (kbd "C-S-T") 'org-metaright
    (kbd "C-S-D") 'org-metaleft)

  (general-define-key
    :states 'normal
    :keymaps 'org-mode-map
    (kbd "E") 'org-toggle-heading))

;; notify on timestamps (this is broken)
;; (ns/use-package org-wild-notifier "akhramov/org-wild-notifier.el"
;;   :config
;;   ;; org-wild-notifier-en
;;   (setq-ns org-wild-notifier
;;     alert-time 1 ;; min
;;     notification-title "Reminder"
;;     keyword-whitelist '()
;;     keyword-blacklist '()
;;     tags-whitelist '("testerino")
;;     tags-blacklist nil
;;     alert-times-property "alert_times")

;;   (org-wild-notifier-mode)
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

;; standalone capture experience
(defun! ns/org-capture-popup ()
  (ns/shell-exec-dontcare "popup_window.sh -r")
  (select-frame (make-frame '((name . "org-protocol-capture"))))
  (org-capture))

;; cf https://fuco1.github.io/2017-09-02-Maximize-the-org-capture-buffer.html
(defvar ns/my-org-capture-before-config nil)

(defadvice org-capture (before save-config activate)
  "Save the window configuration before `org-capture'."
  (setq ns/my-org-capture-before-config (current-window-configuration)))

(add-hook 'org-capture-mode-hook 'delete-other-windows)

(defun my-org-capture-cleanup ()
  "Clean up the frame created while capturing via org-protocol."
  (when ns/my-org-capture-before-config
    (set-window-configuration ns/my-org-capture-before-config))

  (-when-let ((&alist 'name name) (frame-parameters))
    (when (equal name "org-protocol-capture")
      (delete-frame))))

(add-hook 'org-capture-after-finalize-hook 'my-org-capture-cleanup)
