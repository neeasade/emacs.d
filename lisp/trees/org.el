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

  ;; current file or any of the agenda-files, max 9 levels deep
  refile-targets '((nil :maxlevel . 9)
                    (org-agenda-files :maxlevel . 9))

  outline-path-complete-in-steps nil         ; Refile in a single go
  refile-use-outline-path t                  ; Show full paths for refiling

  ;; this way we don't haev to create annoying outlines
  refile-allow-creating-parent-nodes 'confirm
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
  )

(ns/bind-mode 'org "op" 'org-pomodoro)

;; give us easy templates/tab completion like yasnippet and the like
;; form is '<<key><tab>', eg <s<tab> expands to src block
;; todo: reference what all this gives us: https://orgmode.org/manual/Easy-templates.html
(require 'org-tempo)

(defun! ns/org-set-unique-property (&optional property value)
  (let ((prop (or property "focus")))
    (org-delete-property-globally prop)
    (org-set-property prop (or value "t"))))

(defun! ns/org-goto-active (&optional property)
  "Go to the currently clocked in task, or the next task under PROPERTY"
  (find-file org-default-notes-file)

  (if org-clock-current-task
    (progn (org-clock-goto)
      (ns/org-jump-to-element-content))
    (->> (org-find-property (or property "focus"))
      (om-parse-headline-at)
      (ns/notes-current-standup-task) cadr
      ((lambda (props)
         (or (plist-get props :contents-begin)
           (plist-get props :begin))))
      (goto-char)))

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

  (defun ns/pomodoro-start-hook ()
    (ns/toggle-music-play)
    (ns/shell-exec-dontcare "notify-send DUNST_COMMAND_PAUSE"))

  (defun ns/pomodoro-finish-hook ()
    (ns/toggle-music-pause)
    (ns/shell-exec-dontcare "notify-send DUNST_COMMAND_RESUME"))

  (add-hook 'org-pomodoro-started-hook 'ns/pomodoro-start-hook)
  (add-hook 'org-pomodoro-finished-hook 'ns/pomodoro-finish-hook)
  (add-hook 'org-pomodoro-break-finished-hook 'ns/toggle-music-play))


(defun ns/org-jump-to-element-content ()
  "Jump from a anywhere in a headline to the start of it's content"
  ;; org mode is cursed

  ;; (org-show-context)
  ;; (org-show-siblings)
  ;; (org-show-subtree)
  (org-show-all)

  (let* ((props (cadr (om-parse-this-headline)))
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
        (when
          (s-starts-with-p "*" (thing-at-point 'line))
          (evil-open-above 1)
          (evil-normal-state)
          ))))

  ;; todo: consider ensuring drawers are collapsed after this
  (ns/focus-line)
  )

;; insert an org link to the current location on the focused heading in notes.org
(defun! ns/make-org-link-to-here ()
  (let* ((line-content (s-trim (thing-at-point 'line)))
          (line (number-to-string (line-number-at-pos)))
          (link (format "file:%s::%s" (buffer-file-name) line))
          ;; (label (format "%s:%s" (buffer-name) line))
          (filepath (s-replace (s-replace "\\" "/" (~ "")) "~/" (buffer-file-name)))
          (label (format "%s:%s" filepath line))
          (org-link (format "[[%s][%s]]" link label)))

    (with-current-buffer (find-file-noselect org-default-notes-file)
      ;; todo: save excursion is not working here?
      (save-excursion
        (if org-clock-current-task
          (org-clock-goto)
          ;; todo: this should instead be the same logic as elisp_org_task?
          (goto-char (org-find-property (or property "focus"))))

        (ns/org-jump-to-element-content)
        
        ;; file link staleness concerns:
        ;; could also store checksum of file?
        ;; maybe note the date?
        ;; maybe just have a validation function that checks that the link content matches/red flag
        (insert
          (format "%s : ~%s~\n"
            (s-pad-right 20 " " org-link)
            line-content))))))

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
  "oo" (fn!  (let* ((buffer-file-name (buffer-file-name))
                     (project-notes (if buffer-file-name
                                      (concat (projectile-root-bottom-up buffer-file-name) "notes.org") org-default-notes-file)))
               ;; todo: this doesn't work if the notes file isn't already open? what?
               (ns/find-or-open (if (and (f-exists-p project-notes)
                                      (not (string= buffer-file-name project-notes)))
                                  project-notes org-default-notes-file))))
  "of" 'ns/org-goto-active
  "oc" 'org-capture
  ;; "or" 'org-refile
  "ol" 'ns/make-org-link-to-here
  ;; "om" 'ns/insert-mark-org-links
  "ow" (fn! (widen) (ns/focus-line))
  "on" 'org-narrow-to-subtree
  "oa" 'org-agenda

  "no" (fn!
         (counsel-org-goto-all)
         (ns/org-jump-to-element-content)))


(add-hook 'org-mode-hook 'ns/set-buffer-face-variable)
;; (add-hook 'org-mode-hook 'flyspell-mode)

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

(setq org-archive-subtree-save-file-p t) ; save target buffer after archiving
