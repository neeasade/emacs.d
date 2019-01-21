(use-package org
  :straight (:host github
              :repo "emacsmirror/org"
              :files ("lisp/*.el" "contrib/lisp/*.el")))

(when ns/enable-evil-p
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle))

(setq-ns org
  directory (~ "notes")
  agenda-files (list org-directory)
  default-notes-file  (concat org-directory "/notes.org")
  default-diary-file  (concat org-directory "/journal.org")
  default-habits-file  (concat org-directory "/habits.org")

  ellipsis "_"
  startup-indented nil
  startup-folded t
  src-fontify-natively t
  startup-align-all-tables t
  html-checkbox-type 'html
  export-with-section-numbers nil

  ;; days before expiration where a deadline becomes active
  deadline-warn-days 14
  todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "INACTIVE(i@/!)" "|" "CANCELLED(c@/!)" "MEETING"))

  blank-before-new-entry '((heading . t) (plainlist-item . nil))
  tag-alist '(
               ("test" . ?t)
               ("endtest" . ?e)
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

  ;; todo: a timer that checks that you are not in pomodoro mode and alerts every once in awhile

  ;; capture
  ;; todo: into templates
  capture-templates
  `(
     ("t" "Todo" entry (file+olp ,org-default-notes-file "Inbox" "Tasks") "* TODO %^{todo}" :prepend t :immediate-finish t)
     ("T" "Todo with details" entry (file+olp ,org-default-notes-file "Inbox" "Tasks") "* TODO %^{todo}" :prepend t)
     ("i" "Idea" entry (file+olp ,org-default-notes-file "Inbox" "Ideas") "* %^{idea}" :prepend t :immediate-finish t)
     ("I" "Idea with details" entry (file+olp ,org-default-notes-file "Inbox" "Ideas") "* %^{idea}" :prepend t)
     ("r" "Reminder" entry (file+olp ,org-default-notes-file "Inbox" "Reminders") "* %i%? \n %U")
     ("j" "Journal" entry (file+datetree ,org-default-diary-file) "* %?\n%U\n" :clock-in t :clock-resume t)
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
  "p" 'org-pomodoro
  "f" 'ns/org-set-active
  "b" 'ns/org-open-url
  )

;; give us easy templates/tab completion like yasnippet and the like
;; form is '<<key><tab>', eg <s<tab> expands to src block
;; todo: reference what all this gives us: https://orgmode.org/manual/Easy-templates.html
(require 'org-tempo)

(defcommand org-open-url() (browse-url (org-entry-get nil "url")))

(defcommand org-set-active()
  (org-delete-property-globally "focus")
  (org-set-property "focus" "t")

  (setq ns/org-active-story (substring-no-properties (org-get-heading t t t t)))
  )

;; for externals to call into
(defun ns/org-get-active()
  (if (not (bound-and-true-p ns/org-active-story))
    (progn
      (ns/org-goto-active)
      (ns/org-set-active))
    ns/org-active-story
    ))

(defcommand org-goto-active()
  (ns/find-or-open org-default-notes-file)
  (goto-char (org-find-property "focus"))
  (org-show-context)
  (org-show-subtree)
  (ns/focus-line))

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
(defcommand make-org-link-to-here ()
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

;; todo: use this more it's cool
;; todo: consider removing marks after inserting org link
(defcommand insert-mark-org-links ()
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
  "oo" (fn! (ns/find-or-open org-default-notes-file))
  "of" 'ns/org-goto-active
  "oc" 'org-capture
  "or" 'org-refile
  "ol" 'ns/make-org-link-to-here
  "om" 'ns/insert-mark-org-links
  "ow" 'widen
  "on" 'org-narrow-to-subtree

  "no" 'counsel-org-goto-all
  )

(add-hook 'org-mode-hook 'ns/set-buffer-face-variable)
(add-hook 'org-mode-hook 'org-indent-mode)

(advice-add #'ns/style :after #'ns/style-org)
(defun ns/style-org ()
  (ns/set-faces-monospace '(org-block org-code org-table company-tooltip company-tooltip-common company-tooltip-selection))

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

  (when ns/enable-colemak
    (setq evil-org-movement-bindings
      '((up . "e")
         (down . "n")
         (left . "h")
         (right . "l"))))

  ;; cf https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
  ;; todo: review textobjects https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org#text-objects
  (setq org-special-ctrl-a/e t)
  (evil-org-set-key-theme '(textobjects navigation))

  ;; todo: consider mapping: org-insert-todo-heading
  (general-define-key
    :states '(normal insert)
    :keymaps 'org-mode-map
    ;; should these be switched? I like carrying trees by default
    (kbd "C-t") 'org-shiftmetaright
    (kbd "C-d") 'org-shiftmetaleft
    (kbd "C-S-T") 'org-metaright
    (kbd "C-S-D") 'org-metaleft
    (kbd "E") 'org-toggle-heading))
