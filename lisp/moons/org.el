(use-package org
  :straight (:host github
                   :repo "emacsmirror/org"
                   :files ("lisp/*.el" "contrib/lisp/*.el"))

  :config
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

  (setq-ns org
	   directory (~ "notes")
	   agenda-files (list org-directory)
	   default-notes-file  (concat org-directory "/notes.org")
	   default-diary-file  (concat org-directory "/diary.org")
	   default-habits-file  (concat org-directory "/habits.org")

	   ellipsis "_"
	   startup-indented t
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

	   ;; capture
	   capture-templates
	   `(
             ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks") "* TODO %^{Brief Description}" :prepend t)
             )

	   ;; current file or any of the agenda-files, max 9 levels deep
	   refile-targets '(
                            (nil :maxlevel . 9)
                            (org-agenda-files :maxlevel . 9)
                            )
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
  (ns/bind "no" 'counsel-org-goto-all)
  (require 'org-tempo)
  )

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
  (ns/focus-line)
  )

(use-package org-pomodoro
  :config
  (defun ns/toggle-music(action)
    (let ((command (concat (if ns/enable-home-p "player.sh" "mpc") " " action)))
      (shell-command command)))

  (add-hook 'org-pomodoro-started-hook
	    (apply-partially #'ns/toggle-music "play"))

  (add-hook 'org-pomodoro-break-finished-hook
	    (apply-partially #'ns/toggle-music "play"))

  (add-hook 'org-pomodoro-finished-hook
	    (apply-partially #'ns/toggle-music "pause"))
  )

(defcommand jump-org () (ns/find-or-open org-default-notes-file))

;; todo: make this insert at focused story?
(defcommand make-org-link-to-here ()
  (insert (concat "[[file:" (buffer-file-name) "::"
		  (number-to-string (line-number-at-pos)) "]]")))

(defcommand insert-mark-org-links ()
  (setq ns/markers
	(append (cl-remove-if (lambda (m)
				(or (evil-global-marker-p (car m))
                                    (not (markerp (cdr m)))))
			      evil-markers-alist)
		(cl-remove-if (lambda (m)
				(or (not (evil-global-marker-p (car m)))
				    (not (markerp (cdr m)))))
			      (default-value 'evil-markers-alist)))
	)

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
 "oo" 'ns/org-goto-active
 "oc" 'org-capture
 "or" 'org-refile
 "ol" 'ns/make-org-link-to-here
 "om" 'ns/insert-mark-org-links

 ;; ehh
 "on" 'ns/jump-org
 )

(add-hook 'org-mode-hook 'ns/set-buffer-face-variable)

(advice-add #'ns/style :after #'ns/style-org)
(defun ns/style-org ()
  (ns/set-faces-monospace '(org-block org-code org-table company-tooltip company-tooltip-common company-tooltip-selection))

  (set-face-attribute 'org-block-begin-line nil :height 50)
  (set-face-attribute 'org-block-end-line nil :height 50)

  (let ((height (plist-get (ns/parse-font (get-resource "st.font")) :height)))
    (set-face-attribute 'org-level-1 nil :height (+ height 15) :weight 'semi-bold)
    (set-face-attribute 'org-level-2 nil :height (+ height 10) :weight 'semi-bold)
    (set-face-attribute 'org-level-3 nil :height (+ height 5) :weight 'semi-bold)
    (set-face-attribute 'org-level-4 nil :height height :weight 'semi-bold)
    ))

;; todo: into org agendas
;; https://emacs.stackexchange.com/questions/477/how-do-i-automatically-save-org-mode-buffers
