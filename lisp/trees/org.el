;; -*- lexical-binding: t; -*-
;; note: org package is loaded in dirt

(require 'org-fold)
(require 'org-habit)
(require 'org-tempo)

(ns/use (org-ml :host github :repo "ndwarshuis/org-ml" :files ("*.el") :branch "update_org_9_7"))

(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

;; org-element updates cache on every buffer change (slow)
;; having this early means we don't break on capture target setup
;; <2024-11-11 Mon 07:49> experiment with turning this back on
(setq org-element-use-cache t)

(setq org-directory
  (if (f-exists-p (~ "sync/main/notes"))
    (~ "sync/main/notes")
    (~ "notes")))

(setq
  org-default-notes-file (concat org-directory "/notes.org")
  org-default-diary-file (concat org-directory "/journal.org"))

(defun ns/refresh-org-files ()
  (setq org-agenda-files
    (f-entries org-directory
      (lambda (f)
        (and
          (s-ends-with-p ".org" f)
          (not (string= f org-default-diary-file))
          (not (or
                 (s-contains? ".sync-conflict" f)
                 (s-starts-with-p (ns/str org-directory "/private") f)
                 (s-starts-with-p (ns/str org-directory "/archive") f)))))
      t)))

(add-hook 'after-save-hook 'ns/refresh-org-files)

(ns/refresh-org-files)

;; config
(setq-ns org
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

(defun! ns/org-get-active-point (&optional property)
  (cond
    ;; todo: return headline? sync with org_task_elisp
    ((org-clocking-p)
      (org-goto-marker-or-bmk org-clock-marker))
    ((ns/get-notes-nodes '(property "focus"))
      ;; todo: uniq prop setting across agenda-files
      (org-goto-marker-or-bmk
        (ns/headline-marker (first (ns/get-notes-nodes '(property "focus"))))))
    (t (message "nothing found!"))))

(defun! ns/org-goto-active (&optional property)
  (find-file org-default-notes-file)
  (goto-char (ns/org-get-active-point property))
  (ns/org-jump-to-element-content))

(ns/use org-pomodoro
  (setq ns/macos-vol (if ns/enable-mac-p (sh "macos-vol get") 0))
  (defun ns/toggle-music (action)
    (let ((target (or (executable-find "player.sh") "mpc")))
      (sh-toss (concat target " " action))))

  ;; named functions so we don't append lambdas on init reload
  (defun ns/toggle-music-play () (ns/toggle-music "play"))
  (defun ns/toggle-music-pause () (ns/toggle-music "pause"))

  (defun! ns/focus-mode-enter (&optional ignore-music-state)
    (when-not ignore-music-state
      (ns/toggle-music-play))

    (when ns/enable-home-p
      (sh-toss "pkill telegram-desktop")
      (sh-toss "pkill signal-desktop")
      ;; (sh-toss "pkill Discord")

      (sh-toss "bash -ic 'scu-restart panel'")

      (spit (~ ".config/qutebrowser/adblock.txt")
        (slurp (~ ".config/qutebrowser/adblock_bad.txt")))

      (sh-toss "qb_command :adblock-update")
      ;; todo: reload is annoying if eg we are actively typing something into a box on qutebrowser
      ;; is there a way to prompt or ask instead of the rude boot
      (sh-toss "qb_command :reload")))

  (defun! ns/focus-mode-quit ()
    (when (or (not (called-interactively-p 'any))
            (and (called-interactively-p 'any)
              (y-or-n-p "are you SURE you want to quit focus mode?")))

      (ns/toggle-music-pause)

      (when ns/enable-home-p
        (sh-toss "bash -ic 'scu-restart panel'"))

      (spit (~ ".config/qutebrowser/adblock.txt") "")
      (sh-toss "qb_command :adblock-update")))

  (add-hook 'org-pomodoro-extend-last-clock 'ns/focus-mode-enter)
  (add-hook 'org-pomodoro-started-hook 'ns/focus-mode-enter)
  (add-hook 'org-pomodoro-finished-hook 'ns/focus-mode-quit)
  (add-hook 'org-pomodoro-killed-hook 'ns/focus-mode-quit)

  (defun ns/org-pomodoro-break-finished ()
    ;; reset break timers
    ;; rel for ns/org-adhoc-timer
    (setq org-pomodoro-short-break-length 5
      org-pomodoro-long-break-length 20)

    (ns/toggle-music "toggle")
    (ns/focus-mode-enter t))

  (add-hook 'org-pomodoro-break-finished-hook 'ns/org-pomodoro-break-finished))

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

  (let* ((props (org-ml-get-all-properties (org-ml-parse-this-headline)))
          (contents-begin (plist-get props :contents-begin))
          (begin (plist-get props :begin)))
    (goto-char (or contents-begin begin))

    ;; broken: org-element-property
    ;; (if contents-begin
    ;;   (progn
    ;;     (let ((first-element (org-element-at-point)))
    ;;       (when (eq 'property-drawer (car first-element))
    ;;         (goto-char (org-element-property :end first-element))))

    ;;     (let ((first-element (org-element-at-point)))
    ;;       (when (eq 'drawer (car first-element))
    ;;         (goto-char (org-element-property :end first-element))))

    ;;     ;; if we're looking at a headline, we went too far
    ;;     ;; (easily possible with blank headlines)
    ;;     (when (s-starts-with-p "*" (thing-at-point 'line))
    ;;       (evil-previous-line)))

    ;;   ;; empty headline
    ;;   (when (s-starts-with-p "*" (thing-at-point 'line))
    ;;     (evil-next-line)
    ;;     (when (s-starts-with-p "*" (thing-at-point 'line))
    ;;       (evil-open-above 1)
    ;;       (evil-normal-state))))
    )

  (recenter))

(defun! ns/make-org-link-to-here ()
  ;; todo: this appears to ignore org headline properties/disrupts them
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
    (message "saved link!")))

;; dunno about this one
(ns/use (evil-org :host github :repo "hlissner/evil-org-mode"
          ;; "Somelauw/evil-org-mode"
          )
  (require 'evil-org-agenda))

;; putting in this file to make sure it's after org mode
;; (when ns/enable-evil-p
;;   (add-hook 'org-mode-hook 'evil-org-mode)

;;   (setq evil-org-movement-bindings
;;     '((up . "e")
;;        (down . "n")
;;        (left . "h")
;;        (right . "l")))

;;   ;; cf https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org
;;   (setq org-special-ctrl-a/e t)
;;   (evil-org-set-key-theme '(textobjects navigation))

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
  (kbd "M-n") 'org-metadown)

(general-define-key
  :states 'normal
  :keymaps 'org-mode-map
  (kbd "E") 'org-toggle-heading)

(ns/use org-present
  (defun ns/org-present-init ()
    ;; remove the height tweaking we do in (ns/style) so that scaling works right
    (ns/face '(org-level-1
                org-level-2
                org-level-3
                org-level-4
                org-level-5
                org-level-6
                org-block
                org-code
                org-table
                org-verbatim
                org-block-begin-line
                org-block-end-line)
      :height 1.0)

    (ns/face 'org-block :height 0.7)

    (org-present-big)
    (org-present-hide-cursor)
    (org-display-inline-images)
    (org-present-read-only))

  (setq org-present-text-scale 5)

  ;; todo: maybe here presentation mode -- gg for start, G for end
  ;; a 'temp cursor' mode? to click links on slides and stuff
  ;; line wrap should be on/and indent
  (ns/inmap 'org-present-mode-keymap
    "q" 'org-present-quit
    ">" 'org-present-next
    "<" 'org-present-prev
    "n" 'org-present-next
    "e" 'org-present-prev)

  (general-imap :keymaps 'org-present-mode-keymap
    "q" 'org-present-quit)

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
  (fn!! org-eval
    (cond
      ((org-in-clocktable-p)
        (org-clock-report))
      ((org-in-src-block-p)
        ;; living dangerously
        (let ((org-confirm-babel-evaluate (fn nil)))
          (org-babel-execute-src-block))))))

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
    (global-set-key (kbd "C-'") #'mw-thesaurus-lookup-at-point)
    ;; override default binding
    (define-key org-mode-map (kbd "C-'") #'mw-thesaurus-lookup-at-point))

  ;; words are hard (spelling)
  (ns/use flyspell-correct-avy-menu
    (setq flyspell-correct-interface #'flyspell-correct-avy-menu)

    ;; todo: I think think this should include a "jump to next spelling err as well"
    (defun! ns/spellcheck-at-point ()
      (when-not (and (boundp 'flyspell-mode)
                  flyspell-mode)
        (flyspell-mode 1))
      (flyspell-correct-at-point))

    (define-key flyspell-mode-map (kbd "C-;") 'ns/spellcheck-at-point)
    (global-set-key (kbd "C-;") 'ns/spellcheck-at-point)
    (general-nmap "gs" 'ns/spellcheck-at-point)))

(defun ns/org-mode-hook ()
  (interactive)

  (ns/set-buffer-face-variable)
  (olivetti-mode)
  (git-gutter-mode 0)
  (flyspell-mode 0)
  (setq-local comment-auto-fill-only-comments nil)
  (auto-fill-mode t)

  (setq flyspell-generic-check-word-predicate
    (lambda ()
      ;; don't spellcheck links
      (when-not (-contains-p (-list (get-char-property (point) 'face))
                  'org-link)
        (org-mode-flyspell-verify))))

  ;; (setq mode-line-format nil)

  (setq-local comment-auto-fill-only-comments nil))

(add-hook 'org-mode-hook 'ns/org-mode-hook)

(defun! ns/style-org ()
  (ns/set-faces-monospace '(org-block
                             org-code
                             org-checkbox
                             org-table
                             org-macro
                             org-formula
                             org-verbatim
                             org-block-begin-line
                             org-block-end-line))

  ;; smol
  (ns/face '(org-block-begin-line org-block-end-line) :height 65)
  (ns/face 'org-ellipsis :underline nil)

  (->> `((1 2 3 4 5 6)
          (15 10 5 0 0 0)
          ;; ,(-repeat 6 0)
          )
    (apply #'-interleave)
    (-partition 2)
    (-map (-lambda ((level height-mod))
            (ns/face (intern (format "org-level-%s" level))
              :underline nil
              :weight 'semi-bold
              :height (+ (face-attribute 'default :height) height-mod)))))

  (when (called-interactively-p 'any)
    (ns/set-buffers-face-variable (ns/buffers-by-mode 'org-mode))))

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
  "oo" (fn!! goto-notes  (let* ((buffer-file-name (buffer-file-name))
                                 (project-notes (if buffer-file-name
                                                  (concat (projectile-root-bottom-up buffer-file-name) "notes.org") org-default-notes-file)))
                           ;; todo: this doesn't work if the notes file isn't already open? what?
                           (ns/find-or-open (if (and (f-exists-p project-notes)
                                                  (not (string= buffer-file-name project-notes)))
                                              project-notes org-default-notes-file))))

  "os" 'org-sort
  "of" 'ns/org-goto-active
  "oF" (fn!! org-goto-active
         (ns/org-clock-out)
         (ns/org-goto-active))

  "oc" (fn!! org-capture
         (if (use-region-p)
           ;; todo: currently ns/capture-current-region is very opinionated it should maybe allow you
           ;; to SEE what you are capturing instead of defaulting to refile-like behavior
           (ns/capture-current-region)
           (org-capture)))

  "ol" 'ns/make-org-link-to-here
  ;; "om" 'ns/insert-mark-org-links
  "ow" (fn!! org-widen (widen) (recenter))
  "on" 'org-narrow-to-subtree
  "oa" 'org-agenda

  ;; trash? idk
  "ot" 'org-archive-subtree

  ;; this is just a nice homerow roll on my layout
  "oi" 'ns/org-clock-in
  "oI" 'ns/org-clock-out

  ;; org-ql-find should popup preview of heading
  "no" (fn!! org-ql-notes
         (org-ql-find
           (-uniq
             (-concat org-agenda-files
               (when (eq 'org-mode major-mode)
                 (list (buffer-file-name (current-buffer)))))))))

(ns/bind-mode 'org
  "or" (fn!! refile-headline-or-region
         (if (use-region-p)
           (ns/capture-current-region)
           (ns/capture-current-subtree)))

  ;; "org move"
  "om" 'org-refile

  "op" 'org-pomodoro

  ;; query org element
  "qo" (fn!! org-query (-> (point) org-ml-parse-element-at org-ml-get-type pr-str message)))


(ns/bind-leader-mode
  'org
  "," 'org-ctrl-c-ctrl-c
  "t" 'org-todo
  "T" 'org-show-todo-tree
  "v" 'org-mark-element
  "a" 'org-agenda
  "f" 'ns/org-set-unique-property
  "F" (fn!! org-set-property (org-set-property (read-string "property name: ") (read-string "value: ")))

  "b" (fn!! org-set-blog-slug (org-set-property "blog_slug" (read-string "slug: ")))

  "s" (fn!! org-set-share (org-set-property "share" "t"))
  )

(add-to-list 'auto-mode-alist '("qutebrowser-editor-" . org-mode))
