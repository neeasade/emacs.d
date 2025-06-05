;; -*- lexical-binding: t; -*-

;; standalone capture experience

;; cf https://fuco1.github.io/2017-09-02-Maximize-the-org-capture-buffer.html
(defvar ns/my-org-capture-before-config nil)

(defadvice org-capture (before save-config activate)
  "Save the window configuration before `org-capture'."
  (setq ns/my-org-capture-before-config (current-window-configuration)))

(add-hook 'org-capture-mode-hook 'delete-other-windows)

(defun my-org-capture-cleanup ()
  "Clean up the frame created while capturing via org-protocol."
  (when ns/my-org-capture-before-config
    (set-window-configuration ns/my-org-capture-before-config)))

(add-hook 'org-capture-after-finalize-hook 'my-org-capture-cleanup)

(defun ns/org-find-olp (&rest args)
  "a soft version of org-find-olp which doesn't throw"
  (condition-case msg
    (apply 'org-find-olp args)
    (error
      ;; show what went wrong:
      ;; (nth 1 msg)
      nil)))

(defun ns/make-org-tree (filename &rest headings)
  ;; ensure the first heading exists
  (when (not (ns/org-find-olp `(,filename ,(car headings))))
    ;; just add it to the bottom of the file
    (save-window-excursion
      (find-file filename)
      (goto-char (point-max))
      (insert (format "\n* %s" (car headings)))))

  (let ((full-path '())
         (full-paths '()))
    (-each headings (lambda (part)
                      (setq full-path (-snoc full-path part))
                      (setq full-paths (-snoc full-paths full-path))))
    (setq full-paths (-map (lambda (path) (cons filename path)) full-paths))
    (setq full-paths (-drop 1 full-paths))

    ;; now for each potential path, go to it, and maybe create a child.
    ;; full-paths
    (save-window-excursion
      (find-file filename)
      (-each full-paths
        (lambda (path)
          ;; assume the parent exists, go there
          (goto-char (marker-position (org-find-olp (-drop-last 1 path))))
          ;; check if the current child exists, if not, make it
          (when (not (ns/org-find-olp path))
            (org-insert-heading-respect-content)
            (org-demote-subtree)
            (insert (-last-item path))))))))

(ns/use (org-doct :host github :repo "progfolio/doct")
  (require 'doct))

(defun ns/make-project-capture (project &optional template-override key)
  `(,project
     :keys ,(or key (-> project string-to-list first char-to-string))
     :file ,org-default-notes-file
     ;; todo: maybe want: a way to override the olp path and file? (eg, project level notes)
     ;; alternatively, just import the notes into your main ones
     :children (("task" :keys "t" :todo-state "TODO"
                  :immediate-finish t
                  :template ,(or template-override (list "* %{todo-state} %^{Description}"
                                                     ":PROPERTIES:" ":captured: %U" ":END:"
                                                     "%?"))
                  :olp ("projects" ,project "tasks"))
                 ("capture" :keys "c" :todo-state "TODO"
                   :immediate-finish t
                   :template ,(or template-override (list "* %{todo-state} %^{Description}"
                                                      ":PROPERTIES:" ":captured: %U" ":END:"
                                                      "%?"))
                   :olp ("projects" ,project "captures"))
                 ("note" :keys "n"
                   :immediate-finish t
                   :template ,(or template-override (list "* %^{Description}"
                                                      ":PROPERTIES:" ":captured: %U" ":END:"
                                                      "%?"))
                   :olp ("projects" ,project "notes"))

                 ("task" :keys "T" :todo-state "TODO"
                   :template ,(or template-override (list "* %{todo-state} %{Description}"
                                                      ":PROPERTIES:" ":captured: %U" ":END:"
                                                      "%?"))
                   :olp ("projects" ,project "tasks"))
                 ("capture" :keys "C" :todo-state "TODO"
                   :template ,(or template-override (list "* %{todo-state} %{Description}"
                                                      ":PROPERTIES:" ":captured: %U" ":END:"
                                                      "%?"))
                   :olp ("projects" ,project "captures"))
                 ("note" :keys "N"
                   :template ,(or template-override (list "* %{Description}"
                                                      ":PROPERTIES:" ":captured: %U" ":END:"
                                                      "%?"))
                   :olp ("projects" ,project "notes"))
                 ;; in the future if we want to nest projects under a heading:
                 ;; ("Projects" :keys "p"
                 ;;   :children
                 ;;   (;; projects:
                 ;;     ,(ns/make-project-capture "other")
                 ;;     ))

                 )))

;; want: creating a new project should be easy
;; regen captures on save of notes file is ns/org-capture-project-list has updated
(setq ns/org-capture-project-list
  (when (f-exists-p org-default-notes-file)
    (ns/with-notes
      (-some->> (org-find-property "projects")
        (org-ml-parse-subtree-at)
	      (org-ml-get-children)
        cdr
        (-map 'org-ml-headline-get-path)
        (-map 'last)
        -flatten))))

;; ensure capture hierarchy exists for capture targets
;; TODO: presuppose this at time of capture

(setq ns/linkmark-file (ns/path org-directory "linkmarks.org"))
(setq ns/org-inbox-file (ns/path org-directory "inbox.org"))

(defun! linkmark-select ()
  (llet [link-label (ns/pick (-map 'org-ml-headline-get-path
                               (ns/get-notes-nodes t (list ns/linkmark-file))))
          link-headline (first (ns/get-notes-nodes `(outline-path ,link-label) (list ns/linkmark-file)))
          link (first
                 (-keep 'org-ml-to-trimmed-string
                   (org-ml-headline-get-contents nil link-headline)))]
    (with-temp-buffer
      (insert link)
      (beginning-of-line)
      ;; (org-mode)
      (ns/follow))))

(ns/bind
  "nl" #'linkmark-select
  "nL" (fn!! linkmark (find-file ns/linkmark-file))
  )

;; context ideas (store in properties):
;; major-mode
;; date
;; file location
;; hostname?
(setq ns/org-capture-project-templates
  (doct
    `(
       ;; ,(ns/make-project-capture "meta" nil "c")
       ,@(-map 'ns/make-project-capture ns/org-capture-project-list)

       ("story" :keys "s"
         :file ,org-default-notes-file
         :olp ("stories")
         :template ("* titleme"
                     "# put the why here:"
                     ;; ""
                     "%?"
                     "** Acceptance criteria" ""
                     ;; "" "** Why"
                     ;; todo: optionals: plan, additional context, out of scope -- "# ** thing"
                     ;; could maybe include a second line with context/wants
                     ;; "" "** Why"

                     ""
                     "** Plan" ""
                     "# ** Additional Context" ""

                     ;; "[[%?]]"
                     ))

       ("LinkMark" :keys "l"
         :file ,ns/linkmark-file
         :template ("* %?"
                     ":PROPERTIES:"
                     ":captured: %U"
                     ":END:" ""))

       ("Inbox" :keys "i"
         :file ,ns/org-inbox-file
         :template ("** %?"
                     ":PROPERTIES:"
                     ":captured: %U"
                     ":END:" ""))


       ("Journal" :keys "j"
         :template "* %?\n%U\n"
         :clock-in t :clock-resume t
         :datetree t :file ,org-default-diary-file
         ))))

(setq ns/org-capture-region-templates
  (doct
    `(
       ;; this is wrapped in a progn so the lines after the first don't get made into headings
       ;; I hate org mode
       ,@(-map (fn (ns/make-project-capture <> "* %(progn \"%i\")"))
           ns/org-capture-project-list))))

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
  (let* ((ns/region-points (list (region-beginning) (region-end))))
    (setq org-capture-templates ns/org-capture-region-templates)
    (when (condition-case ()
            (org-capture)
            (quit nil))

      ;; if we captured to somewhere in the current buffer, our point might have changed
      (when (not (= (region-beginning) (first ns/region-points)))
        (setq ns/region-points
          (llet [start (first ns/region-points)
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

  (setq org-capture-templates ns/org-capture-project-templates))

;; so I have something to Mx search for
(defun! ns/org-refresh-capture-templates ()
  (ns/conf-org-capture))
