;; -*- lexical-binding: t; -*-

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

(ns/use-package org-doct "progfolio/doct"
  :config
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

(setq ns/org-capture-project-list
  (if (f-exists-p org-default-notes-file)
    (with-current-buffer (find-file-noselect org-default-notes-file)
      (->> (org-find-property "projects")
        (org-ml-parse-subtree-at)
        ;; (org-ml-parse-headline-at )
	      (org-ml-get-children)
        cdr
        (-map 'org-ml-headline-get-path)
        (-map 'last)
        -flatten))
    "no notes file here"))

;; todo: follow up on this idea, maybe blend qutebrowser quickmarks
;; (ns/use-package linkmarks "dustinlacewell/linkmarks"
;;   :config (setq linkmarks-file (concat org-directory "/linkmarks.org")))

(setq ns/org-capture-project-templates
  (doct
    `(
       ;; ,(ns/make-project-capture "meta" nil "c")
       ,@(-map 'ns/make-project-capture ns/org-capture-project-list)

       ;; ("Reminder" :keys "r"
       ;;   :template "* %?\n%U\n"
       ;;   )

       ;; ("LinkMark" :keys "l"
       ;;   :file ,linkmarks-file
       ;;   :template ("* %^{Title}"
       ;;               ":PROPERTIES:"
       ;;               ":captured: %U"
       ;;               ":END:"
       ;;               "[[%?]]"
       ;;               ))

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
