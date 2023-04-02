
(defun ns/org-normalize (node)
  "Move an org headline to be top level"
  (let ((difference (- 1 (org-ml-get-property :level node))))
    (->> node
      (org-ml-map-children*
        (if (eq (org-ml-get-type it) 'headline)
          (org-ml-shift-property :level difference it)
          it))
      (org-ml-set-property :level 1))))

(defun ns/write-node-to-post (node)
  "Org headline node to blog post. assumes the presence of blog_slug."
  (let*
    ((slug (org-ml-headline-get-node-property "blog_slug" node))
      (dest (ns/blog-path (format "notes/%s.org" slug)))
      (exists? (f-exists-p dest))
      (old-content (if exists? (f-read dest) "")))

    (f-mkdir (f-dirname dest))
    (when exists? (f-delete dest))
    (f-write
      (format "
#+title: %s
#+title_extra: %s
#+filetags: %s
#+pubdate: %s
#+post_type: note
%s"
        (or (ns/blog-get-prop "title" old-content) (-> node cadr cadr))
        (or (ns/blog-get-prop "title_extra" old-content) "")
        (or (ns/blog-get-prop "filetags" old-content) "")
        (or (ns/blog-get-prop "pubdate" old-content) (ns/shell-exec "date '+<%Y-%m-%d>'"))
        (->> node
          (org-ml-headline-map-node-properties (lambda (_) nil))
          (org-ml-to-trimmed-string)

          ;; remove through the end of the PROPERTIES drawer:
          (s-split "\n" )
          (cdr)
          (s-join "\n")))

      'utf-8
      dest)
    ;; return the path:
    dest))

(defun! ns/org-export-shared ()
  (defun ns/org-match-note-share (heading)
    (org-ml-headline-get-node-property "share" heading))
  (let ((content (->> (ns/get-notes-nodes 'ns/org-match-note-share)
                   (-map 'ns/org-normalize)
                   (-map 'org-ml-to-string)
                   (s-join "\n")))
         (labs-folder (pass "labs-folder")))
    (when (f-exists-p labs-folder)
      (f-write (format "Exported on: %s\n\n %s" (ts-format (ts-now)) content)
        'utf-8
        (format "%s/notes.org" labs-folder)))))

(defun! ns/export-blog-note-targets ()
  (defun ns/org-match-blog (headline)
    (org-ml-headline-get-node-property "blog_slug" headline))
  (->> (ns/get-notes-nodes 'ns/org-match-blog)
    (-map 'ns/org-normalize)
    (-map 'ns/write-node-to-post)
    ((lambda (valid)
       (dolist (file (ns/blog-get-org "notes"))
         (when (not (-contains? valid file))
           (f-delete file)))))))

