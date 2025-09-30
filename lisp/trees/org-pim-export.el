
(defun ns/write-node-to-post (node)
  "Org headline node to blog post. assumes the presence of blog_slug."
  ;; nb: props like pubdate and title_extra come from the existing exported note file
  (let* ((slug (org-ml-headline-get-node-property "blog_slug" node))
          (dest (ns/blog-path (format "notes/%s.org" slug)))
          (exists? (f-exists-p dest))
          (old-content (if exists? (slurp dest) ""))
          (props (ns/blog-get-properties old-content)))

    (f-mkdir (f-dirname dest))
    (when exists? (f-delete dest))
    (spit dest
      (format "%s\n%s"
        (s-join "\n"
          (ht-map (lambda (k v) (ns/str "#+" k ": " v))
            (-ht :title (ht-get props "title"
                          (-last-item (org-ml-headline-get-path node))
                          ;; (org-ml-get-property :title node)
                          ;; (-> node cadr cadr)
                          )
              :title_extra (ht-get props "title_extra")
              :filetags (ht-get props "filetags")
              :pubdate (ht-get props "pubdate" (sh "date '+<%Y-%m-%d>'"))
              )))
        (->> node
          (org-ml-headline-map-node-properties (lambda (_) nil))
          (org-ml-to-trimmed-string)

          ;; remove through the end of the PROPERTIES drawer:
          (s-split "\n" )
          (cdr)
          (s-join "\n"))))
    dest))

(defun ns/org-normalize (node)
  "Move an org headline to be top level"
  (let ((difference (- 1 (org-ml-get-property :level node))))
    (->> node
      (org-ml-map-children*
        (if (eq (org-ml-get-type it) 'headline)
          (org-ml-shift-property :level difference it)
          it))
      (org-ml-set-property :level 1))))

(defun! ns/export-blog-note-targets ()
  (->> (ns/get-notes-nodes '(property "blog_slug"))
    (-map 'ns/org-normalize)
    (-map 'ns/write-node-to-post)
    ;; ((lambda (valid)
    ;;    (dolist (file (ns/blog-get-org "notes"))
    ;;      (when (not (-contains? valid file))
    ;;        (f-delete file)))))
    ))
