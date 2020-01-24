;; for fontifying src blocks
(use-package htmlize)

;; todo:
;; - elisp publish function?
;; - sitemap? of the pages that aren't posts
;; - rss feed
;; - it's really slow right now (4s? we also have 0 caching so consider that)
;; (measure-time (ns/blog-generate)) ;; 4.269

(defun ns/blog-publish ()
  (when (ns/shell-exec "cd '%s'; git status | grep 'working tree clean'")
    (error "unclean working tree in blog dir"))
  )

(defun ns/blog-path (ext)
  (format (~ "git/neeasade.github.io/%s") ext))

;; todo: consider making this title
(defun! ns/jump-to-blog-post ()
  (ivy-read "post: "
    (f-entries (ns/blog-path "posts") (fn (s-ends-with-p ".org" <>)))
    :action 'find-file))

;; ehhhh
(ns/bind "nq" 'ns/jump-to-blog-post)

(defun ns/blog-file-to-meta (path is-post)

  (defun ns/blog-make-nav-strip (&rest items)
    (apply 'concat
      (list "\n#+BEGIN_CENTER\n[ "
        (->> items (s-join " ¦ "))
        " ]\n#+END_CENTER\n")))

  (let* ((last-edited
           (let ((git-query-result
                   (ns/shell-exec (format "cd %s; git log -1 --format=%%cI %s" (f-dirname path) path))))
             (if (s-blank-p git-query-result)
               nil (substring git-query-result 0 10))))
          (published-date (when is-post (substring (f-base path) 0 10)))

          (history-link
            (format "https://github.com/neeasade/neeasade.github.io/commits/source/posts/%s"
              (f-filename path)))

          (post-org-content-lines
            (-non-nil
              `(,(format "#+SETUPFILE: %s" (ns/blog-path "site/assets/org/setup.org"))

                 ,(when is-post
                    (ns/blog-make-nav-strip
                      (format "published <%s>" published-date)
                      (format "[[%s][edited %s]]" history-link last-edited)
                      "[[file:./index.html][Index]]"))

                 ,@(s-split "\n" (org-file-contents path))

                 ,(ns/blog-make-nav-strip "[[file:./index.html][Index]]" "[[https://neeasade.net][Root]]")
                 )))

          (post-title
            (->> post-org-content-lines
              (-first (fn (s-starts-with-p "#+title:" <>)))
              car (s-replace "#+title: " "")))
          (post-is-draft
            (car (-first (fn (s-contains-p "#+draft: t" <>)) post-org-content-lines)))
          )
    (a-list
      :path path
      :org-content (s-join "\n" post-org-content-lines)
      :is-draft post-is-draft
      :title post-title
      :publish-date published-date
      :html-dest (format "%s/%s.html" (ns/blog-path "site") (f-base path))
      :edited-date last-edited
      :history-link history-link)))

(defun ns/blog-publish-file (org-file-meta)
  (with-temp-buffer
    (insert (a-get org-file-meta :org-content))
    (org-export-to-file 'html (a-get org-file-meta :html-dest))))

(defun! ns/blog-generate-and-open-current-file ()
  (let* ((post-meta (ns/blog-file-to-meta (buffer-file-name (current-buffer)) t))
          (post-html-file (a-get post-meta :html-dest)))
    (ns/blog-publish-file post-meta)
    (message post-html-file)
    (if (string= (concat "file://" post-html-file) (ns/shell-exec "qb_active_url"))
      (ns/shell-exec "qb_command :reload")
      (browse-url post-html-file))))

(defun! ns/blog-generate ()
  ;; note to self: if blog generation fails,
  ;; simply comment out the (error) below to see where it's failing.
  (condition-case ()
    (progn
      (let ((ns/blog-posts-dir (ns/blog-path "posts"))
             (ns/blog-pages-dir (ns/blog-path "pages"))
             (ns/blog-site-dir (ns/blog-path "site"))
             (default-directory (ns/blog-path "site"))
             (org-export-with-toc nil)
             (org-export-with-timestamps nil)
             (org-export-with-date nil)
             (org-html-html5-fancy t)

             (org-time-stamp-custom-formats '("%Y-%m-%d"))

             (org-display-custom-times t)

             ;; don't ask about generation when exporting
             (org-confirm-babel-evaluate (fn nil)))

        ;; cleanup
        (mapcar 'f-delete
          (f-entries ns/blog-site-dir
            (fn (s-ends-with-p ".html" <>))))

        (let ((org-post-metas (mapcar (fn (ns/blog-file-to-meta <> t)) (f-entries ns/blog-posts-dir (fn (s-ends-with-p ".org" <>)))))
               (org-page-metas (mapcar (fn (ns/blog-file-to-meta <> nil)) (f-entries ns/blog-pages-dir (fn (s-ends-with-p ".org" <>))))))
          (mapcar 'ns/blog-publish-file org-page-metas)
          (mapcar 'ns/blog-publish-file org-post-metas))
        )
      (alert "blog generated")
      )
    (error (alert "blog generation failed"))
    )
  t ;; for calling from elisp script
  )
