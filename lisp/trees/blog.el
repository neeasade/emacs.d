;; for fontifying src blocks
(use-package htmlize)

;; todo:
;; - elisp publish function?
;; - jump to most recently edited file (atime should be fine)

;; - sitemap? of the pages that aren't posts
;; - rss feed
;; - it's really slow right now (4s? we also have 0 caching so consider that)
;; (measure-time (ns/blog-generate)) ;; 4.269

(defun ns/blog-path (ext)
  (format (~ "git/neeasade.github.io/%s") ext))

;; todo: consider making this title
(defun! ns/jump-to-blog-post ()
  (ivy-read "post: "
    (f-entries (ns/blog-path "posts")
      (fn (s-ends-with-p ".org" <>)))
    :action 'find-file))

(ns/bind-soft "nq" 'ns/jump-to-blog-post)

(defun ns/blog-file-to-meta (path)
  (defun ns/blog-make-nav-strip (&rest items)
    (apply 'concat
      (list "\n#+BEGIN_CENTER\n[ "
        (->> items (s-join " ¦ "))
        " ]\n#+END_CENTER\n")))

  (let* ((is-post
           (-contains-p (f-entries (ns/blog-path "posts") (fn (s-ends-with-p ".org" <>))) path))
          (last-edited
            (let ((git-query-result (ns/shell-exec (format "cd %s; git log -1 --format=%%cI %s" (f-dirname path) path))))
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
                      (format "[[%s][%s]]" history-link last-edited)
                      published-date
                      ))

                 ,@(s-split "\n" (org-file-contents path))

                 ,(ns/blog-make-nav-strip
                    "[[file:./index.html][Index]]"
                    "[[https://neeasade.net][Root]]"
                    (format "[[%s][Source]]"
                      (concat
                        "https://raw.githubusercontent.com/neeasade/neeasade.github.io/source/posts/"
                        (f-filename path)
                        )
                      )
                    )
                 )))

          (post-title
            (->> post-org-content-lines
              (-first (fn (s-starts-with-p "#+title:" <>)))
              (s-replace "#+title: " "")))
          (post-is-draft
            (-first (fn (s-contains-p "#+draft: t" <>)) post-org-content-lines))
          )

    ;; idea: make this one big nested hash table instead of many little ones
    (ht
      (:path path)
      (:org-content (s-join "\n" post-org-content-lines))
      (:is-draft post-is-draft)
      (:title post-title)
      (:publish-date published-date)
      (:html-dest (format "%s/%s.html"
                    (ns/blog-path "site")
                    (f-base path)

                    ;; remove the date
                    ;; (if is-post
                    ;;   (substring (f-base path) 0 10)
                    ;;   (f-base path))

                    ))
      (:edited-date last-edited)
      (:history-link history-link)
      )))

(defun ns/blog-publish-file (org-file-meta)
  (with-temp-buffer
    (insert (ht-get org-file-meta :org-content))
    (org-export-to-file 'html (ht-get org-file-meta :html-dest))))

(defun! ns/blog-generate-and-open-current-file ()
  (let* ((post-meta (ns/blog-file-to-meta (buffer-file-name (current-buffer))))
          (post-html-file (ht-get post-meta :html-dest)))
    (ns/blog-publish-file post-meta)
    (message post-html-file)
    (if (string= (concat "file://" post-html-file) (ns/shell-exec "qb_active_url"))
      (ns/shell-exec "qb_command :reload")
      (browse-url post-html-file))))

(defun! ns/blog-generate ()
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

    (let ((org-post-metas (mapcar 'ns/blog-file-to-meta (f-entries ns/blog-posts-dir (fn (s-ends-with-p ".org" <>)))))
           (org-page-metas (mapcar 'ns/blog-file-to-meta (f-entries ns/blog-pages-dir (fn (s-ends-with-p ".org" <>))))))
      (mapcar 'ns/blog-publish-file org-page-metas)
      (mapcar 'ns/blog-publish-file org-post-metas)))
  t ;; for calling from elisp script
  )
