;; -*- lexical-binding: t; -*-
;; for fontifying src blocks
(use-package htmlize)

;; todo:
;; - jump to most recently edited file (atime should be fine)
;; - it's really slow right now (4s? we also have 0 caching so consider that)
;; (measure-time (ns/blog-generate)) ;; 4.269
;; (ns/blog-generate)

(defun ns/blog-path (ext)
  (format (~ "git/neeasade.github.io/%s") ext))

(setq ns/blog-posts-dir (ns/blog-path "posts"))
(setq ns/blog-pages-dir (ns/blog-path "pages"))
(setq ns/blog-site-dir (ns/blog-path "site"))

(defun! ns/jump-to-blog-post ()
  (ivy-read "post: "
    (f-entries (ns/blog-path "posts")
      (fn (s-ends-with-p ".org" <>)))
    :action 'find-file))

(defun! ns/jump-to-blog-post-draft ()
  (ivy-read "drafted post: "
    (let ((default-directory (ns/blog-path "posts")))
      (->> (append
             (->> "git ls-files -m" ns/shell-exec  (s-split "\n"))
             (->>  "grep -r '#+draft' . | sed -E 's/:#\\+draft.*//'"ns/shell-exec  (s-split "\n")))
        (mapcar (fn (format "%s/%s" (ns/blog-path "posts") <>)))))
    :action 'find-file))

(ns/bind-soft "nq" 'ns/jump-to-blog-post-draft)
(ns/bind-soft "nQ" 'ns/jump-to-blog-post)

(defun ns/blog-file-to-meta (path)
  ;; a helper
  (defun ns/blog-make-nav-strip (&rest items)
    (apply 'concat
      (list "\n#+BEGIN_CENTER\n"
        "[ " (->> (-remove 'not items) (s-join " | ")) " ]"
        "\n#+END_CENTER\n")))

  (let* ((is-post
           (-contains-p (f-entries (ns/blog-path "posts") (fn (s-ends-with-p ".org" <>))) path))
          (last-edited
            (let ((git-query-result (ns/shell-exec (format "cd %s; git log -1 --format=%%cI %s" (f-dirname path) path))))
              (if (s-blank-p git-query-result)
                nil (substring git-query-result 0 10))))

          (history-link
            (format "https://github.com/neeasade/neeasade.github.io/commits/source/%s/%s"
              (if is-post "posts" "pages")
              (f-filename path)
              ))

          (org-file-content (s-split "\n" (org-file-contents path)))

          (published-date
            (when is-post
              (let ((internal-pubdate (-first (fn (s-starts-with-p "#+pubdate:" <>)) org-file-content)))
                (if internal-pubdate
                  (first (s-match (pcre-to-elisp "[0-9]{4}-[0-9]{2}-[0-9]{2}") internal-pubdate))
                  (substring (f-base path) 0 10)
                  ))))

          (post-org-content-lines
            (-non-nil
              `(,(format "#+SETUPFILE: %s" (ns/blog-path "site/assets/org/setup.org"))

                 ,(when is-post
                    (ns/blog-make-nav-strip
                      (format "[[%s][%s]]" history-link last-edited)
                      published-date
                      ))

                 ,@org-file-content

                 ,(ns/blog-make-nav-strip
                    "[[file:./index.html][Index]]"
                    "[[https://neeasade.net][Root]]"
                    (format "[[%s][Source]]"
                      (concat
                        "https://raw.githubusercontent.com/neeasade/neeasade.github.io/source/"
                        (if is-post "posts/" "pages/")
                        (f-filename path)))
                    "[[https://notes.neeasade.net/sitemap.html][Sitemap]]"
                    (when (not is-post) (format "[[%s][History]]" history-link))
                    )
                 )))

          (post-title
            (->> org-file-content
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

      (:published-link
        (format "https://notes.neeasade.net/%s.html"
          (if is-post
            (substring (f-base path) 11) ;; remove the date
            (f-base path))))

      (:rss-title
        (->> post-org-content-lines
          (-first (fn (s-starts-with-p "#+rss_title:" <>)))
          ((lambda (found)
             (when found
               (s-replace "#+rss_title: " "" found))))
          ))

      (:html-dest (format "%s/%s.html"
                    (ns/blog-path "site")
                    ;; (f-base path)
                    (->> (if is-post
                           (substring (f-base path) 11) ;; remove the date
                           (f-base path))
                      ;; this is crack code
                      ;; build a bunch of removers, compose them, apply to path
                      (funcall (apply '-compose (mapcar (lambda (char) (lambda (s) (s-replace (char-to-string char) "" s))) ";/?:@&=+$,")))
                      )

                    ))

      (:edited-date last-edited)
      (:history-link history-link)
      )))

(defun! ns/blog-generate-from-metas (org-metas)
  ;; publish with our org html export settings
  (let ((default-directory (ns/blog-path "site"))
         (org-export-with-toc nil)
         (org-export-with-timestamps nil)
         (org-export-with-date nil)
         (org-html-html5-fancy t)

         ;; affects timestamp export format
         (org-time-stamp-custom-formats '("%Y-%m-%d"))
         (org-display-custom-times t)
         )

    (mapcar
      (fn (with-temp-buffer
            (message (format "BLOG: making %s " (ht-get <> :path)))
            (insert (ht-get <> :org-content))
            (org-export-to-file 'html (ht-get <> :html-dest))))
      org-metas)
    ))

;; idea: auto refresh on save or on change might be nice
(defun! ns/blog-generate-and-open-current-file ()
  (let* ((file-meta (-> (current-buffer) buffer-file-name ns/blog-file-to-meta))
          (post-html-file (ht-get file-meta :html-dest)))

    (ns/blog-generate-from-metas (list file-meta))

    (message post-html-file)
    (if (string= (concat "file://" post-html-file) (ns/shell-exec "qb_active_url"))
      (ns/shell-exec "qb_command :reload")
      (browse-url post-html-file))))

(defun! ns/blog-generate ()
  ;; cleanup
  (mapcar 'f-delete
    (f-entries ns/blog-site-dir
      (fn (s-ends-with-p ".html" <>))))

  ;; need to define these here for index listings
  (let* ((get-org-files (fn (f-entries <> (fn (s-ends-with-p ".org" <>)))))
          (org-post-metas (->> ns/blog-posts-dir (funcall get-org-files) (mapcar 'ns/blog-file-to-meta)))
          (org-page-metas (->> ns/blog-pages-dir (funcall get-org-files) (mapcar 'ns/blog-file-to-meta)))

          ;; don't ask about generation when exporting
          (org-confirm-babel-evaluate (fn nil)))
    (ns/blog-generate-from-metas (append org-post-metas org-page-metas))

    (message "BLOG: making site rss!")
    (require 'ox-rss)

    (with-temp-buffer
      (insert (org-file-contents (ns/blog-path "rss/rss.org")))
      (org-export-to-file 'rss (ns/blog-path "site/rss.xml")))

    )
  t
  )


(ns/use-package om "ndwarshuis/om.el")
(require 'om)

;; cf https://writequit.org/articles/emacs-org-mode-generate-ids.html#the-problem
(defun! eos/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
        ((and id (stringp id) (string-match "\\S-" id)) id)
        (create
          (setq id (s-replace ":" "-" (org-id-new (concat prefix "h"))))
          (org-entry-put pom "CUSTOM_ID" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))

(defun! eos/org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the
   current file which do not already have one."
  (org-map-entries (lambda () (eos/org-custom-id-get (point) 'create))))

(defun! ns/blog-new-post ()
  (let*
    ((title (read-from-minibuffer "new blog post title: "))
      (date (ns/shell-exec "date +%Y-%m-%d"))
      (file (format (~ "git/neeasade.github.io/posts/%s-%s.org") date
              (s-replace " " "-" title)))
      )
    (find-file file)
    (insert (format "#+title: %s\n" title))
    (insert "#+draft: t\n\n")
    ))
