;; -*- lexical-binding: t; -*-

;; for fontifying src blocks
(use-package htmlize)

;; todo: check for conflicting html-destinations?

(defun ns/blog-mustache (text table)
  (-reduce-from
    (lambda (text key)
      (s-replace
        (format "{{%s}}" key)
        (or (ht-get table key) "")
        text))
    text (ht-keys table)))

;; later: set weak version of theme export
;; (setq htmlize-face-overrides
;;   nil
;;   )

;; (let ((theme-colors (append
;;                       (tarp/map-to-base16 :weak)
;;                       (ht-to-plist (ht-get tarp/theme* :weak)))))
;;   (setq htmlize-face-overrides
;;     (-mapcat
;;       (lambda (definition)
;;         (list
;;           (car definition)
;;           (cdr definition)))
;;       (tarp/theme-make-faces
;;         theme-colors
;;         ))
;;     ))


(defun ns/blog-path (ext)
  (format (~ "git/neeasade.github.io/%s") ext))

(setq ns/blog-posts-dir (ns/blog-path "posts"))
(setq ns/blog-pages-dir (ns/blog-path "pages"))
(setq ns/blog-site-dir (ns/blog-path "site"))

;; todo: make this jump by title, not file name
(defun! ns/jump-to-blog-post ()
  (ivy-read "post: "
    (reverse
      (f-entries (ns/blog-path "posts")
        (fn (s-ends-with-p ".org" <>))))
    :action 'find-file))

(defun! ns/jump-to-blog-post-draft ()
  (ivy-read "drafted post: "
    (or
      (let ((default-directory (ns/blog-path "posts")))
        (->> (append
               (->> "git ls-files -m" ns/shell-exec  (s-split "\n") reverse)
               (->>  "grep -r '#+draft' . | sed -E 's/:#\\+draft.*//'"ns/shell-exec  (s-split "\n")))
          (-filter (fn (not (s-blank-p <>))))
          (-map (fn (format "%s/%s" (ns/blog-path "posts") <>)))
          (-map (fn (s-replace "/./" "/" <>)))
          (-uniq)))
      ;; if there are no drafts, fall over to all posts:
      (reverse
        (f-entries (ns/blog-path "posts")
          (fn (s-ends-with-p ".org" <>)))))
    :action 'find-file))

(ns/bind-soft
  "nq" 'ns/jump-to-blog-post
  ;; "nq" 'ns/jump-to-blog-post-draft
  )

(defun ns/blog-make-hsep ()
  (format "\n#+begin_center\n%s\n#+end_center\n"
    (let* (
            (options "🍇🍉🍓🍅🍄🍈🍍")
            (options "🍂🌿🌱🍁🍀")
            (index (random (length options)))
            (char (substring options index (+ index 1))))
      (format "%s %s %s" char char char)
      )))

(defun ns/blog-file-to-meta (path)

  (defun ns/blog-make-nav-strip (&rest items)
    (apply 'concat
      (list "\n#+BEGIN_CENTER\n"
        (->> (-remove 'not items) (s-join " "))
        "\n#+END_CENTER\n")))

  (defun ns/blog-get-prop (propname text)
    (-some--> (format "#\\+%s:.*$" propname)
      (pcre-to-elisp it)
      (s-match it text)
      (first it)
      (s-replace (format "#+%s:" propname) "" it)
      (s-trim it)))

  (message (format "BLOG: generating meta for %s" path))
  (let* (
          (org-file-content (s-replace-regexp "^-----$" "{{{hsep}}}" (f-read path)))

          (post-type
            ;; XXXXX -- no content here for type
            (or (ns/blog-get-prop "post_type" org-file-content)
              ;; this could maybe just be changed to the parent folder name
              (if (-contains-p
                    (f-entries (ns/blog-path "posts")
                      (fn (s-ends-with-p ".org" <>)))
                    path)
                "post"
                "page")))

          (last-edited
            (let ((git-query-result (ns/shell-exec (format "cd '%s'; git log -1 --format=%%cI '%s'"
                                                     ;; appease the shell.
                                                     (s-replace "'" "'\\''" (f-dirname path))
                                                     (s-replace "'" "'\\''" path)))))

              (if (s-blank-p git-query-result)
                nil (substring git-query-result 0 10))))

          (published-date
            (when (string= post-type "post")
              (let ((internal-pubdate (ns/blog-get-prop "pubdate" org-file-content)))
                (if internal-pubdate
                  (first (s-match (pcre-to-elisp "[0-9]{4}-[0-9]{2}-[0-9]{2}") internal-pubdate))
                  (substring (f-base path) 0 10)))))

          (post-title
            ;; todo: consider untitled name/maybe filename
            (or
              (ns/blog-get-prop "title" org-file-content)
              "untitled"
              )
            )

          (post-subtitle (ns/blog-get-prop "title_extra" org-file-content))

          (post-org-content
            (ns/blog-mustache
              (f-read (~ ".emacs.d/org/blog_template.org"))
              (ht
                ("csslinks"
                  (s-join "\n"
                    (-map
	                    (fn (let* ((file-path (ns/blog-path (format "site/assets/css/%s.css" <>)))
		                              (include-path (format "/assets/css/%s.css" <>))
		                              (sum (ns/shell-exec (format "md5sum '%s' | awk '{print $1}'" file-path))))
	                          (concat
		                          (format "#+HTML_HEAD: <link rel='stylesheet' href='%s?sum=%s'>" include-path sum)
		                          (format "\n#+HTML_HEAD: <link rel='stylesheet' href='.%s?sum=%s'>" include-path sum))))
	                    '("org" "colors" "notes" "new"))))

                ("title" post-title)

                ("up"
                  (if (s-starts-with-p "index" (f-filename path))
                    "<a href='https://neeasade.net'>Up: Splash</a>"
                    "<a href='/index.html'>Up: ＧＲＯＶＥ</a>"
                    )
                  )

                ("last-edited" last-edited)
                ("subtitle" post-subtitle)
                ("content" org-file-content)
                ("page-markup-link"
                  (format "https://raw.githubusercontent.com/neeasade/neeasade.github.io/source/%ss/%s"
                    post-type (f-filename path)))

                ("page-history-link"
                  (format "https://github.com/neeasade/neeasade.github.io/commits/source/%ss/%s"
                    post-type (f-filename path)))

                ("footer-left"
                  (if (s-starts-with-p "index" (f-filename path))
                    "<a href='/sitemap.html'>Sitemap</a>"
                    "<div class='footer-left'><a href='/index.html'>🍃🌳ＧＲＯＶＥ🍃🌳</a></div>"))

                ("footer-center"
                  (when (s-starts-with-p "index" (f-filename path))
                    ;; todo: don't hotlink
                    "#+BEGIN_EXPORT html
<div class=footer-center>
<a href='https://webring.xxiivv.com/#random' target='_blank'><img style='width:40px;height:40px' src='./assets/img/logos/xxiivv.svg'/></a>
<a href='https://github.com/nixers-projects/sites/wiki/List-of-nixers.net-user-sites' target='_blank'><img style='width:35px;height:40px' src='./assets/img/logos/nixers.png'/></a>
<a href='https://webring.recurse.com'><img alt='Recurse Center Logo' src='./assets/img/logos/recurse.png' style='height:40px;width:40px;'></a>
</div>
#+end_export
"))

                ("flair"
                  (when (string= post-type "post")
                    "@@html:<div class='title flair'><img class='flair-border' src='./assets/img/backgrounds/bark.jpg' /> </div>@@"))))))

    (ht
      (:path path)
      (:org-content post-org-content)
      (:is-draft (not (s-blank-p (ns/blog-get-prop "draft" post-org-content))))
      (:title post-title)
      (:publish-date published-date)

      (:rss-title (ns/blog-get-prop "rss_title" post-org-content))

      (:html-dest (format "%s/%s.html"
                    (ns/blog-path "site")
                    ;; (f-base path)
                    (->> (if
                           ;; is-post
                           (string= post-type "post")

                           (substring (f-base path) 11) ;; remove the date
                           (f-base path))
                      ;; this is crack code
                      ;; build a bunch of removers, compose them, apply to path
                      (funcall
                        (apply '-compose
                          (mapcar (lambda (char)
                                    (lambda (s) (s-replace (char-to-string char) "" s))) ";/?:@&=+$,'"))))))

      (:edited-date last-edited)
      )))

(defun! ns/blog-generate-from-metas (org-metas)
  ;; publish with our org html export settings
  (let ((default-directory (ns/blog-path "site"))
         (org-export-with-toc nil)
         (org-export-with-section-numbers t)
         (org-export-with-timestamps nil)
         (org-export-with-date nil)
         (org-html-html5-fancy t)
         (org-export-with-title nil)


         ;; affects timestamp export format
         ;; (org-time-stamp-custom-formats '("%Y-%m-%d" . "%Y-%m-%d %I:%M %p"))
         (org-time-stamp-custom-formats '("[%Y-%m-%d]" . "[%Y-%m-%d %I:%M %p]"))
         (org-display-custom-times t)

         ;; don't ask about generation when exporting
         (org-confirm-babel-evaluate (fn nil))
         )

    (-map
      (lambda (post)
        (with-temp-buffer
          (ht-with-context post
            (message (format "BLOG: making %s " :path))
            (insert :org-content)
            (org-export-to-file 'html :html-dest))))
      org-metas)))

;; idea: auto refresh on save or on change might be nice
(defun! ns/blog-generate-and-open-current-file ()
  (save-buffer)
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

  ;; need to define these here for index listings and rss:
  (setq
    org-post-metas (-map 'ns/blog-file-to-meta (f-entries ns/blog-posts-dir (fn (s-ends-with-p ".org" <>))))
    org-page-metas (-map 'ns/blog-file-to-meta (f-entries ns/blog-pages-dir (fn (s-ends-with-p ".org" <>)))))

  (let* (
          ;; don't ask about generation when exporting
          (org-confirm-babel-evaluate (fn nil)))

    (message "BLOG: making pages!")
    (ns/blog-generate-from-metas (append org-post-metas org-page-metas))

    (message "BLOG: making site rss!")
    (require 'ox-rss)

    (with-temp-buffer
      (insert (org-file-contents (ns/blog-path "rss/rss.org")))
      (org-export-to-file 'rss (ns/blog-path "site/rss.xml")))

    (with-temp-buffer
      (insert (org-file-contents (ns/blog-path "rss/rss_full.org")))
      (org-export-to-file 'rss (ns/blog-path "site/rss_full.xml")))
    )
  t
  )

;; cf https://writequit.org/articles/emacs-org-mode-generate-ids.html#the-problem
;; enhancing this to also turn the header into an anchor link
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

(defun! ns/blog-enhance-headings ()
  "make headings links to themselves -- uses om.el to do so"
  (org-map-entries
    (lambda ()
      ;; ensure the headlines have some custom_id
      (eos/org-custom-id-get (point) 'create)

      ;; make the heading a link to itself if it's not already a link
      (org-ml-update-headline-at (point)
        (lambda (old-heading)
          (org-ml-set-property :title
            ;; get the old heading title text
            (->> (-> old-heading org-ml-headline-get-path last car)
              ;; make it a link if it's not already one
              ((lambda (old-heading-plain)
                 (if (s-contains-p "[[" old-heading-plain)
                   old-heading-plain
                   (format "[[#%s][%s]]"
                     ;; first need to get id
                     (org-entry-get (point) "CUSTOM_ID")
                     ;; (om-get-property :custom-id old-heading)
                     ;; id
                     old-heading-plain
                     ))))
              ;; (it expects a listed title)
              (list))
            old-heading
            ))))))

(defun! ns/blog-new-post ()
  (let* ((title (read-from-minibuffer "new blog post title: "))
          (date (ns/shell-exec "date +%Y-%m-%d"))
          (file (format (~ "git/neeasade.github.io/posts/%s-%s.org") date
                  (s-replace " " "-" title))))
    (find-file file)
    (insert (format "#+title: %s\n" title))
    (insert (format "#+pubdate: <%s>\n" date))
    (insert "#+draft: t\n\n")))

(defun ns/blog-make-color-preview (color &optional text)
  ;; assumes a dark FG and light BG
  ;; (message (format "arst %s %s" color text))
  ;; (message (format "arst %s" (stringp text)))
  (format
    "@@html:<code style=\"background: %s;color: %s; padding: 2px; border: 1px solid %s\">%s</code>@@"
    (tarp/get (if (ct-is-light-p color) :foreground :background))
    (if (ct-is-light-p color) (tarp/get :foreground) color)
    (if (not (s-equals? "" (or text "")))
      text color)))


;; # #+MACRO:  detail @@html: <div class="detail"> $1 </div> @@
(defun ns/blog-make-detail (&rest parts)
  (format "@@html: <div class=\"detail\"> %s </div> @@"
    (s-join ","
      (-filter (fn (not (string-empty-p <>)))
        parts))))

(defun ns/blog-make-color-block (width color &optional text foreground class)
  ;; assumes a dark FG and light BG
  (format
    ;; "@@html:<code style=\"background: %s;color: %s; padding: 2px; border: 1px solid %s\">%s</code>@@"
    "@@html:<div class=\"%s\" style=\"background: %s;color: %s; width: %s%%;\">%s</div>@@"
    (or class "colorblock colorcenter")
    color
    (if foreground foreground
      (tarp/get (if (ct-is-light-p color) :foreground :background)))
    width (or text "")))

(defun ns/blog-make-color-strip (colors &optional labels)
  (s-join "\n"
    `(
       "@@html: <div style='display: flex; flex-wrap: wrap; justify-content: center;'>  @@"
       ,@(-map
           (lambda (c)
             (ns/blog-make-color-block
               (/ 100.0 (length colors))
               (first c)
               (cdr c)))

           (-zip
             (-map (lambda (c)
                     (if (listp c)
                       (-map 'ct-shorten c)
                       (ct-shorten c))) colors)
             (or labels (-map (lambda (_) "") (range (length colors))))))

       "@@html: </div> @@"
       )))

;; (ns/blog-make-color-preview #cccccc")
