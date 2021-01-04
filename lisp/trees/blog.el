;; -*- lexical-binding: t; -*-

;; for fontifying src blocks
(use-package htmlize)

;; todo: check for conflicting html-destinations?

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
  "nq" 'ns/jump-to-blog-post-draft
  "nQ" 'ns/jump-to-blog-post
  )

(defun ns/blog-make-hsep ()
  (format "#+begin_center\n%s\n#+end_center"
    (let* ((options "🍇🍉🍓🍅🍄🍈🍍")
            (index (random (length options))))
      (substring options index (+ index 1)))))

(defun ns/blog-file-to-meta (path)
  ;; a helper
  (defun ns/blog-make-nav-strip (&rest items)
    (apply 'concat
      (list "\n#+BEGIN_CENTER\n"
        (->> (-remove 'not items) (s-join " "))
        "\n#+END_CENTER\n")))

  (message (format "BLOG: generating meta for %s" path))
  (let* ((is-post
           (-contains-p (f-entries (ns/blog-path "posts") (fn (s-ends-with-p ".org" <>))) path))
          (last-edited
            (let ((git-query-result (ns/shell-exec (format "cd '%s'; git log -1 --format=%%cI '%s'"
                                                     ;; appease the shell.
                                                     (s-replace "'" "'\\''" (f-dirname path))
                                                     (s-replace "'" "'\\''" path))
                                      )))

              (if (s-blank-p git-query-result)
                nil (substring git-query-result 0 10))))

          (history-link
            (format "https://github.com/neeasade/neeasade.github.io/commits/source/%s/%s"
              (if is-post "posts" "pages")
              (f-filename path)
              ))

          (org-file-content
            (-map
              ;; call our custom hsep macro for delimiters
              (fn (if (string= <> "-----") "{{{hsep}}}" <>))
              (s-split "\n" (org-file-contents path))))

          (published-date
            (when is-post
              (let ((internal-pubdate (-first (fn (s-starts-with-p "#+pubdate:" <>)) org-file-content)))
                (if internal-pubdate
                  (first (s-match (pcre-to-elisp "[0-9]{4}-[0-9]{2}-[0-9]{2}") internal-pubdate))
                  (substring (f-base path) 0 10)))))

          (post-title
            (->> org-file-content
              (-first (fn (s-starts-with-p "#+title:" <>)))
              (s-replace "#+title: " "")))

          (post-subtitle
            (->> org-file-content
              (-first (fn (s-starts-with-p "#+title_extra:" <>)))
              ((lambda (found)
                 (when found
                   (s-replace "#+title_extra: " "" found))))))

          (post-org-content-lines
            (-non-nil
              `(,(format "#+SETUPFILE: %s" (ns/blog-path "site/assets/org/setup.org"))
                 ,@(-map
	                   (fn (let* ((file-path (ns/blog-path (format "site/assets/css/%s.css" <>)))
		                             (include-path (format "/assets/css/%s.css" <>))
		                             (sum (ns/shell-exec (format "md5sum '%s' | awk '{print $1}'" file-path))))
	                         (concat
		                         (format "#+HTML_HEAD: <link rel='stylesheet' href='%s?sum=%s'>" include-path sum)
		                         (format "\n#+HTML_HEAD: <link rel='stylesheet' href='.%s?sum=%s'>" include-path sum))))
	                   '("org" "colors" "notes" "new"))

                 ,(format "@@html:<h1 class=title><a href=\"%s\">%s</a> </h1>@@" "/index.html" post-title)

                 ,(when is-post
                    (ns/blog-make-nav-strip
                      (if (s-equals-p published-date last-edited)
                        (format "%s" published-date)
                        (format
                          "%s (edited [[%s][%s]])"
                          published-date
                          history-link
                          last-edited
                          ))

                      ;; (format
                      ;;   "%s (edited [[%s][%s]])"
                      ;;   published-date
                      ;;   history-link
                      ;;   last-edited
                      ;;   )

                      ;; (format "[[%s][%s]]" history-link last-edited)
                      ))

                 ,@(when (not (s-blank-p post-subtitle))
                     (list
                       ;; "#+begin_center"
                       post-subtitle
                       ;; "#+end_center"
                       ))

                 "-----"
                 ,@org-file-content
                 "-----"

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
                 "#+begin_center"
                 "#+BEGIN_EXPORT html"
                 "<a href='https://webring.xxiivv.com/#random' target='_blank'><img style='width:40px;height:40px' src='https://webring.xxiivv.com/icon.black.svg'/></a>"
                 "<a href='https://github.com/nixers-projects/sites/wiki/List-of-nixers.net-user-sites' target='_blank'><img style='width:35px;height:40px' src='https://i.imgur.com/cttKKiq.png'/></a>"
                 "<a href='https://webring.recurse.com'><img alt='Recurse Center Logo' src='https://resevoir.net/webring/icon.png' style='height:40px;width:40px;'></a>"
                 "#+END_EXPORT "
                 "#+end_center")))


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
                      (funcall
                        (apply '-compose
                          (mapcar (lambda (char)
                                    (lambda (s) (s-replace (char-to-string char) "" s))) ";/?:@&=+$,"))))))

      (:edited-date last-edited)
      (:history-link history-link)
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
      (lambda (post) (with-temp-buffer
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
    (ct/shorten color)
    (ht-get ns/theme (if (ct/is-light-p color) :foreground :background))
    (if (ct/is-light-p color) (ht-get ns/theme :foreground) color)
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
      (ht-get ns/theme (if (ct/is-light-p color) :foreground :background)))
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
                       (-map 'ct/shorten c)
                       (ct/shorten c))
                     ) colors)
             (or labels (-map (lambda (_) "") (range (length colors))))))

       "@@html: </div> @@"
       )))

;; (ns/blog-make-color-preview #cccccc")
