;; -*- lexical-binding: t; -*-

(setq ns/blog-title "notes")
(setq ns/blog-cache (-ht))

;;* compat
(defalias 'tarp/get 'myron-get)
(defalias 'ns/shell-exec 'sh)

(defun range (one &optional two step)
  (let* ((start (if two one 0))
          (end (if two two one))
          (step (or step (if (> end start) 1 -1))))
    (cond
      ((= end start) (list start))
      ((> end start)
        (number-sequence start (- end 1) step))
      ((< end start)
        (number-sequence start (+ 1 end) step)))))

;;* macros/content
(defun ns/blog-make-nav-strip (&rest items)
  (->> (-remove 'not items) (s-join " ")))

(defun ns/blog-make-color-preview (color &optional text)
  ;; assumes a dark FG and light BG
  (format
    "@@html:<code style=\"background: %s;color: %s; padding: 2px; border: 1px solid %s\">%s</code>@@"
    color
    (tarp/get (if (ct-is-light-p color) :foreground :background))
    (if (ct-is-light-p color) (tarp/get :foreground) color)
    ;; (tarp/get :background :strong)
    (if (not (s-equals? "" (or text "")))
      text color)))

(defun ns/blog-make-color-preview-extended (bg fg text)
  ;; assumes a dark FG and light BG
  (format
    "@@html:<code style=\"background: %s;color: %s; padding: 2px; border: 1px solid %s\">%s</code>@@"
    bg fg
    (if (ct-is-light-p bg) (tarp/get :foreground) bg)
    text))

(defun ns/blog-make-detail (&rest parts)
  ;; this is done so I don't have to escape commas in details
  (format "@@html:<detail>@@%s@@html:</detail>@@"
    (s-join "," (-remove 'not parts))))

(defun ns/blog-make-color-block (width color &optional text foreground class)
  ;; assumes a dark FG and light BG
  (format
    "@@html:<div class=\"%s\" style=\"background: %s;color: %s; width: %s%%;\">%s</div>@@"
    (or class "colorblock colorcenter")
    color
    (if foreground foreground
      (tarp/get (if (ct-is-light-p color) :foreground :background)))
    width (or text "")))

(defun ns/blog-make-color-strip (colors &optional labels)
  (s-join "\n"
    `("@@html: <div style='display: flex; flex-wrap: wrap; justify-content: center;'>  @@"
       ,@(-map
           (lambda (c)
             (ns/blog-make-color-block
               (/ 100.0 (length colors))
               (first c)
               (cdr c)))
           (-zip-pair colors
             (or labels (-map (lambda (_) "") (-iota (length colors))))))
       "@@html: </div>@@")))

;;* render helpers

(defun ns/blog-make-hsep ()
  (s-join "\n"
    '("#+BEGIN_EXPORT html"
       "<div class=separator> ∗ ∗ ∗ </div>"
       "#+END_EXPORT")))

(defun ns/blog-get-csslinks ()
  (ht-get-cache ns/blog-cache :csslinks
    (fn (->>
          ;; '("colors" "new" "org" "notes")
          '("colors" "normalize" "magick" "notes")
	        (--mapcat (llet [file-path (ns/blog-path (format "published/assets/css/%s.css" it))
		                        include-path (format "/assets/css/%s.css" it)
		                        sum (sh (format "md5sum '%s' | awk '{print $1}'" file-path)) ]
	                    (list
		                    (format "#+HTML_HEAD: <link rel='stylesheet' href='%s?sum=%s'>" include-path sum)
		                    (format "#+HTML_HEAD: <link rel='stylesheet' href='.%s?sum=%s'>" include-path sum))))
          (s-join "\n")))))

(defun ns/blog-make-anchors ()
  ;; nb: this is also used in the rice.org file
  "turn headlines into anchor links within a string org-content."
  (org-ml-update-headlines 'all
    (lambda (headline)
      (let* ((heading-text (-> headline org-ml-headline-get-path last car))
              (id (org-ml-headline-get-node-property "CUSTOM_ID" headline))
              (id (if id id
                    (->> heading-text
                      (s-replace " " "-")
                      (s-replace-regexp (pcre-to-elisp/cached "\\[\\[(.*)\\]\\[") "")
                      (s-replace-regexp (pcre-to-elisp/cached "\\]\\]") "")))))
        (->> headline
          (org-ml-headline-set-node-property "CUSTOM_ID" id)
          (org-ml-set-property :title
            (let ((heading-text (-if-let (m (s-match ; remove old heading anchor style
                                              (pcre-to-elisp/cached "\\[\\[\\#(.*)\\]\\[(.*)\\]\\]")
                                              heading-text))
                                  (third m) heading-text)))
              (list
                (format "%s %s"
                  heading-text
                  (format "[[#%s][%s]]" id "#"))))))))))

(defun ns/blog-next-map ()
  "Return a plist of (post-url (prev next))"
  (llet [posts (->> (ns/blog-get-metas)
                 (--filter (string= (ht-get it :type) "post"))
                 (--remove (ht-get it :draft-p))
                 (--filter (ht-get it :edited-date))
                 (--sort (string<
                           (ht-get it :published-date)
                           (ht-get other :published-date)))
                 (--map (list
                          (ht-get it :url)
                          (ht-get it :title))))]
    (apply '-ht
      `(,(first (first posts)) (nil ,(second posts))
         ,@(-mapcat
             (-lambda ((prev current next))
               (list (first current) (list prev next)))
             (-partition-in-steps 3 1 posts))
         ,(first (-last-item posts)) (,(first (-take-last 2 posts)) nil)))))

(ns/use mustache)
(require 'xml)

(defun ns/mustache (text table)
  (llet [mustache-key-type 'keyword]
    ;; debug, print nil keys (can be used as bools for mustache but not templated)
    (comment
      (ht-map
        (lambda (k v)
          (when (and (not v) (not (s-starts-with? "is-" (ns/str k))))
            (message (ns/str "nil key: " k))))
        table))
    (-> text
      (mustache-render (ht->alist table))
      (xml-substitute-special))))

;;* render
;; for fontifying src blocks
(ns/use htmlize)

(ns/use (ox-rss
          :host github
          :repo "emacsmirror/ox-rss"))

(defun ns/blog-path (ext)
  (ns/str (or (getenv "NS_BLOG_PATH") (~ "code/neeasade.github.io/")) ext))

(defun ns/blog-get-properties (text)
  "org string to properties ht. if a value is blank it is not included"
  (->> (org-ml--from-string text)
    (org-ml-match '(keyword))
    (-keep (-lambda ((_ (&plist :key :value)))
             (when-not (s-blank? value)
               (list (downcase key) value))))
    (apply '-concat)
    (apply '-ht)))

(defun ns/path-to-slug (path)
  "File path to html slug (basename)"
  (->> (--reduce-from (s-replace it "" acc)
         (f-base path)
         (s-split "" ";/?:@&=+$,'"))
    (s-replace-regexp (pcre-to-elisp "[0-9]{4}-[0-9]{2}-[0-9]{2}-") "")
    (s-downcase)))

(defun ns/blog-file-to-meta (path)
  "File path to metadata ."
  ;; (message (format "BLOG: generating meta for %s" path))
  (llet [org-file-content (f-read path)
          props (ns/blog-get-properties org-file-content)
          type (llet [parent-dir (->> path f-parent f-base)]
                 (substring parent-dir 0 (1- (length parent-dir))))]
    (-ht :path path
      :content (s-replace-regexp "^-----$" (ns/blog-make-hsep) org-file-content)
      :draft-p (ht-get props "draft")
      :title (ht-get props "title" "(untitled)")
      :rss-title (ht-get props "rss_title")
      :subtitle (ht-get props "title_extra" "")
      :is-index (s-starts-with-p "index" (f-filename path))
      :type type
      :is-page (string= type "page")
      :is-post (string= type "post")
      :is-note (string= type "note")
      :is-doodle (string= type "doodle")
      :published-date (first
                        (--keep (first (s-match (pcre-to-elisp "[0-9]{4}-[0-9]{2}-[0-9]{2}") it))
                          (list (ht-get props "pubdate" "")
                            (f-base path))))
      :edited-date (let ((git-query-result (sh (format "cd '%s'; git log --follow -1 --format=%%cI '%s'"
                                                 ;; appease the shell.
                                                 (s-replace "'" "'\\''" (f-dirname path))
                                                 (s-replace "'" "'\\''" path)))))
                     (if (s-blank-p git-query-result) ""
                       (substring git-query-result 0 10)))
      :url (format "https://notes.neeasade.net/%s.html" (ns/path-to-slug path))
      :html-dest (format "%s/%s.html" (ns/blog-path "published") (ns/path-to-slug path))
      :csslinks (ns/blog-get-csslinks))))

(defun ns/blog-render-org (org-meta-table)
  (ns/mustache (f-read (~e "org/blog_template.org"))
    (ht-merge org-meta-table
      (llet ((&hash :type :path :subtitle :published-date :edited-date :title :url) org-meta-table
              next-map (ht-get-cache ns/blog-cache :next-map 'ns/blog-next-map)
              ((prev-url prev-title) (next-url next-title)) (ht-get next-map url))
        (-ht
          :up (llet [(dest label)
                      (cond
                        ((s-starts-with-p "index" (f-filename path)) '("https://neeasade.net" "splash"))
                        ((and (string= type "page") (not (string= (f-base path) "sitemap")))
                          '("/sitemap.html" "sitemap"))
                        (t `("/index.html" ,ns/blog-title)))]
                (format "<a href='%s'>../%s</a>" dest label))
          :og-description (->> (or subtitle "")
                            (s-replace-regexp "{{{.*(" "")
                            (s-replace ")}}}" ""))
          :page-markup-link (format "https://raw.githubusercontent.com/neeasade/neeasade.github.io/source/%ss/%s"
                              type (f-filename path))
          :page-history-link (format "https://github.com/neeasade/neeasade.github.io/commits/source/%ss/%s"
                               type (f-filename path))
          :page-title (if (s-starts-with-p "index" (f-filename path)) ns/blog-title title)
          :next-post (and next-url (format "<a href='%s'>newer: %s</a>" next-url next-title))
          :prev-post (and prev-url (format "<a href='%s'>older: %s</a>" prev-url prev-title))
          :is-edited (and (not (s-blank? edited-date)) (not (string= published-date edited-date))))))))

(defun! ns/blog-publish-meta (org-meta)
  (llet (default-directory (ns/blog-path "published")
          org-html-divs '((preamble  "div" "preamble")
                           (content   "main" "content")
                           (postamble "div" "postamble"))

          org-export-time-stamp-file nil
          org-export-with-date nil
          org-export-with-timestamps nil
          org-export-with-toc nil
          org-html-head ""
          org-html-head-extra ""
          org-html-head-include-default-style nil
          org-html-head-include-scripts nil
          org-html-postamble nil
          org-html-preamble nil
          org-html-use-infos nil

          org-export-with-section-numbers t
          org-export-with-smart-quotes t
          org-export-with-title nil
          org-html-doctype "html5"
          org-html-html5-fancy t
          org-html-table-align-individual-fields nil ; sus - speeds up export
          org-use-sub-superscripts "{}"

          ;; affects timestamp export format
          org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %I:%M %p>")
          org-display-custom-times t

          ;; don't ask about generation when exporting
          org-confirm-babel-evaluate (fn nil))

    (message "BLOG: making %s " (ht-get org-meta :path))
    (shut-up
      (with-temp-buffer
        (org-mode)
        (insert (ns/blog-render-org org-meta))
        (ns/blog-make-anchors)
        (org-export-to-file 'html (ht-get org-meta :html-dest))))))

;; idea: auto refresh on save or on change might be nice
(defun! ns/blog-generate-and-open-current-file ()
  (when-not (s-contains? (ns/blog-path "") default-directory)
    (error "not a blog file"))

  (setq ns/theme (ht-get myron-themes-colors :normal)) ; compat
  (save-buffer)
  (llet (file-meta (-> (current-buffer) buffer-file-name ns/blog-file-to-meta)
          post-html-file (ht-get file-meta :html-dest))
    (ns/blog-publish-meta file-meta)

    (message post-html-file)
    (if (string= (concat "file://" post-html-file) (sh "qb_active_url"))
      (sh "qb_command :reload")
      (browse-url post-html-file))))

(defun ns/blog-changed-files-metas ()
  (llet [default-directory (ns/blog-path "published")
          last-published-date (sh "git log -n 1 --format=%cd")
          default-directory (ns/blog-path ".")
          files-changed (sh (format "git diff --name-only --since=\"{%s}\" --until=now" last-published-date))]
    (->> files-changed
      (s-split "\n")
      (-remove (-partial 's-starts-with-p "rss"))
      (-map 'ns/blog-path)
      (-filter (-partial #'s-ends-with-p ".org"))
      ;; force indexes to be regenerated all the time
      (append
        (f-entries (ns/blog-path "pages")
          (lambda (f) (s-starts-with-p "index" (f-base f)))))
      (-uniq)
      (-map 'ns/blog-file-to-meta))))

(defun ns/get-blog-files ()
  "return a map of title -> filepath"
  (ht-get-cache ns/blog-cache :blog-files
    (lambda ()
      (->> '("posts" "pages" "notes" "doodles")
        (--mapcat (f-entries (ns/blog-path it) (-partial 's-ends-with-p ".org")))
        (--remove (s-starts-with? ".#" (f-base it)))
        (reverse)
        (--mapcat (list it (ht-get (ns/blog-get-properties (slurp it)) "title" "untitled")))
        (-flatten)
        (apply '-ht)))))

(defun ns/blog-get-metas ()
  (ht-get-cache ns/blog-cache :blog-metas
    (fn (-map 'ns/blog-file-to-meta
          (ht-keys (ns/get-blog-files))))))

(defun ns/blog-generate (metas)
  (setq
    ns/theme (ht-get myron-themes-colors :normal) ; compat
    ns/blog-cache (-ht))                  

  ;; need to define these here for index listings and rss:
  (message "BLOG: making pages!")

  (when-not (= (length (--map (ht-get it :html-dest) metas))
              (length (-uniq (--map (ht-get it :html-dest) metas))))
    (error "BLOG: conflicting html-dests! not generating"))

  (llet (;; don't ask about generation when exporting
          org-confirm-babel-evaluate (fn nil))

    ;; todo redirects
    (-map #'ns/blog-publish-meta metas)

    ;; (message "BLOG: making site rss!")
    (with-current-buffer (find-file-noselect (ns/blog-path "rss/rss.org"))
      (org-export-to-file 'rss (ns/blog-path "published/rss.xml")))
    (with-current-buffer (find-file-noselect (ns/blog-path "rss/rss_full.org"))
      (org-export-to-file 'rss (ns/blog-path "published/rss_full.xml"))))

  (message "BLOG: done! ✨✨✨✨")
  t)

(defun! ns/blog-generate-changed-files ()
  (ns/blog-generate (ns/blog-changed-files-metas)))

(defun! ns/blog-generate-all-files ()
  (ns/blog-generate (ns/blog-get-metas)))

(defun org-publish-ignore-mode-hooks (orig-func &rest args)
  (let ((lexical-binding nil))
    (cl-letf (((symbol-function #'run-mode-hooks) #'ignore))
      (apply orig-func args))))

(advice-add 'ns/blog-generate :around #'org-publish-ignore-mode-hooks)

(defun! ns/blog-new-post ()
  (let* ((title (s-trim (read-from-minibuffer "new blog post title: ")))
          (file (format (~ "code/neeasade.github.io/posts/%s.org")
                  (s-replace " " "-" title))))
    (find-file file)
    (insert (->> (-ht :title title
                   :title_extra nil
                   :filetags nil
                   :rss_title nil
                   :draft t
                   :pubdate (sh "date '+<%Y-%m-%d>'"))
              (ht-map (lambda (k v) (ns/str "#+" k ": " v)))
              (s-join "\n"))))
  (setq ns/blog-cache (-ht)))

(ns/bind
  "ob" 'ns/blog-generate-and-open-current-file
  "ii" (fn!! insert-org-image  ;todo: this can maybe be a more general concept?
         (when-not (s-contains? (ns/blog-path "") default-directory)
           (error "not a blog file"))
         (llet [name (read-string "image slug (copying from shot.png): ")
                 name (ns/str (f-base name) ".png")
                 dest (ns/path (ns/blog-path "published/assets/posts") name)]
           (f-copy (~ "Last_Shot/shot.png") dest)
           (insert (format "{{{image(%s)}}}" name))))
  ;; broken, should be html slug or something
  ;; "iq" (fn!! insert-blog-link
  ;;        (llet [posts (ns/get-blog-files)
  ;;                title (ns/pick (ht-values posts))
  ;;                file (first (ht-keep (lambda (k v) (and (string= title v) k)) posts))]
  ;;          (insert (format "[[./%s.org][%s]]" (f-base file) title))))
  "nq" (fn!! surf-blog-posts
         (llet [posts (ns/get-blog-files)
                 title (ns/pick (ht-values posts))
                 file (first (ht-keep (lambda (k v) (and (string= title v) k)) posts))]
           (find-file file)))
  "nQ" (fn!! surf-blog-posts-draft
         (llet [posts (ns/get-blog-files)
                 title (ns/pick (ht-keep (lambda (file title)
                                           (and (ht-get (ns/blog-get-properties (slurp file)) "draft")
                                             title)) posts))
                 file (first (ht-keep (lambda (k v) (and (string= title v) k)) posts))]
           (find-file file))))
