;; -*- lexical-binding: t; -*-

;; for fontifying src blocks
(ns/use htmlize)

(ns/use (ox-rss
          :host github
          :repo "emacsmirror/ox-rss"))

(setq ns/blog-title "🍃🌳ＧＲＯＶＥ🍃🌳")
(setq ns/blog-title  "𝔊𝔄𝔗𝔈𝔚𝔄𝔜⟴" )

(defun ns/blog-set-htmlize-colors ()
  ;; this is for weak emphasis in code blocks
  (llet [theme-colors (append
                        (tarp/map-to-base16 :weak)
                        (ht-to-plist (ht-get tarp/theme* :weak)))]
    (setq htmlize-face-overrides
      (->> (tarp/theme-make-faces theme-colors)
        (-mapcat
          (-lambda ((face . spec))
            (list face (base16-theme-transform-spec spec theme-colors))))))))

(defun ns/mustache (text table)
  "Basic mustache templating."
  ;; This function exists because of https://github.com/Wilfred/mustache.el/issues/14
  (-reduce-from
    (lambda (text key)
      (s-replace
        (format "{{%s}}"
          (cond
            ((stringp key) key)
            ((keywordp key) (substring (pr-str key) 1))
            (t (pr-str key))))
        ;; nil vals can exist
        (or (ht-get table key) "")
        text))
    text
    (ht-keys table)))

(defun ns/blog-path (ext)
  (format (~ "git/neeasade.github.io/%s") ext))

(ns/bind-soft
  ;; todo: jump by site title
  "nq" (fn!
         (ivy-read "post: "
           (reverse
             (f-entries (ns/blog-path "posts")
               (fn (s-ends-with-p ".org" <>))))
           :action 'find-file))

  "nQ" (fn!
         (ivy-read "draft post: "
           (reverse
             (f-filter
               (fn

                 (ns/blog-get-prop "draft")
                 (f-read <>))

               (f-entries (ns/blog-path "posts")
                 (fn (s-ends-with-p ".org" <>)))))
           :action 'find-file)))

(defun ns/blog-get-prop (propname text)
  "Get an org property out of text"
  ;; nb: this is case insensitive
  (-some--> (format "#\\+%s:.*$" propname)
    (pcre-to-elisp it)
    (s-match it text)
    (first it)
    (s-replace (format "#+%s:" propname) "" it)
    (s-trim it)
    (if (s-blank-p it) nil it)))

(defun ns/blog-make-nav-strip (&rest items)
  ;; ??????????????????????????
  (apply 'concat
    (list "\n#+BEGIN_CENTER\n"
      (->> (-remove 'not items) (s-join " "))
      "\n#+END_CENTER\n")))

(defun ns/html-link (link label)
  (format "<a href='%s'>%s</a>" dest label))

(defun ns/blog-render-org (path &rest args)
  (llet [(&plist :post-type :title :last-edited :subtitle :content :html-dest) args]
    (ns/mustache
      (f-read (~e "org/blog_template.org"))
      (-ht
        :blog-title ns/blog-title
        :title title
        :last-edited last-edited
        :subtitle subtitle
        :content content
        :html-dest html-dest

        ;; cache invalidation
        :csslinks (s-join "\n"
                    (-map
	                    (fn (let* ((file-path (ns/blog-path (format "site/assets/css/%s.css" <>)))
		                              (include-path (format "/assets/css/%s.css" <>))
		                              (sum (ns/shell-exec (format "md5sum '%s' | awk '{print $1}'" file-path))))
	                          (concat
		                          (format "#+HTML_HEAD: <link rel='stylesheet' href='%s?sum=%s'>" include-path sum)
		                          (format "\n#+HTML_HEAD: <link rel='stylesheet' href='.%s?sum=%s'>" include-path sum))))
	                    '("colors" "new" "org" "notes")))

        :up (llet [(dest label)
                    (cond ((s-starts-with-p "index" (f-filename path)) '("https://neeasade.net" "Splash"))
                      ((and (string= post-type "page") (not (string= (f-base path) "sitemap")))
                        '("/sitemap.html" "Sitemap"))
                      (t `("/index.html" ,ns/blog-title)))]
              (format "<a href='%s'>Up: %s</a>" dest label))

        ;; remove a common macro for og:description:
        :og-description (->> (or subtitle "") (s-replace "{{{center(" "") (s-replace ")}}}" ""))

        :page-markup-link (format "https://raw.githubusercontent.com/neeasade/neeasade.github.io/source/%ss/%s"
                            post-type (f-filename path))

        :page-history-link (format "https://github.com/neeasade/neeasade.github.io/commits/source/%ss/%s"
                             post-type (f-filename path))

        :footer-left (if (s-starts-with-p "index" (f-filename path))
                       "<a href='/sitemap.html'>Sitemap</a>"
                       (format "<a href='/index.html'>%s</a>" ns/blog-title))

        :footer-center (if-not (s-starts-with-p "index" (f-filename path))
                         (format "@@html:<div class=footer-center>type: %s</div>@@" post-type)
                         "#+BEGIN_EXPORT html
  <div class=footer-center>
  <a href='https://webring.xxiivv.com/#random' target='_blank'><img style='width:40px;height:40px' src='./assets/img/logos/xxiivv.svg'/></a>
  <a href='https://github.com/nixers-projects/sites/wiki/List-of-nixers.net-user-sites' target='_blank'><img style='width:35px;height:40px' src='./assets/img/logos/nixers.png'/></a>
  <a href='https://webring.recurse.com'><img alt='Recurse Center Logo' src='./assets/img/logos/recurse.png' style='height:40px;width:40px;'></a>
  </div>
  #+end_export
  ")

        :flair (when (or (string= post-type "post")
                       (string= post-type "note"))
                 "@@html:<div class='title flair'><img class='flair-border' src='./assets/img/backgrounds/pattern_125.gif' /> </div>@@")))))


(defun ns/blog-file-to-meta (path)
  (message (format "BLOG: generating meta for %s" path))
  (llet (org-file-content (->> (f-read path)
                            (s-replace-regexp "^-----$" "{{{hsep}}}")
                            (ns/blog-make-anchors))

          post-type (or (ns/blog-get-prop "post_type" org-file-content)
                      (llet [parent-dir (->> path f-parent f-base)]
                        (substring parent-dir 0 (1- (length parent-dir)))))

          last-edited (let ((git-query-result (ns/shell-exec (format "cd '%s'; git log -1 --format=%%cI '%s'"
                                                               ;; appease the shell.
                                                               (s-replace "'" "'\\''" (f-dirname path))
                                                               (s-replace "'" "'\\''" path)))))
                        (when-not (s-blank-p git-query-result)
                          (substring git-query-result 0 10)))

          published-date (when (-contains-p '("post" "note") post-type)
                           (-if-let (internal-pubdate (ns/blog-get-prop "pubdate" org-file-content))
                             (first (s-match (pcre-to-elisp "[0-9]{4}-[0-9]{2}-[0-9]{2}") internal-pubdate))
                             (substring (f-base path) 0 10)))

          post-title (or (ns/blog-get-prop "title" org-file-content) "untitled")
          post-subtitle (ns/blog-get-prop "title_extra" org-file-content)

          html-dest (format "%s/%s.html" (ns/blog-path "site")
                      (->> (f-base path)
                        (s-replace-regexp (pcre-to-elisp "[0-9]{4}-[0-9]{2}-[0-9]{2}-") "")
                        ;; remove forbidden characters from url
                        (funcall
                          (apply '-compose
                            (-map (lambda (char)
                                    (lambda (s) (s-replace (char-to-string char) "" s))) ";/?:@&=+$,'")))))

          post-org-content (ns/blog-render-org path
                             :title post-title
                             :last-edited last-edited
                             :subtitle post-subtitle
                             :post-type post-type
                             :content org-file-content
                             :html-dest html-dest))

    (ht
      (:path path)
      (:org-content post-org-content)
      (:is-draft (not (s-blank-p (ns/blog-get-prop "draft" post-org-content))))
      (:title post-title)
      (:type post-type)
      (:publish-date published-date)
      (:rss-title (ns/blog-get-prop "rss_title" post-org-content))
      (:html-dest html-dest)
      (:edited-date last-edited))))

(defun! ns/blog-publish-meta (org-meta)
  (let ((default-directory (ns/blog-path "site"))
         (org-export-with-toc nil)
         (org-export-with-section-numbers t)
         (org-export-with-timestamps nil)
         (org-export-with-date nil)
         (org-html-html5-fancy t)
         (org-export-with-title nil)
         (org-export-with-smart-quotes t)
         (org-html-doctype "html5")

         ;; affects timestamp export format
         ;; (org-time-stamp-custom-formats '("%Y-%m-%d" . "%Y-%m-%d %I:%M %p"))
         (org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %I:%M %p>"))
         (org-display-custom-times t)

         ;; don't ask about generation when exporting
         (org-confirm-babel-evaluate (fn nil)))

    (with-temp-buffer
      (llet ((&hash :path :html-dest :org-content) org-meta)
        (message (format "BLOG: making %s " path))
        (org-mode)
        (insert org-content)
        (org-export-to-file 'html html-dest)))))

;; idea: auto refresh on save or on change might be nice
(defun! ns/blog-generate-and-open-current-file ()
  (save-buffer)
  (llet (file-meta (-> (current-buffer) buffer-file-name ns/blog-file-to-meta)
          post-html-file (ht-get file-meta :html-dest))

    (ns/blog-publish-meta file-meta)

    (message post-html-file)
    (if (string= (concat "file://" post-html-file) (ns/shell-exec "qb_active_url"))
      (ns/shell-exec "qb_command :reload")
      (browse-url post-html-file))))

(defun ns/blog-get-org (path)
  "get org files in PATH relative to blog repo"
  (f-entries (ns/blog-path path) (fn (s-ends-with-p ".org" <>))))

(defun! ns/blog-generate ()
  ;; cleanup
  ;; (mapcar 'f-delete
  ;;   (f-entries ns/blog-site-dir
  ;;     (fn (s-ends-with-p ".html" <>))))

  ;; need to define these here for index listings and rss:
  (setq
    org-post-metas (-map 'ns/blog-file-to-meta (ns/blog-get-org "posts"))
    org-page-metas (-map 'ns/blog-file-to-meta (ns/blog-get-org "pages"))
    org-note-metas (-map 'ns/blog-file-to-meta (ns/blog-get-org "notes")))

  (llet (;; don't ask about generation when exporting
          org-confirm-babel-evaluate (fn nil))

    (message "BLOG: making pages!")
    (-map 'ns/blog-publish-meta (append org-post-metas org-page-metas org-note-metas))

    (message "BLOG: making site rss!")
    (require 'ox-rss)

    (with-current-buffer (find-file-noselect (ns/blog-path "rss/rss.org"))
      (org-export-to-file 'rss (ns/blog-path "site/rss.xml")))

    (with-current-buffer (find-file-noselect (ns/blog-path "rss/rss_full.org"))
      (org-export-to-file 'rss (ns/blog-path "site/rss_full.xml"))))

  (message "BLOG: done! ✨✨✨✨")
  t)

(defun ns/blog-make-anchors (org-content)
  ;; todo: this is a rather expensive part
  "turn headlines into anchor links within a string org-content."
  (with-temp-buffer
    (insert org-content)
    (->> (org-ml-parse-headlines 'all)
      (-map (-lambda ((kind props))
              (plist-get props :begin)))
      (reverse)
      (-map (lambda (point)
              ;; give a textual id if one doesn't exist
              (org-ml-update-headline-at point
                (lambda (headline)
                  (let* ((heading-text (-> headline org-ml-headline-get-path last car))
                          (id
                            ;; todo: use org-ml-headline-get-node-properties
                            ;; and check contains -- couldn't get that to work though
                            ;; <2021-01-25 Mon 09:18>
                            (catch 'error
                              (condition-case err
                                (org-ml-headline-get-node-property "CUSTOM_ID" headline)
                                (error nil))))
                          (id (if id id
                                (->> heading-text
                                  (s-replace " " "-")
                                  (s-replace-regexp (pcre-to-elisp/cached "\\[\\[(.*)\\]\\[") "")
                                  (s-replace-regexp (pcre-to-elisp/cached "\\]\\]") "")))))

                    (->> headline
                      (org-ml-headline-set-node-property "CUSTOM_ID" id)
                      (org-ml-set-property
                        :title
                        (->> heading-text
                          ;; remove old headling anchor style:
                          ((lambda (s)
                             (-if-let (m (s-match
                                           (pcre-to-elisp/cached "\\[\\[\\#(.*)\\]\\[(.*)\\]\\]")
                                           s))
                               (third m)
                               s)))
                          ;; add anchor at the end:
                          ((lambda (s)
                             (format "%s @@html:<span class=anchor>@@%s@@html:</span>@@" s
                               (format "[[#%s][%s]]" id "⚓"))))
                          (list)))))))
              )))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun! ns/blog-new-post ()
  (let* ((title (read-from-minibuffer "new blog post title: "))
          (file (format (~ "git/neeasade.github.io/posts/%s.org")
                  (s-replace " " "-" title))))
    (find-file file)
    (insert (format "
#+title: %s
#+title_extra:
#+post_type: post
#+filetags:
#+rss_title:
#+draft: t
#+pubdate: %s
 "
              title
              (ns/shell-exec "date '+<%Y-%m-%d>'")))))

;;;
;;; here down are content helpers -- many of these have correlating macros in the template
;;;

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
             (or labels (-map (lambda (_) "") (-iota (length colors))))))

       "@@html: </div>@@"
       )))

(defun ns/blog-make-hsep ()
  (format "\n#+begin_center\n%s\n#+end_center\n"
    (let* ((options "🍇🍉🍓🍅🍄🍈🍍")
            (options "🍂🌿🌱🍁🍀")
            (options "🪨⛰️🗿🌋")
            (index (random (length options)))
            (char (substring options index (+ index 1))))
      (format "%s %s %s" char char char))))

(ns/comment
  (measure-time
    (progn
      (-map 'ns/blog-file-to-meta (ns/blog-get-org "posts"))
      nil
      )


    )
  )
