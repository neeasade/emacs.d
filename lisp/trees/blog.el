;; -*- lexical-binding: t; -*-

;; compat
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

;; for fontifying src blocks
(ns/use htmlize)

(ns/use (ox-rss
          :host github
          :repo "emacsmirror/ox-rss"))

(setq ns/blog-title "⯨𝓟𝓸𝓬𝓴𝓮𝓽 𝓟𝓪𝓻𝓬𝓮𝓵𝓼 ⯩")

(defun ns/blog-set-htmlize-colors ()
  ;; this is for weak emphasis in code blocks
  (llet [theme-colors (append
                        (myron-theme-to-base16 :weak)

                        (ht-to-plist (ht-get myron-theme* :weak)))]
    (setq htmlize-face-overrides
      (->> (tarp/theme-make-faces theme-colors)
        (-mapcat
          (-lambda ((face . spec))
            (list face (base16-theme-transform-spec spec theme-colors))))))))

(defun ns/blog-path (ext)
  (format (~ "git/neeasade.github.io/%s") ext))

(defun ns/mustache (text table)
  "Basic mustache templating."
  (llet [replace-map (ht-amap `(,(format "{{%s}}" (ns/str key)) . ,(or value "")) table)]
    ;; doing this twice is what enables a #+title: of {{blog-title}}
    (->> text
      (s-replace-all replace-map)
      (s-replace-all replace-map))))

(ns/bind
  "nq" (fn!! surf-blog-posts
         (llet [posts (->> (f-entries (ns/blog-path "posts")
                             (-partial 's-ends-with-p ".org"))
                        (reverse)
                        (--mapcat (list (or (ht-get (ns/blog-get-properties (f-read it)) "title") it)
                                    it))
                        (apply '-ht))]
           (find-file (ht-get posts (ns/pick (ht-keys posts))))))

  "nQ" (fn!! surf-blog-drafts
         (find-file
           (ns/pick "draft post"
             (reverse
               (--filter
                 (ht-get (ns/blog-get-properties (f-read it)) "draft")
                 (f-entries (ns/blog-path "posts")
                   (fn (s-ends-with-p ".org" <>)))))))))

(defun ns/blog-get-properties (text)
  "org string to properties ht"
  (->> (org-ml--from-string text)
    (org-ml-match '(keyword))
    (-mapcat (-lambda ((_ (&plist :key :value)))
               (list (downcase key) value)))
    (apply '-ht)))

(defun ns/blog-make-nav-strip (&rest items)
  ;; ??????????????????????????
  (apply 'concat
    (list "\n#+BEGIN_CENTER\n"
      (->> (-remove 'not items) (s-join " "))
      "\n#+END_CENTER\n")))

(defun ns/blog-render-org (post-table)
  (ns/mustache
    (->> (f-read (~e "org/blog_template.org"))
      (s-replace "{{content}}"
        (->> (ht-get post-table :text)
          (s-replace-regexp "^-----$" (ns/blog-make-hsep)))))

    (ht-merge post-table
      (llet ((&hash :type :path :subtitle) post-table)
        (-ht
          :blog-title ns/blog-title

          :up (llet [(dest label)
                      (cond ((s-starts-with-p "index" (f-filename path)) '("https://neeasade.net" "Splash"))
                        ((and (string= type "page") (not (string= (f-base path) "sitemap")))
                          '("/sitemap.html" "Sitemap"))
                        (t `("/index.html" ,ns/blog-title)))]
                (format "<a href='%s'>Up: %s</a>" dest label))


          :og-description (->> (or subtitle "")
                            (s-replace-regexp "{{{.*(" "")
                            (s-replace ")}}}" ""))

          :page-markup-link (format "https://raw.githubusercontent.com/neeasade/neeasade.github.io/source/%ss/%s"
                              type (f-filename path))

          :page-history-link (format "https://github.com/neeasade/neeasade.github.io/commits/source/%ss/%s"
                               type (f-filename path))

          :footer-left (apply 'format "<a href='/%s.html'>%s</a>"
                         (if (s-starts-with-p "index" (f-filename path))
                           '("sitemap" "Sitemap")
                           `("index" ,ns/blog-title)))

          :footer-center (when (s-starts-with-p "index" (f-filename path))
                           "<a href='https://webring.xxiivv.com/#random' target='_blank'><img style='width:40px;height:40px' src='./assets/img/logos/xxiivv.svg'/></a>
  <a href='https://github.com/nixers-projects/sites/wiki/List-of-nixers.net-user-sites' target='_blank'><img style='width:35px;height:40px' src='./assets/img/logos/nixers.png'/></a>
  <a href='https://webring.recurse.com'><img alt='Recurse Center Logo' src='./assets/img/logos/recurse.png' style='height:40px;width:40px;'></a> ")

          :flair (when (or (string= type "post")
                         (string= type "note"))
                   "@@html:<div class='flair'></div>@@"))))))

(defun ns/blog-file-to-meta (path)
  "File path to meta. Does not do anything that might need file processing."
  ;; (message (format "BLOG: generating meta for %s" path))
  (llet [org-file-content (f-read path)
          props (ns/blog-get-properties org-file-content)
          html-slug (->> (f-base path)
                      (s-replace-regexp (pcre-to-elisp "[0-9]{4}-[0-9]{2}-[0-9]{2}-") "")
                      ;; remove forbidden characters from url
                      (funcall
                        (apply '-compose
                          (-map (lambda (char)
                                  (lambda (s) (s-replace (char-to-string char) "" s))) ";/?:@&=+$,'"))))]
    (-ht
      :path path
      :text org-file-content
      :draft-p (not (s-blank-p (ht-get props "draft")))
      :title (ht-get props "title" "(untitled)")
      :rss-title (ht-get props "rss_title")
      :subtitle (ht-get props "title_extra")
      :type (llet [parent-dir (->> path f-parent f-base)]
              (substring parent-dir 0 (1- (length parent-dir))))

      :published-date (first
                        (--keep (first (s-match (pcre-to-elisp "[0-9]{4}-[0-9]{2}-[0-9]{2}") it))
                          (list (ht-get props "pubdate" "")
                            (f-base path))))

      :edited-date (let ((git-query-result (sh "cd '%s'; git log --follow -1 --format=%%cI '%s'"
                                             ;; appease the shell.
                                             (s-replace "'" "'\\''" (f-dirname path))
                                             (s-replace "'" "'\\''" path))))
                     (when-not (s-blank-p git-query-result)
                       (substring git-query-result 0 10)))
      :url (format "https://notes.neeasade.net/%s.html" html-slug)
      :html-dest (format "%s/%s.html" (ns/blog-path "published") html-slug)
      :csslinks (ns/blog-get-csslinks))))

(setq ns/blog-csslinks nil)
(defun ns/blog-get-csslinks ()
  (if ns/blog-csslinks ns/blog-csslinks
    (setq ns/blog-csslinks
      (s-join "\n"
        (-map
	        (fn (let* ((file-path (ns/blog-path (format "published/assets/css/%s.css" <>)))
		                  (include-path (format "/assets/css/%s.css" <>))
		                  (sum (sh (format "md5sum '%s' | awk '{print $1}'" file-path))))
	              (concat
		              (format "#+HTML_HEAD: <link rel='stylesheet' href='%s?sum=%s'>" include-path sum)
		              (format "\n#+HTML_HEAD: <link rel='stylesheet' href='.%s?sum=%s'>" include-path sum))))
	        '("colors" "new" "org" "notes"))))))

(defun! ns/blog-publish-meta (org-meta)
  (let ((default-directory (ns/blog-path "published"))
         (org-export-with-toc nil)
         (org-export-with-section-numbers t)
         (org-export-with-timestamps nil)
         (org-export-with-date nil)
         (org-html-html5-fancy t)
         (org-export-with-title nil)
         (org-export-with-smart-quotes t)
         (org-use-sub-superscripts "{}")
         (org-html-doctype "html5")

         ;; affects timestamp export format
         ;; (org-time-stamp-custom-formats '("%Y-%m-%d" . "%Y-%m-%d %I:%M %p"))
         (org-time-stamp-custom-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %I:%M %p>"))
         (org-display-custom-times t)

         ;; don't ask about generation when exporting
         (org-confirm-babel-evaluate (fn nil)))

    (with-temp-buffer
      (llet ((&hash :text :path :html-dest :org-content) org-meta)
        (message "BLOG: making %s " path)
        (insert (ns/blog-render-org org-meta))
        (ns/blog-make-anchors)
        (org-export-to-file 'html html-dest)))))

;; idea: auto refresh on save or on change might be nice
(defun! ns/blog-generate-and-open-current-file ()
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

(setq ns/blog-metas nil)

(defun ns/blog-get-metas ()
  (if ns/blog-metas ns/blog-metas
    (setq ns/blog-metas
      (->> '("posts" "pages" "notes")
        (--mapcat
          (f-entries (ns/blog-path it)
            (-partial #'s-ends-with-p ".org")))
        (-map 'ns/blog-file-to-meta)))))

(defun! ns/blog-generate ()
  (setq ns/theme (ht-get myron-theme* :normal)) ; compat
  (setq ns/blog-metas nil)                      ; cache bust
  (setq ns/blog-csslinks nil)                   ; cache bust

  ;; need to define these here for index listings and rss:
  (llet (;; don't ask about generation when exporting
          org-confirm-babel-evaluate (fn nil))

    (message "BLOG: making pages!")
    (llet [;; metas (ns/blog-changed-files-metas)
            metas (ns/blog-get-metas)]
      ;; todo here: check conflicting html-dest
      (-map #'ns/blog-publish-meta metas))

    (message "BLOG: making site rss!")

    (with-current-buffer (find-file-noselect (ns/blog-path "rss/rss.org"))
      (org-export-to-file 'rss (ns/blog-path "published/rss.xml")))

    (with-current-buffer (find-file-noselect (ns/blog-path "rss/rss_full.org"))
      (org-export-to-file 'rss (ns/blog-path "published/rss_full.xml"))))

  (message "BLOG: done! ✨✨✨✨")
  t)

(defun org-publish-ignore-mode-hooks (orig-func &rest args)
  (let ((lexical-binding nil))
    (cl-letf (((symbol-function #'run-mode-hooks) #'ignore))
      (apply orig-func args))))

(advice-add 'ns/blog-generate :around #'org-publish-ignore-mode-hooks)

(defun ns/blog-make-anchors ()
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
          (org-ml-set-property
            :title
            (let ((heading-text (-if-let (m (s-match ; remove old heading anchor style
                                              (pcre-to-elisp/cached "\\[\\[\\#(.*)\\]\\[(.*)\\]\\]")
                                              heading-text))
                                  (third m) heading-text)))
              (list
                (format "%s @@html:<span class=anchor>@@%s@@html:</span>@@"
                  heading-text
                  (format "[[#%s][%s]]" id "#"))))))))))

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
              (sh "date '+<%Y-%m-%d>'")))))

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
  (format "@@html:<div class=separator>@@%s@@html:</div>@@"
    "∗ ∗ ∗"))
