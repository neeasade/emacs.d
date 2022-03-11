;; some helpers for turning org into a nested kv-store thingy

(defun ht-remove* (table &rest values)
  (if (= 1 (length values))
    (apply 'ht-remove table values)
    (ht-remove
      (apply 'ht-get* table (-drop-last 1 values))
      (-last-item values))))

(defun ht-set* (table &rest values)
  ;; must be called with at least 2 values
  (if (= 2 (length values))
    (apply 'ht-set! table values)
    (let ((key (first values))
           (next (-drop 1 values)))
      (when-not (ht-contains-p table key)
        (ht-set! table key (ht)))
      (when-not (hash-table-p (ht-get table key))
        (message "warn: ht-set*: trying to set value on non-table"))
      (apply 'ht-set* (ht-get table key) next)))
  table)

(defun ns/ht-walk-leaves (table callback-fn &optional path)
  (if-not (hash-table-p table)
    (funcall callback-fn path table)
    (-map (fn (ns/ht-walk-leaves
                (ht-get table <>)
                callback-fn
                (-snoc path <>)))
      (ht-keys table))))

(defun ns/org-ml-walk (walk-fn node)
  (funcall walk-fn node)
  (-map (-partial 'ns/org-ml-walk walk-fn)
    (org-ml-get-children node)))

(defun ns/org-get-types (node)
  (let ((seen (list)))
    (ns/org-ml-walk
      (fn (setq seen (-snoc seen (org-ml-get-type <>))))
      node)
    (-uniq seen)))

(defun ns/current-org-config ()
  ;; org-ml's CONFIG arg
  `(:log-into-drawer ,org-log-into-drawer
     :clock-into-drawer ,org-clock-into-drawer
     :clock-out-notes ,org-log-note-clock-out))

(defun ns/org-headline-content-string (headline)
  "Get org markup under a headline as a string (excluding comments, keywords, drawers, properties)"
  (->> headline
    (org-ml-headline-get-contents (ns/current-org-config))
    (-remove (-partial 'org-ml-is-type 'keyword))
    (-remove (-partial 'org-ml-is-type 'comment))
    (-map 'org-ml-to-string)
    (s-join "")
    (s-trim)))

(defun ns/org-headline-find-merge-directives (node)
  "Return a list of '#+merge: %s' %s values"
  (->> node
    (org-ml-headline-get-contents (ns/current-org-config))
    (-keep
      (lambda (content-node)
        (and (org-ml-is-type 'keyword content-node)
          (ns/blog-get-prop "merge" (org-ml-to-string content-node)))))))

(defun ns/org-headline-content (node)
  "headline to tree content"
  (let* ((content-nodes (org-ml-headline-get-contents (ns/current-org-config) node))
          (result (ns/org-headline-content-string node)))

    ;; see if we see anything that sticks out, override result if so:
    (-map
      (-partial 'ns/org-ml-walk
        (lambda (at)
          (cond
            ((org-ml-is-type 'src-block at)
              (setq result
                (->> (org-ml-to-trimmed-string at)
                  (s-split "\n")
                  ;; remove #+{begin,end}_src
                  (-drop-last 1)
                  (-drop 1)
                  (s-join "\n"))))
            ((org-ml-is-type 'plain-list at)
              (setq result
                (->> at
                  (org-ml-get-children)
                  (-filter (-partial 'org-ml-is-type 'item))
                  ;; this doesn't work like I expected: (-map 'org-ml-item-get-paragraph)
                  (-map 'org-ml-to-trimmed-string)
                  (-map (fn (substring <> 2))) ; remove frontleading "- "
                  ))))))
      content-nodes)
    result))

(defun ns/org-leaf-reject-p (leaf)
  (-any-p (lambda (tag) (-> tag read eval not))
    (or (org-ml--get-property-nocheck :tags leaf) '("t"))))

(defun ns/org-leaf-p (node)
  "Return if an org node is a headline with no headline childen"
  ;; todo: think there was an org-ml-*-childless or something
  (and (eq (org-ml-get-type node) 'headline)
    (->> node
      org-ml-get-children
      (-filter (fn (org-ml-is-type 'headline <>)))
      length
      (= 0))))

(defun ns/gather-list-children (collect-fn path node)
  "Construct paths from org's descriptive lists"
  (if (org-ml-is-type 'item node)
    ;; I don't see a reference to descriptive items in org-ml?
    (let* ((item-string (first (s-split "\n" (org-ml-to-string node))))
            ;; remove "- "
            (item-string (substring item-string 2))
            (parts (s-split "::" item-string))
            (k (-> parts first s-clean s-trim))
            (v (-some-> parts second s-clean s-trim))
            (path (-non-nil (-snoc path k v))))
      (if (> (length (org-ml--item-get-subitems node))
            0)
        ;; nested items list!
        (-map (-partial 'ns/gather-list-children collect-fn path)
          (org-ml--item-get-subitems node))
        ;; go forth :)
        (funcall collect-fn path)))
    ;; continue right along, looking for other lists:
    (-map (-partial 'ns/gather-list-children collect-fn path)
      (org-ml-get-children node)))
  nil)

(defun ns/org-headline-to-data (leaf)
  "leaf headline to paths data + merge directives"
  (let ((merge-paths (list))
         (tree-paths (list))
         (collect (lambda (i) (setq tree-paths (-snoc tree-paths i))))
         (leaf-path (org-ml-headline-get-path leaf)))

    (setq merge-paths
      (append merge-paths
        (-map
          (lambda (merge-target)
            (list leaf-path (s-split "\\." merge-target)))
          (ns/org-headline-find-merge-directives leaf))))

    (if (and
          (-contains-p (ns/org-get-types leaf) 'item)
          (->> leaf
            (org-ml-headline-get-contents (ns/current-org-config))
            (-filter (-partial 'org-ml-is-type 'plain-list))
            (-map 'org-ml-to-trimmed-string)
            (-any-p (-partial 's-contains-p "::"))))

      ;; we have description list items! go find them:
      (ns/gather-list-children
        (lambda (full-path)
          (funcall collect full-path))
        leaf-path leaf)

      ;; headline -> data
      (if (s-starts-with-p "!" (-last-item leaf-path))
        ;; ! headline indicates literal toml.
        ;; assume it has a src block with conf-toml
        (->> (ns/org-headline-content leaf)
          (s-split "\n")
          (-remove 's-blank-p)
          (-remove (-partial 's-starts-with-p "#" ))
          ;; fragile
          ;; a.c.b = "woww"
          (-map
            (lambda (line)
              (seq-let (path value) (s-split "=" line)
                (funcall collect
                  (-snoc
                    (s-split "\\." path)
                    (read value)))))))

        (funcall collect (-snoc leaf-path (ns/org-headline-content leaf)))))
    `(:merges ,merge-paths
       :tree-paths ,tree-paths)))

(defun ns/org-to-toml (&rest org-files)
  (->> org-files
    (-mapcat
      (lambda (f)
        (let* ((org-default-notes-file f))
          (ns/get-notes-nodes 'ns/org-leaf-p))))

    (-map 'ns/org-headline-to-data)

    ((lambda (data)
       (let ((merges (-mapcat (fn (plist-get <> :merges)) data))
              (tree-paths (-mapcat (fn (plist-get <> :tree-paths)) data))
              (conf-tree (ht)))

         (message "performing sets")
         (-map (lambda (path)
                 (message (pr-string path))
                 (apply 'ht-set* conf-tree path))
           tree-paths)

         (-map
           (lambda (parts)
             (seq-let (to from) parts
               (message (format "merge: %s to %s" (s-join "." from) (s-join "." to)))

               ;; currently only meant for table-to-table (ht-copy call)
               (apply 'ht-set*
                 conf-tree
                 (-snoc to
                   (ht-merge
                     (apply 'ht-get* conf-tree to)
                     (ht-copy (apply 'ht-get* conf-tree from)))))))
           merges)

         conf-tree
         )))

    ;; use this in org-ml-filter


    ((lambda (conf-tree)

       (->> org-files
         (-mapcat
           (lambda (f)
             (let* ((org-default-notes-file f))
               (ns/get-notes-nodes 'ns/org-leaf-reject-p))))
         (-map 'org-ml-headline-get-path)
         (-map (lambda (p) (apply 'ht-remove* conf-tree p))))

       conf-tree))

    ((lambda (conf-tree)
       (let ((flattened (ht)))
         (ns/ht-walk-leaves conf-tree
           (lambda (path value)
             (ht-set flattened
               (format "'%s'" (s-join "." path))
               (cond
                 ;; arrays
                 ((listp value)
                   (->> value
                     (-map 'pr-string)
                     (s-join ",")
                     (format "[%s]")))

                 ;; multiline values
                 ((s-contains-p "\n" value)
                   (format "'''\n%s\n'''" value))

                 ;; numbers (string-to-number value is 0 in case of failure)
                 ((and (not (= 0 (string-to-number value)))
                    (not (string= "0" value)))
                   (string-to-number value))

                 (t (pr-string value))))))
         flattened)))

    (ht-map
      (lambda (k v) (format "%s = %s" k v)))

    (s-join "\n")
    )
  )


(ns/comment

  (f-write
    (ns/org-to-toml "/home/neeasade/.dotfiles/theming/base.org")

    ;; (ns/org-to-toml (~ "test.org") )

    'utf-8
    (~ "test.toml"))

  (setq ns/wow (ns/org-to-toml (~ "test.org")))
  (ht-keys ns/wow)

  (ht-keys (ht-get* ns/wow "a" "b"))

  (ht-get* ns/wow "a" "b" "merge me"))
