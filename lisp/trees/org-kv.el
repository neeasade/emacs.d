;; some helpers for turning org into a nested kv-store thingy

(defun ht-set* (table &rest values)
  ;; must be called with at least 2 values
  (if (= 2 (length values))
    (apply 'ht-set! table values)
    (let ((key (first values))
           (next (-drop 1 values)))
      (when-not (ht-contains-p table key)
        (ht-set! table key (ht)))
      (when-not (hash-table-p (ht-get table key))
        (message "warn: ns/ht-set*: trying to set value on non-table"))
      (apply 'ht-set* (ht-get table key) next)))
  table)

(defun ns/ht-walk-leaves (table callback-fn &optional path)
  (message (format "walking %s" (pr-string path)) )
  (if-not (hash-table-p table)
    (funcall callback-fn path table)
    (-map (fn
            (ns/ht-walk-leaves
              (ht-get table <>)
              callback-fn
              (-snoc path <>)))
      (ht-keys table))))

(defun ns/org-ml-walk (walk-fn &rest nodes)
  ;; nb: this function being multi-arity is to cope with the funkiness of
  ;; org-ml-headline-get-contents (returns a list of content nodes)
  (-map
    (lambda (node)
      (funcall walk-fn node)
      (-map (-partial 'ns/org-ml-walk walk-fn)
        (org-ml-get-children node)))
    nodes))

(defun ns/org-get-types (node)
  (let ((seen (list)))
    (ns/org-ml-walk
      (lambda (n)
        (setq seen (append seen (list (org-ml-get-type n)))))
      node)
    (-uniq seen)))

(defun ns/current-org-config ()
  ;; org-ml's CONFIG arg
  `(:log-into-drawer ,org-log-into-drawer
     :clock-into-drawer ,org-clock-into-drawer
     :clock-out-notes ,org-log-note-clock-out))

(defun ns/org-headline-content-string (headline)
  (->> headline
    (org-ml-headline-get-contents (ns/current-org-config))
    (-remove (-partial 'org-ml-is-type 'keyword))
    (-remove (-partial 'org-ml-is-type 'comment))
    (-map 'org-ml-to-string)
    (s-join "")))

(defun ns/org-headline-find-merge-directives (node)
  (->> node
    (org-ml-headline-get-contents (ns/current-org-config))
    (-keep
      (lambda (item)
        (and (org-ml-is-type 'keyword item)
          (ns/blog-get-prop "merge" (org-ml-to-string item)))))))

(defun ns/org-headline-content (node)
  (let* ((content-nodes (org-ml-headline-get-contents (ns/current-org-config) node))
          (result (ns/org-headline-content-string node)))

    ;; see if we see anything that sticks out, override result if so:
    (apply 'ns/org-ml-walk
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
                )))))
      content-nodes)
    result))

(defun ns/org-leaf-p (node)
  ;; is a headline a leaf headline
  ;; todo: think there was an org-ml-*-childless or something
  (and (eq (org-ml-get-type node) 'headline)
    (when
      ;; todo here: tags as conditional inclusion
      t
      ;; (->> (org-ml-parse-this-element) (org-ml-get-property :call))
      t
      )
    (->> node
      org-ml-get-children
      (-filter (fn (org-ml-is-type 'headline <>)))
      length
      (= 0))))

(defun ns/gather-list-children (collect-fn path node)
  ;; act on lists of items to build paths, pass in current path
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
  ;; a leaf might emit multiple values (leaf == headline, not true leaf)
  ;; return: ((merge-paths: (merge-to merge-from)) (paths: (node path)))
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
          ;; (message (format "arst: full-path: %s" (pr-string full-path)))
          (funcall collect full-path))
        leaf-path leaf)

      ;; headline -> data
      (if (s-starts-with-p "!" (-last-item leaf-path))
        (progn
          ;; ! headline indicates literal toml.
          ;; (setq result "todo: toml data")
          )
        (funcall collect (-snoc leaf-path (ns/org-headline-content leaf)))))
    `(:merges ,merge-paths
       :tree-paths ,tree-paths)))

(defun ns/org-to-toml (&rest org-files)
  (->> org-files
    (-map
      (lambda (f)
        (let* ((org-default-notes-file f))
          (ns/get-notes-nodes 'ns/org-leaf-p))))
    (-flatten-n 1)

    (-map 'ns/org-headline-to-data)

    ((lambda (data)
       ;; "it's all just data"
       (let ((merges (->> data
                       (-map (fn (plist-get <> :merges)))
                       (-flatten-n 1)))
              (tree-paths (->> data
                            (-map (fn (plist-get <> :tree-paths)))
                            (-flatten-n 1)))
              (conf-tree (ht)))

         (message "performing sets")
         (-map (lambda (path)
                 (message (pr-string path))
                 (apply 'ht-set* conf-tree path))
           tree-paths)

         (-map
           (lambda (parts)
             (seq-let (to from) parts
               (message (format "merge: %s to %s. value: %s"
                          (s-join "." from)
                          (s-join "." to)
                          (-snoc to (ht-copy (apply 'ht-get* conf-tree from)))
                          ))
               ;; only meant for table-to-table (ht-copy call)

               (apply 'ht-set*
                 conf-tree
                 (-snoc to
                   (ht-merge
                     (apply 'ht-get* conf-tree to)
                     (ht-copy (apply 'ht-get* conf-tree from)))))))
           merges)

         conf-tree
         )))

    ((lambda (conf-tree)
       (let ((flattened (ht)))
         (ns/ht-walk-leaves conf-tree
           (lambda (path value)
             (ht-set flattened
               (s-join "." path)
               (if (listp value)
                 (->> value
                   (-map 'pr-string)
                   (s-join ",")
                   (format "[%s]"))
                 ;; (format "\"value\"")
                 (pr-string value)))))
         flattened)))

    (ht-map
      (lambda (k v) (format "%s = %s" k v)))

    (s-join "\n")
    )
  )


(ns/comment
  (f-write
    (ns/org-to-toml (~ "test.org"))
    'utf-8
    (~ "test.toml")
    )

  (setq ns/wow (ns/org-to-toml (~ "test.org")))
  (ht-keys ns/wow)

  (ht-keys (ht-get* ns/wow "a" "b"))

  (ht-get* ns/wow "a" "b" "merge me")
  )

