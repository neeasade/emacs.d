;; -*- lexical-binding: t; -*-
;; surfers

(defun! ns/toggle-bloat()
  "toggle bloat in the current buffer"
  (llet [toggle (not (bound-and-true-p font-lock-mode))]
    (message "setting bloat-local: %s" (if toggle "enabled" "disabled"))
    (--map (funcall it (if toggle t 0))
      '(corfu-mode
         flycheck-mode
         font-lock-mode
         git-gutter-mode))))

(defun! ns/toggle-bloat-global (&optional toggle)
  "Toggle global bloat"
  (llet [toggle (or toggle (not global-font-lock-mode))]
    (message "setting bloat-global: %s" (if toggle "enabled" "disabled"))
    (--map (funcall it (if toggle t 0))
      '(global-corfu-mode
         global-flycheck-mode
         global-font-lock-mode
         global-git-gutter-mode))))

(defmacro ns/install-dashdoc (docset mode-hook)
  "Install dash DOCSET if dashdocs enabled, add mode hook to narrow dash search targets."
  `(when (bound-and-true-p ns/enable-dashdocs-p)

     (when nil
       (message (format "Installing %s docset..." ,docset))
       (counsel-dash-install-docset (subst-char-in-string ?\s ?_ ,docset)))
     (add-hook ,mode-hook (fn (setq-local counsel-dash-docsets '(,docset))))))

;; using this package only for a tramp aware 'open file as root' function
;; initially went to steal but turned out to be many functions to steal
(ns/use crux (crux-reopen-as-root-mode t))

(ns/use simpleclip)

(defun! ns/paste-from-clipboard-url ()
  "GET the clipboard contents into current point"

  ;; pls seems iffy
  ;; see https://github.com/alphapapa/plz.el/issues/3
  ;; (insert (plz 'get (simpleclip-get-contents)))

  (request
    :type "GET"
    :parser 'buffer-string
    :success
    (function*
      (lambda (&key data &allow-other-keys)
        (interactive)
        (insert data)))))

(defun! ns/insert-history ()
  (llet [shell-name (if (eq major-mode 'shell-mode)
                      (file-name-nondirectory (car (process-command (get-buffer-process (current-buffer)))))
                      "bash")
          vertico-prescient-enable-sorting nil
          history-item (ns/pick "history"
                         (->> (append
                                ;; current history across all open shells:
                                (-mapcat
                                  (fn (with-current-buffer <>
                                        (when (boundp 'comint-input-ring)
                                          (when (> (ring-size comint-input-ring) 0)
                                            (mapc 's-clean (ring-elements comint-input-ring)
                                              )))))
                                  (ns/buffers-by-mode 'shell-mode))

                                (->> (~ (format ".%s_history" shell-name))
                                  (f-read)
                                  (s-split "\n")
                                  (reverse)
                                  (-map
                                    (fn ;; shared history format: ': 1556747685:0;cmd'
                                      (if (s-starts-with-p ":" <>)
                                        (s-replace-regexp (pcre-to-elisp "^:[^;]*;") "" <>)
                                        <>)))))
                           (-uniq)
                           (-remove (-partial #'s-starts-with-p " "))))]

    (when (eq major-mode 'shell-mode)
      (goto-char (point-max)))

    (insert history-item)))

(defun! ns/insert-qute-url (&optional description-in)
  (llet [url (sh "qb_active_url")]
    (if (s-blank-p url)
      (message "failed to get url!")
      (llet [desc (if description-in description-in
                    (when (-contains-p '(org-mode adoc-mode markdown-mode) major-mode)
                      (if (region-active-p)
                        (let ((result (buffer-substring (region-beginning) (region-end))))
                          (delete-region (region-beginning) (region-end))
                          result)
                        (read-string (format  "link description for %s (blank for none): " url)))))
              desc (if (and (stringp desc) (s-blank-p desc))
                     nil
                     desc)]

        (insert
          (cond
            ((not desc) url)
            ((eq major-mode 'org-mode) (format "[[%s][%s]]" url desc))
            ((eq major-mode 'adoc-mode) (format "%s[%s]" url desc))
            ((eq major-mode 'markdown-mode) (format "[%s](%s)" desc url))
            (t url)))))))

(defun! ns/insert-qute-url-title ()
  (ns/insert-qute-url (sh "qb_active_url .title")))

(ns/bind
  "fE" 'crux-sudo-edit
  "tb" 'ns/toggle-bloat

  "iu" 'ns/insert-qute-url
  "iU" 'ns/insert-qute-url-title
  "ih" 'ns/insert-history)
