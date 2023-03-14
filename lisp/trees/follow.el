;; -*- lexical-binding: t; -*-
;; follow <thing> at point
;; <thing> can be a file location, one of many kinds of emacs links, a code definition, whatever.

;; todo: handle relative paths if they can be found from the git root

;; handles many kinds of links
(ns/use link-hint)

;; layer on top of dumb-jump
(ns/use smart-jump
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg)
  (smart-jump-setup-default-registers)
  (ns/bind
    "n" '(:ignore t :which-key "Jump")
    ;; "ng" 'smart-jump-go
    "nb" 'smart-jump-back
    "nr" 'smart-jump-references
    ))

(defun ns/handle-potential-file-link (file)
  "Jump to a file with org if it exists - handles <filename>[:<row>][:<col>]
  return nil if FILE doesn't exist"
  ;; untested on tramp wrt speed

  (let ((file (->> file
                (s-replace "$HOME" (getenv "HOME"))
                (s-replace "~" (getenv "HOME")))))
    (message (concat "trying file: " file))
    (cond
      ((s-blank-p file) nil)
      ;; ((not (f-exists-p file)) nil)
      ((s-matches-p (pcre-to-elisp ".*:[0-9]+:[0-9]+") file)
        (let* ((parts (s-split ":" file))
                (filepath (s-join ":" (-remove-at-indices (list (- (length parts) 2) (- (length parts) 1)) parts))))
          (when (f-exists-p filepath)
            (org-link-open-from-string
              (format "[[file:%s::%s]]" filepath (cadr (reverse parts))))
            (move-to-column (string-to-number (-last-item parts)))
            t)))

      ((s-matches-p (pcre-to-elisp ".*:[0-9]+") file)
        (let* ((parts (s-split ":" file))
                (filepath (s-join ":" (-remove-at-indices (list (- (length parts) 1)) parts))))
          (when (f-exists-p filepath)
            (org-link-open-from-string
              (format "[[file:%s::%s]]" filepath (car (last parts))))
            t)))

      (t (when (f-exists-p file)
           (org-link-open-from-string
             (format "[[file:%s]]" file))
           t)))))

(defun ns/follow-log (msg)
  (message "ns/follow: %s" msg))

(defun! ns/follow ()
  "This is my home rolled DWIM at point function -- maybe it could be considered to be 'bad hyperbole'
   Tries to integrate a few meta solutions
   org link --> our own peek where we build an org file link --> jump to definition with smart-jump"

  ;; examples of kinds handled:
  ;; /home/neeasade/My Games/Skyrim/RendererInfo.txt:10:2
  ;; /home/neeasade/.vimrc:50
  ;; /home/neeasade/:10
  ;; =~/.local/share/fonts/=
  ;; org links (so you can use org link types here)
  ;; [[file:/home/neeasade/.vimrc::50]]

  ;; todo:
  ;; could add the handling for this in handle-potential-file-link
  ;; clojure.lang.ExceptionInfo: Cannot call  with 2 arguments [at /home/neeasade/.dotfiles/bin/bin/btags, line 134, column 3]

  ;; todo: handle the bash/shell line number format:
  ;; $HOME/.wm_theme: line 155:
  ;; /Users/nathan/.wm_theme: line 155:

  (or
    ;; first try to open with org handling (includes urls)
    (when (not (eq 'fail (condition-case nil (org-open-at-point) (error 'fail))))
      (ns/follow-log "resolved with org-open-at-point")
      t)

    ;; note: ffap-string-at-point is region if one is selected
    (let* ((candidate (ffap-string-at-point))
            (fallback-candidates
              (->>
                ;; line to end
                (buffer-substring
                  (car ffap-string-at-point-region)
                  (save-excursion
                    (goto-char (car ffap-string-at-point-region))
                    (end-of-line) (point)))

                (s-clean)

                ;; handle link type: /home/neeasade/.wm_theme: line 155:
                ;; by converting cases to EG: '/home/neeasade/.wm_theme:155 '
                ;; (in-progress)
                ;; ((lambda (line-to-end)
                ;;    (-if-let (matches (s-match (pcre-to-elisp ": line ([0-9]+):")
                ;;                        line-to-end))
                ;;      (seq-let (match line) matches
                ;;        (s-replace match (format ":%s " line)
                ;;          line-to-end))
                ;;      line-to-end)))

                ;; assemble potential spaced out file-names
                ((lambda (line-to-end)
                   (let ((parts (s-split " " line-to-end)))
                     (reverse
                       (-map
                         (fn (s-join " "
                               (-remove-at-indices
                                 (-map (lambda (i) (- (length parts) i)) (-iota <>))
                                 parts)))
                         (-iota (length parts) 1))))))))

            (candidates
              (-concat
                ;; regions get priority
                (when (region-active-p) (list (buffer-substring (region-beginning) (region-end))))

                (list
                  ;; candidate

                  ;; handling case: '/path/to/file:some content'
                  ;; doesn't handle spaces
                  (let ((parts (s-split ":" candidate)))
                    (s-join ":" (-remove-at-indices (list (- (length parts) 1)) parts))))
                fallback-candidates)))

      (let ((match (-first 'ns/handle-potential-file-link candidates)))
        (when match
          (ns/follow-log (format "resolved with org link after building: %s" match))))

      ;; fun, but let's not do this for now:
      ;; (let* ((rg-initial-result (ns/shell-exec (format "rg --files -g '%s'" file-name)))
      ;;         (rg-result (if (s-contains-p "\n" rg-initial-result)
      ;;                      (ns/pick (s-split "\n" rg-initial-result))
      ;;                      rg-initial-result)))
      ;;   (when (and (not (s-blank-p rg-result))
      ;;           (f-exists-p (or rg-result "nil doesn't exist don't  use me")))
      ;;     (org-open-link-from-string
      ;;       (format "file:%s%s" rg-result
      ;;         (if file-line
      ;;           ;; the string-to-number is done to coerce non-numbers (EG grep results with file name appended) to 0
      ;;           (format "::%s" (string-to-number file-line)) "")))

      ;;     (ns/follow-log "ns/follow: resolved with ripgrep")
      ;;     t
      ;;     ))
      )

    (when (not (string= (link-hint-open-link-at-point) "There is no link supporting the :open action at the point."))
      (ns/follow-log "resolved with link-hint")
      t)

    ;; fall back to definitions with smart jump
    (progn
      (ns/follow-log "resolving with smart-jump-go")
      (shut-up (smart-jump-go))))

  (recenter)
  )

(ns/bind "nn" 'ns/follow)
