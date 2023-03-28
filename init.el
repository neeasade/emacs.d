;; -*- lexical-binding: t; -*-
;;; init.el --- iykyk
;;; commentary:
;;; code:

(load (concat user-emacs-directory "lisp/dirt.el"))
(shut-up-load (~e "lisp/forest.el"))

(defun ns/summon (confs)
  (->> confs
    (-map '-list)
    (-remove
      (-lambda ((f . enables))
        (when enables
          (-any-p (fn (null (eval <>))) enables))))
    (--map (intern (format "ns/conf-%s" (first (-list it)))))
    (-map 'funcall)))

;; alias to signal intent -- layers I don't use but might be nice for reference later
(setq ns/outdated nil)

(defun ns/conf-core ()
  (ns/summon
    `(
       sanity
       evil
       interface
       editing
       shell
       projectile
       util
       git
       org org-capture org-pim
       server
       follow-dwim
       )))

(defun ns/conf-extra ()
  (ns/summon
    `(
       company
       flycheck
       (dashdocs nil)

       (restclient ns/outdated)
       (latex ns/enable-home-p)
       search-engines

       (blog ns/enable-home-p)
       scripting
       funtext
       rice-integrations
       (pdf ns/enable-linux-p)
       (ledger ns/outdated)
       (filehooks ns/enable-home-p)
       (macos-integrations ns/enable-mac-p)
       (irc ns/enable-home-p)
       graphviz)))

(defun ns/conf-development ()
  (ns/summon
    `(
       (c ns/outdated)
       (common-lisp ns/outdated)

       clojure
       elisp
       (javascript ns/outdated)
       (typescript ns/outdated)
       markdown
       adoc
       sql
       plantuml
       go
       (python ns/outdated)
       minor-langs

       ;; keep after $langs on purpose
       ;; lsp
       )))

(if (getenv "NS_EMACS_BATCH")
  ;; doing a batch job, eval some lisp, message the result
  (progn
    (ns/conf-scripting)
    (-> "NS_EMACS_BATCH" getenv read eval pr-str message)
    ;; (-> "NS_EMACS_BATCH" getenv read eval pr-str message)
    (kill-emacs))

  ;; normal MO:
  (ns/conf-core)
  (ns/conf-extra)
  (ns/conf-development)

  (ns/conf-staging)
  (ns/check-for-orphans)
  (ns/conf-style)
  (message "::: done! 🔥🔥🔥")

  (defun! ns/initial-startup-hook ()
    (when (not (boundp 'ns/after-init-hook))
      (setq ns/after-init-hook t)

      ;; Emacs is terribly slow on windows
      (ns/toggle-bloat-global (not ns/enable-windows-p))

      (named-timer-idle-run :garbage-collect 2 t 'garbage-collect)

      (->> recentf-list
        (-filter (fn
                   (and
                     ;; tramp slow
                     (not (file-remote-p <>))
                     (not (s-ends-with-p "recentf" <>))
                     (f-exists-p <>))))
        (-take 6)
        (mapc 'find-file))

      (when (f-exists-p (~ "extend.el"))
        (load (~ "extend.el")))

      (ns/load-theme (intern (get-resource "emacs.theme")))))

  (add-hook 'emacs-startup-hook 'ns/initial-startup-hook))

(provide 'init)
;;; init.el ends here
