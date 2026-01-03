;; -*- lexical-binding: t; -*-
;;; init.el --- iykyk
;;; commentary:
;;; code:

;; useful when debugging, useless for startup time
(setq ns/enable-init-logs? nil)         ; see ns/message

(load (concat user-emacs-directory "lisp/dirt.el"))
(shut-up-load (~e "lisp/forest.el"))

(defun ns/summon (confs)
  (->> confs
    (-map '-list)
    (-remove
      (-lambda ((f . enables))
        (when enables
          (-any-p (fn (null (symbol-value <>))) enables))))
    (--map (intern (format "ns/conf-%s" (first (-list it)))))
    (-map 'funcall)))

;; alias to signal intent -- layers I don't use but might be nice for reference later
(setq ns/outdated nil)

(defun ns/conf-core ()
  (ns/summon
    `(
       resources
       sanity
       evil

       minibuffer
       interface
       buffers-and-windows
       dired

       editing
       shell
       projectile
       util
       git
       org org-capture org-pim org-pim-export
       follow-dwim
       )))

(defun ns/conf-extra ()
  (ns/summon
    `(
       server                          ; do not put this in core - elisp scripts

       corfu
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
       graphviz


       llm
       whisper
       )))

(defun ns/conf-development ()
  (ns/summon
    `(
       clojure
       elisp
       go
       sql
       python

       (c ns/outdated)
       (common-lisp ns/outdated)
       (javascript ns/outdated)
       (typescript ns/outdated)
       (plantuml ns/outdated)

       markdown adoc

       minor-langs
       ;; keep after $langs on purpose
       lsp
       )))

(if (getenv "NS_EMACS_BATCH")
  (progn
    ;; not running interactively -- eval some lisp, message the result
    (when (getenv "NS_REDIR_LOG")
      (defun ns/message-stdout (message-fn &rest args)
        (print (s-trim (apply 'format args)) #'external-debugging-output)
        (apply message-fn args))
      (advice-add #'message :around #'ns/message-stdout))

    (ns/conf-scripting)

    (llet [result (-> "NS_EMACS_BATCH" getenv read eval pr-str)] ; intentional
      (message "COOL_DELIMITER")
      (message result))

    ;; (-> "NS_EMACS_BATCH" getenv read eval pr-str message)
    (kill-emacs))

  ;; normal MO:
  (ns/conf-core)

  (ns/conf-style)
  (funcall-interactively 'ns/load-theme (intern (get-resource "emacs.theme")))

  (ns/conf-extra)
  (ns/conf-development)

  (ns/check-for-orphans)

  (ns/conf-staging)
  (message "🏁🏁🏁 init sequence complete 🏁🏁🏁")

  (defun! ns/initial-startup-hook ()
    (when (not (boundp 'ns/after-init-hook))
      (setq ns/after-init-hook t)

      ;; refreshing modeline here allows for correct frame divider height -.-
      (ns/refresh-all-modeline t)

      (when (f-exists-p (~ "extend.el"))
        (load (~ "extend.el")))

      (named-timer-idle-run :garbage-collect 2 t 'garbage-collect)

      (->> recentf-list
        (-filter (fn
                   (and
                     ;; tramp slow
                     (not (file-remote-p <>))
                     (not (s-ends-with-p "recentf" <>))
                     (not (s-ends-with-p ".org" <>))
                     (f-exists-p <>))))
        (-take 6)
        (mapc 'find-file))))

  ;; (add-hook 'emacs-startup-hook 'ns/initial-startup-hook)
  (run-at-time 0.1 nil 'ns/initial-startup-hook))

