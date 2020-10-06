;; -*- lexical-binding: t; -*-
;;; init.el --- this file has the knobs
;;; commentary:
;;; code:

(setq
  ns/enable-windows-p (eq system-type 'windows-nt)
  ns/enable-linux-p (eq system-type 'gnu/linux)
  ns/enable-home-p (string= (getenv "USER") "neeasade")
  ns/enable-work-p ns/enable-windows-p
  ;; for when we're away from $HOME.
  ns/xrdb-fallback-values
  `(
     ;; ("*.background"         . ,(face-attribute 'default :background))
     ("*.background"         . nil)
     ("Emacs.powerlinescale" . "1.1")
     ("Emacs.doomlineheight" . "1")
     ("Emacs.theme"          . "lab-theme")
     ;; ("Emacs.theme"          . "apropospriate-light")
     ("Emacs.powerline"      . "bar")
     ("Emacs.padding_source" . "st") ;; font or st
     ("st.borderpx"          . "10")
     ;; default to whatever loads
     ("st.font"              .
       ,(when (not (string-equal "unspecified" (face-attribute 'default :font)))
          (font-get (face-attribute 'default :font) :name)))
     ("st.font_variable"     .
       ,(when (not (string-equal "unspecified" (face-attribute 'default :font)))
          (font-get (face-attribute 'default :font) :name)))))

(setq load-prefer-newer t)
(load "~/.emacs.d/lisp/dirt.el")
(load "~/.emacs.d/lisp/forest.el")

(defmacro ns/compose (name &rest targets)
  `(defconfig ,name
     ,@(-map (fn (list (intern (concat "ns/" (prin1-to-string <>)))))
         targets)))

(ns/compose
  core

  ;; use-package
  ;;straight
  sanity
  evil
  interface
  editing
  shell
  org
  git
  util
  server
  )

(ns/compose
  extra

  colors
  company
  flycheck
  jump
  dashdocs

  ;; zoom
  ;; dimmer
  projectile
  restclient
  latex
  search-engines

  blog
  scripting
  ;; music
  pdf
  ledger
  emoji
  filehooks
  writing
  deadgrep
  elasticsearch
  graphiz

  follow-dwim
  )

(ns/compose
  development
  c
  ;; common-lisp
  fennel

  autohotkey
  clojure
  ;; csharp
  elisp
  nix
  ;; javascript
  ;; typescript
  markdown
  powershell
  ;; guix
  ;; lsp
  ;; terraform
  ;; sql
  ;; jekyll
  ;; plantuml
  python
  )

(ns/compose
  communication

  ;; irc
  email
  rss
  )

(if (getenv "NS_EMACS_BATCH")
  ;; doing a batch job, eval some lisp, message the result
  (-> "NS_EMACS_BATCH" getenv read eval prn message)

  ;; normal MO:
  (ns/load core extra development communication staging check-for-orphans)
  (add-hook 'after-init-hook
    (lambda ()
      ;; Emacs is terribly slow on windows
      (ns/toggle-bloat-global ns/enable-linux-p)

      (run-with-idle-timer 2 t 'garbage-collect)

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

      (ns/style))))

(provide 'init)
;;; init.el ends here
