;; -*- lexical-binding: t; -*-
;;; init.el --- this file has the knobs
;;; commentary:
;;; code:

(setq
  ns/enable-windows-p (eq system-type 'windows-nt)
  ns/enable-linux-p (eq system-type 'gnu/linux)
  ns/enable-mac-p (eq system-type 'darwin)
  ns/enable-home-p (string= (getenv "USER") "neeasade")
  ns/enable-work-p ns/enable-mac-p

  ns/home-directory (getenv (if ns/enable-windows-p "USERPROFILE" "HOME"))
  ns/emacs-directory user-emacs-directory

  ;; maybe swap these term
  mac-option-modifier 'meta
  mac-command-modifier 'super
  mac-control-modifier 'control

  ;; for when we're away from $HOME.
  ns/xrdb-fallback-values
  `(("Emacs.doomlineheight" . "24")
     ("Emacs.powerline"      . "bar")
     ("Emacs.padding_source" . "st") ;; (font or st)

     ;; default to whatever loads, use nil if there is no default
     ("st.font"              .
       ,(when (stringp (face-attribute 'default :font))
          (font-get (face-attribute 'default :font) :name)))
     ("st.font_variable"     .
       ,(when (stringp (face-attribute 'default :font))
          (font-get (face-attribute 'default :font) :name)))))

(setq load-prefer-newer t)
(load (concat ns/emacs-directory "lisp/dirt.el"))
(load (~e "lisp/forest.el"))

(defmacro ns/compose (name &rest targets)
  `(defconfig ,name
     ,@(-map (fn (list (intern (concat "ns/" (prin1-to-string <>)))))
         targets)))

(ns/compose
  core

  ;; use-package
  ;; straight
  sanity
  evil
  interface
  editing
  shell
  org org-capture org-pim
  git
  util
  server
  follow-dwim)

(ns/compose
  extra

  company
  flycheck
  jump
  dashdocs

  zoom
  projectile
  restclient
  latex
  ;; search-engines

  blog
  scripting
  ;; music
  pdf
  ledger
  emoji
  filehooks
  deadgrep
  elasticsearch
  graphiz
  )

(ns/compose
  development
  c
  ;; common-lisp

  autohotkey
  clojure
  ;; csharp
  elisp
  nix
  javascript
  typescript
  markdown
  powershell
  ;; guix
  ;; lsp
  ;; terraform
  ;; sql
  ;; jekyll
  ;; plantuml
  python
  irc
  )

(if (getenv "NS_EMACS_BATCH")
  ;; doing a batch job, eval some lisp, message the result
  (progn
    (ns/scripting)
    (-> "NS_EMACS_BATCH" getenv read eval pr-string message))

  ;; normal MO:
  (ns/core)
  (ns/extra)
  (ns/development)
  (ns/staging)
  (ns/check-for-orphans)
  (ns/style)

  (defun! ns/initial-startup-hook ()
    (when (not (boundp 'ns/after-init-hook))
      (setq ns/after-init-hook t)

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

      (ns/load-theme 'tarp-mcfay)))

  (add-hook 'emacs-startup-hook 'ns/initial-startup-hook))

(provide 'init)
;;; init.el ends here
