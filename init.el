;;; init.el --- this file has the knobs
;;; commentary:
;;; code:

(if (not (boundp 'ns/firstrun)) (setq ns/firstrun t))
(setq ns/firstrun-action '())

(defun ns/add-firstrun-action (action)
  (setq ns/firstrun-action
    (cons action ns/firstrun-action)))

(setq
  ns/enable-windows-p (eq system-type 'windows-nt)
  ns/enable-linux-p (eq system-type 'gnu/linux)
  ns/enable-home-p (string= (getenv "USER") "neeasade")
  ns/enable-docker-p (string= (getenv "USER") "emacser")
  ns/enable-work-p ns/enable-windows-p
  ns/enable-colemak t
  ;; for when we're away from $HOME.
  ns/xrdb-fallback-values
  `(
     ;; ("*.background"         . ,(face-attribute 'default :background))
     ("*.background"         . nil)
     ("Emacs.powerlinescale" . "1.1")
     ("Emacs.theme"          . "base16-grayscale-light")
     ("Emacs.powerline"      . "bar")
     ("Emacs.padding_source" . "auto") ;; auto or st
     ("st.borderpx"          . "30")
     ("st.font"              . "Go Mono-10")
     ("st.font_variable"     . "Go-10")
     ))

(setq load-prefer-newer t)
(eval-and-compile (load "~/.emacs.d/lisp/theworld.el"))

(defmacro ns/load (&rest targets)
  `(mapc (lambda(target)
           (funcall (intern (concat "ns/" (prin1-to-string target)))))
     ',targets))

(defmacro ns/compose (name &rest targets)
  `(defconfig ,name (ns/load ,@targets)))

(ns/compose
  core

  ;; use-package
  straight
  bedrock
  sanity
  evil
  interface
  editing
  shell
  git
  org
  util
  server
  )

(ns/compose
  extra

  company
  flycheck
  jump
  dashdocs

  terminal
  ;; zoom
  ;; dimmer
  projectile
  treemacs
  restclient
  latex
  search-engines

  blog
  targetprocess
  music
  pdf
  ledger
  emoji
  filehooks
  writing
  deadgrep
  elasticsearch
  graphiz
  )

(ns/compose
  development

  autohotkey
  clojure
  csharp
  elisp
  nix
  javascript
  typescript
  markdown
  powershell
  lua
  guix
  ;; lsp
  ;; terraform
  ;; sql
  ;; jekyll
  ;; plantuml
  )

(ns/compose
  communication

  irc slack twitter email
  reddit stackexchange elfeed
  )

;; liftoff
(ns/load core extra development communication staging check-for-orphans)

(when ns/firstrun
  (setq ns/firstrun nil)
  ;; Emacs is terribly slow on windows
  (ns/toggle-bloat-global ns/enable-linux-p)
  (ns/style)
  (eval (cons 'progn ns/firstrun-action)))

(provide 'init)
;;; init.el ends here
