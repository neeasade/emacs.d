;;; init.el --- pull in the world, mold it to my liking.
;;; commentary:
;;; code:

(if (not (boundp 'ns/firstrun))
    (setq ns/firstrun t))

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

 target-process
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
(ns/load core extra development communication staging)
(ns/check-for-orphans)

;; Emacs is terribly slow on windows
(ns/toggle-bloat-global ns/enable-linux-p)

(ns/style) ;; also gets spaceline

(setq ns/firstrun nil)

(provide 'init)
;;; init.el ends here
