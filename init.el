;;; init.el --- pull in the world, mold it to my liking.
;;; commentary:
;;; code:

(eval-and-compile (load "~/.emacs.d/lisp/theworld.el"))

(neeasade/compose
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
 )

(neeasade/compose
 extra

 company
 flycheck
 jump
 dashdocs

 ;; zoom
 ;; dimmer
 projectile
 treemacs
 restclient
 latex
 search-engines

 target-process
 emms
 pdf
 ledger
 emoji
 )

(neeasade/compose
 communication
 irc slack twitter email
 )

(neeasade/compose
 development

 clojure
 csharp
 elisp
 nix
 javascript
 typescript
 markdown
 ;; terraform
 ;; sql
 ;; jekyll
 ;; plantuml
 autohotkey
 filehooks
 )

;; liftoff
(neeasade/load core extra communication development style)
(neeasade/check-for-orphans)

;; Emacs is terribly slow on windows
(neeasade/toggle-bloat-global enable-linux?)

(provide 'init)
;;; init.el ends here
