;;; init.el --- pull in the world, mold it to my liking.
;;; commentary:
;;; code:

(eval-and-compile (load "~/.emacs.d/lisp/theworld.el"))

(defmacro neeasade/load (&rest targets)
  `(mapc (lambda(target)
           (funcall (intern (concat "neeasade/" (prin1-to-string target)))))
	 ',targets))

(defmacro neeasade/compose (name &rest targets)
  `(defconfig ,name (neeasade/load ,@targets)))

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

 terminal
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
 filehooks
 )

(neeasade/compose
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
 ;; terraform
 ;; sql
 ;; jekyll
 ;; plantuml
 )

(neeasade/compose
 communication

 irc slack twitter email
 reddit stackexchange elfeed
 )

;; liftoff
(neeasade/load core extra development communication style)
(neeasade/check-for-orphans)
(neeasade/staging)

;; Emacs is terribly slow on windows
(neeasade/toggle-bloat-global enable-linux?)

(provide 'init)
;;; init.el ends here
