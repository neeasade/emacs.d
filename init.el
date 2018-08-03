;;; init.el --- pull in the world, mold it to my liking.
;;; commentary:
;;; code:

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

(provide 'init)
;;; init.el ends here
