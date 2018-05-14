;;; init.el --- pull in the world, mold it to my liking.
;;; commentary:
;;; code:

(eval-and-compile (load "~/.emacs.d/lisp/theworld.el"))

;; (init-use-package)
(init-straight)

(defun neeasade/core()
  (neeasade/load
   helpers
   sanity
   evil
   interface
   editing
   shell
   eshell
   interactive
   git
   org
   ))

(defun neeasade/extra()
  (neeasade/load
   company
   flycheck
   jump
   dashdocs

   ;; window-management
   projectile
   treemacs
   restclient
   latex

   target-process
   )

  (when neeasade/home?
    (neeasade/load
     emms
     pdf
     ledger))
  )

(defun neeasade/communication()
  (when neeasade/home?
	(neeasade/load irc slack twitter email)
	)
  )

(defun neeasade/development()
  (neeasade/load
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
   )

  (when sys/windows?
    (neeasade/load autohotkey)
    )
  )

;; liftoff
(neeasade/load core extra communication development style)

;; Emacs is terribly slow on windows
(neeasade/toggle-bloat-global sys/linux?)

(provide 'init)
;;; init.el ends here
