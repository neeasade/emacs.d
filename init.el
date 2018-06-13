;;; init.el --- pull in the world, mold it to my liking.
;;; commentary:
;;; code:

(eval-and-compile (load "~/.emacs.d/lisp/theworld.el"))

;; (init-use-package)
(init-straight)

(defconfig core
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

(defconfig extra
    (neeasade/load
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
     )
  )

(defconfig communication
    (neeasade/load irc slack twitter email)
  )

(defconfig development
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
     autohotkey
     filehooks
     )
  )

;; liftoff
(neeasade/load core extra communication development style)
(neeasade/check-for-orphans)

;; Emacs is terribly slow on windows
(neeasade/toggle-bloat-global sys/linux?)

(provide 'init)
;;; init.el ends here
