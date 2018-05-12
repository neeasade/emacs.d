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
   ))

(defun neeasade/extra()
  (neeasade/load
   org
   git

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
   emms
   pdf
   ledger
   ))

(defun neeasade/communication()
  (neeasade/load irc slack twitter email))

(defun neeasade/development()
  (neeasade/load
   clojure
   csharp
   elisp
   nix
   javascript
   typescript
   terraform
   markdown
   sql
   jekyll
   plantuml
   ))

(defun neeasade/windows()
  ;; TODO: windows-scripts layer from spacemacs
  (neeasade/load autohotkey))

(neeasade/load core extra communication development style)

(if sys/windows? (neeasade/load '(windows)))

;; Emacs is terribly slow on windows
(neeasade/toggle-bloat-global sys/linux?)

(provide 'init)
;;; init.el ends here
