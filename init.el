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

  ;; maybe swap these when in a terminal term
  mac-option-modifier 'meta
  mac-command-modifier 'super
  mac-control-modifier 'control

  ;; for when we're away from $HOME.
  ns/xrdb-fallback-values
  `(("panel.height" . "24")
     ("emacs.theme" . "tarp-mcfay")
     ("font.mono.spec" .
       ,(when (stringp (face-attribute 'default :font))
          (font-get (face-attribute 'default :font) :name)))
     ("font.variable.spec" .
       ,(when (stringp (face-attribute 'default :font))
          (font-get (face-attribute 'default :font) :name)))))

(setq load-prefer-newer t)
(load (concat ns/emacs-directory "lisp/dirt.el"))
(shut-up-load (~e "lisp/forest.el"))

(defun ns/call-conf (confs)
  (->> confs
    (-map '-list)
    (-remove
      (-lambda ((f . enables))
        (when enables
          (-any-p (fn (null (eval <>))) enables))))
    (--map (intern (format "ns/conf-%s" (first (-list it)))))
    (-map 'funcall)))

;; alias to signal intent -- layers I don't use but might be nice for reference later
(setq ns/outdated nil)

(ns/defconfig core
  (ns/call-conf
    `(
       sanity
       evil
       interface
       editing
       shell
       projectile
       util
       ;; nb: git must happen before org
       git
       org org-capture org-pim
       server
       follow-dwim
       )))

(ns/defconfig extra
  (ns/call-conf
    `(
       company
       flycheck
       (dashdocs nil)

       (restclient ns/outdated)
       (latex ns/enable-home-p)
       search-engines

       (blog ns/enable-home-p)
       scripting
       (music ns/enable-home-p (executable-find "mpd"))
       (pdf ns/enable-linux-p)
       (ledger ns/enable-home-p)
       emoji
       (filehooks ns/enable-home-p)
       graphviz)))

(ns/defconfig development
  (ns/call-conf
    `(
       (c ns/outdated)
       (common-lisp ns/outdated)

       (autohotkey ns/enable-windows-p)
       clojure
       elisp
       nix
       (javascript ns/outdated)
       (typescript ns/outdated)
       markdown
       (powershell ns/enable-windows-p)
       lsp
       terraform
       sql
       plantuml
       go
       (python ns/outdated)
       (irc ns/enable-home-p))))

(if (getenv "NS_EMACS_BATCH")
  ;; doing a batch job, eval some lisp, message the result
  (progn
    (ns/scripting)
    (-> "NS_EMACS_BATCH" getenv read eval pr-str)
    ;; (-> "NS_EMACS_BATCH" getenv read eval pr-str message)
    (kill-emacs))

  ;; normal MO:
  (ns/conf-core)
  (ns/conf-extra)
  (ns/conf-development)

  (ns/conf-staging)
  (ns/check-for-orphans)
  (ns/conf-style)

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

      (ns/load-theme (intern (get-resource "emacs.theme")))))

  (add-hook 'emacs-startup-hook 'ns/initial-startup-hook))

(provide 'init)
;;; init.el ends here
