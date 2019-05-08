;;; forest.el --- forest.el
;;; commentary:
;;; functions             | ns/asdf
;;; pred functions        | ns/asdf-p
;;; interactive functions | ns/asdf
;;; enable vars           | ns/enable-asdf-p
;;; vars                  | ns/asdf
;;; buffer local vars     | ns/enable-asdf
;;; code:

(defmacro defconfig-base (label &rest body)
  `(defun ,(intern (concat "ns/" (prin1-to-string label)))
     nil ,@body))

(defmacro defconfig (label &rest body)
  `(defconfig-base ,label
     (let ((config-name ,(prin1-to-string label)))
       (message (concat "loading " config-name "..."))
       (catch 'config-catch
         (setq ,(intern (format "ns/enable-%s-p" (prin1-to-string label))) nil)
         ,@body
         (setq ,(intern (format "ns/enable-%s-p" (prin1-to-string label))) t)))))

(defmacro ns/guard (&rest conditions)
  (if (not (eval (cons 'and conditions)))
    '(when t (throw 'config-catch (concat "config guard " config-name)))))

(defmacro defcommand (label args &rest body)
  `(defun ,(intern (concat "ns/" (prin1-to-string label))) ,args
     (interactive) ,@body))

(defconfig use-package
  (require 'package)
  (setq package-enable-at-startup nil)
  (add-to-list 'package-archives
    '("melpa" . "https://melpa.org/packages/"))

  (package-initialize)

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile
    (require 'use-package))

  (setq use-package-always-ensure t))

(defconfig straight
  (let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
         (bootstrap-version 2))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
        (url-retrieve-synchronously
          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
          'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t)
  (setq straight-cache-autoloads t)
  )

(defconfig elisp
  (ns/install-dashdoc "Emacs Lisp" 'emacs-lisp-mode-hook)

  (setq lisp-indent-function 'common-lisp-indent-function)

  (use-package helpful
    :config
    (global-set-key (kbd "C-h f") #'helpful-callable)
    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (global-set-key (kbd "C-h i") #'counsel-info-lookup-symbol)

    ;; todo: make this work, figure out what goes to helpful-callable in interactive arg
    (defun ns/helpful-or-dashdoc ()
      (interactive)
      (if (eq 'emacs-lisp-mode major-mode)
        (helpful-callable)
        (if ns/enable-dashdocs-p
          (ns/counsel-dash-word)
          (message "dash docs not enabled!")
          )))

    ;; (ns/bind "nd" 'ns/helpful-or-dash-doc)
    )

  (use-package eros
    :config
    (setq eros-eval-result-duration 20)
    (eros-mode 1)

    (defun ns/smart-elisp-eval ()
      (interactive)
      (if (use-region-p)
        (eval-region (region-beginning) (region-end))
        (if (s-blank-p (s-trim (thing-at-point 'line)))
          (eros-eval-last-sexp nil)
          (eros-eval-defun nil))))

    (ns/bind-mode 'emacs-lisp "e" 'ns/smart-elisp-eval)

    (ns/bind-leader-mode
      'emacs-lisp
      "er" 'eval-region
      "ei" 'eros-eval-last-sexp
      "ee" 'eros-eval-defun
      )))

(defconfig flycheck
  (use-package flycheck
    :config

    ;; cf http://www.flycheck.org/en/latest/user/syntax-checks.html#check-automatically
    (setq-ns flycheck
      check-syntax-automatically (if ns/enable-windows-p
                                   '(save mode-enabled idle-change)
                                   '(save mode-enabled idle-change new-line))

      idle-change-delay 1
      global-modes '(not circe-channel-mode circe-query-mode)
      )
    )

  ;; disable jshint since we prefer eslint checking
  (setq-default
    flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (general-nmap
    "]e" 'flycheck-next-error
    "[e" 'flycheck-previous-error
    )

  (use-package flycheck-pos-tip)
  (eval-after-load 'flycheck '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(defconfig treemacs
  (use-package treemacs)
  (use-package treemacs-evil)
  (use-package treemacs-projectile))

(defconfig company

  (use-package company
    :config
    (setq-ns company
      idle-delay 0.3
      selection-wrap-around t
      tooltip-align-annotations t
      dabbrev-downcase nil
      dabbrev-ignore-case t
      tooltip-align-annotations t
      tooltip-margin 2
      global-modes '(not
                      org-mode
                      shell-mode
                      circe-chat-mode
                      circe-channel-mode
                      circe-query-mode
                      )
      tooltip-align-annotations t
      )

    (define-key company-active-map [tab] 'company-complete)
    )

  (use-package company-quickhelp
    :init
    (company-quickhelp-mode 1)
    (setq company-quickhelp-delay 0.3)))

(defconfig dashdocs
  (defmacro ns/install-dashdoc (docset mode-hook)
    "Install dash DOCSET if dashdocs enabled, add mode hook to narrow dash search targets."
    `(when (bound-and-true-p ns/enable-dashdocs-p)
       (when (not (helm-dash-docset-installed-p ,docset))
         (message (format "Installing %s docset..." ,docset))
         (counsel-dash-install-docset (subst-char-in-string ?\s ?_ ,docset)))
       (add-hook ,mode-hook (fn (setq-local counsel-dash-docsets '(,docset))))))

  ;; (ns/guard ns/enable-home-p)

  (use-package dash)
  (use-package counsel-dash)

  (setq-ns counsel-dash
    min-length 0
    docsets-path (~ ".local/share/Zeal/Zeal/docsets")
    browser-func 'ns/eww-browse-existing-or-new)

  (make-directory counsel-dash-docsets-path t)

  (defcommand counsel-dash-word ()
    (if (region-active-p)
      (counsel-dash (buffer-substring (region-beginning) (region-end)))
      (counsel-dash "")))

  (ns/bind "nd" 'ns/counsel-dash-word))

(defconfig zoom
  (use-package zoom
    :config
    (setq zoom-size '(80 . 24))
    (setq zoom-size '(0.58 . 0.618))
    (zoom-mode 1)))

(defconfig python
  (ns/install-dashdoc "Python 2" 'python-mode-hook)

  ;; todo: mode eval-in-repl to it's own thing, probably
  (use-package eval-in-repl)
  (require 'eval-in-repl)
  (require 'eval-in-repl-python)

  (setq eir-jump-after-eval nil)
  (setq eir-always-split-script-window nil)
  (setq eir-delete-other-windows nil)
  (setq eir-repl-placement 'left)

  ;; run this first to start the repl
  ;; (eir-run-python)
  (ns/bind-mode 'python "e" 'eir-eval-in-python)
  )

(defconfig clojure
  (use-package clojure-mode)
  (use-package cider)
  (ns/install-dashdoc "Clojure" 'clojure-mode-hook)

  ;; TODO: learn lispyville
  ;; (use-package lispy)

  (defun ns/smart-cider-eval ()
    (interactive)
    (if (use-region-p)
      (cider-eval-region (region-beginning) (region-end))
      (if (s-blank-p (s-trim (thing-at-point 'line)))
        (cider-eval-last-sexp nil)
        (cider-eval-defun-at-point nil))))

  (ns/bind-mode 'clojure
    "e" 'ns/smart-cider-eval)

  (ns/bind-leader-mode
    'clojure
    "er" 'cider-eval-region
    "ei" 'cider-eval-last-sexp
    "eb" 'cider-eval-file
    "ee" 'cider-eval-defun-at-point
    )

  (if (executable-find "joker")
    (use-package flycheck-joker
      :config (require 'flycheck-joker))
    ;; todo: consider auto-getting this
    (message "init: if you want clojure flycheck support, install joker"))

  )

(defconfig nix
  (ns/guard ns/enable-home-p)
  (use-package nix-mode))

(defconfig music
  (ns/guard ns/enable-home-p)
  (ns/guard (executable-find "mpd"))

  (use-package emms)

  (defun emms-start()
    (require 'emms-player-mpd)
    (setq-ns emms-player-mpd
      server-name "localhost"
      server-port "6600"
      music-directory (~ "Music")
      ;; server-password "mypassword"
      )

    (add-to-list 'emms-info-functions 'emms-info-mpd)
    (add-to-list 'emms-player-list 'emms-player-mpd)

    ;; sync with mpd db, connect
    (emms-cache-set-from-mpd-all)
    (emms-player-mpd-connect)
    )

  (ns/bind "am" 'emms-start))

(defconfig projectile
  (use-package projectile)

  ;; still assuming git command, maybe lean on projectile for file listing
  (defun get-project-files (project-root)
    (let* ((default-directory (expand-file-name project-root))
            (project-files-relative
              (s-split "\n"
                (shell-command-to-string
                  counsel-git-cmd
                  ) t)
              ))

      (mapcar (fn (concat default-directory <>)) project-files-relative))
    )

  (defun ns/current-project-files()
    ;; current project here is just the one associated with the current buffer
    (let ((project-root (condition-case nil (projectile-project-root) (error nil))))
      (if project-root
        (get-project-files project-root)
        '())))

  (defun ns/all-project-files(open-buffers)
    (-flatten
      (mapcar 'get-project-files
        (-remove (lambda(file) (not file))
          (mapcar 'projectile-root-bottom-up open-buffers)
          ))))

  (defcommand jump-file ()
    (let* (
            (open-buffers
              ;; remove nils
              (-remove (lambda(file) (not file))
                (mapcar 'buffer-file-name (buffer-list))))

            (project-files
              (ns/current-project-files)
              ;; (if ns/enable-linux-p (ns/all-project-files open-buffers) (ns/current-project-files))
              )
            )

      (ivy-read "file: " (append recentf-list project-files open-buffers)
        :action #'find-file)))

  (ns/bind "ne" 'ns/jump-file ))

(defconfig javascript
  ;; note: this is huge, takes a bit.
  ;; (ns/install-dashdoc "JavaScript" 'web-mode-hook)

  (defun js-jsx-indent-line-align-closing-bracket ()
    "Workaround sgml-mode and align closing bracket with opening bracket"
    (save-excursion
      (beginning-of-line)
      (when (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))

  (advice-add #'js-jsx-indent-line :after #'js-jsx-indent-line-align-closing-bracket)

  ;; use web-mode for .js files
  (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

  (use-package rjsx-mode)
  (defun ns/webhook ()
    ;; (if (string-equal "tsx" (file-name-extension buffer-file-name))
    ;;   (if (equal web-mode-content-type "javascript")
    ;;     (progn
    ;;       (web-mode-set-content-type "jsx")
    ;;       (setup-tide-mode)
    ;;       )
    ;;     )
    ;;   (message "now set to: %s" web-mode-content-type))
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode)))

  (use-package web-mode
    :config
    (add-hook 'web-mode-hook
      'ns/webhook))

  (use-package prettier-js
    :config
    (add-hook 'typescript-mode-hook 'prettier-js-mode)
    (add-hook 'web-mode-hook 'prettier-js-mode)
    (add-hook 'js-mode-hook 'prettier-js-mode))

  ;; notes for using this
  ;; kill shx-mode
  ;; doesn't work with multiline input, or import command/multiple files
  (use-package nodejs-repl
    :config
    (ns/bind-leader-mode
      'nodejs-repl
      "er "'nodejs-repl-send-region
      "eb" 'nodejs-repl-load-file
      "ee" 'nodejs-repl-send-line
      "ei" 'nodejs-repl-send-last-expression
      )))

(defconfig typescript
  (ns/install-dashdoc "TypeScript" 'typescript-mode-hook)
  (use-package tide
    :config
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1))

    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (flycheck-add-mode 'typescript-tslint 'web-mode)

    ;; (add-hook 'before-save-hook 'tide-format-before-save)
    ))

(defconfig csharp
  ;; limitation: can only work with one server/solution at a time currently
  (ns/guard ns/enable-work-p)
  (ns/guard ns/enable-windows-p)
  (use-package omnisharp
    :config
    (when (not (omnisharp--resolve-omnisharp-server-executable-path))
      (omnisharp-install-server))

    (add-hook 'csharp-mode-hook 'omnisharp-mode)
    (add-to-list 'company-backends 'company-omnisharp)

    (ns/bind-mode 'csharp "nr" 'omnisharp-find-usages)
    (ns/bind-mode 'csharp "nR" 'omnisharp-find-usages-with-ido)))

(defconfig jump
  (use-package smart-jump
    :config
    (setq dumb-jump-selector 'ivy)
    (setq dumb-jump-force-searcher 'rg)
    (smart-jump-setup-default-registers)
    (ns/bind
      "n" '(:ignore t :which-key "Jump")
      "ng" 'smart-jump-go
      "nb" 'smart-jump-back
      ;; todo: bind "nr" to references in different modes
      )

    (advice-add #'smart-jump-go :after #'ns/focus-line)))


(defconfig pdf
  (ns/guard ns/enable-linux-p)
  (use-package pdf-tools))

(defconfig terraform
  (use-package terraform-mode))

(defconfig email
  (ns/guard ns/enable-home-p)
  ;; TODO
  )

(defconfig jekyll
  (use-package jekyll-modes))

(defconfig autohotkey
  (ns/guard ns/enable-windows-p)
  (use-package xahk-mode))

(defconfig markdown
  ;; (use-package markdownmode)
  )

(defconfig restclient
  (use-package restclient
    :config
    (ns/bind-leader-mode
      'restclient
      "ei" 'restclient-http-send-current-stay-in-window))

  (use-package company-restclient))

(defconfig sql
  ;; todo
  ;; (ns/install-dashdoc "SQLite" ')

  ;; setup: https://github.com/kostafey/ejc-sql#install-jdbc-drivers
  ;; (use-package ejc-sql
  ;;   :config
  ;;   ;; test local sqlite
  ;; )
  )

(defconfig latex
  (ns/bind-leader-mode 'latex
    "\\"  'TeX-insert-macro                            ;; C-c C-m
    "-"   'TeX-recenter-output-buffer                  ;; C-c C-l
    "%"   'TeX-comment-or-uncomment-paragraph          ;; C-c %
    ";"   'TeX-comment-or-uncomment-region             ;; C-c ; or C-c :
    ;; TeX-command-run-all runs compile and open the viewer
    "a"   'TeX-command-run-all                         ;; C-c C-a
    "b"   'latex/build
    "k"   'TeX-kill-job                                ;; C-c C-k
    "l"   'TeX-recenter-output-buffer                  ;; C-c C-l
    "m"   'TeX-insert-macro                            ;; C-c C-m
    "v"   'TeX-view                                    ;; C-c C-v
    )

  (ns/install-dashdoc "LaTeX" 'latex-mode-hook)

  ;; todo: https://github.com/The-Compiler/dotfiles/blob/543e48cd594750188dd3e935ef6dfd77f867ca71/spacemacs#L497
  ;; todo: this doesn't build?
  ;; ref: https://github.com/raxod502/straight.el/issues/240
  ;; (use-package company-auctex)
  )


(defconfig plantuml
  (use-package plantuml)
  (use-package flycheck-plantuml))

(defconfig ledger
  (ns/guard ns/enable-home-p)
  (use-package ledger-mode)
  (use-package flycheck-ledger)
  (use-package evil-ledger
    :config
    (add-hook 'ledger-mode-hook #'evil-ledger-mode)))

(defconfig lsp
  ;; want to use eglot + flycheck, hrm

  ;; (use-package cquery)
  )

(defconfig search-engines
  (use-package engine-mode
    :config

    ;; bind spc s 'hotkey' to a search url with a label
    (defmacro bind-search (label url hotkey)
      `(progn
         (defengine ,label ,url)
         (ns/bind
           (concat "s" ,hotkey) (intern (concat "engine/search-" (prin1-to-string ',label))))))

    (bind-search google "https://google.com/search?q=%s" "s")
    (bind-search melpa "https://melpa.org/#/?q=%s" "m")
    (bind-search stack-overflow "https://stackoverflow.com/search?q=%s" "o")
    (bind-search github "https://github.com/search?ref=simplesearch&q=%s" "g")
    (bind-search youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s" "y")
    (engine-mode t)))

(defconfig filehooks
  (ns/guard ns/enable-home-p)

  (defvar *afilename-cmd*
    `((,(~ ".Xresources") . "xrdb -merge ~/.Xresources && pkill -x --signal USR1 xst")
       (,(~ ".Xmodmap") . "xmodmap ~/.Xmodmap"))
    "File association list with their respective command.")

  (defun my/cmd-after-saved-file ()
    "Execute a command after saved a specific file."
    (setq filenames (mapcar 'car *afilename-cmd*))
    (dolist (file filenames)
      (let ((cmd (cdr (assoc file *afilename-cmd*))))
        (if (file-exists-p file)
          (when (equal (buffer-file-name) file)
            (shell-command cmd))
          (error "No such file %s" file)))))

  (add-hook 'after-save-hook 'my/cmd-after-saved-file)
  )

(defconfig emoji
  (use-package emojify
    :init (setq emojify-emoji-styles '(unicode github))
    :config
    ;; emojify-mode seems to mess with input, causing a character to
    ;; occasionally skip, so disabling (global-emojify-mode)
    (ns/bind
      "ie" 'emojify-insert-emoji
      "te" 'emojify-mode)))

(defconfig writing
  (ns/guard ns/enable-home-p)
  ;; todo
  ;; https://www.reddit.com/r/emacs/comments/8rxm7h/tip_how_to_better_manage_your_spelling_mistakes/

  (use-package writeroom-mode)
  (add-hook 'writeroom-mode-hook 'flyspell-mode)

  (setq-default fill-column 100)
  (add-hook 'writeroom-mode-hook 'auto-fill-mode)

  ;; The original value is "\f\\|[      ]*$", so we add the bullets (-), (+), and (*).
  ;; There is no need for "^" as the regexp is matched at the beginning of line.
  (setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")

  ;; toggle focus?
  (ns/bind "tf" 'writeroom-mode)

  (use-package mw-thesaurus)
  ;; (ns/bind-leader-mode 'org "q" 'mw-thesaurus--lookup-at-point)
  )

(defconfig elfeed
  (ns/guard ns/enable-home-p)
  (ns/guard nil)
  (use-package elfeed
    :config
    (ns/bind "af" 'elfeed)
    (setq elfeed-feeds
      '(
         "https://hnrss.org/newest?q=emacs"
         "http://pragmaticemacs.com/feed/"
         "http://xkcd.com/rss.xml"
         ))

    ;; Entries older than 2 weeks are marked as read
    (add-hook 'elfeed-new-entry-hook
      (elfeed-make-tagger :before "2 weeks ago"
        :remove 'unread))
    )

  (use-package elfeed-goodies
    :config (elfeed-goodies/setup))

  (use-package elfeed-org
    ;; todo - could then get feeds out of this file
    ))

(defconfig powershell
  (ns/guard ns/enable-windows-p)
  (use-package powershell))

(defconfig c
  (ns/install-dashdoc "C" 'c-mode-hook)

  ;; note: depends on clang and cmake
  (use-package irony)
  ;; fyi:
  ;; (irony-install-server)

  (use-package flycheck-irony)
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

  (defcommand recompile ()
    (let ((compilation-directory (projectile-project-root)))
      (ns/shell-exec-dontcare
        (format "cd '%s' && make clean" compilation-directory))
      (recompile)))

  )

(defconfig lua
  (use-package lua-mode)
  ;; note: lua-mode comes with some repl stuff that might come in handy
  )

(defconfig graphiz
  (use-package graphviz-dot-mode
    :config
    ;; (ns/bind)
    ;; todo: should we have a shorter automatic eval default binding?
    (ns/bind-leader-mode 'graphviz-dot "," 'graphviz-dot-preview))

  ;; use dot in org mode
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((dot . t)))

  (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "dot")))  ; don't ask for dot

  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  )

(defconfig deadgrep
  (ns/use-package deadgrep "Wilfred/deadgrep"
    :config
    (ns/bind "ss" 'deadgrep)
    (setq deadgrep-max-line-length 180)
    (general-nmap deadgrep-mode-map
      "RET" 'deadgrep-visit-result-other-window)))

(defconfig guix
  (ns/guard ns/enable-home-p)
  (use-package guix))

(defconfig elasticsearch
  (use-package es-mode))

(defconfig server
  (require 'server)
  (unless (server-running-p)
    (when ns/enable-windows-p
      (setq-ns server
        auth-dir (~ ".emacs.d/server")
        name "emacs-server-file"))
    (server-start)))

(defconfig blog
  (use-package htmlize)
  (use-package org-static-blog)
  (defun ns/blog-dir (dir) (concat "~/git/notes.neeasade.net/" dir))
  (ns/guard (f-exists-p (ns/blog-dir "")))

  (setq-ns org-static-blog
    publish-url "https://notes.neeasade.net"
    publish-title "Notes"
    publish-directory (ns/blog-dir "site/")
    posts-directory (ns/blog-dir "posts/")
    ;; abuse drafts to make static pages (drafts are not listed on the index screen)
    drafts-directory (ns/blog-dir "pages/")
    enable-tags nil)

  (defun ns/org-blog-clean-all ()
    (mapc (fn (f-delete <>))
      (f-entries org-static-blog-publish-directory
        (fn (and (s-ends-with-p ".html" <>)
              (not (s-equals-p "archive.html" <>)))))))
  ;; (ns/org-blog-clean-all)

  (defun ns/blog-after-hook ()
    (f-delete (ns/blog-dir "site/index.html") t)
    (f-copy (ns/blog-dir "site/archive.html") (ns/blog-dir "site/index.html")))

  (defun ns/blog-before-hook ()
    ;; pages can get edits in place
    (dolist (page (mapcar 'org-static-blog-matching-publish-filename (org-static-blog-get-draft-filenames)))
      (f-delete page))

    (setq org-static-blog-page-header (f-read (ns/blog-dir "inc/header"))
      org-static-blog-page-preamble (f-read (ns/blog-dir "inc/preamble"))
      org-static-blog-page-postamble (f-read (ns/blog-dir "inc/postamble"))))

  (advice-add #'org-static-blog-publish :before #'ns/blog-before-hook)
  (advice-add #'org-static-blog-publish :after #'ns/blog-after-hook))


(defconfig common-lisp
  (use-package slime)
  (setq inferior-lisp-program (which "sbcl"))
  (setq slime-contribs '(slime-fancy))

  ;; depends on eros package
  ;; I really like eval overlays.
  (defun slime-eval-last-sexp-overlay ()
    (interactive)
    (destructuring-bind (output value)
      (slime-eval `(swank:eval-and-grab-output ,(slime-last-expression)))
      (eros--make-result-overlay (concat output value)
        :where (point)
        :duration eros-eval-result-duration)))

  (ns/install-dashdoc "Common Lisp" 'lisp-mode-hook)

  (defun ns/smart-slime-eval ()
    (interactive)
    (if (use-region-p)
      (slime-eval-region (region-beginning) (region-end))
      (if (s-blank-p (s-trim (thing-at-point 'line)))
        (slime-eval-last-sexp-overlay)
        (save-excursion (end-of-defun) (slime-eval-last-sexp-overlay)))))

  ;; common-lisp-mode -> lisp-mode
  (ns/bind-mode 'lisp "e" 'ns/smart-slime-eval)
  )

;; big bois
;; having them listed like this gives ns/jump-config something to search for
(defconfig bedrock       (load "~/.emacs.d/lisp/trees/bedrock.el"))
(defconfig editing       (load "~/.emacs.d/lisp/trees/editing.el"))
(defconfig evil          (load "~/.emacs.d/lisp/trees/evil.el"))
(defconfig git           (load "~/.emacs.d/lisp/trees/git.el"))
(defconfig interface     (load "~/.emacs.d/lisp/trees/interface.el"))
(defconfig irc           (load "~/.emacs.d/lisp/trees/irc.el"))
(defconfig org           (load "~/.emacs.d/lisp/trees/org.el"))
(defconfig sanity        (load "~/.emacs.d/lisp/trees/sanity.el"))
(defconfig shell         (load "~/.emacs.d/lisp/trees/shell.el"))
(defconfig spaceline     (load "~/.emacs.d/lisp/trees/spaceline.el"))
(defconfig staging       (load "~/.emacs.d/lisp/trees/staging.el"))
(defconfig util          (load "~/.emacs.d/lisp/trees/util.el"))
(defconfig-base style    (interactive) (load "~/.emacs.d/lisp/trees/style.el"))

(provide 'forest)

;;; forest.el ends here
