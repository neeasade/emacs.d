;;; forest.el --- forest.el
;;; Commentary:
;;; functions             | ns/asdf
;;; pred functions        | ns/asdf-p
;;; interactive functions | ns/asdf
;;; enable vars           | ns/enable-asdf-p
;;; vars                  | ns/asdf
;;; buffer local vars     | ns/enable-asdf
;;; code:

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
    (defun! ns/helpful-or-dashdoc ()
      (if (eq 'emacs-lisp-mode major-mode)
        ;; todo: this
        (helpful-callable "")
        (if ns/enable-dashdocs-p
          (ns/counsel-dash-word)
          (message "dash docs not enabled!")
          )))

    (ns/bind "nd" 'ns/helpful-or-dashdoc)

    )

  (use-package eros
    :config
    (setq eros-eval-result-duration 20)
    (eros-mode 1)

    (defun! ns/smart-elisp-eval ()
      (if (use-region-p)
        (eval-region (region-beginning) (region-end))
        (if (s-blank-p (s-trim (thing-at-point 'line)))
          (eros-eval-last-sexp nil)
          (eros-eval-defun nil))))

    (ns/bind-mode 'emacs-lisp "e" 'ns/smart-elisp-eval)))

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
       (when nil
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

  (defun! ns/counsel-dash-word ()
    (if (region-active-p)
      (counsel-dash (buffer-substring (region-beginning) (region-end)))
      (counsel-dash "")))

  (ns/bind "nd" 'ns/counsel-dash-word)

  )

(defconfig zoom
  (use-package zoom
    :config
    (setq zoom-size '(80 . 24))
    (setq zoom-size '(0.58 . 0.618))
    (zoom-mode 1)))

(defconfig python
  (ns/install-dashdoc "Python 2" 'python-mode-hook)
  (use-package elpy))

(defconfig clojure
  (use-package clojure-mode)
  (use-package cider)
  (setq cider-eval-result-duration 20)

  (ns/inmap 'cider-repl-mode-map (kbd "C-e") 'cider-repl-previous-input)
  (ns/inmap 'cider-repl-mode-map (kbd "C-n") 'cider-repl-next-input)


  (ns/install-dashdoc "Clojure" 'clojure-mode-hook)

  ;; TODO: learn lispyville
  ;; (use-package lispy)

  (defun! ns/smart-cider-eval ()
    (if (use-region-p)
      (cider-eval-region (region-beginning) (region-end))
      (if (s-blank-p (s-trim (thing-at-point 'line)))
        (cider-eval-last-sexp nil)
        (cider-eval-defun-at-point nil))))

  (ns/bind-mode 'clojure
    "e" 'ns/smart-cider-eval)

  (when (executable-find "joker")
    (use-package flycheck-joker :config (require 'flycheck-joker)))
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
    (emms-player-mpd-connect))

  (ns/bind "am" 'emms-start))

(defconfig projectile
  (use-package projectile)

  ;; still assuming git command, maybe lean on projectile for file listing
  (defun ns/get-project-files (project-root)
    (let* ((default-directory (expand-file-name project-root))
            (project-files-relative
              (s-split "\n"
                (s-replace (char-to-string ?\0) "\n"
                  (shell-command-to-string
                    counsel-git-cmd
                    )) t)))

      (mapcar (fn (concat default-directory <>)) project-files-relative)))

  (defun ns/all-project-files (open-buffers)
    (-flatten
      (mapcar 'ns/get-project-files
        (-remove (lambda(file) (not file))
          (mapcar 'projectile-root-bottom-up open-buffers)
          ))))

  ;; putting this in a function so it can be used
  ;; by dmenu_switcher
  (defun ns/jump-file-candidates ()
    (let* ((open-buffers
             ;; remove nils
             (-remove (lambda (file) (not file))
               (mapcar 'buffer-file-name (buffer-list))))

            (project-files
              ;; (ns/current-project-files)
              (let ((current-file-name (buffer-file-name (current-buffer))))
                (if current-file-name
                  (ns/all-project-files (list current-file-name)) '()))

              ;; (ns/get-project-files (current-file))
              ;; (if ns/enable-linux-p (ns/all-project-files open-buffers) (ns/current-project-files))
              ))

      (append recentf-list project-files open-buffers)))

  ;; todo: piper emacs from emacsconf 2019 would be a nice thing to have here

  ;; maybe consider also a jump-qutebrowser-history-candidates -- something like urls from the past month? idk
  (defun ns/jump-qutebrowser-candidates ()
    (s-split "\n"
      (ns/shell-exec (format "grep -A 6 '    \\- active: true' %s | grep title | sed 's/.*title: //'"
                       (~ ".local/share/qutebrowser/sessions/default.yml")))))

  (defun! ns/jump-file ()
    (ivy-read "file: "
      (ns/jump-file-candidates)
      :action #'find-file))

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
    (defun! setup-tide-mode ()
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
      "nr" 'smart-jump-references
      )

    (advice-add #'smart-jump-go :after #'ns/focus-line)))


(defconfig pdf
  (ns/guard ns/enable-linux-p)
  (use-package pdf-tools))

(defconfig terraform
  (use-package terraform-mode))

(defconfig email
  (ns/guard ns/enable-home-p)
  (ns/guard nil)

  ;; (use-package wanderlust :init (autoload 'wl "wl" "Wanderlust" t))

  ;; IMAP
  (setq elmo-imap4-default-server "imap.gmail.com")
  (setq elmo-imap4-default-user (pass "gmail/user"))
  (setq elmo-imap4-default-authenticate-type 'clear)
  (setq elmo-imap4-default-port '993)
  (setq elmo-imap4-default-stream-type 'ssl)

  (setq elmo-imap4-use-modified-utf7 t)

  ;; SMTP
  (setq wl-smtp-connection-type 'starttls)
  (setq wl-smtp-posting-port 587)
  (setq wl-smtp-authenticate-type "plain")
  (setq wl-smtp-posting-user (pass "gmail/user"))
  (setq wl-smtp-posting-server "smtp.gmail.com")
  (setq wl-local-domain "gmail.com")

  (setq wl-default-folder "%inbox")
  (setq wl-default-spec "%")
  (setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
  (setq wl-trash-folder "%[Gmail]/Trash")

  (setq wl-folder-check-async t)

  ;; ???
  (setq elmo-imap4-use-modified-utf7 t)

  (autoload 'wl-user-agent-compose "wl-draft" nil t)
  (setq mail-user-agent 'wl-user-agent)

  (define-mail-user-agent
    'wl-user-agent
    'wl-user-agent-compose
    'wl-draft-send
    'wl-draft-kill
    'mail-send-hook)

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

  (setq ns/filename-cmd
    (list
      (~ ".Xresources") "xrdb -merge ~/.Xresources && pkill -x --signal USR1 xst"
      (~ ".Xmodmap") "xmodmap ~/.Xmodmap"
      ))

  (defun my/cmd-after-saved-file ()
    "Execute a command after saved a specific file."
    (dolist (pair (-partition 2 ns/filename-cmd))
      (let ((file (car pair))
             (cmd (cadr pair)))
        (when (and (equal (buffer-file-name) file)
                (f-exists-p file))
          (ns/shell-exec-dontcare cmd))
        )))

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
  ;; (ns/guard ns/enable-home-p)
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
        :remove 'unread)))

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

  (defun! ns/recompile ()
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
    (ns/bind-leader-mode 'graphviz-dot "," 'graphviz-dot-preview)
    (ns/bind-mode 'graphviz-dot "e" 'graphviz-dot-preview)
    )

  ;; use dot in org mode
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((dot . t)))

  (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

  (defun my-org-confirm-babel-evaluate (lang body)
    (not (string= lang "dot")))  ; don't ask for dot

  (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

  (defun! ns/refresh-images-org ()
    (org-toggle-inline-images)
    (org-toggle-inline-images))

  ;; todo: CcCc is pretty generic, ideally we only do this after eval'ing a dot source block.
  (advice-add #'org-ctrl-c-ctrl-c :after #'ns/refresh-images-org)
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
  ;; (use-package htmlize)

  (defun ns/blog-file-to-meta (path is-post)
    (let* ((last-edited
             (let ((git-query-result
                     (ns/shell-exec (format "cd %s; git log -1 --format=%%cI %s" (f-dirname path) path))))
               (if (s-blank-p git-query-result)
                 nil (substring git-query-result 0 10))))
            (published-date (when is-post (substring (f-base path) 0 10)))

            (history-link
              (format "https://github.com/neeasade/neeasade.github.io/commits/source/posts/%s"
                (f-filename path)))

            (post-org-content-lines
              (-non-nil
                `(,(format "#+SETUPFILE: %s" (~ "git/neeasade.github.io/site/assets/org/setup.org"))
                   ,@(when is-post
                       (list
                         "#+BEGIN_CENTER"
                         (format "published <%s>" published-date)
                         (format "¦ [[%s][edited %s]]" history-link last-edited)
                         "#+END_CENTER"
                         ))
                   ,@(s-split "\n" (org-file-contents path))
                   ;; footer
                   "#+BEGIN_CENTER"
                   ">>> [[file:./index.html][Index]] <<<"
                   "#+END_CENTER"
                   )))
            (post-title
              (->> post-org-content-lines
                (-filter (fn (s-starts-with-p "#+title:" <>)))
                car (s-replace "#+title: " "")))
            (post-is-draft
              (car (-filter (fn (s-contains-p "#+draft: t" <>)) post-org-content-lines)))
            )
      (a-list
        :path path
        :org-content (s-join "\n" post-org-content-lines)
        :is-draft post-is-draft
        :title post-title
        :publish-date published-date
        :html-dest (format "%s/%s.html" ns/blog-site-dir (f-base path))
        :edited-date last-edited
        :history-link history-link)))

  (defun! ns/blog-generate ()
    (let ((ns/blog-posts-dir (~ "git/neeasade.github.io/posts"))
           (ns/blog-pages-dir (~ "git/neeasade.github.io/pages"))
           (ns/blog-site-dir (~ "git/neeasade.github.io/site"))
           (default-directory (~ "git/neeasade.github.io/site"))
           (org-export-with-toc nil)
           (org-export-with-timestamps nil)
           (org-export-with-date nil)
           (org-html-html5-fancy t)

           (org-time-stamp-custom-formats '("%Y-%m-%d"))

           (org-display-custom-times t)

           ;; don't ask about generation when exporting
           (org-confirm-babel-evaluate (fn nil)))

      ;; cleanup
      (mapcar 'f-delete
        (f-entries ns/blog-site-dir
          (fn (s-ends-with-p ".html" <>))))

      (defun ns/blog-publish-file (org-file-meta)
        (with-temp-buffer
          (insert (a-get org-file-meta :org-content))
          (org-export-to-file 'html (a-get org-file-meta :html-dest))))

      (let ((org-post-metas (mapcar (fn (ns/blog-file-to-meta <> t)) (f-entries ns/blog-posts-dir (fn (s-ends-with-p ".org" <>)))))
             (org-page-metas (mapcar (fn (ns/blog-file-to-meta <> nil)) (f-entries ns/blog-pages-dir (fn (s-ends-with-p ".org" <>))))))
        (mapcar 'ns/blog-publish-file org-page-metas)
        (mapcar 'ns/blog-publish-file org-post-metas))
      )
    t ;; for calling from elisp script
    )

  )

(defconfig common-lisp
  (use-package slime)
  (setq inferior-lisp-program (which "sbcl"))
  (setq slime-contribs '(slime-fancy))

  (defun! slime-eval-last-sexp-overlay ()
    (destructuring-bind (output value)
      (slime-eval `(swank:eval-and-grab-output ,(slime-last-expression)))
      (eros--make-result-overlay (concat output value)
        :where (point)
        :duration eros-eval-result-duration)))

  (ns/install-dashdoc "Common Lisp" 'lisp-mode-hook)

  (defun! ns/smart-slime-eval ()
    (if (use-region-p)
      (slime-eval-region (region-beginning) (region-end))
      (if (s-blank-p (s-trim (thing-at-point 'line)))
        (slime-eval-last-sexp-overlay)
        (save-excursion (end-of-defun) (slime-eval-last-sexp-overlay)))))

  ;; common-lisp-mode -> lisp-mode
  (ns/bind-mode 'lisp "e" 'ns/smart-slime-eval))

(defconfig alda
  (use-package alda-mode)

  (defun! ns/smart-alda-eval ()
    (if (use-region-p)
      (alda-play-region (region-beginning) (region-end))
      (progn
        ;; why does this not restore the mark..
        (alda-play-block)
        (evil-force-normal-state)
        )))


  (ns/bind-mode 'alda "e" 'ns/smart-alda-eval)

  )

(defconfig scripting
  ;; enable calling emacs lisp scripts in running emacs server via 'elisp' script
  (add-to-list 'interpreter-mode-alist '("elisp" . emacs-lisp-mode))

  ;; cf https://stackoverflow.com/questions/30568113/result-value-of-elisp-code-stored-in-a-file
  (defun ns/eval-file (file &rest ns-args)
    "Execute FILE and return the result of the last expression."
    (eval
      ;; (ignore-errors
      (read-from-whole-string
        (concat "(progn "
          (with-temp-buffer
            (insert-file-contents file)
            (buffer-string))
          ")"))))

  ;; helper for unpacking args provided by eval-file
  ;; (ns/let-script-args (named named2) body)
  (defmacro ns/let-script-args (args &rest content)
    `(let (,@(mapcar
               (fn (list (nth <> args)
                     (nth <> ns-args)))
               (number-sequence 0 (- (length ns-args) 1))))
       ,@content
       ))
  )

;; big bois
;; having them listed like this gives ns/jump-config something to search for
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
