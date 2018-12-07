;;; theworld.el --- make the thing
;;; commentary:
;;; functions             | ns/asdf
;;; pred functions        | ns/asdf-p
;;; interactive functions | ns/asdf
;;; enable vars           | ns/enable-asdf-p
;;; vars                  | ns/asdf
;;; buffer local vars     | ns/enable-asdf
;;; code:

(defmacro defconfig-base (label &rest body)
  `(defun ,(intern (concat "ns/" (prin1-to-string label))) nil
     ,@body))

(defmacro defconfig (label &rest body)
  `(defconfig-base ,label
     (let ((config-name ,(prin1-to-string label)))
       (message (concat "loading " config-name "..."))
       (catch 'config-catch
         (setq ,(intern (format "ns/enable-%s-p" (prin1-to-string label))) nil)
         ,@body
         (setq ,(intern (format "ns/enable-%s-p" (prin1-to-string label))) t)
         ))))

(defmacro ns/guard (&rest conditions)
  (if (not (eval (cons 'and conditions)))
    '(when t (throw 'config-catch (concat "config guard " config-name)))))

(defmacro defcommand (label args &rest body)
  `(defun ,(intern (concat "ns/" (prin1-to-string label))) ,args
     (interactive)
     ,@body))

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

  (setq use-package-always-ensure t)
  )

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
    )

  (use-package eros
    :config
    (setq eros-eval-result-duration 20)
    (eros-mode 1)
    (ns/bind-leader-mode
      'emacs-lisp
      "er" 'eval-region
      "ei" 'eros-eval-last-sexp
      "ee" 'eros-eval-defun
      )
    ))


(defconfig flycheck
  (use-package flycheck
    :config
    ;; cf http://www.flycheck.org/en/latest/user/syntax-checks.html#check-automatically
    (setq-ns flycheck
      check-syntax-automatically (if ns/enable-windows-p
                                   '(save mode-enabled idle-change)
                                   '(save mode-enabled idle-change new-line))
      idle-change-delay 1
      )

    ;; (flycheck) disable jshint since we prefer eslint checking
    (setq-default
      flycheck-disabled-checkers
      (append flycheck-disabled-checkers
        '(javascript-jshint)))

    ;; use eslint with web-mode for jsx files
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    )

  ;; (ns/bind
  ;;   "e" '(:ignore t :which-key "Errors")
  ;;   "en" 'flycheck-next-error
  ;;   "ep" 'flycheck-previous-error
  ;;   )

  (general-nmap
    "]e" 'flycheck-next-error
    "[e" 'flycheck-previous-error
    ))

(defconfig treemacs
  (use-package treemacs)
  (use-package treemacs-evil)
  (use-package treemacs-projectile)
  )

(defconfig company
  (use-package company
    :config
    (setq-ns company
      idle-delay (if ns/enable-windows-p 0.2 0)
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
                      )
      tooltip-align-annotations t
      )

    ;; TODO: investigate tab handling like VS completely
    (define-key company-active-map [tab] 'company-complete)
    )

  (use-package company-quickhelp
    :init
    (company-quickhelp-mode 1)
    (setq company-quickhelp-delay 0.3)
    )
  )



(defconfig dashdocs
  (defmacro ns/install-dashdoc (docset mode-hook)
    "Install dash DOCSET if dashdocs enabled."
    (when (bound-and-true-p ns/enable-dashdocs-p)
      (if (helm-dash-docset-installed-p docset)
        `(progn
           (message (format "%s docset is already installed!" ,docset))
           (add-hook ,mode-hook (lambda() (setq-local counsel-dash-docsets '(,docset))))
           )
        `(progn
           (message (format "Installing %s docset..." ,docset))
           (counsel-dash-install-docset (subst-char-in-string ?\s ?_ ,docset))
           (add-hook ,mode-hook (lambda() (setq-local counsel-dash-docsets '(,docset))))
           ))))

  (ns/guard ns/enable-home-p)

  (use-package dash)
  (use-package counsel-dash
    :config
    (setq-ns counsel-dash
      min-length 2
      docsets-path (concat user-emacs-directory "docsets")
      browser-func 'ns/eww-browse-existing-or-new
      ))

  (defcommand counsel-dash-word ()
    (if (region-active-p)
      (counsel-dash (buffer-substring (region-beginning) (region-end)))
      (counsel-dash (thing-at-point 'word))))

  (ns/bind
    "nd" 'ns/counsel-dash-word)
  )



(defconfig zoom
  (use-package zoom
    :config
    (setq zoom-size '(80 . 24))
    (setq zoom-size '(0.58 . 0.618))
    (zoom-mode 1)))

(defconfig clojure
  (use-package clojure-mode)
  (use-package cider)
  (ns/install-dashdoc "Clojure" 'clojure-mode-hook)

  ;; TODO: learn lispyville
  ;; (use-package lispy)

  (ns/bind-leader-mode
    'clojure
    "er" 'cider-eval-region
    "ei" 'cider-eval-last-sexp
    "eb" 'cider-evil-file
    )
  )

(defconfig nix
  (ns/guard ns/enable-home-p)
  (use-package nix-mode)
  )

(defconfig interface
  ;; todo: into occur/search buffer solution for better finding when don't know what we're looking for
  (use-package ivy
    :config
    (setq-ns ivy
      re-builders-alist '((ivy-switch-buffer . ivy--regex-plus) (t . ivy--regex-fuzzy))
      initial-inputs-alist nil
      fixed-height-minibuffer t
      count-format "%d/%d "
      )

    ;; todo: this will also need a hook on frame focus now -- for when using emacs as term
    (add-hook 'window-configuration-change-hook 'dynamic-ivy-height)

    (defun dynamic-ivy-height()
      (setq ivy-height (/ (frame-total-lines) 2)))

    (dynamic-ivy-height)
    (ivy-mode 1)

    (use-package prescient :config (prescient-persist-mode))
    (use-package ivy-prescient :config (ivy-prescient-mode))
    (use-package company-prescient :config (company-prescient-mode))
    )

  ;; counsel
  (use-package counsel
    :config
    (use-package rg)
    (setq-ns counsel
      grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
      rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")

    ;; counsel-rg is crashing emacs on windows
    ;; (can't C-g/esc, can't send USR2 on windows)
    ;; (toggle-debug-on-error nil)
    ;; (toggle-debug-on-quit nil)
    )

  (use-package ranger
    :init (setq ranger-override-dired t)
    :config
    (setq-ns ranger
      show-literal nil
      show-hidden t
      cleanup-eagerly t
      )

    ;; call with eg 'dired-mode
    (defcommand kill-buffers-by-mode (mode)
      (mapc (lambda (buffer)
              (when (eq mode (buffer-local-value 'major-mode buffer))
                (kill-buffer buffer)))
        (buffer-list)))

    (defcommand kill-ranger-buffers ()
      (ns/kill-buffers-by-mode 'ranger-mode))

    (advice-add #'ranger-close :after #'ns/kill-ranger-buffers)

    (defcommand deer-with-last-shell ()
      (let ((current-buffer (buffer-name (current-buffer))))
        (if (or (s-match "\*spawn-shell.*" current-buffer)
              (s-match "\*shell-[1-9]\*" current-buffer))
          (setq ns/last-shell current-buffer)
          (setq ns/last-shell shell-pop-last-shell-buffer-name)))
      (deer))

    (ns/bind "d" 'ns/deer-with-last-shell)

    (defcommand open () (ns/shell-exec-dontcare (format "xdg-open \"%s\"" (dired-get-file-for-visit))))
    (define-key ranger-normal-mode-map (kbd "RET") 'ns/open)
    )

  (defun my-resize-margins ()
    (let ((margin-size (if ns/center (/ (- (frame-width) 120) 2) 0)))
      (set-window-margins nil margin-size margin-size)))

  (defcommand toggle-margin ()
    (if (not (bound-and-true-p ns/center))
      (setq ns/center nil))

    (if ns/center
      (remove-hook 'window-configuration-change-hook #'my-resize-margins)
      (add-hook 'window-configuration-change-hook #'my-resize-margins))

    (setq ns/center (not ns/center))
    (my-resize-margins))

  (defcommand kill-current-buffer()
    (kill-buffer nil))

  (defcommand follow-mode ()
    (follow-mode)
    (delete-other-windows)
    (evil-window-vsplit))

  (ns/bind
    "/"   'counsel-rg
    "TAB" '(switch-to-other-buffer :which-key "prev buffer")
    "SPC" 'counsel-M-x

    ;; windows
    "w" '(:ignore t :which-key "Windows")
    "wh" 'evil-window-left
    (concat "w" (if ns/enable-colemak "n" "j")) 'evil-window-down
    (concat "w" (if ns/enable-colemak "e" "k")) 'evil-window-up
    "wl" 'evil-window-right
    "wd" 'evil-window-delete
    "ww" 'other-window
    "wf" 'ns/follow-mode
    "wc" 'ns/toggle-margin

    "wm" 'delete-other-windows ;; window-max
    "wo" 'other-frame

    ;; Applications
    "a" '(:ignore t :which-key "Applications")

    "b" '(:ignore t :which-key "Buffers")
    "bd" 'ns/kill-current-buffer

    "n" '(:ignore t :which-key "Jump")
    "nd" 'counsel-imenu
    )

  (use-package alert
    :config (setq alert-default-style
              (if ns/enable-windows-p
                'toaster
                'libnotify
                )))

  (use-package which-key
    :config
    (setq-ns which-key
      idle-delay 1.5
      side-window-max-width 0.33
      sort-order 'which-key-key-order-alpha
      )
    (which-key-setup-side-window-right-bottom)
    (which-key-mode)
    )



  (use-package ace-jump-buffer
    :config
    (setq ajb-sort-function 'bs--sort-by-recentf)

    (add-hook 'window-configuration-change-hook 'dynamic-ajb-height)
    (defun dynamic-ajb-height()
      (setq ajb-max-window-height (/ (frame-total-lines) 2)))

    (dynamic-ajb-height)

    (ns/bind
      "bs" 'ace-jump-buffer
      "bm" 'ace-jump-same-mode-buffers))

  (defcommand kill-other-buffers ()
    "Kill all other buffers."
    (mapc 'kill-buffer
      (delq (current-buffer)
        (remove-if-not 'buffer-file-name (buffer-list)))))

  (ns/bind
    "bb" 'counsel-ibuffer
    "bK" 'ns/kill-other-buffers
    "bk" 'kill-matching-buffers
    ))

(defconfig music
  (ns/guard ns/enable-home-p)
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

  (ns/bind "am" 'emms-start)
  )

(defconfig projectile
  (use-package projectile)

  ;; still assuming git command, maybe lean on projectile for file listing
  (defun get-project-files (project-root)
    (let* ((default-directory (expand-file-name project-root))
            (project-files-relative
              (s-split "\n"
                (shell-command-to-string
                  counsel-git-cmd
                  ) t)))

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
            (recent-files recentf-list)

            (open-buffers
              ;; remove nils
              (-remove (lambda(file) (not file))
                (mapcar 'buffer-file-name (buffer-list))))

            (project-files
              (if ns/enable-linux-p
                (ns/all-project-files open-buffers)
                (ns/current-project-files)))
            )

      (ivy-read "file: "
        (mapcar (lambda (s)
                  (s-replace
                    ;; todo: consider only doing this
                    ;; replace if we are windows
                    (s-replace "\\" "/" (~ ""))
                    "~/" s))
          (-distinct (append open-buffers recent-files project-files)))
        :action #'find-file)))

  ;; idk which of these I like better
  (ns/bind "ne" 'ns/jump-file )
  )

(defconfig javascript
  ;; note: this is huge, takes a bit.
  (ns/install-dashdoc "JavaScript" 'web-mode-hook)

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
      (setup-tide-mode))
    )

  (use-package web-mode
    :config
    (add-hook 'web-mode-hook
      'ns/webhook))

  (use-package prettier-js
    :config
    (when ns/enable-work-p
      (add-hook 'typescript-mode-hook 'prettier-js-mode)
      (add-hook 'web-mode-hook 'prettier-js-mode)
      (add-hook 'js-mode-hook 'prettier-js-mode)))

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
      ))
  )

(defconfig typescript
  (ns/install-dashdoc "TypeScript" 'typescript-mode-hook)
  (use-package tide
    :config
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      )

    (add-hook 'typescript-mode-hook #'setup-tide-mode)

    ;; todo: check this
    (flycheck-add-mode 'typescript-tslint 'web-mode)

    ;; (add-hook 'before-save-hook 'tide-format-before-save)
    )
  )

(defconfig csharp
  ;; limitation: can only work with one server/solution at a time currently
  ;; todo: bind:
  ;; omnisharp-start-omnisharp-server
  ;; omnisharp-stop-omnisharp-server
  (use-package omnisharp
    :config
    (when (not (omnisharp--resolve-omnisharp-server-executable-path))
      (omnisharp-install-server))

    (add-hook 'csharp-mode-hook 'omnisharp-mode)
    (add-to-list 'company-backends 'company-omnisharp)

    ;; (ns/bind-leader-mode 'csharp-mode "s" 'omnisharp-start-omnisharp-server)
    (ns/bind-mode 'csharp "nu" 'omnisharp-find-usages)
    (ns/bind-mode 'csharp "nU" 'omnisharp-find-usages-with-ido)
    ))


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
      )

    (advice-add #'smart-jump-go :after #'ns/focus-line)
    ))


(defconfig pdf
  (ns/guard ns/enable-home-p)
  (use-package pdf-tools)
  )

(defconfig terraform
  (use-package terraform-mode)
  )


(defconfig slack
  (ns/guard ns/enable-home-p)
  (use-package slack
    :commands (slack-start)
    :init
    (setq slack-buffer-emojify t)
    (setq slack-prefer-current-team t)

    :config
    (when ns/enable-windows-p
      ;; https://github.com/yuya373/emacs-slack/issues/161
      (setq request-backend 'url-retrieve)
      (setq slack-request-timeout 50)
      )

    (slack-register-team
      :name (pass "slackteam")
      :default t
      :client-id (pass "slackid")
      :client-secret (pass "slack")
      :token (pass "slacktoken")
      :subscribed-channels '(general random)
      :full-and-display-names t
      )
    )

  ;; todo: where is slack-info/context for this bind
  (ns/bind-leader-mode
    'slack-info
    "u" 'slack-room-update-messages)

  (ns/bind-leader-mode
    'slack
    "c" 'slack-buffer-kill
    "ra" 'slack-message-add-reaction
    "rr" 'slack-message-remove-reaction
    "rs" 'slack-message-show-reaction-users
    "pl" 'slack-room-pins-list
    "pa" 'slack-message-pins-add
    "pr" 'slack-message-pins-remove
    "mm" 'slack-message-write-another-buffer
    "me" 'slack-message-edit
    "md" 'slack-message-delete
    "u" 'slack-room-update-messages
    "2" 'slack-message-embed-mention
    "3" 'slack-message-embed-channel
    )

  ;; todo: something for these maybe
  ;; "\C-n" 'slack-buffer-goto-next-message
  ;; "\C-p" 'slack-buffer-goto-prev-message)

  (ns/bind-leader-mode
    'slack-edit-message
    "k" 'slack-message-cancel-edit
    "s" 'slack-message-send-from-buffer
    "2" 'slack-message-embed-mention
    "3" 'slack-message-embed-channel
    )

  (ns/bind
    "as" 'slack-start))

(defconfig email
  (ns/guard ns/enable-home-p)
  ;; TODO
  )

(defconfig jekyll
  (use-package jekyll-modes)
  )

(defconfig autohotkey
  (ns/guard ns/enable-windows-p)
  (use-package xahk-mode)
  )

(defconfig markdown
  ;; (use-package markdownmode)
  )

(defconfig restclient
  (use-package restclient
    :config
    (ns/bind-leader-mode
      'restclient
      "ei" 'restclient-http-send-current-stay-in-window
      )
    )

  (use-package company-restclient)
  )

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
  (use-package flycheck-plantuml)
  )

(defconfig ledger
  (ns/guard ns/enable-home-p)
  (use-package ledger-mode)
  (use-package flycheck-ledger)
  (use-package evil-ledger
    :config
    (add-hook 'ledger-mode-hook #'evil-ledger-mode)
    )
  )

(defconfig lsp
  (use-package lsp-ui)
  (use-package lsp-javascript-flow)
  (use-package lsp-javascript-typescript)

  (use-package cquery)


  (defun my-company-transformer (candidates)
    (let ((completion-ignore-case t))
      (all-completions (company-grab-symbol) candidates)))

  (defun my-js-hook nil
    (make-local-variable 'company-transformers)
    (push 'my-company-transformer company-transformers))

  (add-hook 'web-mode-hook 'my-js-hook)
  (add-hook 'web-mode-hook #'lsp-javascript-typescript-enable)

  (remove-hook 'web-mode-hook #'lsp-javascript-flow-enable)

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
    (engine-mode t)
    ))

(defconfig filehooks
  (ns/guard ns/enable-home-p)

  (defvar *afilename-cmd*
    ;; todo: consider more here -- sxhkd, bspwmrc? ~/.wm_theme (if smart-load ever comes to fruition)
    `((,(~ ".Xresources") . "xrdb -merge ~/.Xresources")
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
  ;; https://github.com/agzam/mw-thesaurus.el

  (use-package writeroom-mode)
  (add-hook 'writeroom-mode-hook 'flyspell-mode)

  (setq-default fill-column 80)
  (add-hook 'writeroom-mode-hook 'auto-fill-mode)
  ;; The original value is "\f\\|[      ]*$", so we add the bullets (-), (+), and (*).
  ;; There is no need for "^" as the regexp is matched at the beginning of line.
  (setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] ")

  ;; toggle focus?
  (ns/bind "tf" 'writeroom-mode)

  (use-package mw-thesaurus)
  (ns/bind-leader-mode 'org "q" 'mw-thesaurus--lookup-at-point)
  )

;; use shell frames as terminals.
;; todo: consider hiding this in window manager then popping it
;; in rather than making a frame at launch time
(defconfig terminal
  (defcommand stage-terminal ()
    (let ((default-directory (~ "")))
      (shell "*spawn-shell-staged*")
      (ns/toggle-modeline)
      (delete-window)
      ;; todo: find a way to set initial dirtrack to default-directory
      (dirtrack-mode)
      ))

  (ns/stage-terminal)


  (defcommand spawn-terminal ()
    (select-frame (make-frame))
    (ns/pickup-shell))

  (defcommand pickup-shell ()
    (switch-to-buffer (get-buffer "*spawn-shell-staged*"))
    (rename-buffer (concat "*spawn-shell-" (number-to-string (random)) "*"))
    (delete-other-windows)
    (set-window-fringes nil 0 0)
    (ns/stage-terminal)
    )

  (defcommand kill-spawned-shell (frame)
    (let ((windows (window-list frame)))
      (when (eq 1 (length windows))
        (let ((buffer (window-buffer (car windows))))
          (when (s-match "\*spawn-shell.*" (buffer-name buffer))
            (kill-buffer buffer))))))

  (ns/bind "at" 'ns/spawn-terminal)
  (add-hook 'delete-frame-hook 'ns/kill-spawned-shell))


(defconfig elfeed
  (ns/guard ns/enable-home-p)
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

(defconfig stackexchange
  (ns/guard ns/enable-home-p)
  (use-package sx
    :config
    (ns/bind "as" 'sx-tab-all-questions)
    ))

(defconfig reddit
  (ns/guard ns/enable-home-p)
  ;; todo: this needs some rice love
  ;; text wrapping comments don't align
  ;; could use some better keybinds, UX
  (use-package md4rd
    :config
    (ns/bind "ar" 'md4rd)
    (setq md4rd-subs-active
      '(unixporn emacs))

    (general-nmap md4rd-mode-map
      "q" 'ns/kill-current-buffer
      "o" 'md4rd-open
      "r" 'md4rd-reply
      "t" 'md4rd-widget-toggle-line
      ;; "<tab>" 'md4rd-widget-toggle-line
      )

    (add-hook 'md4rd-mode-hook 'ns/md4rd)
    (defun ns/md4rd ()
      ;; (setq wrap-prefix "         ")
      )))

(defconfig powershell
  (ns/guard ns/enable-windows-p)
  (use-package powershell))

(defconfig c
  ;; note: depends on clang and cmake
  (use-package irony)
  (use-package flycheck-irony)

  ;; jump to it
  (defcommand recompile ()
    (let ((compilation-directory (projectile-project-root)))
      (ns/shell-exec-dontcare
        (format "cd '%s' && make clean" compilation-directory))
      (recompile)))

  (ns/bind "jk" 'ns/recompile)
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
      "RET" 'deadgrep-visit-result-other-window)
    ))

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
  (use-package org-static-blog
    :config
    (setq ns/blog-name "kraken.docs")
    (setq-ns org-static-blog
      publish-title "Notes"
      publish-url (concat "https://" ns/blog-name)
      publish-directory (concat "~/git/" ns/blog-name "/blog/")
      posts-directory (concat "~/git/" ns/blog-name "/posts")
      drafts-directory  (concat "~/git/" ns/blog-name "/drafts")
      enable-tags nil
      )

    (defun ns/after-sitegen-action()
      ;; todo: move folders over
      )

    (setq org-static-blog-page-header
      ;; consider generating from directory contents
      "
    <link href=\"https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.0/normalize.css\" rel=\"stylesheet\">
    <link rel=\"stylesheet\" href=\"https://unpkg.com/sakura.css/css/sakura.css\" type=\"text/css\">
    "
      )
    )
  )

;; big bois
(defconfig evil (load "~/.emacs.d/lisp/moons/evil.el"))
(defconfig staging (load "~/.emacs.d/lisp/moons/staging.el"))
(defconfig bedrock (load "~/.emacs.d/lisp/moons/bedrock.el"))
(defconfig util (load "~/.emacs.d/lisp/moons/util.el"))
(defconfig shell (load "~/.emacs.d/lisp/moons/shell.el"))
(defconfig sanity (load "~/.emacs.d/lisp/moons/sanity.el"))
(defconfig editing (load "~/.emacs.d/lisp/moons/editing.el"))
(defconfig spaceline (load "~/.emacs.d/lisp/moons/spaceline.el"))
(defconfig irc (load "~/.emacs.d/lisp/moons/irc.el"))
(defconfig twitter (load "~/.emacs.d/lisp/moons/twitter.el"))
(defconfig git (load "~/.emacs.d/lisp/moons/git.el"))
(defconfig org (load "~/.emacs.d/lisp/moons/org.el"))
(defconfig targetprocess (load "~/.emacs.d/lisp/moons/targetprocess.el"))
(defconfig-base style (interactive) (load "~/.emacs.d/lisp/moons/style.el"))

;; todo: consider https://github.com/Bad-ptr/persp-mode.el
;; todo: consider https://scripter.co/accessing-devdocs-from-emacs/ instead of dashdocs

(provide 'theworld)

;;; theworld.el ends here
