;; -*- lexical-binding: t; -*-
;;; forest.el --- Take a walk through the parentheses.
;;; Commentary:
;;; 🌲🌲👺🌲🌲🌲
;;; Code:

(ns/defconfig elisp
  (ns/install-dashdoc "Emacs Lisp" 'emacs-lisp-mode-hook)

  ;; 'common-lisp-indent-function

  (setq lisp-indent-function 'common-lisp-indent-function)

  (ns/use helpful
    :config
    (global-set-key (kbd "C-h f") #'helpful-callable)
    (global-set-key (kbd "C-h v") #'helpful-variable)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (global-set-key (kbd "C-h i") #'counsel-info-lookup-symbol)

    ;; todo: make this work, figure out what goes to helpful-callable in interactive arg
    (defun! ns/helpful-or-dashdoc ()
      (if (eq 'emacs-lisp-mode major-mode)
        (helpful-callable "")
        (if ns/enable-dashdocs-p
          (ns/counsel-dash-word)
          (message "dash docs not enabled!"))))

    ;; todo: should this be cider/emacslisp apropros in the mix
    (ns/bind "nh" 'ns/helpful-or-dashdoc))

  (ns/use eros
    :config
    (setq eros-eval-result-duration 20)
    (eros-mode 1)

    (defun! ns/smart-elisp-eval ()
      (if (use-region-p)
        (eval-region (region-beginning) (region-end))
        (if (s-blank-p (s-trim (thing-at-point 'line)))
          (eros-eval-last-sexp nil)
          (eros-eval-defun nil))))

    (defun! ns/eval-and-replace ()
      "Replace the preceding sexp with its value."
      (backward-kill-sexp)
      (condition-case nil
        (prin1 (eval (read (current-kill 0)))
          (current-buffer))
        (error (message "Invalid expression")
          (insert (current-kill 0)))))

    (ns/bind-mode 'emacs-lisp "e" 'ns/smart-elisp-eval)
    (ns/bind-mode 'emacs-lisp "E" 'eval-print-last-sexp))

  (ns/use elsa
    :config
    (ns/use flycheck-elsa)
    (add-hook 'emacs-lisp-mode-hook #'flycheck-elsa-setup)

    ;; note: elsa needs cask to do anything:
    ;; (executable-find "cask")
    ))

(ns/defconfig flycheck
  (ns/use flycheck
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

  (ns/use flycheck-pos-tip)
  (eval-after-load 'flycheck '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(ns/defconfig company
  (ns/use company
    :config
    (setq-ns company
      idle-delay 0.5
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

  (ns/use company-quickhelp
    :init
    (company-quickhelp-mode 1)
    (setq company-quickhelp-delay 0.3)))

(ns/defconfig dashdocs
  (defmacro ns/install-dashdoc (docset mode-hook)
    "Install dash DOCSET if dashdocs enabled, add mode hook to narrow dash search targets."
    `(when (bound-and-true-p ns/enable-dashdocs-p)
       (when nil
         (message (format "Installing %s docset..." ,docset))
         (counsel-dash-install-docset (subst-char-in-string ?\s ?_ ,docset)))
       (add-hook ,mode-hook (fn (setq-local counsel-dash-docsets '(,docset))))))

  ;; (ns/guard ns/enable-home-p)
  (ns/guard nil)

  (ns/use dash)
  (ns/use counsel-dash)

  (setq-ns counsel-dash
    min-length 0
    docsets-path (~ ".local/share/Zeal/Zeal/docsets")
    browser-func 'ns/eww-browse-existing-or-new)

  (make-directory counsel-dash-docsets-path t)

  (defun! ns/counsel-dash-word ()
    (if (region-active-p)
      (counsel-dash (buffer-substring (region-beginning) (region-end)))
      (counsel-dash "")))

  ;; doesn't work, dired/switch
  ;; (ns/bind "nd" 'ns/counsel-dash-word)
  )

(ns/defconfig python
  (ns/install-dashdoc "Python 3" 'python-mode-hook)
  (ns/use elpy)
  (when (executable-find "pyflake")
    (ns/use flycheck-pyflakes)))

(ns/defconfig clojure
  (ns/use clojure-mode)
  (ns/use cider)

  ;; babashka
  (add-to-list 'interpreter-mode-alist '("bb" . clojure-mode))

  (setq cider-eval-result-duration 20)

  (ns/inmap 'cider-repl-mode-map (kbd "C-e") 'cider-repl-previous-input)
  (ns/inmap 'cider-repl-mode-map (kbd "C-n") 'cider-repl-next-input)

  ;; https://docs.cider.mx/cider/caveats.html#_injecting_dependencies_and_leiningen_pedantic_abort_mode

  ;; todo: peek and see if we set middleware in our profile, then disable in tha tcase
  (when ns/enable-work-p
    (setq cider-inject-dependencies-at-jack-in nil))
  ;; (setq cider-inject-dependencies-at-jack-in t)

  (setq cljr-suppress-middleware-warnings t)

  (ns/install-dashdoc "Clojure" 'clojure-mode-hook)

  (defun! ns/smart-cider-eval ()
    (if (use-region-p)
      (cider-eval-region (region-beginning) (region-end))
      (if (s-blank-p (s-trim (thing-at-point 'line)))
        (cider-eval-last-sexp nil)
        (cider-eval-defun-at-point nil))))

  (ns/bind-mode 'clojure
    "e" 'ns/smart-cider-eval
    "E" 'cider-eval-print-last-sexp)

  (add-to-list 'interpreter-mode-alist '("bb" . clojure-mode))
  (add-to-list 'interpreter-mode-alist '("joker" . clojure-mode))

  (when (executable-find "joker")
    (ns/use flycheck-joker :config (require 'flycheck-joker)))

  (when (executable-find "clj-kondo")
    (ns/use flycheck-clj-kondo
      :config
      (require 'flycheck-clj-kondo))))

(ns/defconfig nix
  (ns/use nix-mode))

(ns/defconfig music
  (ns/guard ns/enable-home-p)
  (ns/guard (executable-find "mpd"))

  (ns/use emms)

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

(ns/defconfig projectile
  (ns/use projectile)

  ;; still assuming git command, maybe lean on projectile for file listing
  (defun ns/get-project-files (project-root)
    (llet (default-directory (expand-file-name project-root)
            project-files-relative (s-split "\n"
                                     (s-replace (char-to-string ?\0) "\n"
                                       (shell-command-to-string
                                         counsel-git-cmd
                                         )) t))

      (-map (fn (concat default-directory <>)) project-files-relative)))

  (defun ns/all-project-files (open-buffers)
    (-flatten
      (-map 'ns/get-project-files
        (-remove (lambda (file) (not file))
          (-map 'projectile-root-bottom-up open-buffers)))))

  (defun ns/jump-file-candidates (&rest wants)
    (llet (sources
            (list
              ;; XXX these keys are used in dmenu_switcher
              :buffers-without-files
              (-filter (lambda (b) (not (buffer-file-name b)))
                (buffer-list))

              :buffers-with-files
              ;; remove nils
              (-remove 'not
                (-map 'buffer-file-name (buffer-list)))

              :project-files
              (-when-let (current-file-name (buffer-file-name (current-buffer)))
                (ns/all-project-files (list current-file-name)))

              ;; (ns/get-project-files (current-file))
              ;; (if ns/enable-linux-p (ns/all-project-files open-buffers) (ns/get-project-files (current-file)))

              :recentf recentf-list))

      (->> (or wants '(:recentf :project-files :buffers-with-files))
        (-map (-partial #'plist-get sources))
        (-flatten)
        (-uniq)

        ;; if a file is not remote, ensure it exists
        (-filter
          (lambda (f)
            (if (file-remote-p f) t
              (f-exists-p f)))))))

  (ns/bind
    "ne" (fn! (ivy-read "file: " (ns/jump-file-candidates)
                :action 'find-file))

    "nE" (fn! (ivy-read "project file: " (ns/jump-file-candidates :project-files)
                :action 'find-file))))

(ns/defconfig javascript
  ;; note: this is huge, takes a bit.
  (ns/install-dashdoc "JavaScript" 'web-mode-hook)

  (ns/comment
    (defun js-jsx-indent-line-align-closing-bracket ()
      "Workaround sgml-mode and align closing bracket with opening bracket"
      (save-excursion
        (beginning-of-line)
        (when (looking-at-p "^ +\/?> *$")
          (delete-char sgml-basic-offset))))

    (advice-add #'js-jsx-indent-line :after #'js-jsx-indent-line-align-closing-bracket))

  ;; use web-mode for .js files
  (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

  (ns/use rjsx-mode)
  (defun ns/webhook ()
    ;; (if (string-equal "tsx" (file-name-extension buffer-file-name))
    ;;   (if (equal web-mode-content-type "javascript")
    ;;     (progn
    ;;       (web-mode-set-content-type "jsx")
    ;;       (setup-tide-mode)))
    ;;   (message "now set to: %s" web-mode-content-type))
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode)))

  (ns/use web-mode
    :config
    (add-hook 'web-mode-hook 'ns/webhook))

  (ns/use prettier-js
    :config
    (add-hook 'typescript-mode-hook 'prettier-js-mode)
    (add-hook 'web-mode-hook 'prettier-js-mode)
    (add-hook 'js-mode-hook 'prettier-js-mode)))

(ns/defconfig typescript
  (ns/install-dashdoc "TypeScript" 'typescript-mode-hook)

  (ns/use tide
    :config
    (defun! setup-tide-mode ()
      (tide-setup)
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1))

    (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (flycheck-add-mode 'typescript-tslint 'web-mode)

    ;; (add-hook 'before-save-hook 'tide-format-before-save)
    ))

(ns/defconfig pdf
  (ns/guard ns/enable-linux-p)
  (ns/use pdf-tools))

(ns/defconfig terraform
  (ns/use terraform-mode))

(ns/defconfig autohotkey
  (ns/guard ns/enable-windows-p)
  (ns/use xahk-mode))

(ns/defconfig markdown
  ;; todo: file association
  (ns/use markdown-mode
    ;; :mode (("\\.md\\'" . markdown-mode)
    ;;         ("\\.markdown\\'" . markdown-mode))
    )

  (general-define-key
    :states '(normal)
    :keymaps 'markdown-mode-map
    (kbd "<tab>") 'markdown-cycle)

  (defun ns/style-markdown ()
    (require 'markdown-mode)
    (ns/set-faces-monospace '(markdown-code-face))

    (-map #'ns/set-buffer-face-variable
      (ns/buffers-by-mode 'markdown-mode)))

  (defun ns/markdown-mode-hook ()
    (ns/set-buffer-face-variable))

  (add-hook 'markdown-mode-hook 'ns/markdown-mode-hook))

(ns/defconfig restclient
  (ns/use restclient
    :config
    (ns/bind-leader-mode
      'restclient
      "ei" 'restclient-http-send-current-stay-in-window))

  (ns/use company-restclient))

(ns/defconfig sql
  ;; todo
  ;; (ns/install-dashdoc "SQLite" ')

  ;; setup: https://github.com/kostafey/ejc-sql#install-jdbc-drivers
  ;; (ns/use ejc-sql
  ;;   :config
  ;;   ;; test local sqlite
  ;; )
  )

(ns/defconfig latex
  (ns/guard ns/enable-home-p)
  (ns/install-dashdoc "LaTeX" 'latex-mode-hook)
  (ns/use company-auctex))

(ns/defconfig plantuml
  (ns/use plantuml)
  (ns/use flycheck-plantuml))

(ns/defconfig ledger
  (ns/guard ns/enable-home-p)
  (ns/use ledger-mode)
  (ns/use flycheck-ledger)
  (ns/use evil-ledger
    :config
    (add-hook 'ledger-mode-hook #'evil-ledger-mode)))

(ns/defconfig lsp
  ;; want to use eglot + flycheck, hrm
  ;; (ns/use cquery)
  )

(ns/defconfig search-engines
  (ns/use engine-mode
    :config
    ;; bind spc s 'hotkey' to a search url with a label
    (defmacro bind-search (hotkey label url)
      `(progn
         (defengine ,label ,url)
         (ns/bind
           (concat "s" ,hotkey)
           (intern (concat "engine/search-" (prin1-to-string ',label))))))

    ;; (bind-search "s" google "https://google.com/search?q=%s")
    (bind-search "m" melpa "https://melpa.org/#/?q=%s")
    (bind-search "o" stack-overflow "https://stackoverflow.com/search?q=%s")
    (bind-search "g" github "https://github.com/search?ref=simplesearch&q=%s")
    (bind-search "y" youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
    (engine-mode t))

  )

(ns/defconfig filehooks
  (ns/guard ns/enable-home-p)

  (setq ns/filename-cmd
    (list
      (~ ".Xresources") "xrdb -merge ~/.Xresources && pkill -x --signal USR1 xst"
      (~ ".Xmodmap") "xmodmap ~/.Xmodmap"
      ))

  ;; todo here:
  ;; if file exists, and is in the templates dir and not in list
  ;; and has hook -- add it to ns/filename-cmd
  (defun ns/cmd-after-saved-file ()
    "Execute a command after saved a specific file."
    (-map (-lambda ((file cmd))
            (when (equal (buffer-file-name) file)
              (ns/shell-exec-dontcare cmd)))
      (-partition 2 ns/filename-cmd)))

  (add-hook 'after-save-hook 'ns/cmd-after-saved-file))

(ns/defconfig emoji
  (ns/use emojify
    :init
    (setq emojify-emoji-styles '(unicode)) ; only real emoji here thanks

    :config
    ;; emojify-mode seems to mess with input, causing a character to
    ;; occasionally skip, so disabling (global-emojify-mode)

    (ns/bind
      "ie" 'emojify-insert-emoji
      "te" 'emojify-mode)))

(ns/defconfig powershell
  (ns/guard ns/enable-windows-p)
  (ns/use powershell))

(ns/defconfig c
  (ns/install-dashdoc "C" 'c-mode-hook)

  ;; note: depends on clang and cmake
  ;; (ns/use irony)
  ;; fyi:
  ;; (irony-install-server)

  (ns/use flycheck-irony)
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

  (defun! ns/recompile ()
    (let ((compilation-directory (projectile-project-root)))
      (ns/shell-exec-dontcare
        (format "cd '%s' && make clean" compilation-directory))
      (recompile))))

(ns/defconfig graphviz
  (ns/use graphviz-dot-mode
    :config
    (ns/bind-leader-mode 'graphviz-dot "," 'graphviz-dot-preview)
    (ns/bind-mode 'graphviz-dot "e" 'graphviz-dot-preview))

  ;; use dot in org mode
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((dot . t)))

  (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

  (defun ns/org-confirm-babel-evaluate (lang body)
    (not (string= lang "dot")))         ;; don't ask for dot

  (setq org-confirm-babel-evaluate 'ns/org-confirm-babel-evaluate)

  (defun! ns/refresh-images-org ()
    (org-toggle-inline-images)
    (org-toggle-inline-images))

  ;; todo: CcCc is pretty generic, ideally we only do this after eval'ing a dot source block.
  ;; <2020-12-24 Thu 20:08> cancelling this for now
  ;; (advice-add #'org-ctrl-c-ctrl-c :after #'ns/refresh-images-org)
  )

(ns/defconfig server
  (require 'server)

  (when ns/enable-windows-p
    (setq-ns server
      auth-dir (~e "server")
      name "emacs-server-file"))

  (when-not (server-running-p)
    (server-start)))

(ns/defconfig common-lisp
  (ns/use slime)
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

(ns/defconfig scripting
  (add-to-list 'interpreter-mode-alist '("elisp" . emacs-lisp-mode))

  ;; helper for unpacking args provided by elisp script as shebang
  ;; use: (ns/let-script-args (named named2) body)
  (defmacro ns/let-script-args (args &rest content)
    `(let (,@(mapcar
               (fn (list (nth <> args)
                     (nth <> ns-args)))
               (number-sequence 0 (- (length args) 1))))
       ,@content)))

(ns/defconfig go
  (ns/use go-mode))

;; big bois
;; having them listed like this gives ns/jump-config something to search for

(ns/defconfig blog        (load (~e "lisp/trees/blog.el")))
(ns/defconfig doomline    (load (~e "lisp/trees/doomline.el")))
(ns/defconfig editing     (load (~e "lisp/trees/editing.el")))
(ns/defconfig evil        (load (~e "lisp/trees/evil.el")))
(ns/defconfig follow-dwim (load (~e "lisp/trees/follow.el")))
(ns/defconfig git         (load (~e "lisp/trees/git.el")))
(ns/defconfig interface   (load (~e "lisp/trees/interface.el")))
(ns/defconfig irc         (ns/guard ns/enable-home-p) (load (~e "lisp/trees/irc.el")))
(ns/defconfig org         (load (~e "lisp/trees/org.el")))
(ns/defconfig org-capture (load (~e "lisp/trees/org-capture.el")))
(ns/defconfig org-pim     (load (~e "lisp/trees/org-pim.el")))
(ns/defconfig sanity      (load (~e "lisp/trees/sanity.el")))
(ns/defconfig shell       (load (~e "lisp/trees/shell.el")))
(ns/defconfig staging     (load (~e "lisp/trees/staging.el")))
(ns/defconfig style       (load (~e "lisp/trees/style.el")))
(ns/defconfig util        (load (~e "lisp/trees/util.el")))

(provide 'forest)

;;; forest.el ends here
